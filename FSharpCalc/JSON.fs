module JSON

open FParsec

type Json = JString of string
            | JNumber of float
            | JBool   of bool
            | JNull
            | JList   of Json List
            | JObject of Map<string, Json>

type Parser<'t> = Parser<'t, unit>

let stringLiteral : Parser<_> =
    let escape =  anyOf "\"\\/bfnrt"
                  |>> function
                      | 'b' -> "\b"
                      | 'f' -> "\u000C"
                      | 'n' -> "\n"
                      | 'r' -> "\r"
                      | 't' -> "\t"
                      | c   -> string c // every other char is mapped to itself

    let unicodeEscape =
        pstring "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
            let hex2int c = (int c &&& 15) + (int c >>> 6)*9 // hex char to int
            (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
            |> char |> string
        )

    between (pstring "\"") (pstring "\"")
            (stringsSepBy (manySatisfy (fun c -> c <> '"' && c <> '\\'))
                          (pstring "\\" >>. (escape <|> unicodeEscape)))

let jnull : Parser<_> = stringReturn "null" JNull
let jbool : Parser<_> = (stringReturn "true" (JBool true)) <|> (stringReturn "false" (JBool false))
let jnumber : Parser<_> = pfloat |>> JNumber
let jstring : Parser<_> = stringLiteral |>> JString

// jvalue, jlist and jobject are three mutually recursive grammar productions.
// In order to break the cyclic dependency, we make jvalue a parser that
// forwards all calls to a parser in a reference cell.
let jvalue, jvalueRef = createParserForwardedToRef<Json, unit>() // initially jvalueRef holds a reference to a dummy parser
let ws = spaces

let listBetweenStrings sOpen sClose parseElement targetFun =
    between (pstring sOpen) (pstring sClose) (ws >>. sepBy (parseElement .>> ws) (pstring "," >>. ws) |>> targetFun)

let keyValue = tuple2 stringLiteral (ws >>. pstring ":" >>. ws >>. jvalue)

let jlist = listBetweenStrings "[" "]" jvalue JList
let jobject = listBetweenStrings "{" "}" keyValue (Map.ofList >> JObject)

do jvalueRef := choice [jobject
                        jlist
                        jstring
                        jnumber
                        jbool
                        jnull]

let json = ws >>. jvalue .>> ws .>> eof

let parseJsonString str = run json str
