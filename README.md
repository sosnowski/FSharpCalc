Command Line Calculator in F#
==============

### Usage
```
FSharCalc.exe "2 + 2 * (15 / 5) - 2"
FSharCalc.exe "(123 + 321 / 23) - 2"

etc.
```

Internally it uses **FParsec** to parse input string and identify numbers and operators.

Infix notation is then translated to [**Revers Polish Notation**](https://en.wikipedia.org/wiki/Reverse_Polish_notation) using [**Dijkstra's Shunting-yard**](https://en.wikipedia.org/wiki/Shunting-yard_algorithm) algorithm.

Created RPN stack is then evaluated to retrieve the final result.

Unit tests included :)
