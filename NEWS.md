# kappchess 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Added functions to convert bitboard to square representation and vice-versa.
* Added basic board initialization functions

# kappchess 0.1.1

* Added a rook attack set calculator
* Added a bishop attack set calculator
* Combined rook and bishop sets to define the Queen attack set calculator
* print_board function now also prints a single bitboard
* print_board function is prettier, returning ranks and files

# kappchess 0.1.2

* Added a knight attack set calculator
* Making print_board also return rank and files of individual bitboards
* Added queen attack set calculator
* Added pawn attack set calculator
* Now attack set calculators receive ally and enemy bitboards as inputs to increase performance

