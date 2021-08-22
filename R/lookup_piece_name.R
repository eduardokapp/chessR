#' @title Piece lookup table
#' @description A simple lookup function that takes any number from 0 to 12
#' and maps it to a piece name.
#' @param x A number from 0 to 12
#' @returns A character representing the piece name
lookup_piece_name <- function(x) {
    # 0 represents no piece
    if (!x %in% 1:12)
        return("")

    # Values greater than 6 are blacks, so we remove redundancy
    if (x > 6)
        x <- x - 6

    # Now look-up table
    pieces <- c("p", "r", "n", "b", "q", "k")
    return(pieces[x])
}
