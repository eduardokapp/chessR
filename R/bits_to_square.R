#' @title Find out which square is represented in a bitboard
#' @description Given a bitboard representation, find which square a piece is
#' occupying.
#' @param bitboard A bitboard representation, which is a 64 binary vector.
#' @param split If true, also returns the square's rank and file.
#' @returns A list with the square associated with the bitboard.
#' @author Eduardo Kapp
bits_to_square <- function(bitboard, split = FALSE) {
    if (sum(bitboard) != 1)
        return(NA)
    squares <- c(
        "h8", "g8", "f8", "e8", "d8", "c8", "b8", "a8",
        "h7", "g7", "f7", "e7", "d7", "c7", "b7", "a7",
        "h6", "g6", "f6", "e6", "d6", "c6", "b6", "a6",
        "h5", "g5", "f5", "e5", "d5", "c5", "b5", "a5",
        "h4", "g4", "f4", "e4", "d4", "c4", "b4", "a4",
        "h3", "g3", "f3", "e3", "d3", "c3", "b3", "a3",
        "h2", "g2", "f2", "e2", "d2", "c2", "b2", "a2",
        "h1", "g1", "f1", "e1", "d1", "c1", "b1", "a1"
    )
    # Select the square from the square list using the bitboard as
    # a logical index
    square <- squares[as.logical(bitboard)]
    if (split) {
        out <- unlist(strsplit(square, ""))
        file <- out[1]
        rank <- out[2]
        return(list(square = square, file = file, rank = rank))
    }
    return(list(square = square))
}
