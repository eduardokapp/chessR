#' @title Get Occupied Squares
#' @description Return 1 for every square that has a piece in it.
#' @param board A 12 by 64 numeric matrix.
#' @returns A 1 by 64 bitboard where every 1 represent an occupied slot.
get_occupied_squares <- function(board) {
    return(colSums(board))
}
