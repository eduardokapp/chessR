#' @title Print board to user
#' @description Given a board matrix, convert it to a user-friendly 8x8 matrix
#' with every slot represented with piece names.
#' @param board A 12 by 64 numeric matrix.
#' @param single_bitboard If TRUE, treats 'board' as if it is only a single
#' board layer, which is a bitboard representing one of the 12 piece
#' possible variations.
#' @returns A friendly visualization of the board.
#' @author Eduardo Kapp
print_board <- function(board, single_bitboard = FALSE) {
    if (single_bitboard) {
        out <- matrix(data = board, nrow = 8, ncol = 8, byrow = TRUE)
        out <- out[, 8:1]
        rownames(out) <- 8:1
        colnames(out) <- letters[1:8]
        return(out)
    }

    # Differentiate pieces
    board <- board * 1:nrow(board)

    # Reshape 12x64 to 1x64, by selecting non zero values in every column
    board <- colSums(board)

    # Transform 0:12 to piece names
    board <- vapply(board, lookup_piece_name, FUN.VALUE = "char")

    # Reshape 1x64 to 8x8
    out <- matrix(board, nrow = 8, ncol = 8, byrow = TRUE)
    out <- out[, 8:1]
    rownames(out) <- 8:1
    colnames(out) <- letters[1:8]
    return(out)
}
