#' @title Print board to user
#' @description Given a board matrix, convert it to a user-friendly 8x8 matrix
#' with every slot represented with piece names.
#' @param board A 12 by 64 numeric matrix.
print_board <- function(board) {
    # Differentiate pieces
    board <- board * 1:nrow(board)

    # Reshape 12x64 to 1x64, by selecting non zero values in every column
    board <- colSums(board)

    # Transform 0:12 to piece names
    board <- vapply(board, lookup_piece_name, FUN.VALUE = "char")

    # Reshape 1x64 to 8x8
    return(matrix(board, nrow = 8, ncol = 8, byrow = TRUE))
}
