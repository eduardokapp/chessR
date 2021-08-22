#' @title Define Empty Board
#' @description A chess board is represented as a 12 by 64 binary matrix. Each
#' row represents a piece type with a color. There are 6 piece types, so
#' to represent whites and blacks, we use 12 rows. Each row has 64 columns,
#' so that every slot in the 8 by 8 chessboard is represented.
#' @author Eduardo Kapp
#' @returns A 12 by 64 numeric matrix filled with zeros.
create_empty_board <- function() {
    return(matrix(data = 0, nrow = 12, ncol = 64))
}
