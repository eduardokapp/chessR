#' @title Set board to initial state
#' @description Given a board matrix, set it to chess initial state.
#' @param board A 12 by 64 numeric matrix.
#' @importFrom BMS hex2bin
start_board <- function(board) {
    # White Pawns = row 1
    # White Rooks = row 2
    # White Knights = row 3
    # White Bishops = row 4
    # White Queen = row 5
    # White King = row 6
    # Black Pawns = row 7
    # Black Rooks = row 8
    # Black Knights = row 9
    # Black Bishops = row 10
    # Black Queen = row 11
    # Black King = row 12

    # Set Pawns
    board[1, ] <-  hex2bin("000000000000ff00")
    board[7, ] <-  hex2bin("00ff000000000000")

    # Set Rooks
    board[2, ] <-  hex2bin("0000000000000081")
    board[8, ] <-  hex2bin("8100000000000000")

    # Set Knights
    board[3, ] <-  hex2bin("0000000000000042")
    board[9, ] <-  hex2bin("4200000000000000")

    # Set Bishops
    board[4, ] <-  hex2bin("0000000000000024")
    board[10, ] <- hex2bin("2400000000000000")

    # Set Queens
    board[5, ] <-  hex2bin("0000000000000008")
    board[11, ] <- hex2bin("0800000000000000")

    # Set Kings
    board[6, ] <-  hex2bin("0000000000000010")
    board[12, ] <- hex2bin("1000000000000000")

    return(board)
}
