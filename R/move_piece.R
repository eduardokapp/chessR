#' @title Move a Piece
#' @description Given a board state and a piece location and its desired new
#' location, updates the board state. Note that if the piece is not found in
#' its start_square, the board is not updated.
#' @param board A 12 by 64 board binary matrix.
#' @param start_square A valid square name, from "a1" to "h8", containing the
#' piece to-be-moved current location.
#' @param end_square The A valid square name, from "a1" to "h8", containing the
#' piece to-be-moved desired new location.
#' @returns An updated board if the move is valid, or the same board if not.
#' @author Eduardo Kapp
move_piece <- function(board, start_square, end_square) {
    start_square <- square_to_bits(start_square)
    end_square <- square_to_bits(end_square)

    piece_type <- NA
    # Find piece type and colors
    for (row in 1:12) {
        tmp <- bitwAnd(start_square, board[row, ])
        if (any(as.logical(tmp))) {
            piece_type <- row
            break
        }
    }

    if (is.na(piece_type))
        return(board)

    # Now erase piece from starting square
    board[piece_type, ] <- bitwXor(board[piece_type, ], start_square)

    # Place piece into new square
    board[piece_type, ] <- bitwOr(board[piece_type], end_square)

    return(board)
}
