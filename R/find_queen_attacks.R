#' @title Find set of queen attacks
#' @description Given a board state, a side and the queen position, return
#' every attacked squares by that queen
#' @param board A 12 by 64 binary matrix representing the game current status.
#' @param whites If TRUE, the piece is a white piece, else blacks.
#' @param square A character representing a square ("a1" to "h8").
#' @param allies Bitboard with ally positions
#' @param enemies Bitboard with enemy positions
#' @returns A bitboard with all attacked squared by the Queen.
#' @author Eduardo Kapp
find_queen_attacks <- function(board, whites, square, allies, enemies) {
    # First, find the queen bitboard according to the side
    if (whites)
        queen_board <- board[5, ]
    else
        queen_board <- board[11, ]

    # Check if the square is valid
    tmp_queen_board <- bitwAnd(queen_board, square_to_bits(square))
    if (!any(as.logical(tmp_queen_board))) {
        return(NA)
    }

    legal_moves <- bitwOr(
        find_bishop_attacks(board, whites, square, allies, enemies, TRUE),
        find_rook_attacks(board, whites, square, allies, enemies, TRUE)
    )

    return(legal_moves)
}
