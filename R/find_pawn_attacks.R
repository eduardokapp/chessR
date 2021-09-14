#' @title Find set of pawn attacks
#' @description Given a board state, a side and the pawn position, return
#' every attacked squares by that pawn.
#' @param board A 12 by 64 binary matrix representing the game current status.
#' @param whites If TRUE, the piece is a white piece, else blacks.
#' @param square A character representing a square ("a1" to "h8").
#' @param allies Bitboard with ally positions
#' @returns A bitboard with all attacked squares by the pawn
#' @author Eduardo Kapp
find_pawn_attacks <- function(board, whites, square, allies) {
    # First, find the pawn bitboard according to the side
    if (whites)
        pawn_board <- board[1, ]
    else
        pawn_board <- board[7, ]

    # Check if the square is valid
    tmp_pawn_board <- bitwAnd(pawn_board, square_to_bits(square))
    if (!any(as.logical(tmp_pawn_board))) {
        return(NA)
    }

    # Obtain the pawn's current rank and file.
    moves <- matrix(data = tmp_pawn_board, nrow = 8, ncol = 8)
    idx <- which(moves == 1, arr.ind = TRUE)
    # Here, direction matters, so remember that our bitboard is flipped
    rank <- idx[2]
    file <- idx[1]

    # A pawn can only attack its upper diagonals.
    pseudo_legal <- matrix(data = 0, nrow = 8, ncol = 8)
    rank <- ifelse(whites, rank - 1, rank + 1)
    pseudo_legal[file + 1, rank] <- 1
    pseudo_legal[file - 1, rank] <- 1

    # To derive legal moves, we need to Xor the pseudo-legal moves
    # with ally positions.
    allies <- bitwAnd(allies, as.vector(pseudo_legal))

    # Now XOR result intersections with the possible moves
    # so that we only accept legal moves with no intersections
    legal_moves <- bitwXor(as.vector(pseudo_legal), allies)

    # Castling is considered as a special function, treated elsewhere
    return(legal_moves)
}
