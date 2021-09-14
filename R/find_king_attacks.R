#' @title Find set of king attacks
#' @description Given a board state, a side and the king position, return
#' every attacked squares by that king
#' @param board A 12 by 64 binary matrix representing the game current status.
#' @param whites If TRUE, the piece is a white piece, else blacks.
#' @param square A character representing a square ("a1" to "h8").
#' @param allies Bitboard with ally pieces
#' @returns A bitboard with all attacked squares by the king.
#' @author Eduardo Kapp
find_king_attacks <- function(board, whites, square, allies) {
    # First, find the king bitboard according to the side
    if (whites)
        king_board <- board[6, ]
    else
        king_board <- board[12, ]

    # Check if the square is valid
    tmp_king_board <- bitwAnd(king_board, square_to_bits(square))
    if (!any(as.logical(tmp_king_board))) {
        return(NA)
    }

    # Obtain the king's current rank and file.
    moves <- matrix(data = tmp_king_board, nrow = 8, ncol = 8)
    idx <- which(moves == 1, arr.ind = TRUE)
    rank <- idx[1]
    file <- idx[2]

    # A king can go any direction. That would be all combinations
    # between -1, 0, and 1 in ranks and files.
    possible_moves <- expand.grid(-1:1, -1:1)
    names(possible_moves) <- c("rank", "file")

    # Add those to the king's current location, then filter out any
    # out-of-bounds locations.
    possible_moves$rank <- possible_moves$rank + rank
    possible_moves$file <- possible_moves$file + file

    legal_files <- possible_moves$file %in% 1:8
    legal_ranks <- possible_moves$rank %in% 1:8
    # Now we have all pseudo-legal moves.
    pseudo_legal_coord <- possible_moves[legal_files & legal_ranks, ]

    pseudo_legal <- matrix(data = 0, nrow = 8, ncol = 8)
    pseudo_legal[as.matrix(pseudo_legal_coord)] <- 1

    # To derive legal moves, we need to Xor the pseudo-legal moves
    # with ally positions.
    allies <- bitwAnd(allies, as.vector(pseudo_legal))

    # Now XOR result intersections with the possible moves
    # so that we only accept legal moves with no intersections
    legal_moves <- bitwXor(as.vector(pseudo_legal), allies)

    # Castling is considered as a special function, treated elsewhere
    return(legal_moves)
}
