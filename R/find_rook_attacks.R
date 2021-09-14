#' @title Find set of rook attacks
#' @description Given a board state, a side and the rook position, return
#' every attacked squares by that rook.
#' @param board A 12 by 64 binary matrix representing the game current status.
#' @param whites If TRUE, the piece is a white piece, else blacks.
#' @param square A character representing a square ("a1" to "h8").
#' @param allies Bitboard with ally positions
#' @param enemies Bitboard with enemy positions
#' @param queen Internal parameter to be used when calculating queen moves.
#' If TRUE, ignores the fact that no rook exists in that position.
#' @returns A bitboard with all attacked squares.
#' @author Eduardo Kapp
find_rook_attacks <- function(
    board,
    whites,
    square,
    allies,
    enemies,
    queen = FALSE
) {
    # First, find the rook bitboard according to the side
    if (whites)
        rook_board <- board[2, ]
    else
        rook_board <- board[8, ]

    # Select the rook which is in the desired square using AND operator
    tmp_rook_board <- bitwAnd(rook_board, square_to_bits(square))
    if (!any(as.logical(tmp_rook_board))) {
        if (queen)
            tmp_rook_board <- square_to_bits(square)
        else
            return(NA)
    }

    # Determine all pseudo-legal moves for that rook
    # convert the rook bitboard
    moves <- matrix(data = tmp_rook_board, nrow = 8, ncol = 8)
    idx <- which(moves == 1, arr.ind = TRUE)
    moves[idx[1], ] <- 1
    moves[, idx[2]] <- 1

    # Now we need to find any blocking pieces (including the rook itself!)
    # We'll start with ally pieces
    enemies <- matrix(data = enemies, nrow = 8, ncol = 8)

    # Use Xor to find out all places where there are allies and attack moves
    # so we can exclude them
    friendly_fire_moves <- bitwAnd(as.vector(moves), allies)

    # pseudo legal moves (not considering blocking paths)
    pl_moves <- bitwXor(as.vector(moves), friendly_fire_moves)
    pl_moves <- matrix(data = pl_moves, nrow = 8, ncol = 8)

    # At last, we can define the longest non-zero paths on all four directions
    legal_moves <- matrix(data = 0, nrow = 8, ncol = 8)

    # Starting out from the rook square position:
    # For every direction, we'll keep going through the pseudo-legal moves
    # paths and stop whenever there is an enemy encounter or when the pseudo
    # legal moves are over.

    # north
    rank <- idx[1] + 1
    file <- idx[2]
    while (rank >= 1 && rank <= 8) {
        if (pl_moves[rank, file] != 1)
            break
        legal_moves[rank, file] <- 1
        if (enemies[rank, file] == 1)
            break
        rank <- rank + 1
    }

    # south
    rank <- idx[1] -1
    file <- idx[2]
    while (rank >= 1 && rank <= 8) {
        if (pl_moves[rank, file] != 1)
            break
        legal_moves[rank, file] <- 1
        if (enemies[rank, file] == 1)
            break
        rank <- rank - 1
    }

    # east
    rank <- idx[1]
    file <- idx[2] + 1
    while (file >= 1 && file <= 8) {
        if (pl_moves[rank, file] != 1)
            break
        legal_moves[rank, file] <- 1
        if (enemies[rank, file] == 1)
            break
        file <- file + 1
    }

    # west
    rank <- idx[1]
    file <- idx[2] - 1
    while (file >= 1 && file <= 8) {
        if (pl_moves[rank, file] != 1)
            break
        legal_moves[rank, file] <- 1
        if (enemies[rank, file] == 1)
            break
        file <- file - 1
    }

    return(as.vector(legal_moves))
}
