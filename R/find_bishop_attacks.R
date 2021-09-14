#' @title Find set of bishop attacks
#' @description Given a board state, a side and the bishop position, return
#' every attacked squares by that bishop.
#' @param board A 12 by 64 binary matrix representing the game current status.
#' @param whites If TRUE, the piece is a white piece, else blacks.
#' @param square A character representing a square ("a1" to "h8").
#' @param allies Bitboard with ally positions
#' @param enemies Bitboard with enemy positions
#' @param queen Internal parameter to be used when calculating queen moves.
#' If TRUE, ignores the fact that no bishop exists in that position.
#' @returns A bitboard with all attacked squares.
#' @author Eduardo Kapp
find_bishop_attacks <- function(
    board,
    whites,
    square,
    allies,
    enemies,
    queen = FALSE
) {
    # First, find the bishop bitboard according to the side
    if (whites)
        bishop_board <- board[4, ]
    else
        bishop_board <- board[10, ]

    # Select the bishop which is in the desired square using AND operator
    tmp_bishop_board <- bitwAnd(bishop_board, square_to_bits(square))
    if (!any(as.logical(tmp_bishop_board))) {
        if (queen)
            tmp_bishop_board <- square_to_bits(square)
        else
            return(NA)
    }

    # Determine all pseudo-legal moves for that bishop
    # convert the bishop bitboard
    moves <- matrix(data = tmp_bishop_board, nrow = 8, ncol = 8)
    idx <- which(moves == 1, arr.ind = TRUE)

    # Now we need to find any blocking pieces (including the bishop itself!)
    # We'll start with ally pieces
    allies <- matrix(data = allies, nrow = 8, ncol = 8)
    enemies <- matrix(data = enemies, nrow = 8, ncol = 8)

    # At last, we can define the longest non-zero paths on all four diagonals
    # Starting out from the bishop square position:
    # For every direction, we'll keep going until we find an ally or an enemy
    legal_moves <- matrix(data = 0, nrow = 8, ncol = 8)

    # north-east
    rank <- idx[1] + 1
    file <- idx[2] + 1
    while (all(c(rank, file) %in% (1:8))) {
        if (allies[rank, file] == 1)
            break
        legal_moves[rank, file] <- 1
        if (enemies[rank, file] == 1)
            break
        rank <- rank + 1
        file <- file + 1
    }

    # north-west
    rank <- idx[1] + 1
    file <- idx[2] - 1
    while (all(c(rank, file) %in% (1:8))) {
        if (allies[rank, file] == 1)
            break
        legal_moves[rank, file] <- 1
        if (enemies[rank, file] == 1)
            break
        rank <- rank + 1
        file <- file - 1
    }

    # south-west
    rank <- idx[1] - 1
    file <- idx[2] - 1
    while (all(c(rank, file) %in% (1:8))) {
        if (allies[rank, file] == 1)
            break
        legal_moves[rank, file] <- 1
        if (enemies[rank, file] == 1)
            break
        rank <- rank - 1
        file <- file - 1
    }

    # south-east
    rank <- idx[1] - 1
    file <- idx[2] + 1
    while (all(c(rank, file) %in% (1:8))) {
        if (allies[rank, file] == 1)
            break
        legal_moves[rank, file] <- 1
        if (enemies[rank, file] == 1)
            break
        rank <- rank - 1
        file <- file + 1
    }

    return(as.vector(legal_moves))
}
