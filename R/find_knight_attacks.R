#' @title Find set of knight attacks
#' @description Given a board state, a side and the knight position, return
#' every attacked squares by that knight.
#' @param board A 12 by 64 binary matrix representing the game current status.
#' @param whites If TRUE, the piece is a white piece, else blacks.
#' @param square A character representing a square ("a1" to "h8").
#' @returns A bitboard with all attacked squared by the Knight.
#' @author Eduardo Kapp
find_knight_attacks <- function(board, whites, square) {
    # First, find the queen bitboard according to the side
    if (whites)
        knight_board <- board[3, ]
    else
        knight_board <- board[9, ]

    # Check if the square is valid
    tmp_knight_board <- bitwAnd(knight_board, square_to_bits(square))
    if (!any(as.logical(tmp_knight_board))) {
        return(NA)
    }
    moves <- matrix(data = tmp_knight_board, nrow = 8, ncol = 8)
    idx <- which(moves == 1, arr.ind = TRUE)
    rank <- idx[1]
    file <- idx[2]

    if (whites) {
        ally_pieces <- get_occupied_squares(board[1:6, ])
    } else {
        ally_pieces <- get_occupied_squares(board[7:12, ])
    }

    # Note that a Knight can't be blocked.
    # So we simply need to find all squares it can go to, given its
    # position on the board and Xor with the allies positions
    legal_moves <- matrix(data = 0, nrow = 8, ncol = 8)

    # These are all the possible combinations a knight can go from anywhere
    possib_moves <- data.frame(
        rank = c(1, 2,  1,  2, -1, -2, -1, -2),
        file = c(2, 1, -2, -1, -2, -1,  2,  1)
    )

    # Now we add the combination with the current rank and file it occupies
    possib_moves$rank <- possib_moves$rank + rank
    possib_moves$file <- possib_moves$file + file

    # Filter out moves that go out of bounds
    legal_files <- possib_moves$file %in% 1:8
    legal_ranks <- possib_moves$rank %in% 1:8
    possib_moves <- possib_moves[legal_files & legal_ranks, ]

    # Set legal squares to 1 (still pseudo-legal here)
    legal_moves[as.matrix(possib_moves)] <- 1

    # Apply AND to ally pieces with the possible legal moves so we find where
    # are the intersections
    ally_pieces <- bitwAnd(ally_pieces, as.vector(legal_moves))

    # Now XOR result intersections with the possible moves
    # so that we only accept legal moves with no intersections
    legal_moves <- bitwXor(as.vector(legal_moves), ally_pieces)
    return(legal_moves)
}
