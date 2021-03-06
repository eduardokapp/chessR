% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_rook_attacks.R
\name{find_rook_attacks}
\alias{find_rook_attacks}
\title{Find set of rook attacks}
\usage{
find_rook_attacks(board, whites, square, queen = FALSE)
}
\arguments{
\item{board}{A 12 by 64 binary matrix representing the game current status.}

\item{whites}{If TRUE, the piece is a white piece, else blacks.}

\item{square}{A character representing a square ("a1" to "h8").}

\item{queen}{Internal parameter to be used when calculating queen moves.
If TRUE, ignores the fact that no rook exists in that position.}
}
\value{
A bitboard with all attacked squares.
}
\description{
Given a board state, a side and the rook position, return
every attacked squares by that rook.
}
\author{
Eduardo Kapp
}
