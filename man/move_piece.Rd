% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/move_piece.R
\name{move_piece}
\alias{move_piece}
\title{Move a Piece}
\usage{
move_piece(board, start_square, end_square)
}
\arguments{
\item{board}{A 12 by 64 board binary matrix.}

\item{start_square}{A valid square name, from "a1" to "h8", containing the
piece to-be-moved current location.}

\item{end_square}{The A valid square name, from "a1" to "h8", containing the
piece to-be-moved desired new location.}
}
\value{
An updated board if the move is valid, or the same board if not.
}
\description{
Given a board state and a piece location and its desired new
location, updates the board state. Note that if the piece is not found in
its start_square, the board is not updated.
}
\author{
Eduardo Kapp
}
