% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PCepSum}
\alias{PCepSum}
\title{Eq (4.82): the power cepstra of a convolution add, exactly when the}
\usage{
PCepSum(x, h, square = FALSE)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{h}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{square}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
squaring of eq (4.81) is omitted.  square defaults to FALSE here for that reason; TRUE reproduces the book\'s definition and shows how large the neglected cross-term is on the caller\'s own data.
}
\value{
A list with \code{y}, \code{cepstrum_y}, \code{cepstrum_x}, \code{cepstrum_h}, \code{residual}, \code{max_residual}, \code{relative_residual}, \code{squared}, \code{exact}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
PCepSum(V, V)
}
