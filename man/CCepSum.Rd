% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CCepSum}
\alias{CCepSum}
\title{Eq (4.66): y_hat = x_hat + h_hat.  The residual is not exactly zero}
\usage{
CCepSum(x, h) 
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{h}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
because the cepstrum is of infinite duration (eq 4.73) and the DFT truncates it; the size of the residual is the useful number.
}
\value{
A list with \code{y}, \code{cepstrum_y}, \code{cepstrum_x}, \code{cepstrum_h}, \code{residual}, \code{max_residual}, \code{relative_residual}, \code{truncation_note}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
CCepSum(V, V)
}
