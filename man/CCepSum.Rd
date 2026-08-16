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
\item{x}{See Usage.}
\item{h}{See Usage.}
}
\description{
because the cepstrum is of infinite duration (eq 4.73) and the DFT truncates it; the size of the residual is the useful number.
}
\value{
A list with \code{y}, \code{cepstrum_y}, \code{cepstrum_x}, \code{cepstrum_h}, \code{residual}, \code{max_residual}, \code{relative_residual}, \code{truncation_note}, \code{method}.
}
\keyword{internal}
