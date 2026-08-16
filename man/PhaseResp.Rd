% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PhaseResp}
\alias{PhaseResp}
\title{PhaseResp}
\usage{
PhaseResp(b, a = NULL, fs = 1000, n_freqs = 512, unwrap = TRUE)
}
\arguments{
\item{b}{Passed to \code{FreqResp}.}
\item{a}{Passed to \code{FreqResp}.}
\item{fs}{Passed to \code{FreqResp}. Defaults to \code{1000}.}
\item{n_freqs}{Passed to \code{FreqResp}. Defaults to \code{512}.}
\item{unwrap}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the rangayyan_filt2 implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{f}, \code{phase}, \code{wrapped}, \code{unwrapped}, \code{unwrap}, \code{fs}, \code{defined}, \code{n_undefined}, \code{phase_undefined_where_the_response_vanishes}, \code{wrapping_is_an_arctangent_artifact}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
PhaseResp(V)
}
