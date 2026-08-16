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
\item{b}{See Usage.}
\item{a}{Defaults to \code{NULL}.}
\item{fs}{Defaults to \code{1000}.}
\item{n_freqs}{Defaults to \code{512}.}
\item{unwrap}{Defaults to \code{TRUE}.}
}
\description{
Part of the rangayyan_filt2 implementation; see the file header for the source it follows.
}
\value{
A list with \code{f}, \code{phase}, \code{wrapped}, \code{unwrapped}, \code{unwrap}, \code{fs}, \code{defined}, \code{n_undefined}, \code{phase_undefined_where_the_response_vanishes}, \code{wrapping_is_an_arctangent_artifact}, \code{method}.
}
\keyword{internal}
