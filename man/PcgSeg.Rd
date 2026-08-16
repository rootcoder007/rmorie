% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PcgSeg}
\alias{PcgSeg}
\title{PcgSeg}
\usage{
PcgSeg(x, fs, window = NULL, step = NULL, order = 6, threshold = NULL)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{window}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{step}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{order}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{6}.}
\item{threshold}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the rangayyan_adapt implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{sem}, \code{sem_fixed_reference}, \code{times}, \code{boundaries}, \code{n_boundaries}, \code{threshold}, \code{median}, \code{mad}, \code{window}, \code{step}, \code{order}, \code{fs}, \code{reference_restarted_at_boundaries}, \code{robust_threshold}, \code{method}.
}
\keyword{internal}
