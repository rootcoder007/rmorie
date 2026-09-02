% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SeizCohere}
\alias{SeizCohere}
\title{SeizCohere}
\usage{
SeizCohere(channels, fs, window, step = NULL, bands = NULL, nperseg = NULL) 
}
\arguments{
\item{channels}{Iterated over elementwise, with \code{lapply}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{window}{Coerced to integer by the body, with \code{as.integer}.}
\item{step}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{bands}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
\item{nperseg}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the rangayyan_corr implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{times}, \code{coherence}, \code{bands}, \code{window}, \code{step}, \code{nperseg}, \code{n_windows}, \code{n_channels}, \code{sustained_criterion}, \code{method}.
}
\keyword{internal}
