% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.limmav_log_cpm}
\alias{.limmav_log_cpm}
\title{.limmav_log_cpm}
\usage{
.limmav_log_cpm(counts, lib_sizes = NULL, prior_count = 0.5, lib_offset = 1)
}
\arguments{
\item{counts}{A matrix; passed to \code{nrow}.}
\item{lib_sizes}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{prior_count}{Numeric; combined arithmetically in the body. Defaults to \code{0.5}.}
\item{lib_offset}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
}
\description{
A step of the limmav_native implementation. Called by \code{.limmav_voom_weights}. See the file header for the source the module follows.
}
\value{
A list with \code{y}, \code{R}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.limmav_log_cpm(V)
}
