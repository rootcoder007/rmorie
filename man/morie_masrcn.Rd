% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_masrcn}
\alias{morie_masrcn}
\title{morie_masrcn}
\usage{
morie_masrcn(op, ...) 
}
\arguments{
\item{op}{A vector; its length is taken.}
\item{...}{Passed through.}
}
\description{
A step of the masrcn_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{switch}.
}
\keyword{internal}
\examples{
features <- matrix(as.numeric(1:36), 6, 6)
r <- morie_masrcn("roi_pool", features, box = c(0, 0, 4, 4),
                  out_size = 2L)
is.list(r)
}
