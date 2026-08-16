% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dreamr_pack}
\alias{.dreamr_pack}
\title{.dreamr_pack}
\usage{
.dreamr_pack(vals, name)
}
\arguments{
\item{vals}{A vector; its length is taken.}
\item{name}{See Usage.}
}
\description{
A step of the dreamr_native implementation. Called by \code{morie_dreamr_lambda_return}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{returns}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.dreamr_pack(V, V)
}
