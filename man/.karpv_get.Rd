% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.karpv_get}
\alias{.karpv_get}
\title{.karpv_get}
\usage{
.karpv_get(node, path)
}
\arguments{
\item{node}{A list; the body reads \code{$args} from it.}
\item{path}{See Usage.}
}
\description{
A step of the karpV_native implementation. Called by \code{morie_karpV}. See the file header for the source the module follows.
}
\value{
The value of \code{node}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.karpv_get(D, V)
}
