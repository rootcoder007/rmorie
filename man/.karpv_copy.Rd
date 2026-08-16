% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.karpv_copy}
\alias{.karpv_copy}
\title{.karpv_copy}
\usage{
.karpv_copy(node)
}
\arguments{
\item{node}{A list; the body reads \code{$args}, \code{$op}, \code{$term} from it.}
}
\description{
A step of the karpV_native implementation. Called by \code{.karpv_replace}, \code{morie_karpV}. See the file header for the source the module follows.
}
\value{
The value of \code{.karpv_fnode}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.karpv_copy(D)
}
