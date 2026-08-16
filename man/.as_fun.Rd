% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.as_fun}
\alias{.as_fun}
\title{.as_fun}
\usage{
.as_fun(spec)
}
\arguments{
\item{spec}{A list; the body reads \code{$affine}, \code{$f}, \code{$grad}, \code{$hess} from it.}
}
\description{
A step of the barerp_native implementation. Called by \code{barrier_method}, \code{central_point}. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.as_fun(D)
}
