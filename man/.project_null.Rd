% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.project_null}
\alias{.project_null}
\title{.project_null}
\usage{
.project_null(v, aeq)
}
\arguments{
\item{v}{A matrix; passed to \code{\%*\%}.}
\item{aeq}{A matrix; passed to \code{t}.}
}
\description{
A step of the barerp_native implementation. Called by \code{central_point}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
S <- c("a", "b", "c")
.project_null(S, M)
}
