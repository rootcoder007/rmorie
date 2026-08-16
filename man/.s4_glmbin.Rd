% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s4_glmbin}
\alias{.s4_glmbin}
\title{.s4_glmbin}
\usage{
.s4_glmbin(X, y, iters = 25L, ridge = 1e-08)
}
\arguments{
\item{X}{A matrix; passed to \code{nrow}.}
\item{y}{Numeric; combined arithmetically in the body.}
\item{iters}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{25L}.}
\item{ridge}{A matrix; passed to \code{diag}. Defaults to \code{1e-08}.}
}
\description{
A step of the s04core implementation. Called by \code{.s4_tmle}, \code{Hmstrn}, \code{Medmsm} and 19 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{beta}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.s4_glmbin(V, V)
}
