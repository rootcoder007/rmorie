% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.flxipt_lstsq}
\alias{.flxipt_lstsq}
\title{Internal lstsq with optional ridge ----------------------------}
\usage{
.flxipt_lstsq(X, y, ridge = 1e-10)
}
\arguments{
\item{X}{A matrix; passed to \code{ncol}.}
\item{y}{A matrix; passed to \code{\%*\%}.}
\item{ridge}{A matrix; passed to \code{diag}. Defaults to \code{1e-10}.}
}
\description{
A step of the flxipt_native implementation. Called by \code{.flxipt_fit}, \code{super_learner}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.flxipt_lstsq(V, V)
}
