% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_pinv}
\alias{.morie_pinv}
\title{.morie_pinv}
\usage{
.morie_pinv(M, rcond = 1e-15)
}
\arguments{
\item{M}{See Usage.}
\item{rcond}{Numeric; combined arithmetically in the body. Defaults to \code{1e-15}.}
}
\description{
A step of the tail1_core implementation. Called by \code{.schab_disjunctive_kriging}, \code{t3ols}. See the file header for the source the module follows.
}
\value{
The value of \code{\%*\%}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_pinv(V)
}
