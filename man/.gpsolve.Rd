% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gpsolve}
\alias{.gpsolve}
\title{Solve A x = b; rank-deficient systems fall back on the minimum-norm}
\usage{
.gpsolve(A, b)
}
\arguments{
\item{A}{A matrix; passed to \code{solve}.}
\item{b}{A matrix; passed to \code{\%*\%}.}
}
\description{
pseudo-inverse solution, as the Python arm does.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
