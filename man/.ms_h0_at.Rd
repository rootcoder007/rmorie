% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ms_h0_at}
\alias{.ms_h0_at}
\title{.ms_h0_at}
\usage{
.ms_h0_at(ut, H, t)
}
\arguments{
\item{ut}{Passed to \code{<=}.}
\item{H}{A vector; indexed elementwise.}
\item{t}{Iterated over elementwise, with \code{vapply}.}
}
\description{
A step of the survival_more implementation. Called by \code{Coxsnell}, \code{Martingale}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
\examples{
.ms_h0_at(ut = c(1, 2, 3, 4, 5, 6, 7, 8), H = 0.5, t = c(1, 2, 3, 4, 5, 6, 7, 8))
}
