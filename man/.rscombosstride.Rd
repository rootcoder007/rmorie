% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rscombosstride}
\alias{.rscombosstride}
\title{Striding keeps the determinism -- both arms visit the same subsets in}
\usage{
.rscombosstride(n, k, want, max_walk = 5e+06)
}
\arguments{
\item{n}{See Usage.}
\item{k}{See Usage.}
\item{want}{See Usage.}
\item{max_walk}{Defaults to \code{5e+06}.}
}
\description{
the same order -- while spreading the seeds over the whole index range the way random draws would.
}
\value{
The value of \code{repeat}.
}
\keyword{internal}
