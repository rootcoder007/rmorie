% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_ks_psmirnov}
\alias{.mor_ks_psmirnov}
\title{Exact P(D < d) for the two-sample statistic, no ties: the lattice}
\usage{
.mor_ks_psmirnov(d, n1, n2, two_sided = TRUE)
}
\arguments{
\item{d}{Numeric; combined arithmetically in the body.}
\item{n1}{A count; the body uses it as \code{seq_len(...)}.}
\item{n2}{A count; the body uses it as \code{seq_len(...)}.}
\item{two_sided}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
path recursion of R\\'s psmirnov2x.
}
\value{
The value of \code{[}.
}
\keyword{internal}
