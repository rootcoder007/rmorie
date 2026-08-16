% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_simple_kriging_variance}
\alias{.schab_simple_kriging_variance}
\title{Sigma^2_sk at every location, for the E[(Zc - Z)^2] = 2 sigma^2_sk}
\usage{
.schab_simple_kriging_variance(cov_all, n_obs)
}
\arguments{
\item{cov_all}{A matrix; indexed by row and column.}
\item{n_obs}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
identity that closes Sec. 7.2.2.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.schab_simple_kriging_variance(V, V)
}
