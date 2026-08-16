% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_as_empirical_variogram}
\alias{.schab_as_empirical_variogram}
\title{Accept the list the empirical estimator returns, or a plain matrix}
\usage{
.schab_as_empirical_variogram(ev)
}
\arguments{
\item{ev}{A list; the body reads \code{$counts}, \code{$gamma}, \code{$lag}, \code{$lags}, \code{$n_pairs} from it.}
}
\description{
Counts default to 1 so an unweighted table still fits; that makes the WLS weights degenerate to 1/(2 gamma^2), the right limiting form rather than a silent failure.
}
\value{
A list with \code{lags}, \code{gamma}, \code{counts}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.schab_as_empirical_variogram(M)
}
