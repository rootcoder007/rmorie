% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_survrsf_vimp}
\alias{morie_survrsf_vimp}
\title{Sec. 7: random daughter assignment at splits on x. Keys are}
\usage{
morie_survrsf_vimp(fit, X, variables = NULL, seed = 1)
}
\arguments{
\item{fit}{A list; the body reads \code{$events}, \code{$times} from it.}
\item{X}{A matrix; passed to \code{ncol}.}
\item{variables}{Defaults to \code{NULL}.}
\item{seed}{Passed to \code{morie_survrsf_mortality}. Defaults to \code{1}.}
}
\description{
0-based variable indices to match the Python.
}
\value{
A list with \code{estimate}, \code{vimp}, \code{baseline_error}, \code{note}, \code{method}.
}
\references{
----------
Ishwaran, H., Kogalur, U. B., Blackstone, E. H. & Lauer, M. S.
(2008) "Random Survival Forests", Annals of Applied Statistics 2(3),
841-860, doi:10.1214/08-AOAS169.

Harrell, F. et al. (1982) "Evaluating the Yield of Medical Tests",
JAMA 247(18), 2543-2546, doi:10.1001/jama.1982.03320430047030.

Hothorn, T. & Lausen, B. (2003) "On the exact distribution of
maximally selected rank statistics", Computational Statistics & Data
Analysis 43(2), 121-137, doi:10.1016/S0167-9473(02)00225-6.

Ishwaran, H. & Kogalur, U. B. (2007) "Random Survival Forests for
R", R News 7(2), 25-31.
}
\keyword{internal}
