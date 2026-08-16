% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_survrsf_c_index}
\alias{morie_survrsf_c_index}
\title{Harrell\\'s C by the paper\\'s four steps. predicted is a}
\usage{
morie_survrsf_c_index(times, events, predicted)
}
\arguments{
\item{times}{See Usage.}
\item{events}{See Usage.}
\item{predicted}{See Usage.}
}
\description{
worse-outcome score: larger means the case is expected to fail sooner.
}
\value{
A list with \code{c_index}, \code{concordance}, \code{permissible}, \code{prediction_error}.
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
