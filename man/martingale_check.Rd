% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{martingale_check}
\alias{martingale_check}
\title{martingale_check}
\usage{
martingale_check(D, past, tol = 0.2)
}
\arguments{
\item{D}{See Usage.}
\item{past}{See Usage.}
\item{tol}{Defaults to \code{0.2}.}
}
\description{
Part of the tlonts_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{correlation}, \code{is_martingale}, \code{note}.
}
\references{
----------
van der Laan, M. J. & Rose, S. (2018) Targeted Learning in Data
Science, Springer, doi:10.1007/978-3-319-65304-4. Chap. 19 (van der
Laan, Chambaz & Lendle): a time series in which one observes in
chronological order a covariate vector, a treatment and an outcome;
the conditional distribution given the past depending on the past
through a fixed-dimensional summary measure, described by a
time-invariant mechanism in a model space that may be unspecified; a
compatible causal model with a family of causal effects defined by
STOCHASTIC INTERVENTIONS on a subset of the treatment nodes on a
future outcome, and their identifiability from the observed data
distribution; and the observation that empty summary measures recover
i.i.d. targeted learning, parametric conditional densities recover
classical time series models, and group sequential adaptive designs
are included.

Chambaz, A., Zheng, W. & van der Laan, M. J. (2017) "Targeted
sequential design for targeted learning inference of the optimal
treatment rule and its mean reward", Annals of Statistics 45(6),
2537-2564, doi:10.1214/16-AOS1534.

van der Laan, M. J., Rose, S. & Lendle, S. (2018) "Online Targeted
Learning for Time Series", in Targeted Learning in Data Science,
Springer, doi:10.1007/978-3-319-65304-4_19.
}
\keyword{internal}
