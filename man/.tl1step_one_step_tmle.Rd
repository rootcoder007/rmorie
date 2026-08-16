% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tl1step_one_step_tmle}
\alias{.tl1step_one_step_tmle}
\title{Move once along the universal path to where the score vanishes}
\usage{
.tl1step_one_step_tmle(Q, H_fn, Y, eps_max = 3, steps = 600)
}
\arguments{
\item{Q}{See Usage.}
\item{H_fn}{See Usage.}
\item{Y}{See Usage.}
\item{eps_max}{Defaults to \code{3}.}
\item{steps}{Defaults to \code{600}.}
}
\description{
Part of the tl1step_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{psi}, \code{epsilon}, \code{Q_star}, \code{abs_score}, \code{iterations}, \code{path_steps}, \code{method}, \code{note}.
}
\references{
----------
van der Laan, M. J. & Rose, S. (2018) *Targeted Learning in Data
Science*, Springer, doi:10.1007/978-3-319-65304-4. Chap. 5
(one-dimensional universal least favorable parametric submodels for
univariate, multivariate and infinite-dimensional target parameters;
the definition by which the score at every epsilon equals the
canonical gradient; the resulting one-step TMLE solving the efficient
influence curve equation without iteration; the reading of the
universal submodel as a shortest path achieving the desired bias
reduction; the argument that the iterative TMLE can be unstable when
the data provide sparse information about the target; and the worked
treatment-specific survival example).

van der Laan, M. J. & Gruber, S. (2016) "One-step targeted minimum
loss-based estimation based on universal least favorable
one-dimensional submodels", *International Journal of Biostatistics*
12(1), 351-378, doi:10.1515/ijb-2015-0054. The construction this
chapter relies on.
}
\keyword{internal}
