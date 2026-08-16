% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_clpm_coefficients}
\alias{morie_clpm_coefficients}
\title{morie_clpm_coefficients}
\usage{
morie_clpm_coefficients(X, Y)
}
\arguments{
\item{X}{See Usage.}
\item{Y}{See Usage.}
}
\description{
Part of the tmlcll_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{x_on_x}, \code{y_on_x}, \code{x_on_y}, \code{y_on_y}, \code{cross_lag_x_to_y}, \code{cross_lag_y_to_x}, \code{parametrization}, \code{caveat}.
}
\references{
----------
Hamaker, E. L., Kuiper, R. M. & Grasman, R. P. P. P. (2015) "A
critique of the cross-lagged panel model", Psychological Methods
20(1), 102-116, doi:10.1037/a0038889. The conflation of within- and
between-person variance in the traditional CLPM, and the
random-intercept parametrization that separates them.

Allison, P. D., Williams, R. & Moral-Benito, E. (2017) "Maximum
Likelihood for Cross-Lagged Panel Models with Fixed Effects",
Socius 3, 1-17, doi:10.1177/2378023117710578. Maximum likelihood
estimation of cross-lagged panel models with unit fixed effects.

van der Laan, M. J. & Rose, S. (2018) Targeted Learning in Data
Science, Springer, doi:10.1007/978-3-319-65304-4. Chap. 4: the
g-computation formula as iterated conditional expectations and the
sequential TMLE with the clever covariate, which is what identifies
an intervention contrast under time-varying confounding.

Note on provenance: the ledger previously cited this module to
"Allard-Boulet (2024)". No such paper could be located in any
database; the citation appears to be fabricated and has been
replaced with the three verifiable sources above.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
morie_clpm_coefficients(M, M)
}
