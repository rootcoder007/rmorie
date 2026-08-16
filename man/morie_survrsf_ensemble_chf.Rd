% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_survrsf_ensemble_chf}
\alias{morie_survrsf_ensemble_chf}
\title{morie_survrsf_ensemble_chf}
\usage{
morie_survrsf_ensemble_chf(fit, X, t, oob = TRUE, random_variable = NULL, seed = 1)
}
\arguments{
\item{fit}{See Usage.}
\item{X}{See Usage.}
\item{t}{See Usage.}
\item{oob}{Defaults to \code{TRUE}.}
\item{random_variable}{Defaults to \code{NULL}.}
\item{seed}{Defaults to \code{1}.}
}
\description{
Part of the survrsf_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{out}, as built in the body.
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
\examples{
morie_survrsf_ensemble_chf(fit = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), X = c(1, 2, 3, 4, 5, 6, 7, 8), t = c(1, 2, 3, 4, 5, 6, 7, 8))
}
