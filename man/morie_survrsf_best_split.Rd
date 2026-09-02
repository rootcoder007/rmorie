% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_survrsf_best_split}
\alias{morie_survrsf_best_split}
\title{morie_survrsf_best_split}
\usage{
morie_survrsf_best_split(X, times, events, features, min_deaths = 3, rule = "logrank", 
    rng = NULL) 
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{times}{A vector; its length is taken.}
\item{events}{A vector; indexed elementwise.}
\item{features}{See Usage.}
\item{min_deaths}{Passed to \code{<}. Defaults to \code{3}.}
\item{rule}{One of \code{"conserve"}, \code{"logrankrandom"}, \code{"logrankscore"}. Defaults to \code{"logrank"}.}
\item{rng}{Optional; may be \code{NULL}. A list; the body reads \code{$randint} from it.}
}
\description{
A step of the survrsf_native implementation. Called by \code{morie_survrsf_grow_tree}. See the file header for the source the module follows.
}
\value{
The value of \code{best}, as built in the body.
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
set.seed(1)
n <- 120
X <- matrix(rnorm(n * 3), n, 3)
times <- rexp(n, exp(0.3 * (X[, 1] - 0.5 * X[, 2])))
events <- rbinom(n, 1, 0.75)
sp <- morie_survrsf_best_split(X, times, events, features = 0:2,
                               min_deaths = 3)
is.list(sp)
}
