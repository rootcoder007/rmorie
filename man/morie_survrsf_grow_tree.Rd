% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_survrsf_grow_tree}
\alias{morie_survrsf_grow_tree}
\title{morie_survrsf_grow_tree}
\usage{
morie_survrsf_grow_tree(X, times, events, mtry = NULL, min_deaths = 3, rule = "logrank", seed = 0, rng = NULL)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{times}{A vector; its length is taken and its elements indexed.}
\item{events}{A vector; indexed elementwise.}
\item{mtry}{Defaults to \code{NULL}.}
\item{min_deaths}{Numeric; combined arithmetically in the body. Defaults to \code{3}.}
\item{rule}{Passed to \code{.survrsf_check_rule}. Defaults to \code{"logrank"}.}
\item{seed}{Passed to \code{.survrsf_rng}. Defaults to \code{0}.}
\item{rng}{Optional; may be \code{NULL}. A list; the body reads \code{$sample_} from it.}
}
\description{
A step of the survrsf_native implementation. Called by \code{morie_survrsf_forest}. See the file header for the source the module follows.
}
\value{
A list with \code{root}, \code{rule}, \code{mtry}, \code{min_deaths}, \code{n}.
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
morie_survrsf_grow_tree(X = c(1, 2, 3, 4, 5, 6, 7, 8), times = c(1, 2, 3, 4, 5, 6, 7, 8), events = c(1, 2, 3, 4, 5, 6, 7, 8))
}
