% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{super_learner}
\alias{super_learner}
\title{The Super Learner ---------------------------------------------}
\usage{
super_learner(y, X, library = NULL, n_folds = 10, meta = "nnls", binary = NULL, loss = "l2", ridge = 1e-08, honest_level_one = TRUE)
}
\arguments{
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{X}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
\item{library}{The body requires: flxipt: the library is empty.}
\item{n_folds}{Passed to \code{.flxipt_folds}. Defaults to \code{10}.}
\item{meta}{One of \code{"discrete"}, \code{"nnls"}. Defaults to \code{"nnls"}.}
\item{binary}{Optional; may be \code{NULL}. A flag; the body branches on it.}
\item{loss}{Compared against \code{"nll"}. Defaults to \code{"l2"}.}
\item{ridge}{Numeric; passed to \code{max}. Defaults to \code{1e-08}.}
\item{honest_level_one}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the flxipt_native implementation. Called by \code{flexible_iptw}. See the file header for the source the module follows.
}
\value{
A list with \code{fitted}, \code{estimate}, \code{weights}, \code{weight_vector}, \code{cv_risk}, \code{cv_risk_ensemble}, \code{best_candidate}, \code{best_candidate_risk}, \code{discrete_choice}, \code{level_one}, \code{candidate_fits}, \code{library}, \code{n}, \code{n_folds}, \code{meta}, \code{loss}, \code{binary}, \code{honest_level_one}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(1)
n <- 40
X <- matrix(rnorm(n * 2), n, 2)
y <- 1 + X[, 1] - 0.5 * X[, 2] + rnorm(n, 0, 0.3)
r <- super_learner(y, X, n_folds = 4)
str(r, max.level = 1)
}
