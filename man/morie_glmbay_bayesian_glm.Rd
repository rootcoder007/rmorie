% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_glmbay_bayesian_glm}
\alias{morie_glmbay_bayesian_glm}
\title{morie_glmbay_bayesian_glm}
\usage{
morie_glmbay_bayesian_glm(X, y, family = "binomial", prior_sd = 2.5, add_intercept = TRUE, max_iter = 100, tol = 1e-10)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{family}{Passed to \code{.glmbay_links}. Defaults to \code{"binomial"}.}
\item{prior_sd}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{2.5}.}
\item{add_intercept}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{100}.}
\item{tol}{Passed to \code{<}. Defaults to \code{1e-10}.}
}
\description{
A step of the glmbay_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{coefficients}, \code{posterior_sd}, \code{std_error}, \code{ci_lower}, \code{ci_upper}, \code{fitted}, \code{linear_predictor}, \code{loglik}, \code{log_prior}, \code{log_marginal}, \code{log_det_hessian}, \code{iterations}, \code{converged}, \code{family}, \code{prior_sd}, \code{n}, \code{p}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_glmbay_bayesian_glm(V, V)
}
