% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_glm}
\alias{morie_glm}
\title{morie_glm}
\usage{
morie_glm(y, X, family = "binomial", add_intercept = TRUE, weights = NULL, offset = NULL, max_iter = 25L, tol = 1e-08)
}
\arguments{
\item{y}{A vector; its length is taken.}
\item{X}{A matrix; passed to \code{nrow}.}
\item{family}{Coerced to character by the body, with \code{as.character}. Defaults to \code{"binomial"}.}
\item{add_intercept}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{weights}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{offset}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{25L}.}
\item{tol}{Defaults to \code{1e-08}.}
}
\description{
A step of the glm implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{coef}, \code{se}, \code{statistic}, \code{statistic_name}, \code{p_value}, \code{fitted}, \code{linear_predictor}, \code{residuals}, \code{deviance}, \code{null_deviance}, \code{df_residual}, \code{df_null}, \code{dispersion}, \code{pearson_chi2}, \code{aic}, \code{loglik}, \code{converged}, \code{family}, \code{n}, \code{k}, \code{vcov}, \code{method}.
}
\keyword{internal}
\examples{
n <- 40
x1 <- sapply(0:(n - 1), function(i) ((i * 7)\%\%11)/5 - 1)
x2 <- sapply(0:(n - 1), function(i) ((i * 5)\%\%7)/3 - 1)
X <- cbind(x1, x2)
yb <- c(0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 
    0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 
    0, 1, 1)
morie_glm(yb, X, "binomial")
}
