% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_glm_predict}
\alias{morie_glm_predict}
\title{morie_glm_predict}
\usage{
morie_glm_predict(fit, X, add_intercept = TRUE, type = c("response", "link"), offset = NULL) 
}
\arguments{
\item{fit}{A list; the body reads \code{$coef}, \code{$family} from it.}
\item{X}{A matrix; passed to \code{nrow}.}
\item{add_intercept}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{type}{Compared against \code{"link"}. Defaults to \code{c("response", "link")}.}
\item{offset}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the glm implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.morie_glm_families()[[fit$family]]$linkinv}.
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
f <- morie_glm(yb, X, "binomial")
morie_glm_predict(f, X, type = "link")
}
