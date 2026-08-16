% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_deviance_residuals}
\alias{morie_deviance_residuals}
\title{morie_deviance_residuals}
\usage{
morie_deviance_residuals(fit, y)
}
\arguments{
\item{fit}{A list; the body reads \code{$family}, \code{$fitted} from it.}
\item{y}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the glm implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A numeric value.
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
morie_deviance_residuals(f, yb)
}
