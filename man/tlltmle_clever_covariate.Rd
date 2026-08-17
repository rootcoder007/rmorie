% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{tlltmle_clever_covariate}
\alias{tlltmle_clever_covariate}
\title{tlltmle_clever_covariate}
\usage{
tlltmle_clever_covariate(A, g, rule = 1)
}
\arguments{
\item{A}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{g}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{rule}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
A step of the tlltmle_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{H}, \code{max}, \code{mean}, \code{note}.
}
\keyword{internal}
\examples{
set.seed(1)
A <- rbinom(50, 1, 0.5)
g <- runif(50, 0.2, 0.8)
tlltmle_clever_covariate(A, g)
}
