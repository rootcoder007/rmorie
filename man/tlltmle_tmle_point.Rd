% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{tlltmle_tmle_point}
\alias{tlltmle_tmle_point}
\title{tlltmle_tmle_point}
\usage{
tlltmle_tmle_point(A, Y, Q1, Q0, g) 
}
\arguments{
\item{A}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{Y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{Q1}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{Q0}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{g}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the tlltmle_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{psi}, \code{epsilon}, \code{se}, \code{ci}, \code{mean_eic}, \code{solves_eic}, \code{max_clever_covariate}, \code{initial_plugin}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
set.seed(1)
A <- rbinom(50, 1, 0.5)
Y <- rbinom(50, 1, 0.4)
Q1 <- runif(50, 0.3, 0.7)
Q0 <- runif(50, 0.2, 0.5)
g <- runif(50, 0.3, 0.7)
tlltmle_tmle_point(A, Y, Q1, Q0, g)
}
