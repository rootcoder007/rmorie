% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_randIE_decompose}
\alias{morie_randIE_decompose}
\title{morie_randIE_decompose}
\usage{
morie_randIE_decompose(result)
}
\arguments{
\item{result}{A list; the body reads \code{$direct}, \code{$indirect}, \code{$total} from it.}
}
\description{
A step of the randIE_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{total}, \code{direct}, \code{indirect}, \code{residual}, \code{proportion.mediated}.
}
\keyword{internal}
\examples{
set.seed(1)
n <- 200
C <- as.character(rbinom(n, 1, 0.5))
A <- as.character(rbinom(n, 1, plogis(0.5 * (C == "1"))))
M <- as.character(rbinom(n, 1, plogis(0.6 * (A == "1") + 0.3 * (C == "1"))))
Y <- 1 + 0.8 * (A == "1") + 0.5 * (M == "1") + 0.4 * (C == "1") + rnorm(n)
rie <- morie_randIE_randomized_interventional_effect(Y, A, M, C = C)
dec <- morie_randIE_decompose(rie)
abs(dec$residual) < 1e-8
}
