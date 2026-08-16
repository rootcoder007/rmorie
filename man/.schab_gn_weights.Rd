% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_gn_weights}
\alias{.schab_gn_weights}
\title{OLS is R = phi I, so the weights are 1. WLS uses Cressie\\'s (1985)}
\usage{
.schab_gn_weights(kind, fitted, counts)
}
\arguments{
\item{kind}{Passed to \code{identical}.}
\item{fitted}{A vector; its length is taken.}
\item{counts}{Numeric; combined arithmetically in the body.}
}
\description{
approximation (4.33), Var[gamma_hat(h_m)] = 2 gamma^2 / |N(h_m)|, whose reciprocal is the weight in (4.34).
}
\value{
The value of \code{ifelse}.
}
\keyword{internal}
\examples{
.schab_gn_weights(kind = c(1, 2, 3, 4, 5, 6, 7, 8), fitted = c(1, 2, 3, 4, 5, 6, 7, 8), counts = c(1, 2, 3, 4, 5, 6, 7, 8))
}
