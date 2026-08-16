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
\item{kind}{See Usage.}
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
