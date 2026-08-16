% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_k05_lr_uc}
\alias{.morie_k05_lr_uc}
\title{.morie_k05_lr_uc}
\usage{
.morie_k05_lr_uc(p, t, n)
}
\arguments{
\item{p}{Numeric; passed to \code{log}.}
\item{t}{Numeric; combined arithmetically in the body.}
\item{n}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the k05_tranche1 implementation. Called by \code{morie_christoffersen_cc}, \code{morie_kupiec_var_test}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.morie_k05_lr_uc(p = 0.5, t = 5L, n = 5L)
}
