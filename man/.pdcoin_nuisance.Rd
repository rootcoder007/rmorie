% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pdcoin_nuisance}
\alias{.pdcoin_nuisance}
\title{Internal: Pedroni\\'s steps 2-4 nuisance terms for one panel unit}
\usage{
.pdcoin_nuisance(y, Zx, e, bandwidth)
}
\arguments{
\item{y}{See Usage.}
\item{Zx}{A matrix; passed to \code{ncol}.}
\item{e}{A vector; its length is taken and its elements indexed.}
\item{bandwidth}{Passed to \code{.nw_lrv}.}
}
\description{
Returns L11^2, lambda_i and sigma^2_i.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
.pdcoin_nuisance(y = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), Zx = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), e = c(1, 2, 3, 4, 5, 6, 7, 8), bandwidth = 0.5)
}
