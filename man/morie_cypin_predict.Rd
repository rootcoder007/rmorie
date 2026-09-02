% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_cypin_predict}
\alias{morie_cypin_predict}
\title{The inhibition probability of one descriptor row}
\usage{
morie_cypin_predict(x, coefficients) 
}
\arguments{
\item{x}{A descriptor row.}
\item{coefficients}{The intercept followed by one slope per   descriptor.}
}
\description{
The inhibition probability of one descriptor row
}
\value{
A probability.
}
\references{
  Veith, H. et al. (2009) "Comprehensive characterization of
    cytochrome P450 isozyme selectivity across chemical libraries."
    Nature Biotechnology 27(11), 1050-1055. doi:10.1038/nbt.1581.
  Meija, J. et al. (2016) "Atomic weights of the elements 2013."
    Pure and Applied Chemistry 88(3), 265-291.
  McCullagh, P. and Nelder, J.A. (1989) "Generalized Linear Models",
    2nd edition, Chapman and Hall.
}
\keyword{internal}
\examples{
x <- morie_cypin_descriptors("c1ccccc1O")
coef <- c(-0.5, rep(0.01, length(x)))
morie_cypin_predict(x, coef)
}
