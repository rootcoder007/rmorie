% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_cypin_logistic}
\alias{morie_cypin_logistic}
\title{The logistic function, written so it cannot overflow either way}
\usage{
morie_cypin_logistic(z)
}
\arguments{
\item{z}{A numeric scalar.}
}
\description{
A large positive argument would overflow the exponential in the naive form and a large negative one would overflow the other; branching on the sign keeps both exponentials below one.
}
\value{
A number strictly between zero and one.
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
