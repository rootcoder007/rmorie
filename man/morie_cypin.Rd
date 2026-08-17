% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_cypin}
\alias{morie_cypin}
\title{Descriptors for a compound against one P450 isozyme}
\usage{
morie_cypin(smiles, isozyme, model = NULL)
}
\arguments{
\item{smiles}{The compound.}
\item{isozyme}{One of 1A2, 2C9, 2C19, 2D6, 3A4.}
\item{model}{Coefficients from the fit, or the list the fit returns.   NULL means no prediction is made and the reason says so.}
}
\description{
Descriptors for a compound against one P450 isozyme
}
\value{
A list with the descriptors, named; the probability if a   model was given; and otherwise the reason there is none.
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
r <- morie_cypin("c1ccccc1O", isozyme = "3A4", model = coef)
str(r, max.level = 1)
}
