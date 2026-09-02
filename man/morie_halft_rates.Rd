% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_halft_rates}
\alias{morie_halft_rates}
\title{alpha and beta, the roots of the disposition quadratic}
\usage{
morie_halft_rates(V1, V2, CL, Q) 
}
\arguments{
\item{V1}{Central volume.}
\item{V2}{Peripheral volume.}
\item{CL}{Elimination clearance.}
\item{Q}{Inter-compartmental clearance.}
}
\description{
Solved with the numerically stable form of the quadratic formula -- the naive (-b +- sqrt(disc)) / 2 cancels catastrophically when the two rates are far apart, which is exactly the case where the terminal half-life matters most.
}
\value{
A list with alpha, beta and the micro rate constants.
}
\references{
  Rowland, M. and Tozer, T.N. (2011) "Clinical Pharmacokinetics and
    Pharmacodynamics: Concepts and Applications," 4th edition. Wolters
    Kluwer.
  Gibaldi, M. and Perrier, D. (1982) "Pharmacokinetics," 2nd edition.
    Marcel Dekker, chapter 2.
  Boxenbaum, H. and Battle, M. (1995) "Effective half-life in clinical
    pharmacokinetics." Journal of Clinical Pharmacology 35(8),
    763-766.
}
\keyword{internal}
\examples{
morie_halft_rates(V1 = 50, V2 = 30, CL = 5, Q = 2)
}
