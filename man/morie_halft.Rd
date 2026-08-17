% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_halft}
\alias{morie_halft}
\title{Plasma half-life from volume of distribution and clearance}
\usage{
morie_halft(smiles = NULL, Vd = NULL, Cl = NULL, route = "one_compartment", V1 = NULL, V2 = NULL, Q = NULL, dose = NULL)
}
\arguments{
\item{smiles}{Structure, carried through for provenance. Not used in   the arithmetic; see the file header.}
\item{Vd}{Volume of distribution, in litres. The steady-state volume   for the one-compartment and effective routes.}
\item{Cl}{Clearance, in litres per hour.}
\item{route}{A member of the route list.}
\item{V1}{Central volume, for the two-compartment route.}
\item{V2}{Peripheral volume.}
\item{Q}{Inter-compartmental clearance.}
\item{dose}{An intravenous dose; when supplied the result carries the   biexponential coefficients.}
}
\description{
Plasma half-life from volume of distribution and clearance
}
\value{
A list with the half-life, the elimination rate constant,   mean residence time, and for the two-compartment route both phase   half-lives, the fraction of area in each and the effective   half-life.
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
morie_halft(Vd = 50, Cl = 5)
}
