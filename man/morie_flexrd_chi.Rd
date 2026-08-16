% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_flexrd_chi}
\alias{morie_flexrd_chi}
\title{Turn one chi angle: rotate the atoms it moves, and only those}
\usage{
morie_flexrd_chi(coords, chi, degrees)
}
\arguments{
\item{coords}{The receptor coordinates, a list of triples.}
\item{chi}{A list with a, b and moves, all zero-based.}
\item{degrees}{The turn.}
}
\description{
The chi names the two atoms of the bond and the atoms distal to it. Everything else in the receptor stays exactly where it was, which is what makes the side chain flexible and the backbone not.
}
\value{
The receptor with that side chain moved.
}
\references{
  Sherman, W., Day, T., Jacobson, M.P., Friesner, R.A. and Farid, R.
    (2006) "Novel procedure for modeling ligand/receptor induced fit
    effects." Journal of Medicinal Chemistry 49(2), 534-553.
    doi:10.1021/jm050540c.
  Sherman, W., Beard, H.S. and Farid, R. (2006) "Use of an induced fit
    receptor structure in virtual screening." Chemical Biology and
    Drug Design 67(1), 83-84.
  Jones, J.E. (1924) "On the determination of molecular fields. II."
    Proceedings of the Royal Society A 106(738), 463-477.
  Rodrigues, O. (1840) "Des lois geometriques qui regissent les
    deplacements d'un systeme solide." Journal de Mathematiques Pures
    et Appliquees 5, 380-440.
}
\keyword{internal}
\examples{
morie_flexrd_chi(coords = c(1, 2, 3, 4, 5, 6, 7, 8), chi = list(a = 1, b = 2), degrees = c(1, 2, 3, 4, 5, 6, 7, 8))
}
