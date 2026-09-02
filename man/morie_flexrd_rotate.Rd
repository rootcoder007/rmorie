% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_flexrd_rotate}
\alias{morie_flexrd_rotate}
\title{Rodrigues' rotation of a point about the axis from a to b}
\usage{
morie_flexrd_rotate(p, a, b, degrees) 
}
\arguments{
\item{p}{The point to move.}
\item{a,b}{Two points defining the axis.}
\item{degrees}{The turn.}
}
\description{
Exact, and it preserves every distance to the axis -- which is what makes a torsion a torsion rather than a distortion. A zero-length axis has no direction to rotate about and is refused rather than normalised into a division by zero.
}
\value{
The rotated point.
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
morie_flexrd_rotate(p = 0.5, a = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L, degrees = c(1, 2, 3, 4, 5, 6, 7, 8))
}
