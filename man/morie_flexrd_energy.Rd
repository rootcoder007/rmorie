% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_flexrd_energy}
\alias{morie_flexrd_energy}
\title{Lennard-Jones 12-6 between the ligand and the receptor}
\usage{
morie_flexrd_energy(rec, lig, rec_r, lig_r, scale = 1, epsilon = 1, cutoff = 8)
}
\arguments{
\item{rec,lig}{Coordinate lists.}
\item{rec_r,lig_r}{The radii.}
\item{scale}{The softening.}
\item{epsilon}{The well depth.}
\item{cutoff}{The neighbourhood.}
}
\description{
The scale shrinks the summed radii, which is stage one's softening: below one a contact has to be closer before it costs anything, so a pose that a hard receptor would reject survives to the stage where the side chains can move out of its way. Pairs beyond the cutoff are dropped. The cutoff is stated rather than hidden because it makes the energy a sum over a finite neighbourhood, which is what lets two arms of this package agree term for term.
}
\value{
The energy.
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
morie_flexrd_energy(rec = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), lig = c(1, 2, 3, 4, 5, 6, 7, 8), rec_r = c(1, 2, 3, 4, 5, 6, 7, 8), lig_r = c(1, 2, 3, 4, 5, 6, 7, 8))
}
