% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_flexrd}
\alias{morie_flexrd}
\title{Dock a ligand into a receptor whose side chains may move}
\usage{
morie_flexrd(receptor, ligand, flex_residues, angles = NULL, soft = 0.7, epsilon = 1, cutoff = 8, search = "coordinate", passes = 3L, n_keep = 3L)
}
\arguments{
\item{receptor}{A list with coords and radii, one per receptor atom.}
\item{ligand}{A list with coords and radii for a single pose, or   poses for a list of candidate poses to try.}
\item{flex_residues}{One entry per movable chi angle: a and b name   the bond it turns about and moves lists the atoms distal to it.}
\item{angles}{The chi values to try, as offsets from the input   geometry. NULL is zero -- the structure as given -- together with   the staggered positions, so the input rotamer is always in the   search and flexibility can never score worse than rigidity.}
\item{soft}{Stage one's radius scale.}
\item{epsilon}{The well depth.}
\item{cutoff}{The neighbourhood.}
\item{search}{Either coordinate or exhaustive.}
\item{passes}{Sweeps of the coordinate search.}
\item{n_keep}{How many stage-one poses go forward.}
}
\description{
Dock a ligand into a receptor whose side chains may move
}
\value{
A list with the chosen pose, the chosen chi angles, the   softened and hard energies at each stage, and the refined receptor.
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
REC <- list(c(0, 0, 0), c(1.5, 0, 0), c(2.2, 1.2, 0), c(3, 1.2, 
    0), c(0, -4, 0), c(-3, 0, 0))
RR <- rep(1.7, 6)
CHI <- list(list(a = 0L, b = 1L, moves = c(2L, 3L)))
LIG <- list(c(2.4, 1.6, 0), c(3.4, 2.4, 0))
LR <- c(1.7, 1.7)
REC_D <- list(coords = REC, radii = RR)
LIG_D <- list(coords = LIG, radii = LR)
morie_flexrd(REC_D, LIG_D, CHI)
}
