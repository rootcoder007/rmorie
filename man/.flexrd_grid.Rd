% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.flexrd_grid}
\alias{.flexrd_grid}
\title{.flexrd_grid}
\usage{
.flexrd_grid(nchi, angles)
}
\arguments{
\item{nchi}{A count; the body uses it as \code{seq_len(...)}.}
\item{angles}{See Usage.}
}
\description{
A step of the flexrd_native implementation. Called by \code{morie_flexrd}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
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
    deplacements d\'un systeme solide." Journal de Mathematiques Pures
    et Appliquees 5, 380-440.
}
\keyword{internal}
\examples{
.flexrd_grid(nchi = 5L, angles = c(1, 2, 3, 4, 5, 6, 7, 8))
}
