% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_glides}
\alias{morie_glides}
\title{Score a pose in the Glide form}
\usage{
morie_glides(receptor, ligand_pose, radii = list(), depths = list(), charges = list(), lipophilic = character(0), hbonds = list(), dielectric = "constant", epsilon = 1, m = 6, n = 12, r1 = 4.1, r2 = 7.1, metal = 0, buryp = 0, n_rot = 0, rot_penalty = 0.35, site = 0, coefficients = NULL, weights = NULL, cutoff = NULL)
}
\arguments{
\item{receptor}{Atom rows: x, y, z, type.}
\item{ligand_pose}{The ligand atom rows, likewise.}
\item{radii}{A list of list(type, contact radius).}
\item{depths}{A list of list(type, well depth).}
\item{charges}{A list of list(type, partial charge), already REDUCED   on formally charged groups as the paper requires -- that reduction   is a chemical judgement and is the caller's.}
\item{lipophilic}{Atom types treated as lipophilic.}
\item{hbonds}{A list of list(class, strength) per hydrogen bond.}
\item{dielectric}{A member of the dielectric list.}
\item{epsilon}{The permittivity scale.}
\item{m}{The attractive exponent.}
\item{n}{The repulsive exponent.}
\item{r1}{The inner lipophilic radius.}
\item{r2}{The outer lipophilic radius.}
\item{metal}{The metal binding term.}
\item{buryp}{The buried polar penalty.}
\item{n_rot}{Frozen rotatable bonds.}
\item{rot_penalty}{The penalty per frozen bond; POSITIVE, so it   raises the score of a floppy ligand.}
\item{site}{The active site polar term.}
\item{coefficients}{Overrides for the two published coefficients.}
\item{weights}{Overrides for the six unit weights.}
\item{cutoff}{Drop contacts beyond this separation, or NULL.}
}
\description{
Score a pose in the Glide form
}
\value{
A list with the GScore, each weighted contribution, and the   raw energies.
}
\references{
  Friesner, R.A., Banks, J.L., Murphy, R.B., Halgren, T.A., Klicic,
    J.J., Mainz, D.T., Repasky, M.P., Knoll, E.H., Shelley, M., Perry,
    J.K., Shaw, D.E., Francis, P. and Shenkin, P.S. (2004) "Glide: a
    new approach for rapid, accurate docking and scoring. 1. Method
    and assessment of docking accuracy." Journal of Medicinal
    Chemistry 47(7), 1739-1749. doi:10.1021/jm0306430.
  Halgren, T.A., Murphy, R.B., Friesner, R.A., Beard, H.S., Frye,
    L.L., Pollard, W.T. and Banks, J.L. (2004) "Glide ... 2.
    Enrichment factors in database screening." Journal of Medicinal
    Chemistry 47(7), 1750-1759.
  Friesner, R.A., Murphy, R.B., Repasky, M.P., Frye, L.L., Greenwood,
    J.R., Halgren, T.A., Sanschagrin, P.C. and Mainz, D.T. (2006)
    "Extra precision Glide." Journal of Medicinal Chemistry 49(21),
    6177-6196.
  Eldridge, M.D. et al. (1997) "Empirical scoring functions: I."
    Journal of Computer-Aided Molecular Design 11(5), 425-445.
}
\keyword{internal}
