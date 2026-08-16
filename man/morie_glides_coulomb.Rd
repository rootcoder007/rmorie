% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_glides_coulomb}
\alias{morie_glides_coulomb}
\title{Coulomb energy over a list of contacts}
\usage{
morie_glides_coulomb(pairs, dielectric = "constant", epsilon = 1)
}
\arguments{
\item{pairs}{A list of list(distance, q_i, q_j).}
\item{dielectric}{A member of the dielectric list.}
\item{epsilon}{The permittivity scale.}
}
\description{
With a constant permittivity the energy falls off as 1/r. With the distance-dependent form -- the usual choice in docking, where no solvent is present -- the permittivity is epsilon times r and the energy falls off as 1/r squared. That is a different physics, not a different constant, so it is a route and not a parameter tweak.
}
\value{
A list with the total and the per-contact energies.
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
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
morie_glides_coulomb(D)
}
