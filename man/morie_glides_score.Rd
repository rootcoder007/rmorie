% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_glides_score}
\alias{morie_glides_score}
\title{Assemble the eight terms into a GScore}
\usage{
morie_glides_score(vdw = 0, coulomb = 0, lipo = 0, hbond = 0, metal = 0, buryp = 0, rotb = 0, site = 0, coefficients = NULL, weights = NULL)
}
\arguments{
\item{vdw}{The van der Waals energy.}
\item{coulomb}{The Coulomb energy.}
\item{lipo}{The lipophilic contact term.}
\item{hbond}{The hydrogen bonding term.}
\item{metal}{The metal binding term.}
\item{buryp}{The buried polar penalty.}
\item{rotb}{The rotatable bond penalty.}
\item{site}{The active site polar term.}
\item{coefficients}{Overrides for the two published coefficients.}
\item{weights}{Overrides for the six unit weights.}
}
\description{
The two published coefficients multiply the van der Waals and Coulomb energies; the other six enter through weights that default to one.
}
\value{
A list with the total, the weighted parts and their order.
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
morie_glides_score()
}
