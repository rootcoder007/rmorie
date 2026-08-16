% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_glides_lipo}
\alias{morie_glides_lipo}
\title{A ramped contact count over lipophilic atom pairs}
\usage{
morie_glides_lipo(distances, r1 = 4.1, r2 = 7.1)
}
\arguments{
\item{distances}{The lipophilic pair separations.}
\item{r1}{The inner radius.}
\item{r2}{The outer radius.}
}
\description{
One inside the inner radius, zero past the outer, linear between -- the contact ramp of the empirical-scoring literature. Glide's own lipophilic term is not published in a form this module could reproduce, so this is a STAND-IN with stated parameters, and it is the reason the ledger calls this module a proxy.
}
\value{
A list with the total and the per-pair contributions.
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
