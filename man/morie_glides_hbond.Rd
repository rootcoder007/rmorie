% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_glides_hbond}
\alias{morie_glides_hbond}
\title{Hydrogen bonds summed within their three charge classes}
\usage{
morie_glides_hbond(bonds, weights = NULL)
}
\arguments{
\item{bonds}{A list of list(class, strength).}
\item{weights}{Overrides for the class weights, or NULL.}
}
\description{
The classes are weighted separately because a charged-charged bond is a salt bridge and a neutral pair is not; collapsing them to one number is the modelling error this split exists to prevent.
}
\value{
A list with the total, the per-bond contributions and the   three class subtotals.
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
HBN <- list(list("neutral_neutral", 1), list("neutral_charged", 
    0.8), list("charged_charged", 0.6), list("neutral_neutral", 
    0.4))
morie_glides_hbond(HBN)
}
