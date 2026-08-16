% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.glides_ipow}
\alias{.glides_ipow}
\title{X to a small integer power, by repeated multiplication. Not the}
\usage{
.glides_ipow(x, k)
}
\arguments{
\item{x}{Numeric; combined arithmetically in the body.}
\item{k}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
language\\'s power operator: R uses repeated squaring for an integer exponent and Python calls the C library\\'s pow, and they disagree in the last bit.
}
\value{
The value of \code{p}, as built in the body.
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
