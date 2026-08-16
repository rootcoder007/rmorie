% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.glides_merge}
\alias{.glides_merge}
\title{.glides_merge}
\usage{
.glides_merge(base, over)
}
\arguments{
\item{base}{A vector; indexed elementwise.}
\item{over}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
}
\description{
A step of the glides_native implementation. Called by \code{morie_glides_hbond}, \code{morie_glides_score}. See the file header for the source the module follows.
}
\value{
The value of \code{base}, as built in the body.
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
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.glides_merge(V, V)
}
