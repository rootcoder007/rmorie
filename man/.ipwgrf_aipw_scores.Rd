% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ipwgrf_aipw_scores}
\alias{.ipwgrf_aipw_scores}
\title{.ipwgrf_aipw_scores}
\usage{
.ipwgrf_aipw_scores(y, W, mu1, mu0, e, trim = 0.02)
}
\arguments{
\item{y}{A vector; its length is taken and its elements indexed.}
\item{W}{A vector; indexed elementwise.}
\item{mu1}{A vector; indexed elementwise.}
\item{mu0}{A vector; indexed elementwise.}
\item{e}{A vector; indexed elementwise.}
\item{trim}{Defaults to \code{0.02}.}
}
\description{
A step of the ipwgrf_native implementation. Called by \code{morie_ipwgrf}. See the file header for the source the module follows.
}
\value{
A list with \code{g}, \code{weights}.
}
\references{
----------
Robins, J. M., Rotnitzky, A. & Zhao, L. P. (1994).  The augmented IPW
  score.  JASA 89(427), 846-866, doi:10.1080/01621459.1994.10476818.
Athey, S., Tibshirani, J. & Wager, S. (2019).  Generalized Random
  Forests.  Ann. Statist. 47(2), 1148-1178, doi:10.1214/18-AOS1709.
Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen,
  C., Newey, W. & Robins, J. (2018).  Double/debiased machine learning
  for treatment and structural parameters.  Econometrics J. 21(1),
  C1-C68, doi:10.1111/ectj.12097.
Crump, R. K., Hotz, V. J., Imbens, G. W. & Mitnik, O. A. (2009).
  Dealing with limited overlap in estimation of average treatment
  effects.  Biometrika 96(1), 187-199, doi:10.1093/biomet/asn055.
}
\keyword{internal}
