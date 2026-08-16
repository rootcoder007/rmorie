% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.snmcox_logit_irls}
\alias{.snmcox_logit_irls}
\title{.snmcox_logit_irls}
\usage{
.snmcox_logit_irls(X, y, iters = 60L, ridge = 1e-08, tol = 1e-13)
}
\arguments{
\item{X}{A matrix; passed to \code{ncol}.}
\item{y}{Numeric; combined arithmetically in the body.}
\item{iters}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{60L}.}
\item{ridge}{Numeric; combined arithmetically in the body. Defaults to \code{1e-08}.}
\item{tol}{Passed to \code{<}. Defaults to \code{1e-13}.}
}
\description{
A step of the snmcox_native implementation. Called by \code{.snmcox_treat_model}. See the file header for the source the module follows.
}
\value{
The value of \code{beta}, as built in the body.
}
\references{
Robins, J. M. (1992) "Estimation of the time-dependent accelerated failure
time model in the presence of confounding factors", Biometrika 79(2),
321-334, doi:10.1093/biomet/79.2.321.
Robins, J. M., Blevins, D., Ritter, G. and Wulfsohn, M. (1992) "G-estimation
of the effect of prophylaxis therapy for Pneumocystis carinii pneumonia on
the survival of AIDS patients", Epidemiology 3(4), 319-336,
doi:10.1097/00001648-199207000-00007.
Hernan, M. A., Cole, S. R., Margolick, J., Cohen, M. and Robins, J. M. (2005)
"Structural accelerated failure time models for survival analysis in studies
with time-varying treatments", Pharmacoepidemiology and Drug Safety 14(7),
477-491, doi:10.1002/pds.1064.
}
\keyword{internal}
\examples{
.snmcox_logit_irls(X = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), y = 5L)
}
