% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.snmcox_treat_model}
\alias{.snmcox_treat_model}
\title{Fitted E[A | L]: logistic when the treatment is binary, least squares}
\usage{
.snmcox_treat_model(A, L, ridge = 1e-08)
}
\arguments{
\item{A}{See Usage.}
\item{L}{See Usage.}
\item{ridge}{Defaults to \code{1e-08}.}
}
\description{
else Part of the snmcox_native implementation; see the file header for the source it follows.
}
\value{
A list, whose contents depend on the branch taken; across the branches its names are \code{e}, \code{b}, \code{kind}.
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
