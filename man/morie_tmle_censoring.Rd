% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tmle_censoring}
\alias{morie_tmle_censoring}
\title{Tmle_censoring: causal survival under censoring}
\usage{
morie_tmle_censoring(time, event, censor, treatment, covariates, kind = "right", grid = NULL, a = 1, r = NULL, g = NULL, gc = NULL, trim = 0.001)
}
\arguments{
\item{time}{See Usage.}
\item{event}{See Usage.}
\item{censor}{See Usage.}
\item{treatment}{See Usage.}
\item{covariates}{See Usage.}
\item{kind}{Defaults to \code{"right"}.}
\item{grid}{Defaults to \code{NULL}.}
\item{a}{Defaults to \code{1}.}
\item{r}{Defaults to \code{NULL}.}
\item{g}{Defaults to \code{NULL}.}
\item{gc}{Defaults to \code{NULL}.}
\item{trim}{Defaults to \code{0.001}.}
}
\description{
Part of the tmlcen_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{.tmlcen_RichResult}.
}
\references{
----------
Hernan, M. A. & Robins, J. M. (2020) Causal Inference: What If, Boca
Raton: Chapman & Hall/CRC, Ch. 17 "Causal survival analysis" --
Sec. 17.2 hazards to risks, Sec. 17.3 why censoring matters, Sec. 17.4
IP weighting of marginal structural models.

van der Laan, M. J. & Rose, S. (eds.) (2018) Targeted Learning in
Data Science, Springer Series in Statistics,
doi:10.1007/978-3-319-65304-4, Sec. 8.5 "Causal Effect of Binary
Treatment on Interval Censored Time to Event" -- the data structure,
the coarsening C(o) = (L(o), R(o)], and the IPCW estimator above.

Note on the ledger citation: its key "Stitelman-Lendle-vdL (2011)" does
not resolve to any paper; a bibliographic search returns the ltmle
software package instead. The two sources above are what this is built
from.
}
\keyword{internal}
\examples{
morie_tmle_censoring(time = c(1, 2, 3, 4, 5, 6, 7, 8), event = c(1, 2, 3, 4, 5, 6, 7, 8), censor = c(0, 1, 0, 1, 1, 0, 1, 0), treatment = c(0, 1, 0, 1, 1, 0, 1, 0), covariates = c(1, 2, 3, 4, 5, 6, 7, 8))
}
