% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tmlcen_weighted_logit}
\alias{.tmlcen_weighted_logit}
\title{.tmlcen_weighted_logit}
\usage{
.tmlcen_weighted_logit(Z, y, w, iters = 60L, ridge = 1e-10)
}
\arguments{
\item{Z}{A matrix; passed to \code{nrow}.}
\item{y}{Numeric; combined arithmetically in the body.}
\item{w}{Numeric; combined arithmetically in the body.}
\item{iters}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{60L}.}
\item{ridge}{Passed to \code{.tmlcen_ridgesolve}. Defaults to \code{1e-10}.}
}
\description{
A step of the tmlcen_native implementation. Called by \code{.tmlcen_logit_irls}, \code{morie_tmle_censoring}. See the file header for the source the module follows.
}
\value{
The value of \code{b}, as built in the body.
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
.tmlcen_weighted_logit(Z = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), y = 5L, w = 5L)
}
