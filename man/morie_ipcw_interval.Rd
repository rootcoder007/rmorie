% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ipcw_interval}
\alias{morie_ipcw_interval}
\title{Ipcw_interval: Sec. 8.5\'s IPCW estimator of Psi_a = int r(t)}
\usage{
morie_ipcw_interval(W, A, times, deltas, a = 1, r = NULL, g = NULL, gc = NULL, ridge = 1e-08)
}
\arguments{
\item{W}{Passed to \code{.tmlcen_W_mat}.}
\item{A}{See Usage.}
\item{times}{Passed to \code{.tmlcen_coerce_subject_list}.}
\item{deltas}{Passed to \code{.tmlcen_coerce_subject_list}.}
\item{a}{Defaults to \code{1}.}
\item{r}{Defaults to \code{NULL}.}
\item{g}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
\item{gc}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
\item{ridge}{Passed to \code{.tmlcen_logit_irls}. Defaults to \code{1e-08}.}
}
\description{
Fbar_a(t) dt Part of the tmlcen_native implementation; see the file header for the source it follows.
}
\value{
A numeric value.
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
