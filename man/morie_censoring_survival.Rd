% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_censoring_survival}
\alias{morie_censoring_survival}
\title{Censoring_survival: Gbar_c(k | A, W) = prod_{j<=k} (1 - lambda_C(j |}
\usage{
morie_censoring_survival(times, censored, A = NULL, W = NULL, grid = NULL, by_covariate = TRUE, 
    ridge = 1e-08) 
}
\arguments{
\item{times}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{censored}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{A}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{W}{Passed to \code{.tmlcen_W_mat}.}
\item{grid}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
\item{by_covariate}{Accepted by the signature and not used anywhere in the body. Defaults to \code{TRUE}.}
\item{ridge}{Passed to \code{.tmlcen_logit_irls}. Defaults to \code{1e-08}.}
}
\description{
A, W)) Part of the tmlcen_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{G}, \code{grid}, \code{b}.
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
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_censoring_survival(V, V)
}
