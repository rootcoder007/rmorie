% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tmlcen_matvec}
\alias{.tmlcen_matvec}
\title{.tmlcen_matvec}
\usage{
.tmlcen_matvec(Z, b)
}
\arguments{
\item{Z}{A matrix; passed to \code{\%*\%}.}
\item{b}{A matrix; passed to \code{\%*\%}.}
}
\description{
A step of the tmlcen_native implementation. Called by \code{morie_ipcw_interval}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
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
