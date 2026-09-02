% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_motfsr_log_odds_matrix}
\alias{morie_motfsr_log_odds_matrix}
\title{Log-odds classifier matrix}
\usage{
morie_motfsr_log_odds_matrix(motif, background) 
}
\arguments{
\item{motif,background}{Frequency matrices.}
}
\description{
Log-odds classifier matrix
}
\value{
spec with spec[i][k] = log(motif[i][k] / background[k]).
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_motfsr_log_odds_matrix(V, V)
}
