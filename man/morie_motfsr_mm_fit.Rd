% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_motfsr_mm_fit}
\alias{morie_motfsr_mm_fit}
\title{One pass of MM: fit the two-component mixture by EM}
\usage{
morie_motfsr_mm_fit(sequences, w, alphabet = NULL, theta0 = NULL, lambda0 = NULL, beta = 0.01, erasing = NULL, max_iter = 1000, tol = 1e-06, normalize_overlaps = TRUE, erase_by = "letter")
}
\arguments{
\item{sequences}{Dataset.}
\item{w}{Motif width.}
\item{alphabet}{Optional alphabet.}
\item{theta0,lambda0}{Starting point.}
\item{beta}{Pseudo-count total of eq. 13.}
\item{erasing}{Optional erasing factors.}
\item{max_iter,tol}{EM stopping rule.}
\item{normalize_overlaps}{Apply the window constraint.}
\item{erase_by}{"letter" or "start".}
}
\description{
One pass of MM: fit the two-component mixture by EM
}
\value{
A list with theta, motif, background, lambda1, z,   log_likelihood, log_likelihood_trace, n_iter, converged, alphabet,   w.
}
\keyword{internal}
