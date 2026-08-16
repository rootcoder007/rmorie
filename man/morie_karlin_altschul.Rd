% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_karlin_altschul}
\alias{morie_karlin_altschul}
\title{morie_karlin_altschul}
\usage{
morie_karlin_altschul(dist = NULL, match = 5, mismatch = -4, letter_probs = NULL, matrix = NULL, subject_probs = NULL, max_terms = 1000, tol = 1e-12, bound = "upper")
}
\arguments{
\item{dist}{Defaults to \code{NULL}.}
\item{match}{Defaults to \code{5}.}
\item{mismatch}{Defaults to \code{-4}.}
\item{letter_probs}{Defaults to \code{NULL}.}
\item{matrix}{Defaults to \code{NULL}.}
\item{subject_probs}{Defaults to \code{NULL}.}
\item{max_terms}{Defaults to \code{1000}.}
\item{tol}{Defaults to \code{1e-12}.}
\item{bound}{Defaults to \code{"upper"}.}
}
\description{
Part of the blstn_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{lam}, \code{K}, \code{K_upper}, \code{K_lower}, \code{C}, \code{delta}, \code{terms}, \code{series}, \code{mean_score}, \code{distribution}.
}
\keyword{internal}
