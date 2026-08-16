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
\item{dist}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
\item{match}{Passed to \code{morie_score_distribution}. Defaults to \code{5}.}
\item{mismatch}{Passed to \code{morie_score_distribution}. Defaults to \code{-4}.}
\item{letter_probs}{Passed to \code{morie_score_distribution}.}
\item{matrix}{Passed to \code{morie_score_distribution}.}
\item{subject_probs}{Passed to \code{morie_score_distribution}.}
\item{max_terms}{Defaults to \code{1000}.}
\item{tol}{Defaults to \code{1e-12}.}
\item{bound}{One of \code{"lower"}, \code{"mid"}, \code{"upper"}. Defaults to \code{"upper"}.}
}
\description{
A step of the blstn_native implementation. Called by \code{morie_blstn}. See the file header for the source the module follows.
}
\value{
A list with \code{lam}, \code{K}, \code{K_upper}, \code{K_lower}, \code{C}, \code{delta}, \code{terms}, \code{series}, \code{mean_score}, \code{distribution}.
}
\keyword{internal}
\examples{
morie_karlin_altschul()
}
