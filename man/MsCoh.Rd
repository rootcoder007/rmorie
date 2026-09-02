% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MsCoh}
\alias{MsCoh}
\title{C_xy = |S_xy|^2 / (S_xx S_yy) by Welch averaging (Bendat & Piersol}
\usage{
MsCoh(x, y, nperseg = NULL, overlap = 0.5) 
}
\arguments{
\item{x}{Passed to \code{.morie_spx_chkv}.}
\item{y}{Passed to \code{.morie_spx_chkv}.}
\item{nperseg}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{overlap}{Numeric; combined arithmetically in the body. Defaults to \code{0.5}.}
}
\description{
2010, Ch. 5).  NOT in Schabenberger & Gotway -- a fixed-string search for "coherence" returns nothing.  AVERAGING IS NOT OPTIONAL: on a single segment the coherence of ANY two records is exactly 1, so fewer than two segments raises.
}
\value{
A list with \code{omega}, \code{coherence}, \code{sxx}, \code{syy}, \code{n_segments}, \code{nperseg}, \code{step}, \code{single_segment_coherence_is_identically_one}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(5)
n <- 512
s <- sin(2 * pi * 0.1 * (1:n))
x <- s + 0.3 * rnorm(n)
y <- s + 0.3 * rnorm(n)
str(MsCoh(x, y, nperseg = 128), max.level = 1)
}
