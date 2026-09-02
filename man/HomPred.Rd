% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{HomPred}
\alias{HomPred}
\title{Section 4.7.3.  The two lifters must PARTITION the quefrency axis}
\usage{
HomPred(y, cutoff) 
}
\arguments{
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{cutoff}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
(|q| <= k and |q| > k); sharing the cutoff keeps q = 0 and q = k in both halves and the reconstruction fails.  And because the cepstra add (eq 4.66), exponentiating the sum gives the CIRCULAR convolution of the components -- reconstructing linearly leaves a wrap-around error that looks like a failure of the separation.
}
\value{
A list with \code{low_time}, \code{high_time}, \code{cutoff}, \code{n}, \code{reconstruction}, \code{reconstruction_error}, \code{relative_error}, \code{separation_premise}, \code{method}.
}
\keyword{internal}
\examples{
HomPred(y = c(1, 2, 3, 4, 5, 6, 7, 8), cutoff = 1)
}
