% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ma8Rec}
\alias{Ma8Rec}
\title{Eq (3.120): y(n) = y(n-1) + (1/8)x(n) - (1/8)x(n-8).  Two additions a}
\usage{
Ma8Rec(x, n = NULL) 
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
sample instead of eight, and it "clearly depicts the integration aspect".  The cost is that error accumulates -- the recursion never forgets, where the direct form flushes a perturbation after 8 samples.
}
\value{
A list with \code{y}, \code{value}, \code{index}, \code{direct_form}, \code{max_difference}, \code{agrees_with_direct_form}, \code{additions_per_sample}, \code{direct_form_additions}, \code{error_accumulates}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Ma8Rec(V)
}
