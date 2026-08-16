% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Divergence}
\alias{Divergence}
\title{Eq (10.117), the closed form of the symmetric divergence of}
\usage{
Divergence(m1, m2, C1, C2)
}
\arguments{
\item{m1}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{m2}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{C1}{A matrix; passed to \code{as.matrix}.}
\item{C2}{A matrix; passed to \code{as.matrix}.}
}
\description{
eq (10.115): D = (1/2) tr[(Ci - Cj)(Cj^-1 - Ci^-1)] + (1/2) tr[(Ci^-1 + Cj^-1)(mi - mj)(mi - mj)^T] The second term resembles eq (10.112) and vanishes for equal means; the FIRST does not, so unlike d_n the divergence still separates classes differing only in covariance. This is the measure the book uses; Bhattacharyya appears nowhere in it.
}
\value{
A list with \code{divergence}, \code{covariance_term}, \code{mean_term}, \code{nonnegative}, \code{symmetric}, \code{zero_for_identical_pdfs}, \code{separates_equal_means_via_the_covariance_term}, \code{additive_over_independent_features}, \code{method}.
}
\keyword{internal}
