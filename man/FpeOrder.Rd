% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{FpeOrder}
\alias{FpeOrder}
\title{Akaike (1970): FPE(p) = sigma_p^2 (N+p+1)/(N-p-1).  The residual}
\usage{
FpeOrder(errors, n_samples)
}
\arguments{
\item{errors}{See Usage.}
\item{n_samples}{See Usage.}
}
\description{
variance falls monotonically with p (eq 7.39), so without a penalty the criterion would always pick the largest order offered. Rangayyan gives AIC at eq (7.60); FPE is not printed in the book.
}
\value{
A list with \code{order}, \code{criterion}, \code{n}, \code{start_order}, \code{method}.
}
\keyword{internal}
