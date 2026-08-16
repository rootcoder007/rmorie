% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.snpqc1_erfc}
\alias{.snpqc1_erfc}
\title{Complementary error function via pnorm: erfc(x) = 2*pnorm(-x*sqrt2)}
\usage{
.snpqc1_erfc(x)
}
\arguments{
\item{x}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the snpqc1_native implementation. Called by \code{morie_snpqc1_hwe_pvalue}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
