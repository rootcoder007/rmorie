% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.forwsr_norm_ppf}
\alias{.forwsr_norm_ppf}
\title{R\\\'s qnorm IS Wichura AS 241 (PPND16); the Python arm implements the}
\usage{
.forwsr_norm_ppf(p)
}
\arguments{
\item{p}{See Usage.}
}
\description{
same rational approximation with the same coefficients, and the two agree bit for bit. Bisecting on erf/pnorm instead lands on different sides of the root in the last ulp, which is enough to move the consistency factor and, through it, the deletion residual.
}
\value{
The value of \code{qnorm}.
}
\keyword{internal}
