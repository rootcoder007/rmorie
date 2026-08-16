% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bnsadt_expected_p}
\alias{.bnsadt_expected_p}
\title{E[p(X/sigma)] under the marginal law of an unselected study. A finite}
\usage{
.bnsadt_expected_p(sigma, mu, tau, family, params)
}
\arguments{
\item{sigma}{Numeric; combined arithmetically in the body.}
\item{mu}{Numeric; combined arithmetically in the body.}
\item{tau}{Numeric; combined arithmetically in the body.}
\item{family}{Passed to \code{.bnsadt_betas}.}
\item{params}{Passed to \code{.bnsadt_betas}.}
}
\description{
sum of normal increments because p is a step function -- the reason no quadrature appears anywhere in this module.
}
\value{
The value of \code{.w3_csum}.
}
\keyword{internal}
