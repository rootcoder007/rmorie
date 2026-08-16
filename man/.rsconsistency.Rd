% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rsconsistency}
\alias{.rsconsistency}
\title{C0 = alpha / F_chi2_{p+2}(q_alpha), alpha = h/n, q_alpha the chi2_p}
\usage{
.rsconsistency(h, n, p)
}
\arguments{
\item{h}{Numeric; combined arithmetically in the body.}
\item{n}{Numeric; combined arithmetically in the body.}
\item{p}{Numeric; combined arithmetically in the body.}
}
\description{
quantile Hubert, Debruyne and Rousseeuw (2018), arXiv 1709.07045, "Definition". Pchisq and Qchisq are the package\\'s own native mirrors, not stats::.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
