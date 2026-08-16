% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.w3_nppf}
\alias{.w3_nppf}
\title{Standard normal quantile by bisection on the CDF: slower than a}
\usage{
.w3_nppf(p, lo = -40, hi = 40)
}
\arguments{
\item{p}{See Usage.}
\item{lo}{Defaults to \code{-40}.}
\item{hi}{Defaults to \code{40}.}
}
\description{
rational approximation and exactly as accurate as the CDF it inverts, which is the property that matters here.
}
\value{
The value of \code{.w3_bisect}.
}
\keyword{internal}
