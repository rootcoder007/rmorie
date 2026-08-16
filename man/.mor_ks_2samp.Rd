% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_ks_2samp}
\alias{.mor_ks_2samp}
\title{Two-sample Kolmogorov-Smirnov statistic and p-value}
\usage{
.mor_ks_2samp(a, b)
}
\arguments{
\item{a}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{b}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the over_native implementation. Called by \code{morie_over}. See the file header for the source the module follows.
}
\value{
A list with \code{stat}, \code{p}, \code{n1}, \code{n2}, \code{d_plus}, \code{d_minus}, \code{n_ties}.
}
\keyword{internal}
