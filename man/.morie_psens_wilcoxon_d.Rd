% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_psens_wilcoxon_d}
\alias{.morie_psens_wilcoxon_d}
\title{One-sample form: Rosenbaum bounds directly on pair differences}
\usage{
.morie_psens_wilcoxon_d(d, gamma)
}
\arguments{
\item{d}{A vector; its length is taken and its elements indexed.}
\item{gamma}{Numeric; combined arithmetically in the body.}
}
\description{
Exact replica of rbounds::psens(): both bounds share the variance computed at p+ (a documented quirk of that implementation), the lower bound centres at p-, the upper at p+.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
