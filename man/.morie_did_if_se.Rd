% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_did_if_se}
\alias{.morie_did_if_se}
\title{Internal helper: influence-function standard error, two conventions}
\usage{
.morie_did_if_se(IF, se_convention = c("reference", "bessel"))
}
\description{
`"reference"` reproduces the canonical packages (DRDID / did): population-sd scaling, `sd(IF) * sqrt(n-1) / n`. `"bessel"` keeps Bessel\'s correction: `sd(IF) / sqrt(n)`. The two agree asymptotically; the default is `"reference"` so results match the reference implementations to machine precision.
}
\value{
The value of \code{if}.
}
\keyword{internal}
