% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_sobol}
\alias{.morie_sobol}
\title{.morie_sobol}
\usage{
.morie_sobol(n, d)
}
\arguments{
\item{n}{See Usage.}
\item{d}{See Usage.}
}
\description{
Sobol low-discrepancy sequence via gray-code construction with Joe-Kuo direction numbers (dims 1-10) -- replaces randtoolbox::sobol for the QMC helper. Unscrambled; matches randtoolbox\\\'s unscrambled output (same standard direction numbers), cross-validated in tests.
}
\value{
The value of \code{.morie_sobol_cpp}.
}
\keyword{internal}
