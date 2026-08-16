% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bsahhrates}
\alias{.bsahhrates}
\title{Hodgkin-Huxley (1952) alpha/beta rate constants in 1/ms for membrane}
\usage{
.bsahhrates(v)
}
\arguments{
\item{v}{See Usage.}
}
\description{
potential v in mV, modern sign convention, rest -65 mV.  Removable singularities at v = -55 and v = -40 mV replaced by their limits.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
