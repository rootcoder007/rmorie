% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dnnlcg}
\alias{.dnnlcg}
\title{One LCG step, s <- (1103515245 s + 12345) mod 2^31, split so that no}
\usage{
.dnnlcg(s)
}
\arguments{
\item{s}{See Usage.}
}
\description{
intermediate product exceeds 2^53 and R doubles stay exact.
}
\value{
A numeric value.
}
\keyword{internal}
