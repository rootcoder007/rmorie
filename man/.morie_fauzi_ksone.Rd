% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_fauzi_ksone}
\alias{.morie_fauzi_ksone}
\title{Exact one-sided Kolmogorov distribution function, Birnbaum-Tingey.}
\usage{
.morie_fauzi_ksone(d, n)
}
\description{
`P(D_n^+ >= d) = d sum_{j=0}^{floor(n(1-d))} choose(n,j) (d+j/n)^(j-1) (1-d-j/n)^(n-j)`, and this returns `1 -` that. Summed through logarithms so the binomial coefficient cannot overflow, and mirroring `morie.fn._stats_core.ksone` term for term so the Python and R arms agree to the last bit rather than merely to plotting accuracy.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.morie_fauzi_ksone(d = 5L, n = 5L)
}
