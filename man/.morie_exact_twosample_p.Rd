% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_exact_twosample_p}
\alias{.morie_exact_twosample_p}
\title{Exact two-sample permutation distribution p-value for a rank/score sum}
\usage{
.morie_exact_twosample_p(scores, n1, obs, mu, alternative = "two.sided")
}
\description{
For a two-group design the linear statistic is the sum of the response scores in one group; its exact permutation distribution is the convolution of choosing \code{n1} of the \code{n} scores. Ties are handled because arbitrary real scores are convolved on a shifted grid.
}
\value{
A numeric value.
}
\keyword{internal}
