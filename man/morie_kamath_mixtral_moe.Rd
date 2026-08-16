% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_mixtral_moe}
\alias{morie_kamath_mixtral_moe}
\title{morie_kamath_mixtral_moe}
\usage{
morie_kamath_mixtral_moe(x, W_g, expert_weights = NULL)
}
\arguments{
\item{expert_weights}{Optional list of (W1, W3, W2) per expert;   NULL isolates the gate with identity experts.}
}
\description{
morie_kamath_mixtral_moe
}
\value{
A list with \code{output}, \code{gate}, \code{experts_evaluated}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
