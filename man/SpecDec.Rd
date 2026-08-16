% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SpecDec}
\alias{SpecDec}
\title{Alpha = sum_x min(p,q) = 1 - TV(p,q); E[tokens] =}
\usage{
SpecDec(draft, target, gamma = 4L)
}
\arguments{
\item{draft}{Passed to \code{.morie_spx_chkv}.}
\item{target}{Passed to \code{.morie_spx_chkv}.}
\item{gamma}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{4L}.}
}
\description{
(1 - alpha^(gamma+1))/(1 - alpha), capped at gamma+1 (a rejected token is resampled from the residual and still counts).  Leviathan, Kalman & Matias (2023).  NOT in Schabenberger & Gotway. Deterministic: the expectation, not a sampled run.
}
\value{
A list with \code{alpha}, \code{tv_distance}, \code{expected_tokens}, \code{gamma}, \code{max_tokens}, \code{deterministic_expectation_not_a_sampled_run}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
SpecDec(draft = c(0.5, 0.3, 0.2), target = c(0.4, 0.4, 0.2), gamma = 4L)
}
