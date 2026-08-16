% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_likemc}
\alias{morie_likemc}
\title{morie_likemc}
\usage{
morie_likemc(model, data, priors, n_iter, seed = 1, step = 0.15, burn = 0)
}
\arguments{
\item{model}{A list; the body reads \code{$dt}, \code{$I0}, \code{$N}, \code{$S0} from it.}
\item{data}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{priors}{Passed to \code{gv}.}
\item{n_iter}{Coerced to integer by the body, with \code{as.integer}.}
\item{seed}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1}.}
\item{step}{Numeric; combined arithmetically in the body. Defaults to \code{0.15}.}
\item{burn}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{0}.}
}
\description{
A step of the likemc_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{beta_mean}, \code{gamma_mean}, \code{chain}, \code{n_draws}, \code{n_iter}, \code{acceptance_rate}, \code{R0_mean}, \code{R0_q025}, \code{R0_median}, \code{R0_q975}, \code{logpost_final}, \code{seed}, \code{step}, \code{method}.
}
\keyword{internal}
