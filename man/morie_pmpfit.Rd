% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_pmpfit}
\alias{morie_pmpfit}
\title{Pitman-Yor two-parameter Poisson-Dirichlet stick breaking}
\usage{
morie_pmpfit(alpha, theta, K, seed = 0L) 
}
\arguments{
\item{alpha}{Discount, in [0, 1).}
\item{theta}{Concentration, with theta > -alpha.}
\item{K}{Number of sticks, at least 1.}
\item{seed}{Seed for the shared SplitMix64 generator.}
}
\description{
Draws K sticks Y_n ~ Beta(1 - alpha, theta + n alpha) for 0 <= alpha < 1 and theta > -alpha. At alpha = 0 this is exactly the Dirichlet process. The form is forced by Proposition 4: a size-biased permutation admits the residual allocation form with independent factors if and only if the betas take this form.
}
\value{
A list with `weights`, `Y`, `remaining`, `kept_mass`,   `alpha`, `theta`, `note`.
}
\keyword{internal}
\examples{
morie_pmpfit(alpha = 0.5, theta = 0.5, K = 5L)
}
