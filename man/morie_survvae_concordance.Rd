% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_survvae_concordance}
\alias{morie_survvae_concordance}
\title{Harrell\'s C at a horizon.}
\usage{
morie_survvae_concordance(fit_result, X, times, events, horizon = NULL)
}
\arguments{
\item{times}{Numeric vector of survival times.}
\item{events}{Event indicator.}
}
\description{
Mirrors \code{morie.fn.survrsf.c_index}: pairwise count of concordances, discordances and tied risks over the comparable pairs.
}
\value{
Numeric scalar, the concordance index.
}
\keyword{internal}
\examples{
set.seed(2)
X <- lapply(1:40, function(i) rnorm(2))
tt <- rexp(40, 0.1)
ev <- rbinom(40, 1, 0.7)
fit <- morie_survvae(X, tt, ev, K = 2L, restarts = 2L)
morie_survvae_concordance(fit, X, tt, ev)
}
