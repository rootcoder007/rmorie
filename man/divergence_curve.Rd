% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{divergence_curve}
\alias{divergence_curve}
\title{divergence_curve}
\usage{
divergence_curve(y, m = NULL, tau = NULL, dt = 1, min_sep = NULL, max_steps = NULL)
}
\arguments{
\item{y}{A vector; its length is taken.}
\item{m}{Optional; may be \code{NULL}. Passed to \code{morie_lyapun_embed}.}
\item{tau}{Optional; may be \code{NULL}. Passed to \code{morie_lyapun_embed}.}
\item{dt}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{min_sep}{Optional; may be \code{NULL}. Passed to \code{.lyapun_nearest_neighbours}.}
\item{max_steps}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
}
\description{
A step of the lyapun_native implementation. Called by \code{lyapunov_exponent}, \code{morie_lyapun}. See the file header for the source the module follows.
}
\value{
A list with \code{time}, \code{log_divergence}, \code{log_ratio}, \code{n_pairs}, \code{neighbour}, \code{d0}, \code{points}, \code{m}, \code{tau}, \code{min_sep}, \code{n_points}, \code{n_obs}.
}
\keyword{internal}
\examples{
r0 <- 3.9; x <- numeric(300); x[1] <- 0.4
for (i in 2:300) x[i] <- r0 * x[i - 1] * (1 - x[i - 1])
str(divergence_curve(x, m = 3, tau = 1), max.level = 1)
}
