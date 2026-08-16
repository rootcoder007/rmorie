% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{lyapunov_exponent}
\alias{lyapunov_exponent}
\title{lyapunov_exponent}
\usage{
lyapunov_exponent(y, embedding = NULL, tau = NULL, dt = 1, fit = NULL, min_sep = NULL, max_steps = NULL, method = "rosenstein", k = NULL)
}
\arguments{
\item{y}{Passed to \code{divergence_curve}.}
\item{embedding}{Passed to \code{divergence_curve}.}
\item{tau}{Passed to \code{divergence_curve}.}
\item{dt}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{fit}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
\item{min_sep}{Passed to \code{divergence_curve}.}
\item{max_steps}{Passed to \code{divergence_curve}.}
\item{method}{One of \code{"rosenstein"}, \code{"sato"}, \code{"sato_k"}. Defaults to \code{"rosenstein"}.}
\item{k}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
}
\description{
A step of the lyapun_native implementation. Called by \code{morie_lyapun}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{lambda1}, \code{rosenstein}, \code{sato}, \code{sato_k}, \code{sato_k_curve}, \code{se}, \code{r_squared}, \code{intercept}, \code{time}, \code{log_divergence}, \code{log_ratio}, \code{n_pairs}, \code{fit_range}, \code{k}, \code{m}, \code{tau}, \code{min_sep}, \code{n_points}, \code{n}, \code{dt}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
r0 <- 3.9; x <- numeric(400); x[1] <- 0.4
for (i in 2:400) x[i] <- r0 * x[i - 1] * (1 - x[i - 1])
r <- lyapunov_exponent(x, embedding = 3, tau = 1)
str(r, max.level = 1)
}
