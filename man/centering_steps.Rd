% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{centering_steps}
\alias{centering_steps}
\title{Predicted centering steps}
\usage{
centering_steps(m, eps, t0, mu) 
}
\arguments{
\item{m}{Number of inequality constraints.}
\item{eps}{Duality-gap tolerance, positive.}
\item{t0}{Initial \eqn{t}, positive.}
\item{mu}{Growth factor, > 1.}
}
\description{
Boyd equation 11.13: \code{ceiling(log(m/(eps t0)) / log(mu))}, clamped to be non-negative, with a tiny subtraction \code{1e-12} before the ceiling so a perfectly integer value is not bumped up.
}
\value{
Non-negative integer.
}
\keyword{internal}
\examples{
centering_steps(m = 4, eps = 1e-8, t0 = 1, mu = 10)
}
