% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.poissp_fit_mode}
\alias{.poissp_fit_mode}
\title{Joint Newton-Raphson for (beta, u) at fixed (tau, rho)}
\usage{
.poissp_fit_mode(y, X, off, Q, constrain, iters, tol, ridge)
}
\arguments{
\item{y}{A vector; its length is taken.}
\item{X}{A matrix; indexed by row and column.}
\item{off}{Numeric; passed to \code{sum}.}
\item{Q}{A matrix; passed to \code{\%*\%}.}
\item{constrain}{A flag; the body branches on it.}
\item{iters}{Coerced to integer by the body, with \code{as.integer}.}
\item{tol}{See Usage.}
\item{ridge}{Passed to \code{.poissp_ridgesolve}.}
}
\description{
A step of the poissp_native implementation. Called by \code{morie_poissp}. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{u}, \code{m}, \code{eta}.
}
\references{
Banerjee, S., Carlin, B. P. and Gelfand, A. E. (2014) Hierarchical
Modeling and Analysis for Spatial Data, 2nd edn, Monographs on
Statistics and Applied Probability 135, Chapman & Hall/CRC, Boca Raton,
ISBN 978-1-4398-1917-3 -- Ch. 4 (proper and intrinsic CAR) and Ch. 6
(Poisson-CAR disease mapping).
Besag, J., York, J. and Mollie, A. (1991) "Bayesian image restoration,
with two applications in spatial statistics", Annals of the Institute
of Statistical Mathematics 43(1), 1-20, doi:10.1007/BF00116466.
}
\keyword{internal}
