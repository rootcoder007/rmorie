% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.poissp_laplace}
\alias{.poissp_laplace}
\title{.poissp_laplace}
\usage{
.poissp_laplace(y, u, m, Q, constrain)
}
\arguments{
\item{y}{A vector; its length is taken.}
\item{u}{A matrix; passed to \code{\%*\%}.}
\item{m}{A matrix; passed to \code{diag}.}
\item{Q}{A matrix; passed to \code{\%*\%}.}
\item{constrain}{A flag; the body branches on it.}
}
\description{
A step of the poissp_native implementation. Called by \code{morie_poissp}. See the file header for the source the module follows.
}
\value{
A numeric value.
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
\examples{
.poissp_laplace(y = c(1, 2, 3, 4, 5, 6, 7, 8), u = c(1, 2, 3, 4, 5, 6, 7, 8), m = 5L, Q = 0.5, constrain = c(1, 2, 3, 4, 5, 6, 7, 8))
}
