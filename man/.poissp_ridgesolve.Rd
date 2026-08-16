% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.poissp_ridgesolve}
\alias{.poissp_ridgesolve}
\title{.poissp_ridgesolve}
\usage{
.poissp_ridgesolve(A, b, ridge = 1e-10)
}
\arguments{
\item{A}{A matrix; passed to \code{nrow}.}
\item{b}{A matrix; passed to \code{solve}.}
\item{ridge}{Numeric; combined arithmetically in the body. Defaults to \code{1e-10}.}
}
\description{
A step of the poissp_native implementation. Called by \code{.poissp_fit_mode}, \code{morie_poissp}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
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
