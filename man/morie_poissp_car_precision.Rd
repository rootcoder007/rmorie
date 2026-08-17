% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_poissp_car_precision}
\alias{morie_poissp_car_precision}
\title{The proper-CAR precision Q = tau (D_w - rho W).}
\usage{
morie_poissp_car_precision(W, tau = 1, rho = 1)
}
\description{
The proper-CAR precision Q = tau (D_w - rho W).
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
W <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
morie_poissp_car_precision(W, tau = 1.0, rho = 0.5)
}
