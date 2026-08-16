% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.poissp_logdet_gen}
\alias{.poissp_logdet_gen}
\title{Generalised log-determinant: drop the rank_deficit smallest}
\usage{
.poissp_logdet_gen(A, rank_deficit = 0L)
}
\arguments{
\item{A}{See Usage.}
\item{rank_deficit}{Defaults to \code{0L}.}
}
\description{
|eigenvalues| Part of the poissp_native implementation; see the file header for the source it follows.
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
