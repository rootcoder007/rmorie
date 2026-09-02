% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mqtmpl_sample_genotypes}
\alias{morie_mqtmpl_sample_genotypes}
\title{Draw pseudomarker genotypes from p(g | m)}
\usage{
morie_mqtmpl_sample_genotypes(genotypes, positions, grid, n_imp = 16, error_rate = 0, seed = 0) 
}
\arguments{
\item{genotypes,positions,error_rate}{As above.}
\item{grid}{Grid positions.}
\item{n_imp}{Number of imputations.}
\item{seed}{Seed for the shared generator.}
}
\description{
Draw pseudomarker genotypes from p(g | m)
}
\value{
A list of draws.
}
\keyword{internal}
\examples{
set.seed(1)
g <- lapply(1:6, function(i) c(rbinom(1, 1, 0.5), NA, rbinom(1, 1, 0.5)))
pos <- c(0, 0.1, 0.2)
r <- morie_mqtmpl_sample_genotypes(g, pos, grid = pos, n_imp = 4)
str(r, max.level = 1)
}
