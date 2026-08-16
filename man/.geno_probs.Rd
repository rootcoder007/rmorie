% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.geno_probs}
\alias{.geno_probs}
\title{Genotype probabilities at a test position}
\usage{
.geno_probs(left, right, r_left, r_right)
}
\arguments{
\item{left}{Numeric vector of left marker genotype probabilities   over \code{{AA, Aa, aa}}.}
\item{right}{Same for the right marker.}
\item{r_left}{Recombination fraction to the left marker.}
\item{r_right}{Recombination fraction to the right marker.}
}
\description{
Conditional genotype probabilities given the flanking marker scores and the recombination fractions to each flank. Returns \code{c(p(AA), p(Aa))} for one individual under the F2-like three-genotype model.
}
\value{
Numeric vector of length 2, \code{c(P(Q=AA), P(Q=Aa))}.
}
\keyword{internal}
