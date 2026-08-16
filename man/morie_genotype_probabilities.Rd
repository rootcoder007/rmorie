% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_genotype_probabilities}
\alias{morie_genotype_probabilities}
\title{QTL genotype probabilities from flanking markers}
\usage{
morie_genotype_probabilities(left, right, r_left, r_right)
}
\arguments{
\item{left}{Numeric vector of left-marker genotypes (0 or 1).}
\item{right}{Numeric vector of right-marker genotypes (0 or 1).}
\item{r_left}{Recombination fraction on the left interval,   in \code{[0, 0.5]}.}
\item{r_right}{Recombination fraction on the right interval,   in \code{[0, 0.5]}.}
}
\description{
Backcross coding, 0 or 1 at every locus, no interference, so the two intervals contribute independently and the result is the normalised product of the two recombination probabilities.
}
\value{
Numeric vector of length 2, \code{c(G(0), G(1))}.
}
\keyword{internal}
