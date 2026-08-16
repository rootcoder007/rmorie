% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pmfsc_dist}
\alias{.pmfsc_dist}
\title{.pmfsc_dist}
\usage{
.pmfsc_dist(a, b)
}
\arguments{
\item{a}{See Usage.}
\item{b}{See Usage.}
}
\description{
Part of the pmfsc_native implementation; see the file header for the source it follows.
}
\value{
A numeric value.
}
\references{
  Muegge, I. and Martin, Y.C. (1999) "A general and fast scoring
    function for protein-ligand interactions: a simplified potential
    approach." Journal of Medicinal Chemistry 42(5), 791-804.
    doi:10.1021/jm980536j.
  Muegge, I. (2001) "Effect of ligand volume correction on PMF
    scoring." Journal of Computational Chemistry 22(4), 418-425.
  Muegge, I. (2006) "PMF scoring revisited." Journal of Medicinal
    Chemistry 49(20), 5895-5902.
  Sippl, M.J. (1990) "Calculation of conformational ensembles from
    potentials of mean force." Journal of Molecular Biology 213(4),
    859-883.

A note on what is sourced. The functional form and the role of each
factor above are from the papers. The particular realisation of the
volume correction as an excluded-volume ratio, and the capping rule
for unobserved bins, are this module\\'s -- they are described in the
sources in words rather than given as formulas, so they are named here
as choices rather than attributed.
}
\keyword{internal}
