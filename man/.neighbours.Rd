% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.neighbours}
\alias{.neighbours}
\title{.neighbours}
\usage{
.neighbours(n_atoms, bonds)
}
\arguments{
\item{n_atoms}{A count; the body uses it as \code{seq_len(...)}.}
\item{bonds}{See Usage.}
}
\description{
A step of the toptor_native implementation. Called by \code{topological_torsions}. See the file header for the source the module follows.
}
\value{
The value of \code{adj}, as built in the body.
}
\keyword{internal}
\examples{
.neighbours(n_atoms = 5L, bonds = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)))
}
