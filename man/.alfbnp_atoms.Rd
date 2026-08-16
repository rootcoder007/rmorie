% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.alfbnp_atoms}
\alias{.alfbnp_atoms}
\title{.alfbnp_atoms}
\usage{
.alfbnp_atoms(x, what)
}
\arguments{
\item{x}{A matrix; passed to \code{as.matrix}.}
\item{what}{Passed to \code{sprintf}.}
}
\description{
A step of the alfbnp_native implementation. Called by \code{.alfbnp_clean_list}, \code{morie_alfbnp_af3_sample}. See the file header for the source the module follows.
}
\value{
The value of \code{m}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.alfbnp_atoms(V, V)
}
