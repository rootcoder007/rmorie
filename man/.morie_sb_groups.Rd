% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_sb_groups}
\alias{.morie_sb_groups}
\title{.morie_sb_groups}
\usage{
.morie_sb_groups(h, bins, cutoff, exact)
}
\arguments{
\item{h}{A vector; indexed elementwise.}
\item{bins}{Optional; may be \code{NULL}. A vector; its length is taken.}
\item{cutoff}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{exact}{A flag; the body branches on it.}
}
\description{
A step of the schaben_native implementation. Called by \code{morie_cressie_hawkins}, \code{morie_matheron_estimator}. See the file header for the source the module follows.
}
\value{
A list with \code{centres}, \code{idx}.
}
\keyword{internal}
\examples{
.morie_sb_groups(h = 0.5, bins = 5L, cutoff = 0.5, exact = c(1, 2, 3, 4, 5, 6, 7, 8))
}
