% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_vepan_consequence_rank}
\alias{morie_vepan_consequence_rank}
\title{Severity rank of an SO term; 1 is worst. Unknown terms sort last}
\usage{
morie_vepan_consequence_rank(term)
}
\arguments{
\item{term}{See Usage.}
}
\description{
A step of the vepan_native implementation. Called by \code{.vepan_pick_key}, \code{morie_vepan_annotate}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
morie_vepan_consequence_rank(term = 5L)
}
