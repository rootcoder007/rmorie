% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{sixtuple_distance}
\alias{sixtuple_distance}
\title{sixtuple_distance}
\usage{
sixtuple_distance(seqs)
}
\arguments{
\item{seqs}{A vector; its length is taken.}
}
\description{
A step of the mafft_native implementation. Called by \code{iterative_refine}, \code{mafft_alignment}, \code{morie_mafft} and 1 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{D}, as built in the body.
}
\keyword{internal}
\examples{
seqs <- c("MKVLA", "MKVLG", "MRVLA")
sixtuple_distance(seqs)
}
