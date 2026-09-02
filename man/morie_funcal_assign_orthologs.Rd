% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_funcal_assign_orthologs}
\alias{morie_funcal_assign_orthologs}
\title{morie_funcal_assign_orthologs}
\usage{
morie_funcal_assign_orthologs(seeds, groups, taxa = NULL, target_taxa = NULL, target_types = NULL) 
}
\arguments{
\item{seeds}{A vector; indexed elementwise.}
\item{groups}{A vector; indexed elementwise.}
\item{taxa}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
\item{target_taxa}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{target_types}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
}
\description{
A step of the funcal_native implementation. Called by \code{morie_funcal}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_funcal_assign_orthologs(V, V)
}
