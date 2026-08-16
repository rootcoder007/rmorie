% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sctraj_lineages_from_tree}
\alias{morie_sctraj_lineages_from_tree}
\title{Every path from root to a leaf, in order}
\usage{
morie_sctraj_lineages_from_tree(tree, root)
}
\arguments{
\item{tree}{A list; the body reads \code{$adjacency} from it.}
\item{root}{Coerced to character by the body, with \code{as.character}.}
}
\description{
A step of the sctraj_native implementation. Called by \code{morie_sctraj}. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\keyword{internal}
