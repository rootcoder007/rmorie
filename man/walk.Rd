% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{walk}
\alias{walk}
\title{walk}
\usage{
walk(adj, start, length, p = 1, q = 1, seed = 0, weights = NULL) 
}
\arguments{
\item{adj}{Passed to \code{transition_probabilities}.}
\item{start}{Coerced to character by the body, with \code{as.character}.}
\item{length}{Coerced to integer by the body, with \code{as.integer}.}
\item{p}{Passed to \code{transition_probabilities}. Defaults to \code{1}.}
\item{q}{Passed to \code{transition_probabilities}. Defaults to \code{1}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{weights}{Passed to \code{transition_probabilities}.}
}
\description{
A step of the node2v_native implementation. Called by \code{.avalon_paths}, \code{.depth_counts}, \code{.dmlqs_count_totters} and 13 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{path}, as built in the body.
}
\keyword{internal}
