% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_crsfmr_cross_dimension_stage}
\alias{morie_crsfmr_cross_dimension_stage}
\title{Cross-dimension stage through a router}
\usage{
morie_crsfmr_cross_dimension_stage(Z, router = NULL, n_router = NULL) 
}
\arguments{
\item{Z}{A length-L list of length-D lists of d_model vectors.}
\item{router}{Optional list of router vectors; defaults to the   first \code{c} dimensions\' vectors at each time step.}
\item{n_router}{Integer, number of router vectors.}
}
\description{
The gather-then-broadcast construction of Zhang & Yan (2023) Sec. 3.2: a small set of \code{c} vectors first gathers from all dimensions, then broadcasts back, at cost \code{O(cD)} rather than the \code{O(D^2)} of direct all-pairs attention.
}
\value{
A 3D array indexed as \code{out[[i]][[d]]}.
}
\keyword{internal}
