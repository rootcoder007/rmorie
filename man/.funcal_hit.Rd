% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.funcal_hit}
\alias{.funcal_hit}
\title{.funcal_hit}
\usage{
.funcal_hit(h)
}
\arguments{
\item{h}{Optional; may be \code{NULL}. A list; the body reads \code{$evalue}, \code{$query}, \code{$query_cov}, \code{$score}, \code{$target}, \code{$target_cov} from it.}
}
\description{
A step of the funcal_native implementation. Called by \code{morie_funcal}, \code{morie_funcal_seed_orthologs}. See the file header for the source the module follows.
}
\value{
A list with \code{query}, \code{target}, \code{evalue}, \code{score}, \code{query_cov}, \code{target_cov}.
}
\keyword{internal}
