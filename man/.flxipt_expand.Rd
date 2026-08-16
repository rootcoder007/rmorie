% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.flxipt_expand}
\alias{.flxipt_expand}
\title{Design expansion per learner kind ------------------------------}
\usage{
.flxipt_expand(W, spec)
}
\arguments{
\item{W}{A matrix; indexed by row and column.}
\item{spec}{A list; the body reads \code{$cols}, \code{$kind} from it.}
}
\description{
A step of the flxipt_native implementation. Called by \code{.flxipt_fit}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
