% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr2_forward}
\alias{.morie_gr2_forward}
\title{Forward evaluation of the expression DAG. Python memoises on object}
\usage{
.morie_gr2_forward(node, env, st)
}
\arguments{
\item{node}{A vector; its length is taken and its elements indexed.}
\item{env}{A vector; indexed elementwise.}
\item{st}{A list; the body reads \code{$nodes} from it.}
}
\description{
IDENTITY (id(node)); R has no stable object identity for literals, so every occurrence becomes its own node here. For expressions written out literally -- which is every documented use -- the two agree, because distinct Python literals are distinct objects too.
}
\value{
The value of \code{length}.
}
\keyword{internal}
