% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.wrd2v_cbow_step}
\alias{.wrd2v_cbow_step}
\title{CBOW: the projection is the MEAN of the context vectors}
\usage{
.wrd2v_cbow_step(st, ctx, c, size, V, lr)
}
\arguments{
\item{st}{A list; the body reads \code{$O}, \code{$W} from it.}
\item{ctx}{A vector; its length is taken.}
\item{c}{See Usage.}
\item{size}{A count; the body uses it as \code{matrix(...)}.}
\item{V}{See Usage.}
\item{lr}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the wrd2v_native implementation. Called by \code{morie_wrd2v_wrd2v}. See the file header for the source the module follows.
}
\value{
The value of \code{loss}, as built in the body.
}
\keyword{internal}
