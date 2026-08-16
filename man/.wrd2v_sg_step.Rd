% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.wrd2v_sg_step}
\alias{.wrd2v_sg_step}
\title{Skip-gram: input is the centre word, target a context word}
\usage{
.wrd2v_sg_step(st, c, j, size, V, lr)
}
\arguments{
\item{st}{A list; the body reads \code{$O}, \code{$W} from it.}
\item{c}{See Usage.}
\item{j}{See Usage.}
\item{size}{See Usage.}
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
