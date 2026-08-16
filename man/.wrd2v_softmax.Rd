% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.wrd2v_softmax}
\alias{.wrd2v_softmax}
\title{.wrd2v_softmax}
\usage{
.wrd2v_softmax(v)
}
\arguments{
\item{v}{Numeric; passed to \code{max}.}
}
\description{
A step of the wrd2v_native implementation. Called by \code{.wrd2v_cbow_step}, \code{.wrd2v_sg_step}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
