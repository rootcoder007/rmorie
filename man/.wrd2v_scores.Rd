% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.wrd2v_scores}
\alias{.wrd2v_scores}
\title{.wrd2v_scores}
\usage{
.wrd2v_scores(O, h)
}
\arguments{
\item{O}{A matrix; passed to \code{\%*\%}.}
\item{h}{A matrix; passed to \code{\%*\%}.}
}
\description{
A step of the wrd2v_native implementation. Called by \code{.wrd2v_cbow_step}, \code{.wrd2v_sg_step}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
