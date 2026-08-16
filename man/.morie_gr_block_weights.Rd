% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_block_weights}
\alias{.morie_gr_block_weights}
\title{.morie_gr_block_weights}
\usage{
.morie_gr_block_weights(d_model, d_ff, seed)
}
\arguments{
\item{d_model}{A count; the body uses it as \code{numeric(...)}.}
\item{d_ff}{A count; the body uses it as \code{numeric(...)}.}
\item{seed}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the geron_ml_native implementation. Called by \code{morie_geron_albert}, \code{morie_geron_bert}, \code{morie_geron_roberta}. See the file header for the source the module follows.
}
\value{
A list with \code{Wq}, \code{Wk}, \code{Wv}, \code{Wo}, \code{W1}, \code{b1}, \code{W2}, \code{b2}.
}
\keyword{internal}
