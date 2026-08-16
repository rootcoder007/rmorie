% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_encoder_block}
\alias{.morie_gr_encoder_block}
\title{Shared post-layernorm encoder block. `W` is a list of}
\usage{
.morie_gr_encoder_block(X, W, n_heads, keep_attn = FALSE)
}
\arguments{
\item{X}{See Usage.}
\item{W}{See Usage.}
\item{n_heads}{See Usage.}
\item{keep_attn}{Defaults to \code{FALSE}.}
}
\description{
Wq/Wk/Wv/Wo/W1/b1/W2/b2 Part of the geron_ml_native implementation; see the file header for the source it follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
