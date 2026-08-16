% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{multi_head_attention}
\alias{multi_head_attention}
\title{multi_head_attention}
\usage{
multi_head_attention(Q, K, V, n_heads, pad_mask = NULL, causal = FALSE)
}
\arguments{
\item{Q}{A matrix; passed to \code{nrow}.}
\item{K}{Passed to \code{attention_weights}.}
\item{V}{A matrix; indexed by row and column.}
\item{n_heads}{Numeric; combined arithmetically in the body.}
\item{pad_mask}{Passed to \code{attention_weights}.}
\item{causal}{Passed to \code{attention_weights}. Defaults to \code{FALSE}.}
}
\description{
A step of the berte_native implementation. Called by \code{encoder_block}. See the file header for the source the module follows.
}
\value{
A list with \code{out}, \code{weights}.
}
\keyword{internal}
