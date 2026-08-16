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
\item{Q}{See Usage.}
\item{K}{See Usage.}
\item{V}{See Usage.}
\item{n_heads}{See Usage.}
\item{pad_mask}{Defaults to \code{NULL}.}
\item{causal}{Defaults to \code{FALSE}.}
}
\description{
Part of the berte_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{out}, \code{weights}.
}
\keyword{internal}
