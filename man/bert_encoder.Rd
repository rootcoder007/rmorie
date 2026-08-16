% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{bert_encoder}
\alias{bert_encoder}
\title{bert_encoder}
\usage{
bert_encoder(X, blocks, n_heads, pad_mask = NULL, pre_norm = FALSE)
}
\arguments{
\item{X}{See Usage.}
\item{blocks}{See Usage.}
\item{n_heads}{See Usage.}
\item{pad_mask}{Defaults to \code{NULL}.}
\item{pre_norm}{Defaults to \code{FALSE}.}
}
\description{
Part of the berte_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{output}, \code{attention}, \code{pooled}, \code{L}, \code{d}, \code{n_layers}, \code{n_heads}, \code{pre_norm}, \code{bidirectional}, \code{method}.
}
\keyword{internal}
