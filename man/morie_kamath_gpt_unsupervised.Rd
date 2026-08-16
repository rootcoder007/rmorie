% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_gpt_unsupervised}
\alias{morie_kamath_gpt_unsupervised}
\title{GPT objectives and MoE (Eq 2.34-2.41)}
\usage{
morie_kamath_gpt_unsupervised(U, k = NULL, Theta = NULL)
}
\arguments{
\item{U}{Per-token probabilities.}
\item{k}{Context size (recorded).}
\item{Theta}{Unused, kept for the signature.}
}
\description{
GPT objectives and MoE (Eq 2.34-2.41)
}
\value{
A list with \code{estimate}, \code{cross_entropy}, \code{context_size}, \code{n}, \code{method}.
}
\keyword{internal}
