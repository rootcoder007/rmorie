% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.counts}
\alias{.counts}
\title{.counts}
\usage{
.counts(D, Tn, Vn, z)
}
\arguments{
\item{D}{A vector; its length is taken and its elements indexed.}
\item{Tn}{A count; the body uses it as \code{numeric(...)}.}
\item{Vn}{Accepted by the signature and not used anywhere in the body.}
\item{z}{A vector; indexed elementwise.}
}
\description{
A step of the bigtm_native implementation. Called by \code{.log_evidence}, \code{gibbs_bigram_topic}. See the file header for the source the module follows.
}
\value{
A list with \code{N_ijk}, \code{N_jk}, \code{N_kd}, \code{N_d}.
}
\keyword{internal}
