% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{gibbs_bigram_topic}
\alias{gibbs_bigram_topic}
\title{gibbs_bigram_topic}
\usage{
gibbs_bigram_topic(docs, T, V, alpha = 0.5, beta = 0.5, m = NULL, n = NULL, prior = 1, iters = 200L, seed = 0L, burn = 50L)
}
\arguments{
\item{docs}{Iterated over elementwise, with \code{lapply}.}
\item{T}{Coerced to integer by the body, with \code{as.integer}.}
\item{V}{Coerced to integer by the body, with \code{as.integer}.}
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.5}.}
\item{beta}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.5}.}
\item{m}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{n}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{prior}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1}.}
\item{iters}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200L}.}
\item{seed}{Passed to \code{.lcg_uniform}. Defaults to \code{0L}.}
\item{burn}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{50L}.}
}
\description{
A step of the bigtm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{z}, \code{topic_posterior}, \code{theta}, \code{N_ijk}, \code{N_jk}, \code{T}, \code{V}, \code{prior}, \code{iterations}, \code{burn_in}, \code{samples_kept}, \code{method}, \code{caveat}.
}
\keyword{internal}
