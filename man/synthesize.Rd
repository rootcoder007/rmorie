% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{synthesize}
\alias{synthesize}
\title{synthesize}
\usage{
synthesize(target_predict, c, n_features, feature_values = NULL, k_max = NULL, k_min = 1L, conf_min = 0.8, iter_max = 1000L, rej_max = 10L, seed = 0)
}
\arguments{
\item{target_predict}{See Usage.}
\item{c}{Numeric; combined arithmetically in the body.}
\item{n_features}{A count; the body uses it as \code{seq_len(...)}.}
\item{feature_values}{Defaults to \code{NULL}.}
\item{k_max}{Defaults to \code{NULL}.}
\item{k_min}{Numeric; passed to \code{max}. Defaults to \code{1L}.}
\item{conf_min}{Defaults to \code{0.8}.}
\item{iter_max}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{1000L}.}
\item{rej_max}{Defaults to \code{10L}.}
\item{seed}{Defaults to \code{0}.}
}
\description{
A step of the memb_native implementation. Called by \code{morie_memb}. See the file header for the source the module follows.
}
\value{
Nothing; the function is called for its effect.
}
\keyword{internal}
