% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tldapar}
\alias{morie_tldapar}
\title{morie_tldapar}
\usage{
morie_tldapar(n, V = 10L, seed = 0L, define_on_training = NULL, estimate_on_holdout = NULL, fold_estimates = NULL, fold_ics = NULL, effect = NULL, screen = NULL, reuse_fn = NULL, mode = c("split", "combine", "vimp", "reuse"))
}
\arguments{
\item{n}{See Usage.}
\item{V}{Defaults to \code{10L}.}
\item{seed}{Defaults to \code{0L}.}
\item{define_on_training}{Defaults to \code{NULL}.}
\item{estimate_on_holdout}{Defaults to \code{NULL}.}
\item{fold_estimates}{Defaults to \code{NULL}.}
\item{fold_ics}{Defaults to \code{NULL}.}
\item{effect}{Defaults to \code{NULL}.}
\item{screen}{Defaults to \code{NULL}.}
\item{reuse_fn}{Defaults to \code{NULL}.}
\item{mode}{Defaults to \code{c("split", "combine", "vimp", "reuse")}.}
}
\description{
Part of the tldapar_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{variable_importance}.
}
\keyword{internal}
