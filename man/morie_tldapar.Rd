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
\item{n}{Passed to \code{split_sample}.}
\item{V}{Passed to \code{split_sample}. Defaults to \code{10L}.}
\item{seed}{Passed to \code{split_sample}. Defaults to \code{0L}.}
\item{define_on_training}{Accepted by the signature and not used anywhere in the body.}
\item{estimate_on_holdout}{Accepted by the signature and not used anywhere in the body.}
\item{fold_estimates}{Passed to \code{cv_tmle}.}
\item{fold_ics}{Passed to \code{cv_tmle}.}
\item{effect}{Passed to \code{variable_importance}.}
\item{screen}{Passed to \code{variable_importance}.}
\item{reuse_fn}{Passed to \code{naive_reuse}.}
\item{mode}{One of \code{"combine"}, \code{"reuse"}, \code{"split"}.}
}
\description{
A step of the tldapar_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{variable_importance}.
}
\keyword{internal}
\examples{
str(morie_tldapar(30, V = 5, mode = "split"), max.level = 1)
}
