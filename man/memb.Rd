% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{memb}
\alias{memb}
\title{memb}
\usage{
memb(target_predict, shadow_data, eval_in, eval_out, train_fn = NULL, attack_train_fn = NULL, n_shadow = NULL, sort_features = FALSE, threshold = 0.5)
}
\arguments{
\item{target_predict}{See Usage.}
\item{shadow_data}{See Usage.}
\item{eval_in}{See Usage.}
\item{eval_out}{See Usage.}
\item{train_fn}{Defaults to \code{NULL}.}
\item{attack_train_fn}{Defaults to \code{NULL}.}
\item{n_shadow}{Defaults to \code{NULL}.}
\item{sort_features}{Defaults to \code{FALSE}.}
\item{threshold}{Defaults to \code{0.5}.}
}
\description{
Part of the memb_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{metrics}, \code{per_class}, \code{predictions}, \code{scores}, \code{truth}, \code{n_shadow}, \code{attack_train_size}, \code{attack_classes}, \code{threshold}, \code{note}, \code{method}.
}
\keyword{internal}
