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
\item{eval_in}{A vector; indexed elementwise.}
\item{eval_out}{A vector; indexed elementwise.}
\item{train_fn}{Defaults to \code{NULL}.}
\item{attack_train_fn}{Defaults to \code{NULL}.}
\item{n_shadow}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{sort_features}{A flag; the body branches on it. Defaults to \code{FALSE}.}
\item{threshold}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.5}.}
}
\description{
A step of the memb_native implementation. Called by \code{morie_memb}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{metrics}, \code{per_class}, \code{predictions}, \code{scores}, \code{truth}, \code{n_shadow}, \code{attack_train_size}, \code{attack_classes}, \code{threshold}, \code{note}, \code{method}.
}
\keyword{internal}
