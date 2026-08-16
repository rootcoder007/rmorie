% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gpordlatent}
\alias{.gpordlatent}
\title{Chapter 7: ordinal latent-scale predictors, eqs (7.3)-(7.5)}
\usage{
.gpordlatent(n, X_E = NULL, X = NULL, X_EM = NULL, Z_L = NULL, L_g = NULL)
}
\arguments{
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{X_E}{Defaults to \code{NULL}.}
\item{X}{Defaults to \code{NULL}.}
\item{X_EM}{Defaults to \code{NULL}.}
\item{Z_L}{Optional; may be \code{NULL}. Passed to \code{.gpmat}.}
\item{L_g}{Optional; may be \code{NULL}. Passed to \code{.gpmat}.}
}
\description{
---------- Part of the helpers_gp_core implementation; see the file header for the source it follows.
}
\value{
A list with \code{design}, \code{widths}, \code{n_columns}.
}
\keyword{internal}
