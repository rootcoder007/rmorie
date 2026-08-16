% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dmlqs_message_pass}
\alias{.dmlqs_message_pass}
\title{.dmlqs_message_pass}
\usage{
.dmlqs_message_pass(h0, adj, T = 3L, W = NULL, activation = "relu", exclude_reverse = TRUE)
}
\arguments{
\item{h0}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
\item{adj}{Passed to \code{.dmlqs_norm_adj}.}
\item{T}{Defaults to \code{3L}.}
\item{W}{Defaults to \code{NULL}.}
\item{activation}{Defaults to \code{"relu"}.}
\item{exclude_reverse}{Defaults to \code{TRUE}.}
}
\description{
A step of the dmlqs_native implementation. Called by \code{morie_dmlqs}. See the file header for the source the module follows.
}
\value{
A list with \code{edge_states}, \code{T}, \code{excluded_reverse}, \code{note}.
}
\keyword{internal}
