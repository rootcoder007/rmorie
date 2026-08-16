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
\item{h0}{See Usage.}
\item{adj}{See Usage.}
\item{T}{Defaults to \code{3L}.}
\item{W}{Defaults to \code{NULL}.}
\item{activation}{Defaults to \code{"relu"}.}
\item{exclude_reverse}{Defaults to \code{TRUE}.}
}
\description{
Part of the dmlqs_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{edge_states}, \code{T}, \code{excluded_reverse}, \code{note}.
}
\keyword{internal}
