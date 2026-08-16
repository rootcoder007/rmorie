% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{recommend}
\alias{recommend}
\title{recommend}
\usage{
recommend(model, pos, u, n_items, top_k = 5L, activation = "sigmoid")
}
\arguments{
\item{model}{See Usage.}
\item{pos}{See Usage.}
\item{u}{See Usage.}
\item{n_items}{See Usage.}
\item{top_k}{Defaults to \code{5L}.}
\item{activation}{Defaults to \code{"sigmoid"}.}
}
\description{
Part of the cdaeRC_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{ranking}, \code{n_scored}.
}
\keyword{internal}
