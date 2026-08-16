% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{explor}
\alias{explor}
\title{explor}
\usage{
explor(states, actions, next_states, n_actions = NULL, n_features = 8, eta = 1, beta = 0.2, lr = 0.05, epochs = 1, features = "inverse", discrete = TRUE, seed = 0)
}
\arguments{
\item{states}{See Usage.}
\item{actions}{See Usage.}
\item{next_states}{See Usage.}
\item{n_actions}{Defaults to \code{NULL}.}
\item{n_features}{Defaults to \code{8}.}
\item{eta}{Defaults to \code{1}.}
\item{beta}{Defaults to \code{0.2}.}
\item{lr}{Defaults to \code{0.05}.}
\item{epochs}{Defaults to \code{1}.}
\item{features}{Defaults to \code{"inverse"}.}
\item{discrete}{Defaults to \code{TRUE}.}
\item{seed}{Defaults to \code{0}.}
}
\description{
Part of the explor_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{payload}, as built in the body.
}
\keyword{internal}
\examples{
explor(states = c(1, 2, 3, 4, 5, 6, 7, 8), actions = c(1, 2, 3, 4, 5, 6, 7, 8), next_states = c(1, 2, 3, 4, 5, 6, 7, 8))
}
