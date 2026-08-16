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
\item{states}{Passed to \code{.mat}.}
\item{actions}{Passed to \code{.mat}.}
\item{next_states}{Passed to \code{.mat}.}
\item{n_actions}{Defaults to \code{NULL}.}
\item{n_features}{Defaults to \code{8}.}
\item{eta}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{beta}{Numeric; combined arithmetically in the body. Defaults to \code{0.2}.}
\item{lr}{Numeric; combined arithmetically in the body. Defaults to \code{0.05}.}
\item{epochs}{Defaults to \code{1}.}
\item{features}{Compared against \code{"identity"}. Defaults to \code{"inverse"}.}
\item{discrete}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{seed}{Defaults to \code{0}.}
}
\description{
A step of the explor_native implementation. Called by \code{morie_explor}. See the file header for the source the module follows.
}
\value{
The value of \code{payload}, as built in the body.
}
\keyword{internal}
\examples{
explor(states = c(1, 2, 3, 4, 5, 6, 7, 8), actions = c(1, 2, 3, 4, 5, 6, 7, 8), next_states = c(1, 2, 3, 4, 5, 6, 7, 8))
}
