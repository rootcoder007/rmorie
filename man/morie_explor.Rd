% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_explor}
\alias{morie_explor}
\title{Morie entry point}
\usage{
morie_explor(states, actions, next_states, n_actions = NULL, n_features = 8, eta = 1, beta = 0.2, lr = 0.05, epochs = 1, features = "inverse", discrete = TRUE, seed = 0)
}
\arguments{
\item{states}{Passed to \code{explor}.}
\item{actions}{Passed to \code{explor}.}
\item{next_states}{Passed to \code{explor}.}
\item{n_actions}{Passed to \code{explor}.}
\item{n_features}{Passed to \code{explor}. Defaults to \code{8}.}
\item{eta}{Passed to \code{explor}. Defaults to \code{1}.}
\item{beta}{Passed to \code{explor}. Defaults to \code{0.2}.}
\item{lr}{Passed to \code{explor}. Defaults to \code{0.05}.}
\item{epochs}{Passed to \code{explor}. Defaults to \code{1}.}
\item{features}{Passed to \code{explor}. Defaults to \code{"inverse"}.}
\item{discrete}{Passed to \code{explor}. Defaults to \code{TRUE}.}
\item{seed}{Passed to \code{explor}. Defaults to \code{0}.}
}
\description{
A step of the explor_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{explor}.
}
\keyword{internal}
\examples{
morie_explor(states = c(1, 2, 3, 4, 5, 6, 7, 8), actions = c(1, 2, 3, 4, 5, 6, 7, 8), next_states = c(1, 2, 3, 4, 5, 6, 7, 8))
}
