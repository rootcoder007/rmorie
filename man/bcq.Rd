% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{bcq}
\alias{bcq}
\title{bcq}
\usage{
bcq(dataset, states = NULL, actions = NULL, tau = 0.3, gamma = 0.99, lr = 0.5, iters = 2000, loss = "huber", huber_c = 1, behavior = NULL, tol = 1e-12)
}
\arguments{
\item{dataset}{See Usage.}
\item{states}{Defaults to \code{NULL}.}
\item{actions}{Defaults to \code{NULL}.}
\item{tau}{Defaults to \code{0.3}.}
\item{gamma}{Defaults to \code{0.99}.}
\item{lr}{Defaults to \code{0.5}.}
\item{iters}{Defaults to \code{2000}.}
\item{loss}{Defaults to \code{"huber"}.}
\item{huber_c}{Defaults to \code{1}.}
\item{behavior}{Defaults to \code{NULL}.}
\item{tol}{Defaults to \code{1e-12}.}
}
\description{
Part of the bcq_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{q}, \code{policy}, \code{allowed}, \code{behavior}, \code{value}, \code{n_eliminated}, \code{bellman_error}, \code{tau}, \code{gamma}, \code{n_transitions}, \code{method}.
}
\keyword{internal}
