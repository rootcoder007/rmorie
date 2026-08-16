% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_cdaeRC}
\alias{morie_cdaeRC}
\title{morie_cdaeRC}
\usage{
morie_cdaeRC(pos, n_users, n_items, k_dim = 8L, q = 0.2, alpha = 0.05, lam = 0.01, iters = 30L, n_neg = 5L, seed = 0, activation = "sigmoid", init_scale = 0.1)
}
\arguments{
\item{pos}{See Usage.}
\item{n_users}{See Usage.}
\item{n_items}{See Usage.}
\item{k_dim}{Defaults to \code{8L}.}
\item{q}{Defaults to \code{0.2}.}
\item{alpha}{Defaults to \code{0.05}.}
\item{lam}{Defaults to \code{0.01}.}
\item{iters}{Defaults to \code{30L}.}
\item{n_neg}{Defaults to \code{5L}.}
\item{seed}{Defaults to \code{0}.}
\item{activation}{Defaults to \code{"sigmoid"}.}
\item{init_scale}{Defaults to \code{0.1}.}
}
\description{
A step of the cdaeRC_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{fit_cdae}.
}
\keyword{internal}
