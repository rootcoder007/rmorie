% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{fit_cdae}
\alias{fit_cdae}
\title{fit_cdae}
\usage{
fit_cdae(pos, n_users, n_items, k_dim = 8L, q = 0.2, alpha = 0.05, lam = 0.01, iters = 30L, n_neg = 5L, seed = 0, activation = "sigmoid", init_scale = 0.1)
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
Part of the cdaeRC_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{W}, \code{W_prime}, \code{V}, \code{b}, \code{b_prime}, \code{loss_history}, \code{final_loss}, \code{k}, \code{q}, \code{n_neg}, \code{activation}, \code{method}, \code{note}.
}
\keyword{internal}
