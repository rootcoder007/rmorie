% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_cdaeRC}
\alias{morie_cdaeRC}
\title{morie_cdaeRC}
\usage{
morie_cdaeRC(pos, n_users, n_items, k_dim = 8L, q = 0.2, alpha = 0.05, lam = 0.01, 
    iters = 30L, n_neg = 5L, seed = 0, activation = "sigmoid", init_scale = 0.1) 
}
\arguments{
\item{pos}{Passed to \code{fit_cdae}.}
\item{n_users}{Passed to \code{fit_cdae}.}
\item{n_items}{Passed to \code{fit_cdae}.}
\item{k_dim}{Passed to \code{fit_cdae}. Defaults to \code{8L}.}
\item{q}{Passed to \code{fit_cdae}. Defaults to \code{0.2}.}
\item{alpha}{Passed to \code{fit_cdae}. Defaults to \code{0.05}.}
\item{lam}{Passed to \code{fit_cdae}. Defaults to \code{0.01}.}
\item{iters}{Passed to \code{fit_cdae}. Defaults to \code{30L}.}
\item{n_neg}{Passed to \code{fit_cdae}. Defaults to \code{5L}.}
\item{seed}{Passed to \code{fit_cdae}. Defaults to \code{0}.}
\item{activation}{Passed to \code{fit_cdae}. Defaults to \code{"sigmoid"}.}
\item{init_scale}{Passed to \code{fit_cdae}. Defaults to \code{0.1}.}
}
\description{
A step of the cdaeRC_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{fit_cdae}.
}
\keyword{internal}
\examples{
set.seed(1)
pos <- list(c(0L, 1L), c(1L, 2L), c(0L, 2L), c(2L, 3L))
r <- morie_cdaeRC(pos, n_users = 4, n_items = 4, k_dim = 3, iters = 5)
str(r, max.level = 1)
}
