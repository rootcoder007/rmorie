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
\item{model}{A list; the body reads \code{$b}, \code{$b_prime}, \code{$V}, \code{$W}, \code{$W_prime} from it.}
\item{pos}{A vector; indexed elementwise.}
\item{u}{Coerced to integer by the body, with \code{as.integer}.}
\item{n_items}{Coerced to integer by the body, with \code{as.integer}.}
\item{top_k}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5L}.}
\item{activation}{Passed to \code{encode}. Defaults to \code{"sigmoid"}.}
}
\description{
A step of the cdaeRC_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{ranking}, \code{n_scored}.
}
\keyword{internal}
\examples{
set.seed(1)
pos <- list(c(0L, 1L), c(1L, 2L), c(0L, 2L), c(2L, 3L))
m <- fit_cdae(pos, n_users = 4, n_items = 4, k_dim = 3, iters = 5)
recommend(m, pos, u = 0L, n_items = 4, top_k = 2)
}
