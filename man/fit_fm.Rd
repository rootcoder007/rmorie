% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{fit_fm}
\alias{fit_fm}
\title{Least-squares FM by stochastic gradient descent ----------------}
\usage{
fit_fm(X, y, k_dim = 4, iters = 300, alpha = 0.02, lam = 0.01, seed = 0)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{y}{See Usage.}
\item{k_dim}{Defaults to \code{4}.}
\item{iters}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{300}.}
\item{alpha}{Defaults to \code{0.02}.}
\item{lam}{Defaults to \code{0.01}.}
\item{seed}{Defaults to \code{0}.}
}
\description{
A step of the fmFM_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{w0}, \code{w}, \code{V}, \code{mse_history}, \code{final_mse}, \code{k}, \code{n_features}, \code{method}.
}
\keyword{internal}
