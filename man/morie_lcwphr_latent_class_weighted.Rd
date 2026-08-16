% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_lcwphr_latent_class_weighted}
\alias{morie_lcwphr_latent_class_weighted}
\title{morie_lcwphr_latent_class_weighted}
\usage{
morie_lcwphr_latent_class_weighted(y, A, H, K, trim = 0, stabilize = TRUE, max_iter = 500L, tol = 1e-11)
}
\arguments{
\item{y}{See Usage.}
\item{A}{See Usage.}
\item{H}{Passed to \code{.lcwphr_rows}.}
\item{K}{A count; the body uses it as \code{seq_len(...)}.}
\item{trim}{Defaults to \code{0}.}
\item{stabilize}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{max_iter}{Defaults to \code{500L}.}
\item{tol}{Numeric; combined arithmetically in the body. Defaults to \code{1e-11}.}
}
\description{
A step of the lcwphr_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{class_ate}, \code{class_mean_treated}, \code{class_mean_control}, \code{naive_class_ate}, \code{naive_class_mean_treated}, \code{naive_class_mean_control}, \code{ate}, \code{naive_ate}, \code{marginal_ate}, \code{unweighted_ate}, \code{class_prevalence}, \code{item_probabilities}, \code{posterior}, \code{labels}, \code{propensity}, \code{propensity_coefficients}, \code{weights}, \code{effective_sample_size}, \code{weight_max}, \code{weight_mean}, \code{loglik}, \code{loglik_path}, \code{bic}, \code{entropy}, \code{n_parameters}, \code{iterations}, \code{converged}, \code{K}, \code{n}, \code{Q}, \code{stabilized}, \code{trim}, \code{method}, \code{note}.
}
\keyword{internal}
