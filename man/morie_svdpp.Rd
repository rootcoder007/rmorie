% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_svdpp}
\alias{morie_svdpp}
\title{morie_svdpp}
\usage{
morie_svdpp(ratings, n_users, n_items, factors = 4, epochs = 30, lr = 0.007, reg = 0.015, exponent = -0.5, seed = 0, implicit = TRUE)
}
\arguments{
\item{ratings}{Passed to \code{.svdpp_fit}.}
\item{n_users}{Passed to \code{.svdpp_fit}.}
\item{n_items}{Passed to \code{.svdpp_fit}.}
\item{factors}{Passed to \code{.svdpp_fit}. Defaults to \code{4}.}
\item{epochs}{Passed to \code{.svdpp_fit}. Defaults to \code{30}.}
\item{lr}{Passed to \code{.svdpp_fit}. Defaults to \code{0.007}.}
\item{reg}{Passed to \code{.svdpp_fit}. Defaults to \code{0.015}.}
\item{exponent}{Passed to \code{.svdpp_fit}. Defaults to \code{-0.5}.}
\item{seed}{Passed to \code{.svdpp_fit}. Defaults to \code{0}.}
\item{implicit}{Passed to \code{.svdpp_fit}. Defaults to \code{TRUE}.}
}
\description{
A step of the svdpp_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.svdpp_fit}.
}
\references{
----------
Koren, Y. (2008) "Factorization Meets the Neighborhood: a
Multifaceted Collaborative Filtering Model", KDD \'08, 426-434,
doi:10.1145/1401890.1401944. Sec. 1 and 4, eq. (15).

Koren, Y., Bell, R. & Volinsky, C. (2009) "Matrix Factorization
Techniques for Recommender Systems", Computer 42(8), 30-37,
doi:10.1109/MC.2009.263. The baseline decomposition mu + b_u + b_i.

Hu, Y., Koren, Y. & Volinsky, C. (2008) "Collaborative Filtering for
Implicit Feedback Datasets", ICDM 2008, 263-272,
doi:10.1109/ICDM.2008.22. The purely implicit alternative.
}
\keyword{internal}
