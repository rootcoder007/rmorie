% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.svdpp_sgd_step}
\alias{.svdpp_sgd_step}
\title{.svdpp_sgd_step}
\usage{
.svdpp_sgd_step(rating, mu, b_user, b_item, p_u, q_i, rated_items, y, lr = 0.007, reg = 0.015, exponent = -0.5)
}
\arguments{
\item{rating}{See Usage.}
\item{mu}{Passed to \code{.svdpp_predict}.}
\item{b_user}{Passed to \code{.svdpp_predict}.}
\item{b_item}{Passed to \code{.svdpp_predict}.}
\item{p_u}{Passed to \code{.svdpp_predict}.}
\item{q_i}{Passed to \code{.svdpp_predict}.}
\item{rated_items}{A vector; its length is taken.}
\item{y}{A vector; indexed elementwise.}
\item{lr}{Defaults to \code{0.007}.}
\item{reg}{Defaults to \code{0.015}.}
\item{exponent}{Passed to \code{.svdpp_predict}. Defaults to \code{-0.5}.}
}
\description{
A step of the svdpp_native implementation. Called by \code{.svdpp_fit}. See the file header for the source the module follows.
}
\value{
A list with \code{error}, \code{b_user}, \code{b_item}, \code{p_u}, \code{q_i}, \code{y}, \code{note}.
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
