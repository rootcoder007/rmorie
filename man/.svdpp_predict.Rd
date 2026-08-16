% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.svdpp_predict}
\alias{.svdpp_predict}
\title{.svdpp_predict}
\usage{
.svdpp_predict(mu, b_user, b_item, p_u, q_i, rated_items = NULL, y = NULL, exponent = -0.5)
}
\arguments{
\item{mu}{See Usage.}
\item{b_user}{See Usage.}
\item{b_item}{See Usage.}
\item{p_u}{See Usage.}
\item{q_i}{See Usage.}
\item{rated_items}{Defaults to \code{NULL}.}
\item{y}{Defaults to \code{NULL}.}
\item{exponent}{Defaults to \code{-0.5}.}
}
\description{
Part of the svdpp_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{prediction}, \code{effective_user_factor}, \code{implicit}, \code{n_rated}, \code{note}.
}
\references{
----------
Koren, Y. (2008) "Factorization Meets the Neighborhood: a
Multifaceted Collaborative Filtering Model", KDD \\'08, 426-434,
doi:10.1145/1401890.1401944. Sec. 1 and 4, eq. (15).

Koren, Y., Bell, R. & Volinsky, C. (2009) "Matrix Factorization
Techniques for Recommender Systems", Computer 42(8), 30-37,
doi:10.1109/MC.2009.263. The baseline decomposition mu + b_u + b_i.

Hu, Y., Koren, Y. & Volinsky, C. (2008) "Collaborative Filtering for
Implicit Feedback Datasets", ICDM 2008, 263-272,
doi:10.1109/ICDM.2008.22. The purely implicit alternative.
}
\keyword{internal}
