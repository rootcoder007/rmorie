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
\item{mu}{Passed to \code{.svdpp_baseline}.}
\item{b_user}{Passed to \code{.svdpp_baseline}.}
\item{b_item}{Passed to \code{.svdpp_baseline}.}
\item{p_u}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{q_i}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{rated_items}{Optional; may be \code{NULL}. A vector; its length is taken.}
\item{y}{Optional; may be \code{NULL}. A vector; its length is taken.}
\item{exponent}{Passed to \code{.svdpp_implicit_term}. Defaults to \code{-0.5}.}
}
\description{
A step of the svdpp_native implementation. Called by \code{.svdpp_sgd_step}. See the file header for the source the module follows.
}
\value{
A list with \code{prediction}, \code{effective_user_factor}, \code{implicit}, \code{n_rated}, \code{note}.
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
\examples{
.svdpp_predict(mu = c(1, 2, 3, 4, 5, 6, 7, 8), b_user = c(1, 2, 3, 4, 5, 6, 7, 8), b_item = c(1, 2, 3, 4, 5, 6, 7, 8), p_u = c(1, 2, 3, 4, 5, 6, 7, 8), q_i = c(1, 2, 3, 4, 5, 6, 7, 8))
}
