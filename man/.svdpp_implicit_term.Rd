% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.svdpp_implicit_term}
\alias{.svdpp_implicit_term}
\title{.svdpp_implicit_term}
\usage{
.svdpp_implicit_term(rated_items, y, exponent = -0.5)
}
\arguments{
\item{rated_items}{Coerced to list by the body, with \code{as.list}.}
\item{y}{A vector; its length is taken and its elements indexed.}
\item{exponent}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{-0.5}.}
}
\description{
A step of the svdpp_native implementation. Called by \code{.svdpp_predict}. See the file header for the source the module follows.
}
\value{
A list with \code{term}, \code{n_rated}, \code{scale}, \code{exponent}, \code{raw_sum}.
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
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.svdpp_implicit_term(V, D)
}
