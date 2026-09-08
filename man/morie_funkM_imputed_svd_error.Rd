% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_funkM_imputed_svd_error}
\alias{morie_funkM_imputed_svd_error}
\title{What filling the holes and running an SVD actually gives}
\usage{
morie_funkM_imputed_svd_error(ratings, n_users, n_items, rank = 2, fill = "zero") 
}
\arguments{
\item{ratings}{Passed to \code{.funkM_as_ratings}.}
\item{n_users}{Coerced to integer by the body, with \code{as.integer}.}
\item{n_items}{Coerced to integer by the body, with \code{as.integer}.}
\item{rank}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2}.}
\item{fill}{Carried through into a list the body builds. Defaults to \code{"zero"}.}
}
\description{
A step of the funkM_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{rmse_on_observed}, \code{fill}, \code{rank}, \code{note}.
}
\references{
----------
Funk, S. (2006) "Netflix Update: Try This at Home",
https://sifter.org/~simon/journal/20061211.html. The original
description of the incremental, regularised gradient-descent
factorisation of the observed entries during the Netflix Prize.
NOTE: this is a blog post, not a peer-reviewed paper -- it is the
origin of the method and is cited as such; the published statements
of the same model are the two references below, and the
implementation follows those.

Koren, Y. (2008) "Factorization Meets the Neighborhood: a
Multifaceted Collaborative Filtering Model", *KDD \'08*, 426-434,
doi:10.1145/1401890.1401944. The baseline decomposition
b_ui = mu + b_u + b_i and the regularised squared-error objective
over the OBSERVED ratings, optimised by stochastic gradient descent.

Koren, Y., Bell, R. & Volinsky, C. (2009) "Matrix Factorization
Techniques for Recommender Systems", *Computer* 42(8), 30-37,
doi:10.1109/MC.2009.263. The statement that earlier work relying on
imputation to fill in the missing ratings is both expensive and prone
to distortion, and that modelling only the observed entries with
regularisation is preferable.

Salakhutdinov, R. & Mnih, A. (2008) "Probabilistic Matrix
Factorization", *NIPS 2007*, 1257-1264. The probabilistic reading of
the same objective.
}
\keyword{internal}
\examples{
ratings <- list(c(0, 0, 5), c(0, 1, 3), c(1, 0, 4), c(1, 2, 2),
                c(2, 1, 5), c(2, 2, 1))
e <- morie_funkM_imputed_svd_error(ratings, n_users = 3, n_items = 3,
                                   rank = 2, fill = "mean")
is.numeric(e) || is.list(e)
}
