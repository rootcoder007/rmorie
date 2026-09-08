% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_funkM}
\alias{morie_funkM}
\title{morie_funkM}
\usage{
morie_funkM(ratings, n_users, n_items, factors = 8, epochs = 60, lr = 0.005, 
    reg = 0.02, seed = 0, incremental = FALSE, epochs_per_factor = 20) 
}
\arguments{
\item{ratings}{Passed to \code{.funkM_as_ratings}.}
\item{n_users}{Coerced to integer by the body, with \code{as.integer}.}
\item{n_items}{Coerced to integer by the body, with \code{as.integer}.}
\item{factors}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{8}.}
\item{epochs}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{60}.}
\item{lr}{Passed to \code{.funkM_sgd_epoch}. Defaults to \code{0.005}.}
\item{reg}{Passed to \code{.funkM_sgd_epoch}. Defaults to \code{0.02}.}
\item{seed}{Passed to \code{.ghc_rng}. Defaults to \code{0}.}
\item{incremental}{A flag; the body branches on it. Defaults to \code{FALSE}.}
\item{epochs_per_factor}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{20}.}
}
\description{
A step of the funkM_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{result}, as built in the body.
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
fit <- morie_funkM(ratings, n_users = 3, n_items = 3, factors = 2,
                   epochs = 30, seed = 1)
round(fit$rmse, 4)
}
