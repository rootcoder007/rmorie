% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_km2_tox_scores}
\alias{.morie_km2_tox_scores}
\title{.morie_km2_tox_scores}
\usage{
.morie_km2_tox_scores(Yhat, c, name = "Yhat")
}
\arguments{
\item{Yhat}{Coerced to list by the body, with \code{as.list}.}
\item{c}{A function; the body checks with \code{is.function}.}
\item{name}{Passed to \code{sprintf}. Defaults to \code{"Yhat"}.}
}
\description{
A step of the kamath_llm2_native implementation. Called by \code{morie_kamath_ch6_emt_metric}, \code{morie_kamath_ch6_toxic_fraction}, \code{morie_kamath_ch6_toxicity_probability}. See the file header for the source the module follows.
}
\value{
The value of \code{list}.
}
\keyword{internal}
\examples{
A <- matrix(c(1, 0, 2, 1), 2)
B <- diag(2)
x <- c(1, 2, 3, 4)
y <- morie_kamath_ch4_krona_efficient(A, B, x)$y
morie_kamath_ch5_reward_loss_pairwise(function(x, y) c(a = 2, 
    b = 0.5)[[y]], list("p1", "p2"), list("a", "b"), list("b", 
    "a"), c(0, 1))
.morie_km2_tox_scores(Yhat = A, c = B)
}
