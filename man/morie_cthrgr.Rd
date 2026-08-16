% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_cthrgr}
\alias{morie_cthrgr}
\title{Natural direct and indirect effects from three honest forests}
\usage{
morie_cthrgr(y, D, M, X, route = "gcomputed", n_trees = 8L, min_leaf = 3L, max_depth = 3L, seed = 0, n_draw = 8L, newX = NULL)
}
\arguments{
\item{y}{Outcome.}
\item{D}{Binary treatment.}
\item{M}{Mediator.}
\item{X}{Covariates.}
\item{route}{A member of the route list.}
\item{n_trees}{Trees per forest.}
\item{min_leaf}{Minimum leaf size.}
\item{max_depth}{Maximum depth.}
\item{seed}{The random stream.}
\item{n_draw}{Mediator residuals averaged over, for the g-computed   route. They are an evenly spaced sweep through the sorted   residuals rather than a resample, so the estimate is a fixed   function of the data and not of a random stream.}
\item{newX}{Points to report at, or NULL for the training rows.}
}
\description{
Natural direct and indirect effects from three honest forests
}
\value{
A list with per-point direct, indirect and total effects,   their averages, the fitted propensity and its overlap, and the   mediator models.
}
\references{
  Cui, Y. and Tchetgen Tchetgen, E.J. (2024) "Machine intelligence for
    individualized decision making under a counterfactual world: a
    rejoinder." Journal of the American Statistical Association
    119(545), 97-102.
  Pearl, J. (2001) "Direct and indirect effects." Proceedings of the
    Seventeenth Conference on Uncertainty in Artificial Intelligence,
    411-420.
  Imai, K., Keele, L. and Yamamoto, T. (2010) "Identification,
    inference and sensitivity analysis for causal mediation effects."
    Statistical Science 25(1), 51-71.
  Athey, S., Tibshirani, J. and Wager, S. (2019) "Generalized random
    forests." The Annals of Statistics 47(2), 1148-1178.
  VanderWeele, T.J. (2015) "Explanation in Causal Inference: Methods
    for Mediation and Interaction." Oxford University Press.
}
\keyword{internal}
\examples{
N <- 40L
X <- matrix(0, N, 2)
D <- vapply(0:(N - 1L), function(i) if (((i * 3)%%7) >= 3) 1L else 0L, 
    integer(1))
M <- vapply(0:(N - 1L), function(i) round(1 + 2 * (if (((i * 
    3)%%7) >= 3) 1 else 0) + 1.5 * (((i * 7)%%11)/10) + ((i * 
    11)%%5) * 0.2, 4), numeric(1))
Y <- vapply(0:(N - 1L), function(i) round(0.5 + 1.2 * M[i + 1L] + 
    0.9 * D[i + 1L] + 0.7 * (((i * 5)%%7)/6) + ((i * 13)%%7) * 
    0.1, 4), numeric(1))
morie_cthrgr(Y, D, M, X, "gcomputed", 6L, 3L, 3L, 5, 8L)
}
