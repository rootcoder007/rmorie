% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_sdcfst_logistic}
\alias{morie_sdcfst_logistic}
\title{Ridge-penalised logistic regression by Newton-Raphson}
\usage{
morie_sdcfst_logistic(X, z, rows, ridge = 1e-06, iters = 50L)
}
\arguments{
\item{X}{Covariate matrix.}
\item{z}{Binary response.}
\item{rows}{Rows to fit on.}
\item{ridge}{Ridge penalty.}
\item{iters}{Maximum Newton steps.}
}
\description{
The ridge term is not a modelling flourish: with a small fold and a separating covariate the unpenalised likelihood has no maximum, the Hessian goes singular, and the two arms would fail in different places. A fixed tiny ridge makes the problem well posed in both.
}
\value{
The coefficient vector, intercept first.
}
\references{
  Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen,
    C., Newey, W. and Robins, J. (2018) "Double/debiased machine
    learning for treatment and structural parameters." The
    Econometrics Journal 21(1), C1-C68.
  Robinson, P.M. (1988) "Root-N-consistent semiparametric regression."
    Econometrica 56(4), 931-954.
  Robins, J.M., Rotnitzky, A. and Zhao, L.P. (1994) JASA 89(427),
    846-866.
  Wager, S. and Athey, S. (2018) "Estimation and inference of
    heterogeneous treatment effects using random forests." JASA
    113(523), 1228-1242.
}
\keyword{internal}
\examples{
N <- 120L
ii <- 0:(N - 1L)
x1 <- sin(1.7 * ii)
x2 <- cos(0.9 * ii) + 0.4 * sin(0.31 * ii)
x3 <- ((ii\%\%5L) - 2)/2
X <- cbind(x1, x2, x3)
lin <- 0.9 * x1 - 0.7 * x2 + 0.5 * x3
D <- ifelse(lin + 0.35 * sin(4.1 * ii) > 0, 1, 0)
morie_sdcfst_logistic(X, D, seq_len(N))
}
