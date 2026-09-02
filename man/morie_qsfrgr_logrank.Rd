% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_qsfrgr_logrank}
\alias{morie_qsfrgr_logrank}
\title{The log-rank statistic between two groups of row indices}
\usage{
morie_qsfrgr_logrank(time, event, left, right) 
}
\arguments{
\item{time}{Observed times.}
\item{event}{Event indicators.}
\item{left}{Row indices of the left group.}
\item{right}{Row indices of the right group.}
}
\description{
The squared standardised difference between observed and expected deaths in the left group, which is the quantity a survival tree maximises. Zero when the groups are indistinguishable and when neither can contribute a comparison.
}
\value{
The statistic.
}
\references{
  Cui, Y., Kosorok, M.R., Sverdrup, E., Wager, S. and Zhu, R. (2023)
    "Estimating heterogeneous treatment effects with right-censored
    data via causal survival forests." Journal of the Royal
    Statistical Society Series B 85(2), 179-211.
    doi:10.1093/jrsssb/qkac001.
  Wager, S. and Athey, S. (2018) "Estimation and inference of
    heterogeneous treatment effects using random forests." Journal of
    the American Statistical Association 113(523), 1228-1242.
  Athey, S., Tibshirani, J. and Wager, S. (2019) "Generalized random
    forests." The Annals of Statistics 47(2), 1148-1178.
  Ishwaran, H., Kogalur, U.B., Blackstone, E.H. and Lauer, M.S. (2008)
    "Random survival forests." The Annals of Applied Statistics 2(3),
    841-860.
  Kaplan, E.L. and Meier, P. (1958) "Nonparametric estimation from
    incomplete observations." Journal of the American Statistical
    Association 53(282), 457-481.
  Meinshausen, N. (2006) "Quantile regression forests." Journal of
    Machine Learning Research 7, 983-999.
}
\keyword{internal}
\examples{
morie_qsfrgr_logrank(time = c(1, 2, 3, 4, 5, 6, 7, 8), event = c(0, 1, 0, 1, 1, 0, 1, 0), left = c(1, 2, 3, 4, 5, 6, 7, 8), right = c(1, 2, 3, 4, 5, 6, 7, 8))
}
