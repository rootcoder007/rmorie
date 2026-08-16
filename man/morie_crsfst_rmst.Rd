% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_crsfst_rmst}
\alias{morie_crsfst_rmst}
\title{Restricted mean survival time: the area under a step curve}
\usage{
morie_crsfst_rmst(curve, tau)
}
\arguments{
\item{curve}{A curve from the Kaplan-Meier function.}
\item{tau}{The horizon.}
}
\description{
Exact. The curve is one until its first step and constant between steps, so the integral is a finite sum of rectangles and there is nothing to approximate. Anything past the horizon is cut off, which is what restricted means.
}
\value{
The restricted mean.
}
\references{
  Cui, Y., Kosorok, M.R., Sverdrup, E., Wager, S. and Zhu, R. (2023)
    "Estimating heterogeneous treatment effects with right-censored
    data via causal survival forests." Journal of the Royal
    Statistical Society Series B 85(2), 179-211.
    doi:10.1093/jrsssb/qkac001.
  Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen,
    C., Newey, W. and Robins, J. (2018) "Double/debiased machine
    learning for treatment and structural parameters." The
    Econometrics Journal 21(1), C1-C68.
  Royston, P. and Parmar, M.K.B. (2013) "Restricted mean survival
    time: an alternative to the hazard ratio." BMC Medical Research
    Methodology 13, 152.
  Uno, H. et al. (2014) "Moving beyond the hazard ratio in quantifying
    the clinical benefit-risk of therapies." Journal of Clinical
    Oncology 32(22), 2380-2385.
}
\keyword{internal}
