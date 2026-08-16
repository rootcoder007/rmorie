% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sccsno_loglik}
\alias{morie_sccsno_loglik}
\title{The conditional log-likelihood of Sec. 3. params is}
\usage{
morie_sccsno_loglik(params, cells_by_person, n_risk, n_age)
}
\arguments{
\item{params}{A vector; indexed elementwise.}
\item{cells_by_person}{See Usage.}
\item{n_risk}{A count; the body uses it as \code{seq_len(...)}.}
\item{n_age}{Numeric; combined arithmetically in the body.}
}
\description{
(beta_1..beta_s, alpha_1..alpha_{m-1}) with beta_0 = alpha_0 = 0. The individual effects phi_i do not appear -- that is the point.
}
\value{
The value of \code{ll}, as built in the body.
}
\references{
----------
Farrington, C. P. (1995) "Relative Incidence Estimation from Case
Series for Vaccine Safety Evaluation", Biometrics 51(1), 228-235.
JSTOR stable URL https://www.jstor.org/stable/2533328. Secs. 2-3.

Whitaker, H. J., Farrington, C. P., Spiessens, B. & Musonda, P.
(2006) "Tutorial in biostatistics: The self-controlled case series
method", Statistics in Medicine 25, 1768-1797, doi:10.1002/sim.2302.
}
\keyword{internal}
