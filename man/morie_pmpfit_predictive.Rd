% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_pmpfit_predictive}
\alias{morie_pmpfit_predictive}
\title{Pitman-Yor predictive weights}
\usage{
morie_pmpfit_predictive(counts, alpha, theta)
}
\arguments{
\item{counts}{Numeric vector of positive cluster counts.}
\item{alpha}{Discount.}
\item{theta}{Concentration.}
}
\description{
Each occupied cluster gets (n_j - alpha) / (theta + n); a new cluster gets (theta + K*alpha) / (theta + n). The mass removed by discounting every cluster by alpha is exactly the new-cluster term.
}
\value{
A list with `occupied`, `new`, `total`, `n`, `K`,   `discount_transferred`, `note`.
}
\keyword{internal}
