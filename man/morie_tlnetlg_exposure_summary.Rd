% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tlnetlg_exposure_summary}
\alias{morie_tlnetlg_exposure_summary}
\title{morie_tlnetlg_exposure_summary}
\usage{
morie_tlnetlg_exposure_summary(A, friends, kind = "fraction")
}
\arguments{
\item{A}{Passed to \code{.tlnetlg_vec}.}
\item{friends}{A vector; its length is taken and its elements indexed.}
\item{kind}{One of \code{"any"}, \code{"count"}, \code{"fraction"}. Defaults to \code{"fraction"}.}
}
\description{
A step of the tlnetlg_native implementation. Called by \code{morie_tlnetlg_community_estimand}. See the file header for the source the module follows.
}
\value{
A list with \code{summary}, \code{kind}, \code{note}.
}
\references{
----------
van der Laan, M. J. & Rose, S. (2018) *Targeted Learning in Data
Science*, Springer, doi:10.1007/978-3-319-65304-4. Chap. 20 (Sofrygin
& van der Laan): existing causal inference assumes n independent,
causally unconnected units, so causal models need only describe
relations within a unit and inference rests on n independent
realisations; in many cluster randomized trials or observational
studies of few communities the number of independent units is not
large enough for limit-distribution inference; the extreme case of a
single community of causally connected individuals and whether a
community-level intervention\'s effect on a community-level outcome can
still be evaluated; and the requirement that causal models incorporate
all units and that identifiability be established under minimal
assumptions WITHOUT relying on asymptotics in a number of independent
units. Chap. 21 (the restriction of dependence to the known network
and the condition that |F_i|/N vanishes).

Sofrygin, O. & van der Laan, M. J. (2017) "Semi-Parametric Estimation
and Inference for the Mean Outcome of the Single Time-Point
Intervention in a Causally Connected Population", *Journal of Causal
Inference* 5(1), 20160003, doi:10.1515/jci-2016-0003.

Ogburn, E. L. & VanderWeele, T. J. (2014) "Causal Diagrams for
Interference", *Statistical Science* 29(4), 559-578,
doi:10.1214/14-STS501.
"""
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_tlnetlg_exposure_summary(V, V)
}
