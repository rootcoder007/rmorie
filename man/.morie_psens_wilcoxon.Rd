% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_psens_wilcoxon}
\alias{.morie_psens_wilcoxon}
\title{.morie_psens_wilcoxon}
\usage{
.morie_psens_wilcoxon(treated, control, gamma)
}
\arguments{
\item{treated}{A vector; its length is taken.}
\item{control}{A vector; its length is taken.}
\item{gamma}{Passed to \code{.morie_psens_wilcoxon_d}.}
}
\description{
Rosenbaum sensitivity bounds for the Wilcoxon signed-rank statistic (Rosenbaum 2002, ch. 4), the same quantity rbounds::psens() reports. Pairs are formed positionally from equal-length treated/control vectors, matching how psens() was called here.
}
\value{
The value of \code{.morie_psens_wilcoxon_d}.
}
\keyword{internal}
\examples{
.morie_psens_wilcoxon(treated = list(a = 1, b = 2), control = list(a = 1, b = 2), gamma = 1)
}
