% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{surepi_ears_detect}
\alias{surepi_ears_detect}
\title{surepi_ears_detect}
\usage{
surepi_ears_detect(counts, method = "C2", threshold = 3, sigma_floor = 1)
}
\arguments{
\item{counts}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{method}{Compared against \code{"C3"}. Defaults to \code{"C2"}.}
\item{threshold}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{3}.}
\item{sigma_floor}{Passed to \code{.surepi_stat}. Defaults to \code{1}.}
}
\description{
A step of the surepi_native implementation. Called by \code{surepi_c1_mild}, \code{surepi_c2_medium}, \code{surepi_c3_ultra}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{statistic}, \code{flag}, \code{n_flagged}, \code{method}, \code{threshold}, \code{baseline_lag}, \code{baseline_width}, \code{sigma_floor}, \code{n}, \code{n_evaluable}, \code{reference}, \code{caveat}.
}
\references{
----------
Hutwagner, L., Thompson, W., Seeman, G. M. & Treadwell, T. (2003)
"The Bioterrorism Preparedness and Response Early Aberration
Reporting System (EARS)", Journal of Urban Health: Bulletin of the
New York Academy of Medicine 80(2, Supplement 1), i89-i96.

Hutwagner, L. C., Maloney, E. K., Bean, N. H., Slutsker, L. & Martin,
S. M. (1997) "Using laboratory-based surveillance data for
prevention: an algorithm for detecting Salmonella outbreaks",
Emerging Infectious Diseases 3(3), 395-400.
}
\keyword{internal}
