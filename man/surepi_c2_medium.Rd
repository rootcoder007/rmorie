% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{surepi_c2_medium}
\alias{surepi_c2_medium}
\title{surepi_c2_medium}
\usage{
surepi_c2_medium(counts, threshold = 3, sigma_floor = 1)
}
\arguments{
\item{counts}{Passed to \code{surepi_ears_detect}.}
\item{threshold}{Passed to \code{surepi_ears_detect}. Defaults to \code{3}.}
\item{sigma_floor}{Passed to \code{surepi_ears_detect}. Defaults to \code{1}.}
}
\description{
A step of the surepi_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{surepi_ears_detect}.
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
