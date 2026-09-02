% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{surepi_salmonella_cusum}
\alias{surepi_salmonella_cusum}
\title{surepi_salmonella_cusum}
\usage{
surepi_salmonella_cusum(counts, mu0, sigma, k_shift = 1, decision = 0.5, min_count = 5) 
}
\arguments{
\item{counts}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{mu0}{A vector; its length is taken.}
\item{sigma}{A vector; its length is taken.}
\item{k_shift}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{decision}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.5}.}
\item{min_count}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{5}.}
}
\description{
A step of the surepi_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{cusum}, \code{flag}, \code{estimate}, \code{n_flagged}, \code{decision}, \code{k}, \code{min_count}, \code{method}.
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
\examples{
surepi_salmonella_cusum(counts = c(1, 2, 3, 4, 5, 6, 7, 8), mu0 = c(1, 2, 3, 4, 5, 6, 7, 8), sigma = 0.5)
}
