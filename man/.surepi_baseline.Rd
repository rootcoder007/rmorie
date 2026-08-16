% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.surepi_baseline}
\alias{.surepi_baseline}
\title{`t` is 0-based (Python convention); translate to 1-based R indices}
\usage{
.surepi_baseline(counts, t, lag, width, sigma_floor)
}
\arguments{
\item{counts}{A vector; indexed elementwise.}
\item{t}{Numeric; combined arithmetically in the body.}
\item{lag}{Numeric; combined arithmetically in the body.}
\item{width}{Numeric; combined arithmetically in the body.}
\item{sigma_floor}{See Usage.}
}
\description{
A step of the surepi_native implementation. Called by \code{.surepi_stat}. See the file header for the source the module follows.
}
\value{
A list with \code{m}, \code{s}, \code{used}.
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
