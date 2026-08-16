% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CPulseFeat}
\alias{CPulseFeat}
\title{Carotid pulse landmarks (Section 1.2.10) and the systolic time}
\usage{
CPulseFeat(cp, fs, qrs, hr = NULL)
}
\arguments{
\item{cp}{A vector; its length is taken.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{qrs}{Coerced to integer by the body, with \code{as.integer}.}
\item{hr}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
}
\description{
intervals of Section 4.9.  PEPC = PEP + 0.4 HR and ETC = ET + 1.6 HR are the rate corrections that make the intervals comparable between subjects.
}
\value{
A list with \code{upstroke}, \code{percussion}, \code{notch}, \code{dicwave}, \code{pep}, \code{et}, \code{pepc}, \code{etc}, \code{peppmean}, \code{etmean}, \code{pepcmean}, \code{etcmean}, \code{hr}, \code{fs}, \code{normpepc}, \code{normetcmale}, \code{normetcfemale}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(1)
fs <- 250
tv <- seq(0, 2, by = 1 / fs)
ph <- (tv \%\% 0.8) / 0.8
cp <- exp(-((ph - 0.25) / 0.12)^2) + 0.35 * exp(-((ph - 0.55) / 0.05)^2)
qrs <- round(seq(0.05, 1.95, by = 0.8) * fs)
r <- CPulseFeat(cp, fs = fs, qrs = qrs)
str(r, max.level = 1)
}
