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
\item{qrs}{See Usage.}
\item{hr}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
}
\description{
intervals of Section 4.9.  PEPC = PEP + 0.4 HR and ETC = ET + 1.6 HR are the rate corrections that make the intervals comparable between subjects.
}
\value{
A list with \code{upstroke}, \code{percussion}, \code{notch}, \code{dicwave}, \code{pep}, \code{et}, \code{pepc}, \code{etc}, \code{peppmean}, \code{etmean}, \code{pepcmean}, \code{etcmean}, \code{hr}, \code{fs}, \code{normpepc}, \code{normetcmale}, \code{normetcfemale}, \code{method}.
}
\keyword{internal}
