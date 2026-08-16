% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Pcf}
\alias{Pcf}
\title{R(h) = K\\'(h) / (2 h pi), Sec. 3.4.1, with Khat of Sec. 3.4.2 and the}
\usage{
Pcf(points, region = NULL, r = NULL, correction = "border")
}
\arguments{
\item{points}{See Usage.}
\item{region}{Defaults to \code{NULL}.}
\item{r}{Defaults to \code{NULL}.}
\item{correction}{Defaults to \code{"border"}.}
}
\description{
intensity of eq (3.8).  The naive estimator is NEGATIVELY BIASED (the book says so outright), hence the border correction by default.
}
\value{
A list with \code{r}, \code{k}, \code{pcf}, \code{lambda}, \code{area}, \code{csr_k}, \code{csr_pcf_is_one}, \code{correction}, \code{n}, \code{method}.
}
\keyword{internal}
