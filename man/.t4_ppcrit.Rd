% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.t4_ppcrit}
\alias{.t4_ppcrit}
\title{Interpolated Dickey-Fuller critical values at sample size n}
\usage{
.t4_ppcrit(n, kind = "Z(t_alpha)")
}
\arguments{
\item{n}{See Usage.}
\item{kind}{Compared against \code{"Z(alpha)"}. Defaults to \code{"Z(t_alpha)"}.}
}
\description{
A step of the pptest implementation. Called by \code{Pptest}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
\examples{
.t4_ppcrit(n = 5L)
}
