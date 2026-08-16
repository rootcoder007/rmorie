% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rob_tukey_w}
\alias{.rob_tukey_w}
\title{W(u) = psi(u)/u: (1 - (u/c)^2)^2 inside, ZERO outside. The}
\usage{
.rob_tukey_w(u, cc)
}
\arguments{
\item{u}{See Usage.}
\item{cc}{See Usage.}
}
\description{
redescending part is what buys breakdown -- a gross outlier gets no vote at all, where Huber\\\'s psi still gives it a bounded one.
}
\value{
The value of \code{ifelse}.
}
\keyword{internal}
