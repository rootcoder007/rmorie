% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_ddpest_stick_breaking}
\alias{.morie_ddpest_stick_breaking}
\title{Sethuraman stick-breaking for the GEM(alpha) distribution:}
\usage{
.morie_ddpest_stick_breaking(alpha, K, e)
}
\arguments{
\item{alpha}{See Usage.}
\item{K}{See Usage.}
\item{e}{See Usage.}
}
\description{
w_1 = V_1, w_h = V_h * prod_{j<h}(1 - V_j), V_h ~ Beta(1, alpha). Drawn through the inverse CDF V = 1 - U^(1/alpha), consuming ONE uniform per stick from the shared generator `e`. This is the canonical construction used by the Python arm\\\'s sb.stick_breaking when it consumes from the GHC stream. NOTE: the Python source for slowdp.stick_breaking is not held here; the inverse-CDF form is the standard implementation and matches the GHC stream one-for-one.
}
\value{
A list with \code{weights}.
}
\keyword{internal}
