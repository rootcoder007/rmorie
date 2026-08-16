% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ddpest_dependence_kind}
\alias{morie_ddpest_dependence_kind}
\title{What varies with x, and what that buys}
\usage{
morie_ddpest_dependence_kind(kind)
}
\arguments{
\item{kind}{One of "single_weights", "single_atoms", "both",   "independent".}
}
\description{
Returns a description of which part of the stick-breaking form G_x(.) = sum_h w_h(x) delta_{theta_h(x)}(.) is allowed to vary with the covariate, and the qualitative effect that has on clustering.
}
\value{
Named list with kind, varies_with_x, effect.
}
\keyword{internal}
