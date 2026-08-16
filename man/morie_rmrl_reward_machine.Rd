% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rmrl_reward_machine}
\alias{morie_rmrl_reward_machine}
\title{A simple reward machine <U, u0, delta_u, delta_r> (Defs 3.1-3.2)}
\usage{
morie_rmrl_reward_machine(edges, u0 = 0, terminal = c())
}
\arguments{
\item{edges}{See Usage.}
\item{u0}{Defaults to \code{0}.}
\item{terminal}{Defaults to \code{c()}.}
}
\description{
Edges are list(u, formula, u_next, reward). formula is either the string "true" or list(positive, negative) of proposition names. Edges are tested in the order given and the first match wins; if none matches, the machine stays in u and pays 0.
}
\value{
The value of \code{m}, as built in the body.
}
\keyword{internal}
