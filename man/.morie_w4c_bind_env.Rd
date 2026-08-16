% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w4c_bind_env}
\alias{.morie_w4c_bind_env}
\title{.morie_w4c_bind_env}
\usage{
.morie_w4c_bind_env(env)
}
\arguments{
\item{env}{A list; the body reads \code{$reset}, \code{$step} from it.}
}
\description{
A step of the geron_w4c_native implementation. Called by \code{morie_geron_ppo}, \code{morie_geron_reinforcement_learning}. See the file header for the source the module follows.
}
\value{
A list with \code{reset}, \code{step}.
}
\keyword{internal}
