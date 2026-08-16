% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mtdrl_history_features}
\alias{morie_mtdrl_history_features}
\title{History feature vector}
\usage{
morie_mtdrl_history_features(history, n_arms)
}
\arguments{
\item{history}{List of (action, reward) pairs so far.}
\item{n_arms}{Number of arms.}
}
\description{
History feature vector
}
\value{
Numeric feature vector of length n_arms + 2: one-hot   previous action, previous reward, step index.
}
\keyword{internal}
