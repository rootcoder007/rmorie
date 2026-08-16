% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_reward_model_bt}
\alias{morie_alammar_reward_model_bt}
\title{morie_alammar_reward_model_bt}
\usage{
morie_alammar_reward_model_bt(scores_w, scores_l)
}
\arguments{
\item{scores_w,scores_l}{Winner and loser reward scores.}
}
\description{
morie_alammar_reward_model_bt
}
\value{
A list with \code{estimate}, \code{losses}, \code{pair_accuracy}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_alammar_reward_model_bt(V, V)
}
