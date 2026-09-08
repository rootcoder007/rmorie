% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{double_q_target}
\alias{double_q_target}
\title{Double-DQN target}
\usage{
double_q_target(reward, gamma, q_online_next, q_target_next, done = FALSE) 
}
\arguments{
\item{reward}{Scalar immediate reward.}
\item{gamma}{Scalar discount factor.}
\item{q_online_next}{Numeric vector Q_online(s\', .).}
\item{q_target_next}{Numeric vector Q_target(s\', .).}
\item{done}{Logical; if \code{TRUE}, the target is the immediate   reward alone (terminal state).}
}
\description{
Computes the Double-DQN target: the online net\'s argmax selects the action, and the target net values that action. When \code{done} is \code{TRUE} the target collapses to the immediate reward.
}
\value{
Scalar target value.
}
\keyword{internal}
\examples{
double_q_target(reward = c(1, 2, 3, 4, 5, 6, 7, 8), gamma = 0.5,
  q_online_next = c(1, 2, 3, 4, 5, 6, 7, 8), q_target_next = c(1, 2, 3, 4, 5, 6, 7, 8))
}
