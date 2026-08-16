% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{dueling_step}
\alias{dueling_step}
\title{One dueling + Double-DQN update}
\usage{
dueling_step(value, advantage, action, reward, gamma, next_value, next_advantage, next_target_value, next_target_advantage, mode = "mean", done = FALSE)
}
\arguments{
\item{value}{Scalar V(s).}
\item{advantage}{Numeric vector A(s, .).}
\item{action}{0-based action index taken in s.}
\item{reward}{Scalar immediate reward.}
\item{gamma}{Scalar discount factor.}
\item{next_value}{Scalar V_online(s\').}
\item{next_advantage}{Numeric vector A_online(s\', .).}
\item{next_target_value}{Scalar V_target(s\').}
\item{next_target_advantage}{Numeric vector A_target(s\', .).}
\item{mode}{Aggregation mode: \code{"mean"} (eq. 9), \code{"max"}   (eq. 8) or \code{"naive"}.}
\item{done}{Logical; if \code{TRUE}, next-state values are dropped   and the target collapses to the immediate reward.}
}
\description{
Aggregates the current state, aggregates the next state under both the online and the target net, builds the Double-DQN target, and returns the TD residual, the greedy action and the decomposition.
}
\value{
Named list with keys matching the Python RichResult payload:   \code{estimate}, \code{td_error}, \code{q}, \code{q_taken},   \code{target}, \code{greedy_action}, \code{value}, \code{advantage},   \code{mode}, \code{n_actions}, \code{method}.
}
\keyword{internal}
