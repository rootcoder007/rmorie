% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_soft_value_iteration}
\alias{morie_soft_value_iteration}
\title{MaxEnt (soft) value iteration on a deterministic tabular MDP}
\usage{
morie_soft_value_iteration(states, actions, step, reward, gamma = 0.9, iters = 2000L, tol = 1e-14) 
}
\arguments{
\item{states}{Iterable of states.}
\item{actions}{Iterable of actions.}
\item{step}{Function \code{(s, a) -> s\'}.}
\item{reward}{Function \code{s -> r(s)}.}
\item{gamma}{Discount.}
\item{iters}{Maximum number of value-iteration sweeps.}
\item{tol}{Convergence tolerance on the sup-norm of \code{V}.}
}
\description{
\code{Q(s, a) = r(s) + gamma V(s\')}, \code{V(s) = log sum_a exp Q(s, a)}, with the soft-optimal policy \code{pi(a | s) = exp(Q(s, a) - V(s))}. AIRL is derived in the maximum-entropy IRL setting, so this is the \code{V*} that Theorem C.1\'s \code{h* = V* + const} refers to. Provided so a caller -- and the anchors -- can compute the ground truth independently of anything AIRL fitted.
}
\value{
A list with \code{V} (named numeric vector keyed by state)   and \code{pi} (named numeric vector keyed by   \code{"<state>|<action>"}).
}
\keyword{internal}
