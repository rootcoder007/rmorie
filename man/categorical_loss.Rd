% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{categorical_loss}
\alias{categorical_loss}
\title{Cross-entropy of the target against the current distribution}
\usage{
categorical_loss(m, probs, eps = 1e-12)
}
\arguments{
\item{m}{Numeric vector: the projected target.}
\item{probs}{Numeric vector: the current distribution.}
\item{eps}{Floor on the log to keep \code{p = 0} finite.}
}
\description{
\code{-sum_i m_i * log(max(p_i, eps))}, the cross-entropy term of \code{D_KL(Phi T_hat Z || Z)}.
}
\value{
Scalar cross-entropy.
}
\references{
----------
Bellemare, M. G., Dabney, W. & Munos, R. (2017) "A Distributional
Perspective on Reinforcement Learning", ICML 2017 (arXiv:1707.06887)
eq. (7) and Algorithm 1. Rowland, M., Bellemare, M. G., Dabney, W.,
Munos, R. & Teh, Y. W. (2018) "An Analysis of Categorical
Distributional Reinforcement Learning", AISTATS 2018
(arXiv:1802.08163) contraction in Cramer distance.
}
\keyword{internal}
