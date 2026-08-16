% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.alfqud_mcts}
\alias{.alfqud_mcts}
\title{.alfqud_mcts}
\usage{
.alfqud_mcts(inputs, targets, acts, n_reg, max_len, lw, rf, n_sim, c_puct)
}
\arguments{
\item{inputs}{See Usage.}
\item{targets}{See Usage.}
\item{acts}{See Usage.}
\item{n_reg}{See Usage.}
\item{max_len}{See Usage.}
\item{lw}{See Usage.}
\item{rf}{See Usage.}
\item{n_sim}{See Usage.}
\item{c_puct}{See Usage.}
}
\description{
Part of the alfqud_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{prog}, \code{s}, \code{c}, \code{seen}.
}
\references{
  Mankowitz, D.J. et al. (2023) "Faster sorting algorithms discovered
    using deep reinforcement learning." Nature 618, 257-263.
    doi:10.1038/s41586-023-06004-9.
  Silver, D. et al. (2017) "Mastering the game of Go without human
    knowledge." Nature 550, 354-359. The PUCT selection rule.
  Rosin, C.D. (2011) "Multi-armed bandits with episode context."
    Annals of Mathematics and Artificial Intelligence 61(3), 203-230.
}
\keyword{internal}
