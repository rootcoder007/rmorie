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
\item{inputs}{Passed to \code{.alfqud_score}.}
\item{targets}{Passed to \code{.alfqud_score}.}
\item{acts}{A vector; its length is taken and its elements indexed.}
\item{n_reg}{Passed to \code{.alfqud_score}.}
\item{max_len}{Coerced to integer by the body, with \code{as.integer}.}
\item{lw}{Passed to \code{.alfqud_score}.}
\item{rf}{Passed to \code{.alfqud_score}.}
\item{n_sim}{Coerced to integer by the body, with \code{as.integer}.}
\item{c_puct}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the alfqud_native implementation. Called by \code{morie_alfqud}. See the file header for the source the module follows.
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
