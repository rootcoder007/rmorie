% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.alfqud_score}
\alias{.alfqud_score}
\title{.alfqud_score}
\usage{
.alfqud_score(program, inputs, targets, n_reg, lw, rf)
}
\arguments{
\item{program}{A vector; its length is taken.}
\item{inputs}{Passed to \code{morie_alfqud_correctness}.}
\item{targets}{Passed to \code{morie_alfqud_correctness}.}
\item{n_reg}{Passed to \code{morie_alfqud_correctness}.}
\item{lw}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{rf}{See Usage.}
}
\description{
A step of the alfqud_native implementation. Called by \code{.alfqud_bfs}, \code{.alfqud_mcts}. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
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
\examples{
.alfqud_score(program = c(1, 2, 3, 4, 5, 6, 7, 8), inputs = c(1, 2, 3, 4, 5, 6, 7, 8), targets = c(1, 2, 3, 4, 5, 6, 7, 8), n_reg = c(1, 2, 3, 4, 5, 6, 7, 8), lw = c(1, 2, 3, 4, 5, 6, 7, 8), rf = c(1, 2, 3, 4, 5, 6, 7, 8))
}
