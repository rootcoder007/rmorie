% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_alfqud_correctness}
\alias{morie_alfqud_correctness}
\title{How many memory slots end up holding the value they should}
\usage{
morie_alfqud_correctness(program, inputs, targets, n_reg) 
}
\arguments{
\item{program}{A list of instructions.}
\item{inputs}{The test inputs.}
\item{targets}{What each input should become.}
\item{n_reg}{How many registers.}
}
\description{
Summed over the test inputs, so a program that sorts two of three cases scores strictly between one that sorts none and one that sorts all. The paper's alternative measure -- the squared distance from the target -- rewards being close; this one rewards being right, and being right is what a sorting routine has to be.
}
\value{
An integer count.
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
IN <- list(c(2, 1), c(1, 2), c(5, 3))
TGT <- lapply(IN, sort)
ACTS <- list(list("mov", list("M", 0), list("R", 0)), list("cmp", 
    list("M", 1), list("M", 0)), list("cmovl", list("M", 1), 
    list("M", 0)), list("cmovl", list("R", 0), list("M", 1)), 
    list("mov", list("M", 1), list("R", 0)), list("cmovg", list("M", 
        0), list("M", 1)))
SOLN <- ACTS[1:4]
morie_alfqud_correctness(SOLN, IN, TGT, 1L)
}
