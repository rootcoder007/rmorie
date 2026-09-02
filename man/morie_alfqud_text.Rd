% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_alfqud_text}
\alias{morie_alfqud_text}
\title{A program as one readable line per instruction}
\usage{
morie_alfqud_text(program) 
}
\arguments{
\item{program}{A list of instructions.}
}
\description{
A program as one readable line per instruction
}
\value{
A character scalar.
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
ACTS <- list(list("mov", list("M", 0), list("R", 0)), list("cmp", 
    list("M", 1), list("M", 0)), list("cmovl", list("M", 1), 
    list("M", 0)), list("cmovl", list("R", 0), list("M", 1)), 
    list("mov", list("M", 1), list("R", 0)), list("cmovg", list("M", 
        0), list("M", 1)))
SOLN <- ACTS[1:4]
morie_alfqud_text(SOLN)
}
