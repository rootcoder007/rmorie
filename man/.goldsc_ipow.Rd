% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.goldsc_ipow}
\alias{.goldsc_ipow}
\title{X to a small non-negative integer power, by repeated multiplication}
\usage{
.goldsc_ipow(x, k)
}
\arguments{
\item{x}{See Usage.}
\item{k}{See Usage.}
}
\description{
Not the language\\\'s power operator: R raises to an integer exponent by repeated squaring while Python calls the C library\\\'s pow, and the two disagree in the last bit. A potential that is about to be summed over thousands of contacts cannot afford to be a different function in the two arms.
}
\value{
The value of \code{p}, as built in the body.
}
\references{
  Jones, G., Willett, P., Glen, R.C., Leach, A.R. and Taylor, R.
    (1997) "Development and validation of a genetic algorithm for
    flexible docking." Journal of Molecular Biology 267(3), 727-748.
    doi:10.1006/jmbi.1996.0897.
  Jones, G., Willett, P. and Glen, R.C. (1995) "Molecular recognition
    of receptor sites using a genetic algorithm with a description of
    desolvation." Journal of Molecular Biology 245(1), 43-53.
  Verdonk, M.L., Cole, J.C., Hartshorn, M.J., Murray, C.W. and Taylor,
    R.D. (2003) "Improved protein-ligand docking using GOLD."
    Proteins 52(4), 609-623. doi:10.1002/prot.10465.
  Cambridge Crystallographic Data Centre, "GOLD User Guide," section
    8.3 for the four components and the 1.375 external van der Waals
    factor, and section 5.4 for the 6-12 internal, 4-8 external
    defaults and the two split potentials.
}
\keyword{internal}
