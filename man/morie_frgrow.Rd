% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_frgrow}
\alias{morie_frgrow}
\title{Score a set of grown analogues against the fragment they came from}
\usage{
morie_frgrow(fragment, linker_lib, route = "rt", temperature = .FRGROW_T_STANDARD)
}
\arguments{
\item{fragment}{The parent: list(kd, hac, logp, mw, psa). The last   three may be NULL.}
\item{linker_lib}{A list of grown analogues in the same shape,   optionally with a sixth entry naming each.}
\item{route}{A member of the energy-route list.}
\item{temperature}{Kelvin. Only the "rt" route uses it; the shortcut   is fixed at 298.15 K by construction, which is the point of   offering both.}
}
\description{
Score a set of grown analogues against the fragment they came from
}
\value{
A list with the parent's metrics, each analogue's metrics and   group efficiency, the ranking by group efficiency, and which   additions actually improved the ligand efficiency they inherited.
}
\references{
  Hopkins, A.L., Groom, C.R. and Alex, A. (2004) "Ligand efficiency: a
    useful metric for lead selection." Drug Discovery Today 9(10),
    430-431. doi:10.1016/S1359-6446(04)03069-7.
  Verdonk, M.L. and Rees, D.C. (2008) "Group efficiency: a guideline
    for hits-to-leads chemistry." ChemMedChem 3(8), 1179-1180.
  Murray, C.W. and Rees, D.C. (2009) "The rise of fragment-based drug
    discovery." Nature Chemistry 1(3), 187-192.
  Shuker, S.B., Hajduk, P.J., Meadows, R.P. and Fesik, S.W. (1996)
    "Discovering high-affinity ligands for proteins: SAR by NMR."
    Science 274(5292), 1531-1534.
  Leeson, P.D. and Springthorpe, B. (2007) "The influence of drug-like
    concepts on decision-making in medicinal chemistry." Nature
    Reviews Drug Discovery 6(11), 881-890.
  Keseru, G.M. and Makara, G.M. (2009) "The influence of lead
    discovery strategies on the properties of drug candidates."
    Nature Reviews Drug Discovery 8(3), 203-212.
  Abad-Zapatero, C. and Metz, J.T. (2005) "Ligand efficiency indices
    as guideposts for drug discovery." Drug Discovery Today 10(7),
    464-469.
}
\keyword{internal}
\examples{
PARENT <- list(0.001, 12, 0.9, 165, 42, "frag")
LIB <- list(list(2e-04, 15, 1.6, 205, 51, "methylamide"), list(1e-05, 
    20, 2.8, 268, 60, "biaryl"), list(5e-04, 27, 4.4, 360, 66, 
    "greasy_tail"), list(0.001, 14, 1, 190, 45, "inert_stub"), 
    list(0.004, 18, 2.2, 240, 58, "worse"), list(8e-07, 22, NULL, 
        NULL, NULL, "no_descriptors"))
morie_frgrow(PARENT, LIB)
}
