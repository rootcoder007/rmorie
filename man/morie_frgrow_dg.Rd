% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_frgrow_dg}
\alias{morie_frgrow_dg}
\title{Binding free energy in kcal per mole, negative for binding}
\usage{
morie_frgrow_dg(kd, temperature = .FRGROW_T_STANDARD)
}
\arguments{
\item{kd}{The dissociation constant, in molar.}
\item{temperature}{Kelvin.}
}
\description{
Kd is a concentration in molar. A Kd of one molar gives zero, which is the reference the whole scale hangs from.
}
\value{
The binding free energy.
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
