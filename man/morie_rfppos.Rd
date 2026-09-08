% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_rfppos}
\alias{morie_rfppos}
\title{Does this pose present its warhead so the cysteine can attack?}
\usage{
morie_rfppos(pose, cys_residue, mode = "burgi_dunitz", d_min = 1.5, d_max = 3.5, 
    ideal = NULL, angle_tol = 15, warhead = NULL) 
}
\arguments{
\item{pose}{A list with \code{smiles} for the ligand and   \code{coords}, one triple per atom in the order the SMILES lists   them.}
\item{cys_residue}{A list with \code{SG} for the sulfur and   \code{CB} for the beta carbon if the torsion is wanted.}
\item{mode}{Which chemistry, and therefore which ideal angle.}
\item{d_min,d_max}{The window the forming bond must fall in. The   default upper bound is a near-attack conformation, not a bond.}
\item{ideal}{The approach angle. NULL takes 107 degrees for the   Buergi-Dunitz route and 90 for the Michael route, which is   perpendicular to the alkene.}
\item{angle_tol}{How far off the ideal still counts. A setting, not a   constant.}
\item{warhead}{The electrophile, reference and torsion atoms, if the   caller would rather name them than have them found.}
}
\description{
Does this pose present its warhead so the cysteine can attack?
}
\value{
A list with the measured geometry, each criterion separately,   and whether the pose passes all of them.
}
\references{
  Buergi, H.B., Dunitz, J.D. and Shefter, E. (1973) "Geometrical
    reaction coordinates. II. Nucleophilic addition to a carbonyl
    group." Journal of the American Chemical Society 95(15),
    5065-5067. doi:10.1021/ja00796a058.
  Bianco, G., Forli, S., Goodsell, D.S. and Olson, A.J. (2016)
    "Covalent docking using autodock." Protein Science 25(1),
    295-301. doi:10.1002/pro.2733.
  Zhu, K. et al. (2014) "Docking covalent inhibitors: a parameter free
    approach to pose prediction and scoring." Journal of Chemical
    Information and Modeling 54(7), 1932-1940. CovDock, which the
    ledger entry names.
}
\keyword{internal}
