% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_rfppos_warhead}
\alias{morie_rfppos_warhead}
\title{Locate the electrophilic carbon and the atom that orients it}
\usage{
morie_rfppos_warhead(smiles, mode = "burgi_dunitz") 
}
\arguments{
\item{smiles}{The ligand.}
\item{mode}{Either burgi_dunitz or michael.}
}
\description{
For the Buergi-Dunitz route: a carbon holding a double bond to oxygen or nitrogen, or a triple bond to nitrogen. The electrophile is that carbon and the reference is the heteroatom, because the approach angle is measured to the carbon-heteroatom axis. For the Michael route: a carbon-carbon double bond with one end attached to a carbonyl carbon. The electrophile is the FAR end -- the beta carbon, which is where the sulfur adds -- and the reference is the alpha carbon it is doubly bonded to. Getting these two the wrong way round would measure a real angle at the wrong atom.
}
\value{
A zero-based triple of electrophile, reference and torsion   atom, or NULL when the molecule carries no such warhead.
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
\examples{
morie_rfppos_warhead("C=CC(=O)N", mode = "michael")
}
