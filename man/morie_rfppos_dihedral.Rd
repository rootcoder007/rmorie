% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_rfppos_dihedral}
\alias{morie_rfppos_dihedral}
\title{The torsion about the b-c axis, in degrees, signed}
\usage{
morie_rfppos_dihedral(a, b, c, d) 
}
\arguments{
\item{a,b,c,d}{Four coordinate triples.}
}
\description{
Computed from the two plane normals with an arc tangent of two arguments rather than an arc cosine, so the sign survives -- and the sign is the whole point here, because it is what says which FACE the nucleophile is approaching from.
}
\value{
Degrees, signed.
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
morie_rfppos_dihedral(a = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L, c = c(1, 2, 3, 4, 5, 6, 7, 8), d = 5L)
}
