% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_rfppos_angle}
\alias{morie_rfppos_angle}
\title{The angle at b, in degrees, subtended by a and c}
\usage{
morie_rfppos_angle(a, b, c)
}
\arguments{
\item{a,b,c}{Coordinate triples; b is the vertex.}
}
\description{
The cosine is clamped into its own range before the arc cosine: two nearly parallel vectors can produce a ratio a hair outside minus one to one purely by rounding, and an unclamped arc cosine turns that into a domain error rather than the zero degrees it obviously is.
}
\value{
Degrees.
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
