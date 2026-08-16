% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_twfe_weights_native}
\alias{.morie_twfe_weights_native}
\title{Internal helper: feTR weights on the TWFE estimand}
\usage{
.morie_twfe_weights_native(df, group, time, treatment)
}
\description{
Weights attached by the TWFE regression to each treated (g,t) cell: proportional to the cell\'s residual from regressing the treatment on unit and time fixed effects, normalized to sum to one over treated cells (de Chaisemartin & D\'Haultfoeuille 2020, feTR).
}
\value{
A data frame.
}
\keyword{internal}
