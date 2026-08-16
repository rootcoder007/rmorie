% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tsbF_cheatsheet}
\alias{morie_tsbF_cheatsheet}
\title{morie_tsbF_cheatsheet}
\usage{
morie_tsbF_cheatsheet()
}
\description{
Part of the tsbF_native implementation; see the file header for the source it follows.
}
\value{
A character value.
}
\references{
----------
Teunter, R. H., Syntetos, A. A. & Babai, M. Z. (2011) "Intermittent
demand: Linking forecasting to inventory obsolescence", *European
Journal of Operational Research* 214(3), 606-615,
doi:10.1016/j.ejor.2011.05.018. Secs. 2-3: the method, its
unbiasedness, and the obsolescence argument.

Croston, J. D. (1972) "Forecasting and Stock Control for Intermittent
Demands", *Operational Research Quarterly* 23(3), 289-303,
doi:10.2307/3007885. The method TSB modifies.

Syntetos, A. A. & Boylan, J. E. (2005) "The accuracy of intermittent
demand estimates", *International Journal of Forecasting* 21(2),
303-314, doi:10.1016/j.ijforecast.2004.10.001. The SBA deflator.

Syntetos, A. A. & Boylan, J. E. (2001) "On the bias of intermittent
demand estimates", *International Journal of Production Economics*
71(1-3), 457-466, doi:10.1016/S0925-5273(00)00143-2. The inversion
bias itself -- an ASYMPTOTIC result, which is why the anchor measures
it under ``init="known"``.

Prak, D., Teunter, R., Babai, M. Z., Boylan, J. E. & Syntetos, A.
(2021) "Robust compound Poisson parameter estimation for inventory
control", *Omega* 104, 102481, doi:10.1016/j.omega.2021.102481. That
the standard intermittent-demand estimators are severely biased in
finite samples, which is the effect ``burn_in`` and the ``init``
routes exist to separate from the asymptotic bias above.

Teunter, R. H. & Duncan, L. (2009) "Forecasting intermittent demand:
a comparative study", *Journal of the Operational Research Society*
60(3), 321-329, doi:10.1057/palgrave.jors.2602569. That per-period
error measures are the wrong yardstick for intermittent demand, which
is why the anchor compares bias against a known p*mu rather than
ranking methods on RMSE.

Kourentzes, N. (2014) "On intermittent demand model optimisation and
selection", *International Journal of Production Economics* 156,
180-190. The article prints no DOI. That the smoothing constants
and the initial states should be estimated together, and that the
usual squared-error optimisation misbehaves on intermittent series.

Babai, M. Z., Syntetos, A. & Teunter, R. (2014) "Intermittent demand
forecasting: An empirical study on accuracy and the risk of
obsolescence", *International Journal of Production Economics* 157,
212-219. The article prints no DOI. The empirical comparison of
TSB against Croston and SBA under obsolescence risk.

Babai, M. Z., Dallery, Y., Boubaker, S. & Kalai, R. (2019) "A new
method to forecast intermittent demand in the presence of inventory
obsolescence", *International Journal of Production Economics* 209,
30-41, doi:10.1016/j.ijpe.2018.01.026. A later obsolescence-aware
alternative to TSB.

Yang, Y., Ding, C., Lee, S., Yu, L. & Ma, F. (2021) "A modified
Teunter-Syntetos-Babai method for intermittent demand forecasting",
*Journal of Management Science and Engineering* 6(1), 53-63,
doi:10.1016/j.jmse.2021.02.008. A modification of the TSB update.

Svetunkov, I. & Boylan, J. E. (2023) "iETS: State space model for
intermittent demand forecasting", *International Journal of
Production Economics* 265, 109013, doi:10.1016/j.ijpe.2023.109013.
The state-space formulation that puts Croston and TSB in one family.
}
\keyword{internal}
