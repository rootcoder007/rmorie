# SPDX-License-Identifier: AGPL-3.0-or-later

#' srr_stats
#'
#' All of the following standards initially have `@srrstatsTODO` tags.
#' These may be moved at any time to any other locations in your code.
#' Once addressed, please modify the tag from `@srrstatsTODO` to `@srrstats`,
#' or `@srrstatsNA`, ensuring that references to every one of the following
#' standards remain somewhere within your code.
#' (These comments may be deleted at any time.)
#'
#' @srrstatsVerbose TRUE
#'
#' @srrstatsTODO {G1.4a} *All internal (non-exported) functions should also be documented in standard [`roxygen2`](https://roxygen2.r-lib.org/) format, along with a final `@noRd` tag to suppress automatic generation of `.Rd` files or [`@keywords internal`](https://roxygen2.r-lib.org/reference/tags-index-crossref.html?q=keywords%20internal#null) if documentation is still desired.* 
#' @srrstatsTODO {RE1.0} *Regression Software should enable models to be specified via a formula interface, unless reasons for not doing so are explicitly documented.*
#' @srrstatsTODO {RE1.1} *Regression Software should document how formula interfaces are converted to matrix representations of input data.* 
#' @srrstatsTODO {RE1.2} *Regression Software should document expected format (types or classes) for inputting predictor variables, including descriptions of types or classes which are not accepted.* 
#' @srrstatsTODO {RE1.3} *Regression Software which passes or otherwise transforms aspects of input data onto output structures should ensure that those output structures retain all relevant aspects of input data, notably including row and column names, and potentially information from other `attributes()`.*
#' @srrstatsTODO {RE1.3a} *Where otherwise relevant information is not transferred, this should be explicitly documented.* 
#' @srrstatsTODO {RE1.4} *Regression Software should document any assumptions made with regard to input data; for example distributional assumptions, or assumptions that predictor data have mean values of zero. Implications of violations of these assumptions should be both documented and tested.* 
#' @srrstatsTODO {RE2.0} *Regression Software should document any transformations applied to input data, for example conversion of label-values to `factor`, and should provide ways to explicitly avoid any default transformations (with error or warning conditions where appropriate).*
#' @srrstatsTODO {RE2.1} *Regression Software should implement explicit parameters controlling the processing of missing values, ideally distinguishing `NA` or `NaN` values from `Inf` values (for example, through use of `na.omit()` and related functions from the `stats` package).* 
#' @srrstatsTODO {RE2.2} *Regression Software should provide different options for processing missing values in predictor and response data. For example, it should be possible to fit a model with no missing predictor data in order to generate values for all associated response points, even where submitted response values may be missing.*
#' @srrstatsTODO {RE2.3} *Where applicable, Regression Software should enable data to be centred (for example, through converting to zero-mean equivalent values; or to z-scores) or offset (for example, to zero-intercept equivalent values) via additional parameters, with the effects of any such parameters clearly documented and tested.*
#' @srrstatsTODO {RE2.4} *Regression Software should implement pre-processing routines to identify whether aspects of input data are perfectly collinear, notably including:*
#' @srrstatsTODO {RE2.4a} *Perfect collinearity among predictor variables*
#' @srrstatsTODO {RE2.4b} *Perfect collinearity between independent and dependent variables* 
#' @srrstatsTODO {RE3.0} *Issue appropriate warnings or other diagnostic messages for models which fail to converge.*
#' @srrstatsTODO {RE3.1} *Enable such messages to be optionally suppressed, yet should ensure that the resultant model object nevertheless includes sufficient data to identify lack of convergence.*
#' @srrstatsTODO {RE3.2} *Ensure that convergence thresholds have sensible default values, demonstrated through explicit documentation.*
#' @srrstatsTODO {RE3.3} *Allow explicit setting of convergence thresholds, unless reasons against doing so are explicitly documented.* 
#' @srrstatsTODO {RE4.0} *Regression Software should return some form of "model" object, generally through using or modifying existing class structures for model objects (such as `lm`, `glm`, or model objects from other packages), or creating a new class of model objects.*
#' @srrstatsTODO {RE4.1} *Regression Software may enable an ability to generate a model object without actually fitting values. This may be useful for controlling batch processing of computationally intensive fitting algorithms.* 
#' @srrstatsTODO {RE4.2} *Model coefficients (via `coef()` / `coefficients()`)*
#' @srrstatsTODO {RE4.3} *Confidence intervals on those coefficients (via `confint()`)*
#' @srrstatsTODO {RE4.4} *The specification of the model, generally as a formula (via `formula()`)*
#' @srrstatsTODO {RE4.5} *Numbers of observations submitted to model (via `nobs()`)*
#' @srrstatsTODO {RE4.6} *The variance-covariance matrix of the model parameters (via `vcov()`)*
#' @srrstatsTODO {RE4.7} *Where appropriate, convergence statistics* 
#' @srrstatsTODO {RE4.8} *Response variables, and associated "metadata" where applicable.*
#' @srrstatsTODO {RE4.9} *Modelled values of response variables.*
#' @srrstatsTODO {RE4.10} *Model Residuals, including sufficient documentation to enable interpretation of residuals, and to enable users to submit residuals to their own tests.*
#' @srrstatsTODO {RE4.11} *Goodness-of-fit and other statistics associated such as effect sizes with model coefficients.*
#' @srrstatsTODO {RE4.12} *Where appropriate, functions used to transform input data, and associated inverse transform functions.* 
#' @srrstatsTODO {RE4.13} *Predictor variables, and associated "metadata" where applicable.* 
#' @srrstatsTODO {RE4.14} *Where possible, values should also be provided for extrapolation or forecast *errors*.*
#' @srrstatsTODO {RE4.15} *Sufficient documentation and/or testing should be provided to demonstrate that forecast errors, confidence intervals, or equivalent values increase with forecast horizons.* 
#' @srrstatsTODO {RE4.16} *Regression Software which models distinct responses for different categorical groups should include the ability to submit new groups to `predict()` methods.* 
#' @srrstatsTODO {RE4.17} *Model objects returned by Regression Software should implement or appropriately extend a default `print` method which provides an on-screen summary of model (input) parameters and (output) coefficients.*
#' @srrstatsTODO {RE4.18} *Regression Software may also implement `summary` methods for model objects, and in particular should implement distinct `summary` methods for any cases in which calculation of summary statistics is computationally non-trivial (for example, for bootstrapped estimates of confidence intervals).* 
#' @srrstatsTODO {RE5.0} *Scaling relationships between sizes of input data (numbers of observations, with potential extension to numbers of variables/columns) and speed of algorithm.* 
#' @srrstatsTODO {RE6.0} *Model objects returned by Regression Software (see* **RE4***) should have default `plot` methods, either through explicit implementation, extension of methods for existing model objects, or through ensuring default methods work appropriately.*
#' @srrstatsTODO {RE6.1} *Where the default `plot` method is **NOT** a generic `plot` method dispatched on the class of return objects (that is, through an S3-type `plot.<myclass>` function or equivalent), that method dispatch (or equivalent) should nevertheless exist in order to explicitly direct users to the appropriate function.*
#' @srrstatsTODO {RE6.2} *The default `plot` method should produce a plot of the `fitted` values of the model, with optional visualisation of confidence intervals or equivalent.* 
#' @srrstatsTODO {RE6.3} *Where a model object is used to generate a forecast (for example, through a `predict()` method), the default `plot` method should provide clear visual distinction between modelled (interpolated) and forecast (extrapolated) values.* 
#' @srrstatsTODO {RE7.0} *Tests with noiseless, exact relationships between predictor (independent) data.*
#' @srrstatsTODO {RE7.0a} In particular, these tests should confirm ability to reject perfectly noiseless input data.
#' @srrstatsTODO {RE7.1} *Tests with noiseless, exact relationships between predictor (independent) and response (dependent) data.*
#' @srrstatsTODO {RE7.1a} *In particular, these tests should confirm that model fitting is at least as fast or (preferably) faster than testing with equivalent noisy data (see RE2.4b).* 
#' @srrstatsTODO {RE7.2} Demonstrate that output objects retain aspects of input data such as row or case names (see **RE1.3**).
#' @srrstatsTODO {RE7.3} Demonstrate and test expected behaviour when objects returned from regression software are submitted to the accessor methods of **RE4.2**--**RE4.7**.
#' @srrstatsTODO {RE7.4} Extending directly from **RE4.15**, where appropriate, tests should demonstrate and confirm that forecast errors, confidence intervals, or equivalent values increase with forecast horizons.
#' @srrstatsTODO {TS1.0} *Time Series Software should use and rely on explicit class systems developed for representing time series data, and should not permit generic, non-time-series input* 
#' @srrstatsTODO {TS1.1} *Time Series Software should explicitly document the types and classes of input data able to be passed to each function.* 
#' @srrstatsTODO {TS1.2} *Time Series Software should implement validation routines to confirm that inputs are of acceptable classes (or represented in otherwise appropriate ways for software which does not use class systems).*
#' @srrstatsTODO {TS1.3} *Time Series Software should implement a single pre-processing routine to validate input data, and to appropriately transform it to a single uniform type to be passed to all subsequent data-processing functions (the [`tsbox` package](https://www.tsbox.help/) provides one convenient approach for this).*
#' @srrstatsTODO {TS1.4} *The pre-processing function described above should maintain all time- or date-based components or attributes of input data.* 
#' @srrstatsTODO {TS1.5} *The software should ensure strict ordering of the time, frequency, or equivalent ordering index variable.*
#' @srrstatsTODO {TS1.6} *Any violations of ordering should be caught in the pre-processing stages of all functions.* 
#' @srrstatsTODO {TS1.7} *Accept inputs defined via the [`units` package](https://github.com/r-quantities/units/) for attributing SI units to R vectors.*
#' @srrstatsTODO {TS1.8} *Where time intervals or periods may be days or months, be explicit about the system used to represent such, particularly regarding whether a calendar system is used, or whether a year is presumed to have 365 days, 365.2422 days, or some other value.* 
#' @srrstatsTODO {TS2.0} *Time Series Software which presumes or requires regular data should only allow **explicit** missing values, and should issue appropriate diagnostic messages, potentially including errors, in response to any **implicit** missing values.*
#' @srrstatsTODO {TS2.1} *Where possible, all functions should provide options for users to specify how to handle missing data, with options minimally including:*
#' @srrstatsTODO {TS2.1a} *error on missing data; or.
#' @srrstatsTODO {TS2.1b} *warn or ignore missing data, and proceed to analyse irregular data, ensuring that results from function calls with regular yet missing data return identical values to submitting equivalent irregular data with no missing values; or*
#' @srrstatsTODO {TS2.1c} *replace missing data with appropriately imputed values.* 
#' @srrstatsTODO {TS2.2} *Consider stationarity of all relevant moments - typically first (mean) and second (variance) order, or otherwise document why such consideration may be restricted to lower orders only.*
#' @srrstatsTODO {TS2.3} *Explicitly document all assumptions and/or requirements of stationarity*
#' @srrstatsTODO {TS2.4} *Implement appropriate checks for all relevant forms of stationarity, and either:*
#' @srrstatsTODO {TS2.4a} *issue diagnostic messages or warnings; or*
#' @srrstatsTODO {TS2.4b} *enable or advise on appropriate transformations to ensure stationarity.* 
#' @srrstatsTODO {TS2.5} *Incorporate a system to ensure that both row and column orders follow the same ordering as the underlying time series data. This may, for example, be done by including the `index` attribute of the time series data as an attribute of the auto-covariance matrix.*
#' @srrstatsTODO {TS2.6} *Where applicable, auto-covariance matrices should also include specification of appropriate units.* 
#' @srrstatsTODO {TS3.0} *Provide tests to demonstrate at least one case in which errors widen appropriately with forecast horizon.*
#' @srrstatsTODO {TS3.1} *If possible, provide at least one test which violates TS3.0*
#' @srrstatsTODO {TS3.2} *Document the general drivers of forecast errors or horizons, as demonstrated via the particular cases of TS3.0 and TS3.1*
#' @srrstatsTODO {TS3.3} *Either:*
#' @srrstatsTODO {TS3.3a} *Document, preferable via an example, how to trim forecast values based on a specified error margin or equivalent; or*
#' @srrstatsTODO {TS3.3b} *Provide an explicit mechanism to trim forecast values to a specified error margin, either via an explicit post-processing function, or via an input parameter to a primary analytic function.* 
#' @srrstatsTODO {TS4.0} *Return values should either:*
#' @srrstatsTODO {TS4.0a} *Be in same class as input data, for example by using the [`tsbox` package](https://www.tsbox.help/) to re-convert from standard internal format (see 1.4, above); or*
#' @srrstatsTODO {TS4.0b} *Be in a unique, preferably class-defined, format.*
#' @srrstatsTODO {TS4.1} *Any units included as attributes of input data should also be included within return values.*
#' @srrstatsTODO {TS4.2} *The type and class of all return values should be explicitly documented.* 
#' @srrstatsTODO {TS4.3} *Return values should explicitly include all appropriate units and/or time scales* 
#' @srrstatsTODO {TS4.4} *Document the effect of any such transformations on forecast data, including potential effects on both first- and second-order estimates.*
#' @srrstatsTODO {TS4.5} *In decreasing order of preference, either:*
#' @srrstatsTODO {TS4.5a} *Provide explicit routines or options to back-transform data commensurate with original, non-stationary input data*
#' @srrstatsTODO {TS4.5b} *Demonstrate how data may be back-transformed to a form commensurate with original, non-stationary input data.*
#' @srrstatsTODO {TS4.5c} *Document associated limitations on forecast values* 
#' @srrstatsTODO {TS4.6} *Time Series Software which implements or otherwise enables forecasting should return either:*
#' @srrstatsTODO {TS4.6a} *A distribution object, for example via one of the many packages described in the CRAN Task View on [Probability Distributions](https://cran.r-project.org/web/views/Distributions.html) (or the new [`distributional` package](https://pkg.mitchelloharawild.com/distributional/) as used in the [`fable` package](https://fable.tidyverts.org) for time-series forecasting).*
#' @srrstatsTODO {TS4.6b} *For each variable to be forecast, predicted values equivalent to first- and second-order moments (for example, mean and standard error values).*
#' @srrstatsTODO {TS4.6c} *Some more general indication of error associated with forecast estimates.* 
#' @srrstatsTODO {TS4.7} *Ensure that forecast (modelled) values are clearly distinguished from observed (model or input) values, either (in this case in no order of preference) by*
#' @srrstatsTODO {TS4.7a} *Returning forecast values alone*
#' @srrstatsTODO {TS4.7b} *Returning distinct list items for model and forecast values*
#' @srrstatsTODO {TS4.7c} *Combining model and forecast values into a single return object with an appropriate additional column clearly distinguishing the two kinds of data.* 
#' @srrstatsTODO {TS5.0} *Implement default `plot` methods for any implemented class system.*
#' @srrstatsTODO {TS5.1} *When representing results in temporal domain(s), ensure that one axis is clearly labelled "time" (or equivalent), with continuous units.*
#' @srrstatsTODO {TS5.2} *Default to placing the "time" (or equivalent) variable on the horizontal axis.*
#' @srrstatsTODO {TS5.3} *Ensure that units of the time, frequency, or index variable are printed by default on the axis.*
#' @srrstatsTODO {TS5.4} *For frequency visualization, abscissa spanning $[-\pi, \pi]$ should be avoided in favour of positive units of $[0, 2\pi]$ or $[0, 0.5]$, in all cases with appropriate additional explanation of units.*
#' @srrstatsTODO {TS5.5} *Provide options to determine whether plots of data with missing values should generate continuous or broken lines.* 
#' @srrstatsTODO {TS5.6} *By default indicate distributional limits of forecast on plot*
#' @srrstatsTODO {TS5.7} *By default include model (input) values in plot, as well as forecast (output) values*
#' @srrstatsTODO {TS5.8} *By default provide clear visual distinction between model (input) values and forecast (output) values.*
#' @srrstatsTODO {SP1.0} *Spatial software should explicitly indicate its domain of applicability, and in particular distinguish whether the software may be applied in Cartesian/rectilinear/geometric domains, curvilinear/geographic domains, or both.* 
#' @srrstatsTODO {SP1.1} *Spatial software should explicitly indicate its dimensional domain of applicability, in particular through identifying whether it is applicable to two or three dimensions only, or whether there are any other restrictions on dimensionality.* 
#' @srrstatsTODO {SP2.0} *Spatial software should only accept input data of one or more classes explicitly developed to represent such data.*
#' @srrstatsTODO {SP2.0a} *Where new classes are implemented, conversion to other common classes for spatial data in R should be documented.*
#' @srrstatsTODO {SP2.0b} *Class systems should ensure that functions error appropriately, rather than merely warning, in response to data from inappropriate spatial domains.* 
#' @srrstatsTODO {SP2.1} *Spatial Software should not use the [`sp` package](https://cran.r-project.org/package=sp), rather should use [`sf`](https://cran.r-project.org/package=sf).* 
#' @srrstatsTODO {SP2.2} *Geographical Spatial Software should ensure maximal compatibility with established packages and workflows, minimally through:*
#' @srrstatsTODO {SP2.2a} *Clear and extensive documentation demonstrating how routines from that software may be embedded within, or otherwise adapted to, workflows which rely on these established packages; and*
#' @srrstatsTODO {SP2.2b} *Tests which clearly demonstrate that routines from that software may be successfully translated into forms and workflows which rely on these established packages.* 
#' @srrstatsTODO {SP2.3} *Software which accepts spatial input data in any standard format established in other R packages (such as any of the formats able to be read by [`GDAL`](https://gdal.org), and therefore by the [`sf` package](https://cran.r-project.org/package=sf)) should include example and test code which load those data in spatial formats, rather than R-specific binary formats such as `.Rds`.* 
#' @srrstatsTODO {SP2.4} *Geographical Spatial Software should be compliant with version 6 or larger of* [`PROJ`](https://proj.org/), *and with* `WKT2` *representations. The primary implication, described in detail in the articles linked to above, is that:*
#' @srrstatsTODO {SP2.4a} *Software should not permit coordinate reference systems to be represented merely by so-called "PROJ4-strings", but should use at least WKT2.* 
#' @srrstatsTODO {SP2.5} *Class systems for input data must contain meta data on associated coordinate reference systems.*
#' @srrstatsTODO {SP2.5a} *Software which implements new classes to input spatial data (or the spatial components of more general data) should provide an ability to convert such input objects into alternative spatial classes such as those listed above.*
#' @srrstatsTODO {SP2.6} *Spatial Software should explicitly document the types and classes of input data able to be passed to each function.*
#' @srrstatsTODO {SP2.7} *Spatial Software should implement validation routines to confirm that inputs are of acceptable classes (or represented in otherwise appropriate ways for software which does not use class systems).*
#' @srrstatsTODO {SP2.8} *Spatial Software should implement a single pre-processing routine to validate input data, and to appropriately transform it to a single uniform type to be passed to all subsequent data-processing functions.*
#' @srrstatsTODO {SP2.9} *The pre-processing function described above should maintain those metadata attributes of input data which are relevant or important to core algorithms or return values.* 
#' @srrstatsTODO {SP3.0} *Spatial software which considers spatial neighbours should enable user control over neighbourhood forms and sizes. In particular:*
#' @srrstatsTODO {SP3.0a} *Neighbours (able to be expressed) on regular grids should be able to be considered in both rectangular only, or rectangular and diagonal (respectively "rook" and "queen" by analogy to chess).*
#' @srrstatsTODO {SP3.0b} *Neighbourhoods in irregular spaces should be minimally able to be controlled via an integer number of neighbours, an area (or equivalent distance defining an area) in which to include neighbours, or otherwise equivalent user-controlled value.*
#' @srrstatsTODO {SP3.1} *Spatial software which considers spatial neighbours should wherever possible enable neighbour contributions to be weighted by distance (or other continuous weighting variable), and not rely exclusively on a uniform-weight rectangular cut-off.*
#' @srrstatsTODO {SP3.2} *Spatial software which relies on sampling from input data (even if only of spatial coordinates) should enable sampling procedures to be based on local spatial densities of those input data.* 
#' @srrstatsTODO {SP3.3} *Spatial regression software should explicitly quantify and distinguish autocovariant or autoregressive processes from those covariant or regressive processes not directly related to spatial structure alone.* 
#' @srrstatsTODO {SP3.4} *Where possible, spatial clustering software should avoid using standard non-spatial clustering algorithms in which spatial proximity is merely represented by an additional weighting factor in favour of explicitly spatial algorithms.* 
#' @srrstatsTODO {SP3.5} *Spatial machine learning software should ensure that broadcasting procedures for reconciling inputs of different dimensions are **not** applied*. 
#' @srrstatsTODO {SP3.6} *Spatial machine learning software should document (and, where possible, test) the potential effects of different sampling procedures* 
#' @srrstatsTODO {SP4.0} *Return values should either:*
#' @srrstatsTODO {SP4.0a} *Be in same class as input data, or*
#' @srrstatsTODO {SP4.0b} *Be in a unique, preferably class-defined, format.*
#' @srrstatsTODO {SP4.1} *Any aspects of input data which are included in output data (either directly, or in some transformed form) and which contain units should ensure those same units are maintained in return values.*
#' @srrstatsTODO {SP4.2} *The type and class of all return values should be explicitly documented.* 
#' @srrstatsTODO {SP5.0} *Implement default `plot` methods for any implemented class system.*
#' @srrstatsTODO {SP5.1} *Implement appropriate placement of variables along x- and y-axes.*
#' @srrstatsTODO {SP5.2} *Ensure that axis labels include appropriate units.* 
#' @srrstatsTODO {SP5.3} *Offer an ability to generate interactive (generally `html`-based) visualisations of results.* 
#' @srrstatsTODO {SP6.0} *Software which implements routines for transforming coordinates of input data should include tests which demonstrate ability to recover the original coordinates.* 
#' @srrstatsTODO {SP6.1} *All functions which can be applied to both Cartesian and curvilinear data should be tested through application to both.*
#' @srrstatsTODO {SP6.1a} *Functions which may yield inaccurate results when applied to data in one or the other forms (such as the preceding examples of centroids and buffers from ellipsoidal data) should test that results from inappropriate application of those functions are indeed less accurate.*
#' @srrstatsTODO {SP6.1b} *Functions which yield accurate results regardless of whether input data are rectilinear or curvilinear should demonstrate equivalent accuracy in both cases, and should also demonstrate how equivalent results may be obtained through first explicitly transforming input data.* 
#' @srrstatsTODO {SP6.2} *Geographical Software should include tests with extreme geographical coordinates, minimally including extension to polar extremes of +/-90 degrees.* 
#' @srrstatsTODO {SP6.3} *Spatial Software which considers spatial neighbours should explicitly test all possible ways of defining them, and should explicitly compare quantitative effects of different ways of defining neighbours.*
#' @srrstatsTODO {SP6.4} *Spatial Software which considers spatial neighbours should explicitly test effects of different schemes to weight neighbours by spatial proximity.* 
#' @srrstatsTODO {SP6.5} *Spatial Unsupervised Learning Software which uses clustering algorithms should implement tests which explicitly compare results with equivalent results obtained with a non-spatial clustering algorithm.* 
#' @srrstatsTODO {SP6.6} *Spatial Machine Learning Software should implement tests which explicitly demonstrate the detrimental consequences of sampling test and training data from the same spatial region, rather than from spatially distinct regions.
#' @srrstatsTODO {EA1.0} *Identify one or more target audiences for whom the software is intended*
#' @srrstatsTODO {EA1.1} *Identify the kinds of data the software is capable of analysing (see *Kinds of Data* below).*
#' @srrstatsTODO {EA1.2} *Identify the kinds of questions the software is intended to help explore.* 
#' @srrstatsTODO {EA1.3} *Identify the kinds of data each function is intended to accept as input* 
#' @srrstatsTODO {EA2.0} *EDA Software which accepts standard tabular data and implements or relies upon extensive table filter and join operations should utilise an **index column** system*
#' @srrstatsTODO {EA2.1} *All values in an index column must be unique, and this uniqueness should be affirmed as a pre-processing step for all input data.*
#' @srrstatsTODO {EA2.2} *Index columns should be explicitly identified, either:*
#' @srrstatsTODO {EA2.2a} *by using an appropriate class system, or*
#' @srrstatsTODO {EA2.2b} *through setting an `attribute` on a table, `x`, of `attr(x, "index") <- <index_col_name>`.* 
#' @srrstatsTODO {EA2.3} *Table join operations should not be based on any assumed variable or column names* 
#' @srrstatsTODO {EA2.4} *Use and demand an explicit class system for such input (for example, via the [`DM` package](https://github.com/krlmlr/dm)).*
#' @srrstatsTODO {EA2.5} *Ensure all individual tables follow the above standards for Index Columns* 
#' @srrstatsTODO {EA2.6} *Routines should appropriately process vector data regardless of additional attributes* 
#' @srrstatsTODO {EA3.0} *The algorithmic components of EDA Software should enable automated extraction and/or reporting of statistics as some sufficiently "meta" level (such as variable or model selection), for which previous or reference implementations require manual intervention.*
#' @srrstatsTODO {EA3.1} *EDA software should enable standardised comparison of inputs, processes, models, or outputs which previous or reference implementations otherwise only enable in some comparably unstandardised form.* 
#' @srrstatsTODO {EA4.0} *EDA Software should ensure all return results have types which are consistent with input types.* 
#' @srrstatsTODO {EA4.1} *EDA Software should implement parameters to enable explicit control of numeric precision*
#' @srrstatsTODO {EA4.2} *The primary routines of EDA Software should return objects for which default `print` and `plot` methods give sensible results. Default `summary` methods may also be implemented.* 
#' @srrstatsTODO {EA5.0} *Graphical presentation in EDA software should be as accessible as possible or practicable. In particular, EDA software should consider accessibility in terms of:*
#' @srrstatsTODO {EA5.0a} *Typeface sizes, which should default to sizes which explicitly enhance accessibility*
#' @srrstatsTODO {EA5.0b} *Default colour schemes, which should be carefully constructed to ensure accessibility.*
#' @srrstatsTODO {EA5.1} *Any explicit specifications of typefaces which override default values provided through other packages (including the `graphics` package) should consider accessibility* 
#' @srrstatsTODO {EA5.2} *Screen-based output should never rely on default print formatting of `numeric` types, rather should also use some version of `round(., digits)`, `formatC`, `sprintf`, or similar functions for numeric formatting according the parameter described in* **EA4.1**.
#' @srrstatsTODO {EA5.3} *Column-based summary statistics should always indicate the `storage.mode`, `class`, or equivalent defining attribute of each column.* 
#' @srrstatsTODO {EA5.4} *All visualisations should ensure values are rounded sensibly (for example, via `pretty()` function).*
#' @srrstatsTODO {EA5.5} *All visualisations should include units on all axes where such are specified or otherwise obtainable from input data or other routines.* 
#' @srrstatsTODO {EA5.6} *Any packages which internally bundle libraries used for dynamic visualization and which are also bundled in other, pre-existing R packages, should explain the necessity and advantage of re-bundling that library.* 
#' @srrstatsTODO {EA6.0} *Return values from all functions should be tested, including tests for the following characteristics:*
#' @srrstatsTODO {EA6.0a} *Classes and types of objects*
#' @srrstatsTODO {EA6.0b} *Dimensions of tabular objects*
#' @srrstatsTODO {EA6.0c} *Column names (or equivalent) of tabular objects*
#' @srrstatsTODO {EA6.0d} *Classes or types of all columns contained within `data.frame`-type tabular objects *
#' @srrstatsTODO {EA6.0e} *Values of single-valued objects; for `numeric` values either using `testthat::expect_equal()` or equivalent with a defined value for the `tolerance` parameter, or using `round(..., digits = x)` with some defined value of `x` prior to testing equality.* 
#' @srrstatsTODO {EA6.1} *The properties of graphical output from EDA software should be explicitly tested, for example via the [`vdiffr` package](https://github.com/r-lib/vdiffr) or equivalent.* 
#' @srrstatsTODO {PD1.0} *Software should provide references justifying choice and usage of particular probability distributions.* 
#' @srrstatsTODO {PD2.0} *Where possible, software should represent probability distributions using a package for general representation.* 
#' @srrstatsTODO {PD3.0} *Manipulation of probability distributions should very generally be analytic, with numeric manipulations only implemented with clear justification (ideally including references).* 
#' @srrstatsTODO {PD3.1} *Operations on probability distributions should generally be contained within separate functions which themselves accept the names of the distributions as one input parameter.* 
#' @srrstatsTODO {PD3.2} *Use of optimisation routines to estimate parameters from probability distributions should explicitly specify and explain values of all parameters, including all uses of default parameters.*
#' @srrstatsTODO {PD3.3} *Return objects which include values generated from optimisation algorithms should include information on optimisation algorithm and performance, minimally including the name of the algorithm used, the convergence tolerance, and the number of iterations.* 
#' @srrstatsTODO {PD3.4} *Use of routines to integrate probability distributions should explicitly document conditions under which integrals are expected to remain stable, and ideally include pre-processing checks for potentially unstable behaviour.*
#' @srrstatsTODO {PD3.5} *Integration routines should only rely on discrete summation where such use can be justified (for example, through providing a literature reference), in which case the following applies:*
#' @srrstatsTODO {PD3.5a} *Use of discrete summation to approximate integrals must demonstrate that the Reimann sum has a finite limit (or, equivalently, must explicitly describe the conditions under which the sum may be expected to be finite).* 
#' @srrstatsTODO {PD4.0} *The numeric outputs of probability distribution functions should be tested, not just output structures. These tests should generally be tests for numeric equality.* 
#' @srrstatsTODO {PD4.1} *Tests for numeric equality should compare the output of of probability distribution functions with the output of code which explicitly demonstrates how such values are derived (generally defined in the same location in test files).* 
#' @srrstatsTODO {PD4.2} *All functions constructed in accordance with **PD2.1** - that is, which use a fixed distribution, and which name that distribution as an input parameter - should be tested using at least two different distributions.* 
#' @srrstatsTODO {PD4.3} *Tests of optimisation or integration algorithms should compare default results with results generated with alternative values for every parameter, including all parameters for the chosen algorithm (whether exposed as function inputs or not).* 
#' @srrstatsTODO {PD4.4} *Tests of optimisation or integration algorithms should compare equivalent results generated with at least one alternative algorithm.* 
#' @srrstatsTODO {UL1.0} *Unsupervised Learning Software should explicitly document expected format (types or classes) for input data, including descriptions of types or classes which are not accepted; for example, specification that software accepts only numeric inputs in `vector` or `matrix` form, or that all inputs must be in `data.frame` form with both column and row names.*
#' @srrstatsTODO {UL1.1} *Unsupervised Learning Software should provide distinct sub-routines to assert that all input data is of the expected form, and issue informative error messages when incompatible data are submitted.* 
#' @srrstatsTODO {UL1.2} *Unsupervised learning which uses row or column names to label output objects should assert that input data have non-default row or column names, and issue an informative message when these are not provided.* 
#' @srrstatsTODO {UL1.3} *Unsupervised Learning Software should transfer all relevant aspects of input data, notably including row and column names, and potentially information from other `attributes()`, to corresponding aspects of return objects.*
#' @srrstatsTODO {UL1.3a} *Where otherwise relevant information is not transferred, this should be explicitly documented.* 
#' @srrstatsTODO {UL1.4} *Unsupervised Learning Software should document any assumptions made with regard to input data; for example assumptions about distributional forms or locations (such as that data are centred or on approximately equivalent distributional scales). Implications of violations of these assumptions should be both documented and tested, in particular:*
#' @srrstatsTODO {UL1.4a} *Software which responds qualitatively differently to input data which has components on markedly different scales should explicitly document such differences, and implications of submitting such data.*
#' @srrstatsTODO {UL1.4b} *Examples or other documentation should not use `scale()` or equivalent transformations without explaining why scale is applied, and explicitly illustrating and contrasting the consequences of not applying such transformations.* 
#' @srrstatsTODO {UL2.0} *Routines likely to give unreliable or irreproducible results in response to violations of assumptions regarding input data (see UL1.4) should implement pre-processing steps to diagnose potential violations, and issue appropriately informative messages, and/or include parameters to enable suitable transformations to be applied.* 
#' @srrstatsTODO {UL2.1} *Unsupervised Learning Software should document any transformations applied to input data, for example conversion of label-values to `factor`, and should provide ways to explicitly avoid any default transformations (with error or warning conditions where appropriate).*
#' @srrstatsTODO {UL2.2} *Unsupervised Learning Software which accepts missing values in input data should implement explicit parameters controlling the processing of missing values, ideally distinguishing `NA` or `NaN` values from `Inf` values.* 
#' @srrstatsTODO {UL2.3} *Unsupervised Learning Software should implement pre-processing routines to identify whether aspects of input data are perfectly collinear.* 
#' @srrstatsTODO {UL3.0} *Algorithms which apply sequential labels to input data (such as clustering or partitioning algorithms) should ensure that the sequence follows decreasing group sizes (so labels of "1", "a", or "A" describe the largest group, "2", "b", or "B" the second largest, and so on.)* 
#' @srrstatsTODO {UL3.1} *Dimensionality reduction or equivalent algorithms which label dimensions should ensure that that sequences of labels follows decreasing "importance" (for example, eigenvalues or variance contributions).* 
#' @srrstatsTODO {UL3.2} *Unsupervised Learning Software for which input data does not generally include labels (such as `array`-like data with no row names) should provide an additional parameter to enable cases to be labelled.* 
#' @srrstatsTODO {UL3.3} *Where applicable, Unsupervised Learning Software should implement routines to predict the properties (such as numerical ordinates, or cluster memberships) of additional new data without re-running the entire algorithm.* 
#' @srrstatsTODO {UL3.4} *Objects returned from Unsupervised Learning Software which labels, categorise, or partitions data into discrete groups should include, or provide immediate access to, quantitative information on intra-group variances or equivalent, as well as on inter-group relationships where applicable.* 
#' @srrstatsTODO {UL4.0} *Unsupervised Learning Software should return some form of "model" object, generally through using or modifying existing class structures for model objects, or creating a new class of model objects.*
#' @srrstatsTODO {UL4.1} *Unsupervised Learning Software may enable an ability to generate a model object without actually fitting values. This may be useful for controlling batch processing of computationally intensive fitting algorithms.*
#' @srrstatsTODO {UL4.2} *The return object from Unsupervised Learning Software should include, or otherwise enable immediate extraction of, all parameters used to control the algorithm used.* 
#' @srrstatsTODO {UL4.3} *Model objects returned by Unsupervised Learning Software should implement or appropriately extend a default `print` method which provides an on-screen summary of model (input) parameters and methods used to generate results. The `print` method may also summarise statistical aspects of the output data or results.*
#' @srrstatsTODO {UL4.3a} *The default `print` method should always ensure only a restricted number of rows of any result matrices or equivalent are printed to the screen.* 
#' @srrstatsTODO {UL4.4} *Unsupervised Learning Software should also implement `summary` methods for model objects which should summarise the primary statistics used in generating the model (such as numbers of observations, parameters of methods applied). The `summary` method may also provide summary statistics from the resultant model.* 
#' @srrstatsTODO {UL6.0} *Objects returned by Unsupervised Learning Software should have default `plot` methods, either through explicit implementation, extension of methods for existing model objects, through ensuring default methods work appropriately, or through explicit reference to helper packages such as [`factoextra`](https://github.com/kassambara/factoextra) and associated functions.*
#' @srrstatsTODO {UL6.1} *Where the default `plot` method is **NOT** a generic `plot` method dispatched on the class of return objects (that is, through an S3-type `plot.<myclass>` function or equivalent), that method dispatch (or equivalent) should nevertheless exist in order to explicitly direct users to the appropriate function.*
#' @srrstatsTODO {UL6.2} *Where default plot methods include labelling components of return objects (such as cluster labels), routines should ensure that labels are automatically placed to ensure readability, and/or that appropriate diagnostic messages are issued where readability is likely to be compromised (for example, through attempting to place too many labels).* 
#' @srrstatsTODO {UL7.0} *Inappropriate types of input data are rejected with expected error messages.* 
#' @srrstatsTODO {UL7.1} *Tests should demonstrate that violations of assumed input properties yield unreliable or invalid outputs, and should clarify how such unreliability or invalidity is manifest through the properties of returned objects.* 
#' @srrstatsTODO {UL7.2} *Demonstrate that labels placed on output data follow decreasing group sizes (**UL3.0**)*
#' @srrstatsTODO {UL7.3} *Demonstrate that labels on input data are propagated to, or may be recovered from, output data. 
#' @srrstatsTODO {UL7.4} *Demonstrate that submission of new data to a previously fitted model can generate results more efficiently than initial model fitting.* 
#' @srrstatsTODO {UL7.5} *Batch processing routines should be explicitly tested, commonly via extended tests (see **G4.10**--**G4.12**).*
#' @srrstatsTODO {UL7.5a} *Tests of batch processing routines should demonstrate that equivalent results are obtained from direct (non-batch) processing.*
#' @srrstatsTODO {BS1.0} *Bayesian software which uses the term "hyperparameter" should explicitly clarify the meaning of that term in the context of that software.* 
#' @srrstatsTODO {BS1.1} *Descriptions of how to enter data, both in textual form and via code examples. Both of these should consider the simplest cases of single objects representing independent and dependent data, and potentially more complicated cases of multiple independent data inputs.*
#' @srrstatsTODO {BS1.2} *Description of how to specify prior distributions, both in textual form describing the general principles of specifying prior distributions, along with more applied descriptions and examples, within:*
#' @srrstatsTODO {BS1.2a} *The main package `README`, either as textual description or example code* 
#' @srrstatsTODO {BS1.2b} *At least one package vignette, both as general and applied textual descriptions, and example code* 
#' @srrstatsTODO {BS1.2c} *Function-level documentation, preferably with code included in examples* 
#' @srrstatsTODO {BS1.3} *Description of all parameters which control the computational process (typically those determining aspects such as numbers and lengths of sampling processes, seeds used to start them, thinning parameters determining post-hoc sampling from simulated values, and convergence criteria). In particular:*
#' @srrstatsTODO {BS1.3a} *Bayesian Software should document, both in text and examples, how to use the output of previous simulations as starting points of subsequent simulations.* 
#' @srrstatsTODO {BS1.3b} *Where applicable, Bayesian software should document, both in text and examples, how to use different sampling algorithms for a given model.*
#' @srrstatsTODO {BS1.4} *For Bayesian Software which implements or otherwise enables convergence checkers, documentation should explicitly describe and provide examples of use with and without convergence checkers.*
#' @srrstatsTODO {BS1.5} *For Bayesian Software which implements or otherwise enables multiple convergence checkers, differences between these should be explicitly tested.* 
#' @srrstatsTODO {BS2.1} *Bayesian Software should implement pre-processing routines to ensure all input data is dimensionally commensurate, for example by ensuring commensurate lengths of vectors or numbers of rows of tabular inputs.*
#' @srrstatsTODO {BS2.1a} *The effects of such routines should be tested.* 
#' @srrstatsTODO {BS2.2} *Ensure that all appropriate validation and pre-processing of distributional parameters are implemented as distinct pre-processing steps prior to submitting to analytic routines, and especially prior to submitting to multiple parallel computational chains.*
#' @srrstatsTODO {BS2.3} *Ensure that lengths of vectors of distributional parameters are checked, with no excess values silently discarded (unless such output is explicitly suppressed, as detailed below).*
#' @srrstatsTODO {BS2.4} *Ensure that lengths of vectors of distributional parameters are commensurate with expected model input (see example immediately below)*
#' @srrstatsTODO {BS2.5} *Where possible, implement pre-processing checks to validate appropriateness of numeric values submitted for distributional parameters; for example, by ensuring that distributional parameters defining second-order moments such as distributional variance or shape parameters, or any parameters which are logarithmically transformed, are non-negative.* 
#' @srrstatsTODO {BS2.6} *Check that values for computational parameters lie within plausible ranges.* 
#' @srrstatsTODO {BS2.7} *Enable starting values to be explicitly controlled via one or more input parameters, including multiple values for software which implements or enables multiple computational "chains."*
#' @srrstatsTODO {BS2.8} *Enable results of previous runs to be used as starting points for subsequent runs.* 
#' @srrstatsTODO {BS2.9} *Ensure each chain is started with a different seed by default.*
#' @srrstatsTODO {BS2.10} *Issue diagnostic messages when identical seeds are passed to distinct computational chains.*
#' @srrstatsTODO {BS2.11} *Software which accepts starting values as a vector should provide the parameter with a plural name: for example, "starting_values" and not "starting_value".* 
#' @srrstatsTODO {BS2.12} *Bayesian Software should implement at least one parameter controlling the verbosity of output, defaulting to verbose output of all appropriate messages, warnings, errors, and progress indicators.*
#' @srrstatsTODO {BS2.13} *Bayesian Software should enable suppression of messages and progress indicators, while retaining verbosity of warnings and errors. This should be tested.*
#' @srrstatsTODO {BS2.14} *Bayesian Software should enable suppression of warnings where appropriate. This should be tested.*
#' @srrstatsTODO {BS2.15} *Bayesian Software should explicitly enable errors to be caught, and appropriately processed either through conversion to warnings, or otherwise captured in return values. This should be tested.* 
#' @srrstatsTODO {BS3.0} *Explicitly document assumptions made in regard to missing values; for example that data is assumed to contain no missing (`NA`, `Inf`) values, and that such values, or entire rows including any such values, will be automatically removed from input data.* 
#' @srrstatsTODO {BS3.1} *Implement pre-processing routines to diagnose perfect collinearity, and provide appropriate diagnostic messages or warnings*
#' @srrstatsTODO {BS3.2} *Provide distinct routines for processing perfectly collinear data, potentially bypassing sampling algorithms* 
#' @srrstatsTODO {BS4.0} *Packages should document sampling algorithms (generally via literary citation, or reference to other software)*
#' @srrstatsTODO {BS4.1} *Packages should provide explicit comparisons with external samplers which demonstrate intended advantage of implementation (generally via tests, vignettes, or both).* 
#' @srrstatsTODO {BS4.2} *Implement at least one means to validate posterior estimates.* 
#' @srrstatsTODO {BS4.3} *Implement or otherwise offer at least one type of convergence checker, and provide a documented reference for that implementation.*
#' @srrstatsTODO {BS4.4} *Enable computations to be stopped on convergence (although not necessarily by default).*
#' @srrstatsTODO {BS4.5} *Ensure that appropriate mechanisms are provided for models which do not converge.* 
#' @srrstatsTODO {BS4.6} *Implement tests to confirm that results with convergence checker are statistically equivalent to results from equivalent fixed number of samples without convergence checking.*
#' @srrstatsTODO {BS4.7} *Where convergence checkers are themselves parametrised, the effects of such parameters should also be tested. For threshold parameters, for example, lower values should result in longer sequence lengths.* 
#' @srrstatsTODO {BS5.0} *Return values should include starting value(s) or seed(s), including values for each sequence where multiple sequences are included*
#' @srrstatsTODO {BS5.1} *Return values should include appropriate metadata on types (or classes) and dimensions of input data* 
#' @srrstatsTODO {BS5.2} *Bayesian Software should either return the input function or prior distributional specification in the return object; or enable direct access to such via additional functions which accept the return object as single argument.* 
#' @srrstatsTODO {BS5.3} *Bayesian Software should return convergence statistics or equivalent*
#' @srrstatsTODO {BS5.4} *Where multiple checkers are enabled, Bayesian Software should return details of convergence checker used*
#' @srrstatsTODO {BS5.5} *Appropriate diagnostic statistics to indicate absence of convergence should either be returned or immediately able to be accessed.* 
#' @srrstatsTODO {BS6.0} *Software should implement a default `print` method for return objects*
#' @srrstatsTODO {BS6.1} *Software should implement a default `plot` method for return objects*
#' @srrstatsTODO {BS6.2} *Software should provide and document straightforward abilities to plot sequences of posterior samples, with burn-in periods clearly distinguished*
#' @srrstatsTODO {BS6.3} *Software should provide and document straightforward abilities to plot posterior distributional estimates* 
#' @srrstatsTODO {BS6.4} *Software may provide `summary` methods for return objects*
#' @srrstatsTODO {BS6.5} *Software may provide abilities to plot both sequences of posterior samples and distributional estimates together in single graphic* 
#' @srrstatsTODO {BS7.0} *Software should demonstrate and confirm recovery of parametric estimates of a prior distribution*
#' @srrstatsTODO {BS7.1} *Software should demonstrate and confirm recovery of a prior distribution in the absence of any additional data or information*
#' @srrstatsTODO {BS7.2} *Software should demonstrate and confirm recovery of a expected posterior distribution given a specified prior and some input data* 
#' @srrstatsTODO {BS7.3} *Bayesian software should include tests which demonstrate and confirm the scaling of algorithmic efficiency with sizes of input data.* 
#' @srrstatsTODO {BS7.4} *Bayesian software should implement tests which confirm that predicted or fitted values are on (approximately) the same scale as input values.*
#' @srrstatsTODO {BS7.4a} *The implications of any assumptions on scales on input objects should be explicitly tested in this context; for example that the scales of inputs which do not have means of zero will not be able to be recovered.*
#' @srrstatsTODO {ML1.0} *Documentation should make a clear conceptual distinction between training and test data (even where such may ultimately be confounded as described above.)*
#' @srrstatsTODO {ML1.0a} *Where these terms are ultimately eschewed, these should nevertheless be used in initial documentation, along with clear explanation of, and justification for, alternative terminology.*
#' @srrstatsTODO {ML1.1} *Absent clear justification for alternative design decisions, input data should be expected to be labelled "test", "training", and, where applicable, "validation" data.*
#' @srrstatsTODO {ML1.1a} *The presence and use of these labels should be explicitly confirmed via pre-processing steps (and tested in accordance with **ML7.0**, below).*
#' @srrstatsTODO {ML1.1b} *Matches to expected labels should be case-insensitive and based on partial matching such that, for example, "Test", "test", or "testing" should all suffice.* 
#' @srrstatsTODO {ML1.2} *Training and test data sets for ML software should be able to be input as a single, generally tabular, data object, with the training and test data distinguished either by* - *A specified variable containing, for example, `TRUE`/`FALSE` or `0`/`1` values, or which uses some other system such as missing (`NA`) values to denote test data); and/or* - *An additional parameter designating case or row numbers, or labels of test data.* 
#' @srrstatsTODO {ML1.3} *Input data should be clearly partitioned between training and test data (for example, through having each passed as a distinct `list` item), or should enable an additional means of categorically distinguishing training from test data (such as via an additional parameter which provides explicit labels). Where applicable, distinction of validation and any other data should also accord with this standard.* 
#' @srrstatsTODO {ML1.4} *Training and test data sets, along with other necessary components such as validation data sets, should be stored in their own distinctly labelled sub-directories (for distinct files), or according to an explicit and distinct labelling scheme (for example, for database connections). Labelling should in all cases adhere to **ML1.1**, above.* 
#' @srrstatsTODO {ML1.5} *ML software should implement a single function which summarises the contents of test and training (and other) data sets, minimally including counts of numbers of cases, records, or files, and potentially extending to tables or summaries of file or data types, sizes, and other information (such as unique hashes for each component).* 
#' @srrstatsTODO {ML1.6} *ML software which does not admit missing values, and which expects no missing values, should implement explicit pre-processing routines to identify whether data has any missing values, and should generally error appropriately and informatively when passed data with missing values. In addition, ML software which does not admit missing values should:*
#' @srrstatsTODO {ML1.6a} *Explain why missing values are not admitted.*
#' @srrstatsTODO {ML1.6b} *Provide explicit examples (in function documentation, vignettes, or both) for how missing values may be imputed, rather than simply discarded.*
#' @srrstatsTODO {ML1.7} *ML software which admits missing values should clearly document how such values are processed.*
#' @srrstatsTODO {ML1.7a} *Where missing values are imputed, software should offer multiple user-defined ways to impute missing data.*
#' @srrstatsTODO {ML1.7b} *Where missing values are imputed, the precise imputation steps should also be explicitly documented, either in tests (see **ML7.2** below), function documentation, or vignettes.*
#' @srrstatsTODO {ML1.8} *ML software should enable equal treatment of missing values for both training and test data, with optional user ability to control application to either one or both.* 
#' @srrstatsTODO {ML2.0} *A dedicated function should enable pre-processing steps to be defined and parametrized.*
#' @srrstatsTODO {ML2.0a} *That function should return an object which can be directly submitted to a specified model (see section 3, below).*
#' @srrstatsTODO {ML2.0b} *Absent explicit justification otherwise, that return object should have a defined class minimally intended to implement a default `print` method which summarizes the input data set (as per **ML1.5** above) and associated transformations (see the following standard).* 
#' @srrstatsTODO {ML2.1} *ML software which uses broadcasting to reconcile dimensionally incommensurate input data should offer an ability to at least optionally record transformations applied to each input file.* 
#' @srrstatsTODO {ML2.2} *ML software which requires or relies upon numeric transformations of input data (such as change in mean values or variances) should allow optimal explicit specification of target values, rather than restricting transformations to default generic values only (such as transformations to z-scores).*
#' @srrstatsTODO {ML2.2a} *Where the parameters have default values, reasons for those particular defaults should be explicitly described.*
#' @srrstatsTODO {ML2.2b} *Any extended documentation (such as vignettes) which demonstrates the use of explicit values for numeric transformations should explicitly describe why particular values are used.* 
#' @srrstatsTODO {ML2.3} *The values associated with all transformations should be recorded in the object returned by the function described in the preceding standard (**ML2.0**).*
#' @srrstatsTODO {ML2.4} *Default values of all transformations should be explicitly documented, both in documentation of parameters where appropriate (such as for numeric transformations), and in extended documentation such as vignettes.*
#' @srrstatsTODO {ML2.5} *ML software should provide options to bypass or otherwise switch off all default transformations.*
#' @srrstatsTODO {ML2.6} *Where transformations are implemented via distinct functions, these should be exported to a package's namespace so they can be applied in other contexts.*
#' @srrstatsTODO {ML2.7} *Where possible, documentation should be provided for how transformations may be reversed. For example, documentation may demonstrate how the values retained via **ML2.3**, above, can be used along with transformations either exported via **ML2.6** or otherwise exemplified in demonstration code to independently transform data, and then to reverse those transformations.* 
#' @srrstatsTODO {ML3.0} *Model specification should be implemented as a distinct stage subsequent to specification of pre-processing routines (see Section 2, above) and prior to actual model fitting or training (see Section 4, below). In particular,*
#' @srrstatsTODO {ML3.0a} *A dedicated function should enable models to be specified without actually fitting or training them, or if this (**ML3**) and the following (**ML4**) stages are controlled by a single function, that function should have a parameter enabling models to be specified yet not fitted (for example, `nofit = FALSE`).*
#' @srrstatsTODO {ML3.0b} *That function should accept as input the objects produced by the previous Input Data Specification stage, and defined according to **ML2.0**, above.*
#' @srrstatsTODO {ML3.0c} *The function described above (**ML3.0a**) should return an object which can be directly trained as described in the following sub-section (**ML4**).*
#' @srrstatsTODO {ML3.0d} *That return object should have a defined class minimally intended to implement a default `print` method which summarises the model specification, including values of all relevant parameters.*
#' @srrstatsTODO {ML3.1} *ML software should allow the use of both untrained models, specified through model parameters only, as well as pre-trained models. Use of the latter commonly entails an ability to submit a previously-trained model object to the function defined according to **ML3.0a**, above.*
#' @srrstatsTODO {ML3.2} *ML software should enable different models to be applied to the object specifying data inputs and transformations (see sub-sections 1--2, above) without needing to re-define those preceding steps.* 
#' @srrstatsTODO {ML3.3} *Where ML software implements its own distinct classes of model objects, the properties and behaviours of those specific classes of objects should be explicitly compared with objects produced by other ML software. In particular, where possible, ML software should provide extended documentation (as vignettes or equivalent) comparing model objects with those from other ML software, noting both unique abilities and restrictions of any implemented classes.*
#' @srrstatsTODO {ML3.4} *Where training rates are used, ML software should provide explicit documentation both in all functions which use training rates, and in extended form such as vignettes, of the importance of, and/or sensitivity to, different values of training rates. In particular,*
#' @srrstatsTODO {ML3.4a} *Unless explicitly justified otherwise, ML software should offer abilities to automatically determine appropriate or optimal training rates, either as distinct pre-processing stages, or as implicit stages of model training.*
#' @srrstatsTODO {ML3.4b} *ML software which provides default values for training rates should clearly document anticipated restrictions of validity of those default values; for example through clear suggestions that user-determined and -specified values may generally be necessary or preferable.* 
#' @srrstatsTODO {ML3.5} *Parameters controlling optimization algorithms should minimally include:*
#' @srrstatsTODO {ML3.5a} *Specification of the type of algorithm used to explore the search space (commonly, for example, some kind of gradient descent algorithm)*
#' @srrstatsTODO {ML3.5b} *The kind of loss function used to assess distance between model estimates and desired output.*
#' @srrstatsTODO {ML3.6} *Unless explicitly justified otherwise (for example because ML software under consideration is an implementation of one specific algorithm), ML software should:*
#' @srrstatsTODO {ML3.6a} *Implement or otherwise permit usage of multiple ways of exploring search space*
#' @srrstatsTODO {ML3.6b} *Implement or otherwise permit usage of multiple loss functions.* 
#' @srrstatsTODO {ML3.7} *For ML software in which algorithms are coded in C++, user-controlled use of either CPUs or GPUs (on NVIDIA processors at least) should be implemented through direct use of [`libcudacxx`](https://github.com/NVIDIA/libcudacxx).* 
#' @srrstatsTODO {ML4.0} *ML software should generally implement a unified single-function interface to model training, able to receive as input a model specified according to all preceding standards. In particular, models with categorically different specifications, such as different model architectures or optimization algorithms, should be able to be submitted to the same model training function.*
#' @srrstatsTODO {ML4.1} *ML software should at least optionally retain explicit information on paths taken as an optimizer advances towards minimal loss. Such information should minimally include:*
#' @srrstatsTODO {ML4.1a} *Specification of all model-internal parameters, or equivalent hashed representation.*
#' @srrstatsTODO {ML4.1b} *The value of the loss function at each point*
#' @srrstatsTODO {ML4.1c} *Information used to advance to next point, for example quantification of local gradient.*
#' @srrstatsTODO {ML4.2} *The subsequent extraction of information retained according to the preceding standard should be explicitly documented, including through example code.* 
#' @srrstatsTODO {ML4.3} *All parameters controlling batch processing and associated terminology should be explicitly documented, and it should not, for example, be presumed that users will understand the definition of "epoch" as implemented in any particular ML software.* 
#' @srrstatsTODO {ML4.4} *Explicit guidance should be provided on selection of appropriate values for parameter controlling batch processing, for example, on trade-offs between batch sizes and numbers of epochs (with both terms provided as Control Parameters in accordance with the preceding standard, **ML3**).*
#' @srrstatsTODO {ML4.5} *ML software may optionally include a function to estimate likely time to train a specified model, through estimating initial timings from a small sample of the full batch.*
#' @srrstatsTODO {ML4.6} *ML software should by default provide explicit information on the progress of batch jobs (even where those jobs may be implemented in parallel on GPUs). That information may be optionally suppressed through additional parameters.* 
#' @srrstatsTODO {ML4.7} *ML software should provide an ability to combine results from multiple re-sampling iterations using a single parameter specifying numbers of iterations.*
#' @srrstatsTODO {ML4.8} *Absent any additional specification, re-sampling algorithms should by default partition data according to proportions of original test and training data.*
#' @srrstatsTODO {ML4.8a} *Re-sampling routines of ML software should nevertheless offer an ability to explicitly control or override such default proportions of test and training data.* 
#' @srrstatsTODO {ML5.0} *The result of applying the training processes described above should be contained within a single model object returned by the function defined according to **ML4.0**, above. Even where the output reflects application to a test data set, the resultant object need not include any information on model performance (see **ML5.3**--**ML5.4**, below).*
#' @srrstatsTODO {ML5.0a} *That object should either have its own class, or extend some previously-defined class.*
#' @srrstatsTODO {ML5.0b} *That class should have a defined `print` method which summarises important aspects of the model object, including but not limited to summaries of input data and algorithmic control parameters.*
#' @srrstatsTODO {ML5.1} *As for the untrained model objects produced according to the above standards, and in particular as a direct extension of **ML3.3**, the properties and behaviours of trained models produced by ML software should be explicitly compared with equivalent objects produced by other ML software. (Such comparison will generally be done in terms of comparing model performance, as described in the following standard **ML5.3**--**ML5.4**).*
#' @srrstatsTODO {ML5.2} *The structure and functionality of objects representing trained ML models should be thoroughly documented. In particular,*
#' @srrstatsTODO {ML5.2a} *Either all functionality extending from the class of model object should be explicitly documented, or a method for listing or otherwise accessing all associated functionality explicitly documented and demonstrated in example code.*
#' @srrstatsTODO {ML5.2b} *Documentation should include examples of how to save and re-load trained model objects for their re-use in accordance with **ML3.1**, above.*
#' @srrstatsTODO {ML5.2c} *Where general functions for saving or serializing objects, such as [`saveRDS`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/readRDS.html) are not appropriate for storing local copies of trained models, an explicit function should be provided for that purpose, and should be demonstrated with example code.* 
#' @srrstatsTODO {ML5.3} *Assessment of model performance should be implemented as one or more functions distinct from model training.*
#' @srrstatsTODO {ML5.4} *Model performance should be able to be assessed according to a variety of metrics.*
#' @srrstatsTODO {ML5.4a} *All model performance metrics represented by functions internal to a package must be clearly and distinctly documented.*
#' @srrstatsTODO {ML5.4b} *It should be possible to submit custom metrics to a model assessment function, and the ability to do so should be clearly documented including through example code.* 
#' @srrstatsTODO {ML6.0} *Descriptions of ML software should make explicit reference to a workflow which separates training and testing stages, and which clearly indicates a need for distinct training and test data sets.* 
#' @srrstatsTODO {ML6.1} *ML software intentionally designed to address only a restricted subset of the workflow described here should clearly document how it can be embedded within a typical full ML workflow in the sense considered here.*
#' @srrstatsTODO {ML6.1a} *Such demonstrations should include and contrast embedding within a full workflow using at least two other packages to implement that workflow.* 
#' @srrstatsTODO {ML7.0} *Test should explicitly confirm partial and case-insensitive matching of "test", "train", and, where applicable, "validation" data.*
#' @srrstatsTODO {ML7.1} *Tests should demonstrate effects of different numeric scaling of input data (see **ML2.2**).*
#' @srrstatsTODO {ML7.2} *For software which imputes missing data, tests should compare internal imputation with explicit code which directly implements imputation steps (even where such imputation is a single-step implemented via some external package). These tests serve as an explicit reference for how imputation is performed.* 
#' @srrstatsTODO {ML7.3} *Where model objects are implemented as distinct classes, tests should explicitly compare the functionality of these classes with functionality of equivalent classes for ML model objects from other packages.*
#' @srrstatsTODO {ML7.3a} *These tests should explicitly identify restrictions on the functionality of model objects in comparison with those of other packages.*
#' @srrstatsTODO {ML7.3b} *These tests should explicitly identify functional advantages and unique abilities of the model objects in comparison with those of other packages.* 
#' @srrstatsTODO {ML7.4} *ML software should explicit document the effects of different training rates, and in particular should demonstrate divergence from optima with inappropriate training rates.*
#' @srrstatsTODO {ML7.5} *ML software which implements routines to determine optimal training rates (see **ML3.4**, above) should implement tests to confirm the optimality of resultant values.*
#' @srrstatsTODO {ML7.6} *ML software which implement independent training "epochs" should demonstrate in tests the effects of lesser versus greater numbers of epochs.*
#' @srrstatsTODO {ML7.7} *ML software should explicitly test different optimization algorithms, even where software is intended to implement one specific algorithm.*
#' @srrstatsTODO {ML7.8} *ML software should explicitly test different loss functions, even where software is intended to implement one specific measure of loss.*
#' @srrstatsTODO {ML7.9} *Tests should explicitly compare all possible combinations in categorical differences in model architecture, such as different model architectures with same optimization algorithms, same model architectures with different optimization algorithms, and differences in both.*
#' @srrstatsTODO {ML7.9a} *Such combinations will generally be formed from multiple categorical factors, for which explicit use of functions such as [`expand.grid()`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/expand.grid.html) is recommended.* 
#' @srrstatsTODO {ML7.10} *The successful extraction of information on paths taken by optimizers (see **ML5.1**, above), should be tested, including testing the general properties, but not necessarily actual values of, such data.* 
#' @srrstatsTODO {ML7.11} *All performance metrics available for a given class of trained model should be thoroughly tested and compared.*
#' @srrstatsTODO {ML7.11a} *Tests which compare metrics should do so over a range of inputs (generally implying differently trained models) to demonstrate relative advantages and disadvantages of different metrics.*
#' @noRd
NULL

#' NA_standards
#'
#' Any non-applicable standards can have their tags changed from `@srrstatsTODO`
#' to `@srrstatsNA`, and placed together in this block, along with explanations
#' for why each of these standards have been deemed not applicable.
#' (These comments may also be deleted at any time.)
#'
#' @srrstatsNA {G2.5} No estimator distinguishes ordered from unordered
#'   factors; categorical predictors are treated as unordered, so the
#'   `ordered` factor expectations do not apply.
#' @srrstatsNA {G2.11} rmorie does not support `units`-style columns with
#'   non-standard class attributes; tabular inputs are plain numeric /
#'   character / factor columns.
#' @srrstatsNA {G5.4c} Correctness is established by parameter recovery
#'   from simulated data and by cross-implementation parity (R vs the
#'   Python port, and wrappers vs their upstream CRAN packages), not by
#'   stored outputs from a published reference implementation.
#' @srrstatsNA {G5.7} Algorithm performance / scaling with data size is
#'   not part of the test suite; the tested property is statistical
#'   correctness, not computational scaling.
#' @srrstatsNA {G5.10} There is no extended-test switch (e.g. an
#'   environment variable gating a slower test tier); all tests run in
#'   the standard suite.
#' @srrstatsNA {G5.11} No tests require large downloaded data, so there
#'   is no separate extended-data test tier.
#' @srrstatsNA {G5.11a} As no downloaded data is needed for tests, there
#'   is no download helper for reproducing them.
#' @srrstatsNA {G5.12} There are no extended-test conditions to document,
#'   as there is no extended-test tier.
#' @srrstatsNA {G1.5} No associated publication makes performance claims;
#'   the methods/applications paper is in preparation and reports no
#'   comparative benchmarks, so there are no performance results to
#'   reproduce.
#' @srrstatsNA {G1.6} rmorie makes no performance claims against
#'   alternative R packages, so no comparative-performance code applies.
#' @noRd
NULL
