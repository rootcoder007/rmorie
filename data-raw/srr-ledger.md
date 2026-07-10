# srr statistical-standards ledger — rmorie

Generated 2026-07-10 from `R/srr-stats-standards.R` (`srr::srr_stats_roxygen`, srr 1.0.0).

Tracks every rOpenSci statistical-software standard claimed for the whole-package **statistical** track submission. Status starts at `TODO` for all; flip to `done` (`@srrstats`) or `NA` (`@srrstatsNA`) as each is addressed in source. This file is the human progress tracker; `srr::srr_stats_pre_submit()` is the machine gate.

**443 standards** across 9 categories.

| Category | Standards |
|---|---|
| General (G) | 68 |
| Regression & Supervised Learning (RE) | 48 |
| Time Series (TS) | 55 |
| Spatial (SP) | 45 |
| Exploratory Data Analysis (EA) | 34 |
| Probability Distributions (PD) | 14 |
| Dimensionality Reduction, Clustering & Unsupervised (UL) | 33 |
| Bayesian & Monte Carlo (BS) | 56 |
| Machine Learning (ML) | 90 |


## General (G) — 68 standards

| ID | Status | Standard |
|---|---|---|
| G1.0 | done | Statistical Software should list at least one primary reference from published academic literature. |
| G1.1 | done | Statistical Software should document whether the algorithm(s) it implements are:* - *The first implementation of a novel algorithm*; or - *The first implemen... |
| G1.2 | done | Statistical Software should include a* Life Cycle Statement *describing current and anticipated future states of development. |
| G1.3 | done | All statistical terminology should be clarified and unambiguously defined. |
| G1.4 | done | Software should use [`roxygen2`](https://roxygen2.r-lib.org/) to document all functions. |
| G1.4a | done | All internal (non-exported) functions should also be documented in standard [`roxygen2`](https://roxygen2.r-lib.org/) format, along with a final `@noRd` tag ... |
| G1.5 | done | Software should include all code necessary to reproduce results which form the basis of performance claims made in associated publications. |
| G1.6 | done | Software should include code necessary to compare performance claims with alternative implementations in other R packages. |
| G2.0 | done | Implement assertions on lengths of inputs, particularly through asserting that inputs expected to be single- or multi-valued are indeed so. |
| G2.0a | done | Provide explicit secondary documentation of any expectations on lengths of inputs |
| G2.1 | done | Implement assertions on types of inputs (see the initial point on nomenclature above). |
| G2.1a | done | Provide explicit secondary documentation of expectations on data types of all vector inputs. |
| G2.2 | done | Appropriately prohibit or restrict submission of multivariate input to parameters expected to be univariate. |
| G2.3 | done | For univariate character input: |
| G2.3a | done | Use `match.arg()` or equivalent where applicable to only permit expected values. |
| G2.3b | done | Either: use `tolower()` or equivalent to ensure input of character parameters is not case dependent; or explicitly document that parameters are strictly case... |
| G2.4 | done | Provide appropriate mechanisms to convert between different data types, potentially including: |
| G2.4a | done | explicit conversion to `integer` via `as.integer()` |
| G2.4b | done | explicit conversion to continuous via `as.numeric()` |
| G2.4c | done | explicit conversion to character via `as.character()` (and not `paste` or `paste0`) |
| G2.4d | done | explicit conversion to factor via `as.factor()` |
| G2.4e | done | explicit conversion from factor via `as...()` functions |
| G2.5 | done | Where inputs are expected to be of `factor` type, secondary documentation should explicitly state whether these should be `ordered` or not, and those inputs ... |
| G2.6 | done | Software which accepts one-dimensional input should ensure values are appropriately pre-processed regardless of class structures. |
| G2.7 | done | Software should accept as input as many of the above standard tabular forms as possible, including extension to domain-specific forms. |
| G2.8 | done | Software should provide appropriate conversion or dispatch routines as part of initial pre-processing to ensure that all other sub-functions of a package rec... |
| G2.9 | done | Software should issue diagnostic messages for type conversion in which information is lost (such as conversion of variables from factor to character; standar... |
| G2.10 | done | Software should ensure that extraction or filtering of single columns from tabular inputs should not presume any particular default behaviour, and should ens... |
| G2.11 | done | Software should ensure that `data.frame`-like tabular objects which have columns which do not themselves have standard class attributes (typically, `vector`)... |
| G2.12 | done | Software should ensure that `data.frame`-like tabular objects which have list columns should ensure that those columns are appropriately pre-processed either... |
| G2.13 | done | Statistical Software should implement appropriate checks for missing data as part of initial pre-processing prior to passing data to analytic algorithms. |
| G2.14 | done | Where possible, all functions should provide options for users to specify how to handle missing (`NA`) data, with options minimally including: |
| G2.14a | done | error on missing data |
| G2.14b | done | ignore missing data with default warnings or messages issued |
| G2.14c | done | replace missing data with appropriately imputed values |
| G2.15 | done | Functions should never assume non-missingness, and should never pass data with potential missing values to any base routines with default `na.rm = FALSE`-typ... |
| G2.16 | done | All functions should also provide options to handle undefined values (e.g., `NaN`, `Inf` and `-Inf`), including potentially ignoring or removing such values. |
| G3.0 | done | Statistical software should never compare floating point numbers for equality. All numeric equality comparisons should either ensure that they are made betwe... |
| G3.1 | done | Statistical software which relies on covariance calculations should enable users to choose between different algorithms for calculating covariances, and shou... |
| G3.1a | done | The ability to use arbitrarily specified covariance methods should be documented (typically in examples or vignettes). |
| G4.0 | done | Statistical Software which enables outputs to be written to local files should parse parameters specifying file names to ensure appropriate file suffixes are... |
| G5.0 | done | Where applicable or practicable, tests should use standard data sets with known properties (for example, the [NIST Standard Reference Datasets](https://www.i... |
| G5.1 | done | Data sets created within, and used to test, a package should be exported (or otherwise made generally available) so that users can confirm tests and run exam... |
| G5.2 | done | Appropriate error and warning behaviour of all functions should be explicitly demonstrated through tests. In particular, |
| G5.2a | done | Every message produced within R code by `stop()`, `warning()`, `message()`, or equivalent should be unique |
| G5.2b | done | Explicit tests should demonstrate conditions which trigger every one of those messages, and should compare the result with expected values. |
| G5.3 | done | For functions which are expected to return objects containing no missing (`NA`) or undefined (`NaN`, `Inf`) values, the absence of any such values in return ... |
| G5.4 | done | Correctness tests** *to test that statistical algorithms produce expected results to some fixed test data sets (potentially through comparisons using binding... |
| G5.4a | done | For new methods, it can be difficult to separate out correctness of the method from the correctness of the implementation, as there may not be reference for ... |
| G5.4b | done | For new implementations of existing methods, correctness tests should include tests against previous implementations. Such testing may explicitly call those ... |
| G5.4c | done | Where applicable, stored values may be drawn from published paper outputs when applicable and where code from original implementations is not available |
| G5.5 | done | Correctness tests should be run with a fixed random seed |
| G5.6 | done | Parameter recovery tests** *to test that the implementation produce expected results given data with known properties. For instance, a linear regression algo... |
| G5.6a | done | Parameter recovery tests should generally be expected to succeed within a defined tolerance rather than recovering exact values. |
| G5.6b | done | Parameter recovery tests should be run with multiple random seeds when either data simulation or the algorithm contains a random component. (When long-runnin... |
| G5.7 | done | Algorithm performance tests** *to test that implementation performs as expected as properties of data change. For instance, a test may show that parameters a... |
| G5.8 | done | Edge condition tests** *to test that these conditions produce expected behaviour such as clear warnings or errors when confronted with data with extreme prop... |
| G5.8a | done | Zero-length data |
| G5.8b | done | Data of unsupported types (e.g., character or complex numbers in for functions designed only for numeric data) |
| G5.8c | done | Data with all-`NA` fields or columns or all identical fields or columns |
| G5.8d | done | Data outside the scope of the algorithm (for example, data with more fields (columns) than observations (rows) for some regression algorithms) |
| G5.9 | done | Noise susceptibility tests** *Packages should test for expected stochastic behaviour, such as through the following conditions: |
| G5.9a | done | Adding trivial noise (for example, at the scale of `.Machine$double.eps`) to data does not meaningfully change results |
| G5.9b | done | Running under different random seeds or initial conditions does not meaningfully change results |
| G5.10 | done | Extended tests should included and run under a common framework with other tests but be switched on by flags such as as a `<MYPKG>_EXTENDED_TESTS="true"` env... |
| G5.11 | done | Where extended tests require large data sets or other assets, these should be provided for downloading and fetched as part of the testing workflow. |
| G5.11a | done | When any downloads of additional data necessary for extended tests fail, the tests themselves should not fail, rather be skipped and implicitly succeed with ... |
| G5.12 | done | Any conditions necessary to run extended tests such as platform requirements, memory, expected runtime, and artefacts produced that may need manual inspectio... |

## Regression & Supervised Learning (RE) — 48 standards

| ID | Status | Standard |
|---|---|---|
| RE1.0 | done | Regression Software should enable models to be specified via a formula interface, unless reasons for not doing so are explicitly documented. |
| RE1.1 | done | Regression Software should document how formula interfaces are converted to matrix representations of input data. |
| RE1.2 | done | Regression Software should document expected format (types or classes) for inputting predictor variables, including descriptions of types or classes which ar... |
| RE1.3 | done | Regression Software which passes or otherwise transforms aspects of input data onto output structures should ensure that those output structures retain all r... |
| RE1.3a | done | Where otherwise relevant information is not transferred, this should be explicitly documented. |
| RE1.4 | done | Regression Software should document any assumptions made with regard to input data; for example distributional assumptions, or assumptions that predictor dat... |
| RE2.0 | done | Regression Software should document any transformations applied to input data, for example conversion of label-values to `factor`, and should provide ways to... |
| RE2.1 | done | Regression Software should implement explicit parameters controlling the processing of missing values, ideally distinguishing `NA` or `NaN` values from `Inf`... |
| RE2.2 | NA | Regression Software should provide different options for processing missing values in predictor and response data. For example, it should be possible to fit ... |
| RE2.3 | NA | Where applicable, Regression Software should enable data to be centred (for example, through converting to zero-mean equivalent values; or to z-scores) or of... |
| RE2.4 | done | Regression Software should implement pre-processing routines to identify whether aspects of input data are perfectly collinear, notably including: |
| RE2.4a | done | Perfect collinearity among predictor variables |
| RE2.4b | NA | Perfect collinearity between independent and dependent variables |
| RE3.0 | done | Issue appropriate warnings or other diagnostic messages for models which fail to converge. |
| RE3.1 | done | Enable such messages to be optionally suppressed, yet should ensure that the resultant model object nevertheless includes sufficient data to identify lack of... |
| RE3.2 | done | Ensure that convergence thresholds have sensible default values, demonstrated through explicit documentation. |
| RE3.3 | done | Allow explicit setting of convergence thresholds, unless reasons against doing so are explicitly documented. |
| RE4.0 | done | Regression Software should return some form of "model" object, generally through using or modifying existing class structures for model objects (such as `lm`... |
| RE4.1 | NA | Regression Software may enable an ability to generate a model object without actually fitting values. This may be useful for controlling batch processing of ... |
| RE4.2 | done | Model coefficients (via `coef()` / `coefficients()`) |
| RE4.3 | done | Confidence intervals on those coefficients (via `confint()`) |
| RE4.4 | done | The specification of the model, generally as a formula (via `formula()`) |
| RE4.5 | done | Numbers of observations submitted to model (via `nobs()`) |
| RE4.6 | done | The variance-covariance matrix of the model parameters (via `vcov()`) |
| RE4.7 | NA | Where appropriate, convergence statistics |
| RE4.8 | NA | Response variables, and associated "metadata" where applicable. |
| RE4.9 | NA | Modelled values of response variables. |
| RE4.10 | done | Model Residuals, including sufficient documentation to enable interpretation of residuals, and to enable users to submit residuals to their own tests. |
| RE4.11 | done | Goodness-of-fit and other statistics associated such as effect sizes with model coefficients. |
| RE4.12 | done | Where appropriate, functions used to transform input data, and associated inverse transform functions. |
| RE4.13 | NA | Predictor variables, and associated "metadata" where applicable. |
| RE4.14 | NA | Where possible, values should also be provided for extrapolation or forecast *errors*. |
| RE4.15 | NA | Sufficient documentation and/or testing should be provided to demonstrate that forecast errors, confidence intervals, or equivalent values increase with fore... |
| RE4.16 | NA | Regression Software which models distinct responses for different categorical groups should include the ability to submit new groups to `predict()` methods. |
| RE4.17 | done | Model objects returned by Regression Software should implement or appropriately extend a default `print` method which provides an on-screen summary of model ... |
| RE4.18 | NA | Regression Software may also implement `summary` methods for model objects, and in particular should implement distinct `summary` methods for any cases in wh... |
| RE5.0 | NA | Scaling relationships between sizes of input data (numbers of observations, with potential extension to numbers of variables/columns) and speed of algorithm. |
| RE6.0 | NA | Model objects returned by Regression Software (see* **RE4***) should have default `plot` methods, either through explicit implementation, extension of method... |
| RE6.1 | NA | Where the default `plot` method is **NOT** a generic `plot` method dispatched on the class of return objects (that is, through an S3-type `plot.<myclass>` fu... |
| RE6.2 | NA | The default `plot` method should produce a plot of the `fitted` values of the model, with optional visualisation of confidence intervals or equivalent. |
| RE6.3 | NA | Where a model object is used to generate a forecast (for example, through a `predict()` method), the default `plot` method should provide clear visual distin... |
| RE7.0 | done | Tests with noiseless, exact relationships between predictor (independent) data. |
| RE7.0a | done | In particular, these tests should confirm ability to reject perfectly noiseless input data. |
| RE7.1 | done | Tests with noiseless, exact relationships between predictor (independent) and response (dependent) data. |
| RE7.1a | NA | In particular, these tests should confirm that model fitting is at least as fast or (preferably) faster than testing with equivalent noisy data (see RE2.4b). |
| RE7.2 | NA | Demonstrate that output objects retain aspects of input data such as row or case names (see **RE1.3**). |
| RE7.3 | done | Demonstrate and test expected behaviour when objects returned from regression software are submitted to the accessor methods of **RE4.2**--**RE4.7**. |
| RE7.4 | NA | Extending directly from **RE4.15**, where appropriate, tests should demonstrate and confirm that forecast errors, confidence intervals, or equivalent values ... |

## Time Series (TS) — 55 standards

| ID | Status | Standard |
|---|---|---|
| TS1.0 | done | Time Series Software should use and rely on explicit class systems developed for representing time series data, and should not permit generic, non-time-serie... |
| TS1.1 | done | Time Series Software should explicitly document the types and classes of input data able to be passed to each function. |
| TS1.2 | done | Time Series Software should implement validation routines to confirm that inputs are of acceptable classes (or represented in otherwise appropriate ways for ... |
| TS1.3 | done | Time Series Software should implement a single pre-processing routine to validate input data, and to appropriately transform it to a single uniform type to b... |
| TS1.4 | done | The pre-processing function described above should maintain all time- or date-based components or attributes of input data. |
| TS1.5 | done | The software should ensure strict ordering of the time, frequency, or equivalent ordering index variable. |
| TS1.6 | done | Any violations of ordering should be caught in the pre-processing stages of all functions. |
| TS1.7 | done | Accept inputs defined via the [`units` package](https://github.com/r-quantities/units/) for attributing SI units to R vectors. |
| TS1.8 | done | Where time intervals or periods may be days or months, be explicit about the system used to represent such, particularly regarding whether a calendar system ... |
| TS2.0 | done | Time Series Software which presumes or requires regular data should only allow **explicit** missing values, and should issue appropriate diagnostic messages,... |
| TS2.1 | done | Where possible, all functions should provide options for users to specify how to handle missing data, with options minimally including: |
| TS2.1a | done | error on missing data; or. |
| TS2.1b | done | warn or ignore missing data, and proceed to analyse irregular data, ensuring that results from function calls with regular yet missing data return identical ... |
| TS2.1c | done | replace missing data with appropriately imputed values. |
| TS2.2 | done | Consider stationarity of all relevant moments - typically first (mean) and second (variance) order, or otherwise document why such consideration may be restr... |
| TS2.3 | done | Explicitly document all assumptions and/or requirements of stationarity |
| TS2.4 | done | Implement appropriate checks for all relevant forms of stationarity, and either: |
| TS2.4a | done | issue diagnostic messages or warnings; or |
| TS2.4b | done | enable or advise on appropriate transformations to ensure stationarity. |
| TS2.5 | done | Incorporate a system to ensure that both row and column orders follow the same ordering as the underlying time series data. This may, for example, be done by... |
| TS2.6 | done | Where applicable, auto-covariance matrices should also include specification of appropriate units. |
| TS3.0 | done | Provide tests to demonstrate at least one case in which errors widen appropriately with forecast horizon. |
| TS3.1 | done | If possible, provide at least one test which violates TS3.0 |
| TS3.2 | done | Document the general drivers of forecast errors or horizons, as demonstrated via the particular cases of TS3.0 and TS3.1 |
| TS3.3 | done | Either: |
| TS3.3a | done | Document, preferable via an example, how to trim forecast values based on a specified error margin or equivalent; or |
| TS3.3b | done | Provide an explicit mechanism to trim forecast values to a specified error margin, either via an explicit post-processing function, or via an input parameter... |
| TS4.0 | done | Return values should either: |
| TS4.0a | done | Be in same class as input data, for example by using the [`tsbox` package](https://www.tsbox.help/) to re-convert from standard internal format (see 1.4, abo... |
| TS4.0b | done | Be in a unique, preferably class-defined, format. |
| TS4.1 | done | Any units included as attributes of input data should also be included within return values. |
| TS4.2 | done | The type and class of all return values should be explicitly documented. |
| TS4.3 | done | Return values should explicitly include all appropriate units and/or time scales |
| TS4.4 | done | Document the effect of any such transformations on forecast data, including potential effects on both first- and second-order estimates. |
| TS4.5 | done | In decreasing order of preference, either: |
| TS4.5a | done | Provide explicit routines or options to back-transform data commensurate with original, non-stationary input data |
| TS4.5b | done | Demonstrate how data may be back-transformed to a form commensurate with original, non-stationary input data. |
| TS4.5c | done | Document associated limitations on forecast values |
| TS4.6 | done | Time Series Software which implements or otherwise enables forecasting should return either: |
| TS4.6a | done | A distribution object, for example via one of the many packages described in the CRAN Task View on [Probability Distributions](https://cran.r-project.org/web... |
| TS4.6b | done | For each variable to be forecast, predicted values equivalent to first- and second-order moments (for example, mean and standard error values). |
| TS4.6c | done | Some more general indication of error associated with forecast estimates. |
| TS4.7 | done | Ensure that forecast (modelled) values are clearly distinguished from observed (model or input) values, either (in this case in no order of preference) by |
| TS4.7a | done | Returning forecast values alone |
| TS4.7b | done | Returning distinct list items for model and forecast values |
| TS4.7c | done | Combining model and forecast values into a single return object with an appropriate additional column clearly distinguishing the two kinds of data. |
| TS5.0 | done | Implement default `plot` methods for any implemented class system. |
| TS5.1 | done | When representing results in temporal domain(s), ensure that one axis is clearly labelled "time" (or equivalent), with continuous units. |
| TS5.2 | done | Default to placing the "time" (or equivalent) variable on the horizontal axis. |
| TS5.3 | done | Ensure that units of the time, frequency, or index variable are printed by default on the axis. |
| TS5.4 | done | For frequency visualization, abscissa spanning $[-\pi, \pi]$ should be avoided in favour of positive units of $[0, 2\pi]$ or $[0, 0.5]$, in all cases with ap... |
| TS5.5 | done | Provide options to determine whether plots of data with missing values should generate continuous or broken lines. |
| TS5.6 | done | By default indicate distributional limits of forecast on plot |
| TS5.7 | done | By default include model (input) values in plot, as well as forecast (output) values |
| TS5.8 | done | By default provide clear visual distinction between model (input) values and forecast (output) values. |

## Spatial (SP) — 45 standards

| ID | Status | Standard |
|---|---|---|
| SP1.0 | done | Spatial software should explicitly indicate its domain of applicability, and in particular distinguish whether the software may be applied in Cartesian/recti... |
| SP1.1 | done | Spatial software should explicitly indicate its dimensional domain of applicability, in particular through identifying whether it is applicable to two or thr... |
| SP2.0 | NA | Spatial software should only accept input data of one or more classes explicitly developed to represent such data. |
| SP2.0a | NA | Where new classes are implemented, conversion to other common classes for spatial data in R should be documented. |
| SP2.0b | done | Class systems should ensure that functions error appropriately, rather than merely warning, in response to data from inappropriate spatial domains. |
| SP2.1 | done | Spatial Software should not use the [`sp` package](https://cran.r-project.org/package=sp), rather should use [`sf`](https://cran.r-project.org/package=sf). |
| SP2.2 | done | Geographical Spatial Software should ensure maximal compatibility with established packages and workflows, minimally through: |
| SP2.2a | done | Clear and extensive documentation demonstrating how routines from that software may be embedded within, or otherwise adapted to, workflows which rely on thes... |
| SP2.2b | NA | Tests which clearly demonstrate that routines from that software may be successfully translated into forms and workflows which rely on these established pack... |
| SP2.3 | NA | Software which accepts spatial input data in any standard format established in other R packages (such as any of the formats able to be read by [`GDAL`](http... |
| SP2.4 | NA | Geographical Spatial Software should be compliant with version 6 or larger of* [`PROJ`](https://proj.org/), *and with* `WKT2` *representations. The primary i... |
| SP2.4a | NA | Software should not permit coordinate reference systems to be represented merely by so-called "PROJ4-strings", but should use at least WKT2. |
| SP2.5 | NA | Class systems for input data must contain meta data on associated coordinate reference systems. |
| SP2.5a | NA | Software which implements new classes to input spatial data (or the spatial components of more general data) should provide an ability to convert such input ... |
| SP2.6 | done | Spatial Software should explicitly document the types and classes of input data able to be passed to each function. |
| SP2.7 | done | Spatial Software should implement validation routines to confirm that inputs are of acceptable classes (or represented in otherwise appropriate ways for soft... |
| SP2.8 | NA | Spatial Software should implement a single pre-processing routine to validate input data, and to appropriately transform it to a single uniform type to be pa... |
| SP2.9 | NA | The pre-processing function described above should maintain those metadata attributes of input data which are relevant or important to core algorithms or ret... |
| SP3.0 | done | Spatial software which considers spatial neighbours should enable user control over neighbourhood forms and sizes. In particular: |
| SP3.0a | done | Neighbours (able to be expressed) on regular grids should be able to be considered in both rectangular only, or rectangular and diagonal (respectively "rook"... |
| SP3.0b | done | Neighbourhoods in irregular spaces should be minimally able to be controlled via an integer number of neighbours, an area (or equivalent distance defining an... |
| SP3.1 | done | Spatial software which considers spatial neighbours should wherever possible enable neighbour contributions to be weighted by distance (or other continuous w... |
| SP3.2 | NA | Spatial software which relies on sampling from input data (even if only of spatial coordinates) should enable sampling procedures to be based on local spatia... |
| SP3.3 | done | Spatial regression software should explicitly quantify and distinguish autocovariant or autoregressive processes from those covariant or regressive processes... |
| SP3.4 | done | Where possible, spatial clustering software should avoid using standard non-spatial clustering algorithms in which spatial proximity is merely represented by... |
| SP3.5 | NA | Spatial machine learning software should ensure that broadcasting procedures for reconciling inputs of different dimensions are **not** applied*. |
| SP3.6 | NA | Spatial machine learning software should document (and, where possible, test) the potential effects of different sampling procedures |
| SP4.0 | done | Return values should either: |
| SP4.0a | NA | Be in same class as input data, or |
| SP4.0b | done | Be in a unique, preferably class-defined, format. |
| SP4.1 | NA | Any aspects of input data which are included in output data (either directly, or in some transformed form) and which contain units should ensure those same u... |
| SP4.2 | done | The type and class of all return values should be explicitly documented. |
| SP5.0 | NA | Implement default `plot` methods for any implemented class system. |
| SP5.1 | NA | Implement appropriate placement of variables along x- and y-axes. |
| SP5.2 | NA | Ensure that axis labels include appropriate units. |
| SP5.3 | NA | Offer an ability to generate interactive (generally `html`-based) visualisations of results. |
| SP6.0 | NA | Software which implements routines for transforming coordinates of input data should include tests which demonstrate ability to recover the original coordina... |
| SP6.1 | NA | All functions which can be applied to both Cartesian and curvilinear data should be tested through application to both. |
| SP6.1a | NA | Functions which may yield inaccurate results when applied to data in one or the other forms (such as the preceding examples of centroids and buffers from ell... |
| SP6.1b | NA | Functions which yield accurate results regardless of whether input data are rectilinear or curvilinear should demonstrate equivalent accuracy in both cases, ... |
| SP6.2 | NA | Geographical Software should include tests with extreme geographical coordinates, minimally including extension to polar extremes of +/-90 degrees. |
| SP6.3 | NA | Spatial Software which considers spatial neighbours should explicitly test all possible ways of defining them, and should explicitly compare quantitative eff... |
| SP6.4 | NA | Spatial Software which considers spatial neighbours should explicitly test effects of different schemes to weight neighbours by spatial proximity. |
| SP6.5 | NA | Spatial Unsupervised Learning Software which uses clustering algorithms should implement tests which explicitly compare results with equivalent results obtai... |
| SP6.6 | NA | Spatial Machine Learning Software should implement tests which explicitly demonstrate the detrimental consequences of sampling test and training data from th... |

## Exploratory Data Analysis (EA) — 34 standards

| ID | Status | Standard |
|---|---|---|
| EA1.0 | done | Identify one or more target audiences for whom the software is intended |
| EA1.1 | done | Identify the kinds of data the software is capable of analysing (see *Kinds of Data* below). |
| EA1.2 | done | Identify the kinds of questions the software is intended to help explore. |
| EA1.3 | done | Identify the kinds of data each function is intended to accept as input |
| EA2.0 | NA | EDA Software which accepts standard tabular data and implements or relies upon extensive table filter and join operations should utilise an **index column** ... |
| EA2.1 | NA | All values in an index column must be unique, and this uniqueness should be affirmed as a pre-processing step for all input data. |
| EA2.2 | NA | Index columns should be explicitly identified, either: |
| EA2.2a | NA | by using an appropriate class system, or |
| EA2.2b | NA | through setting an `attribute` on a table, `x`, of `attr(x, "index") <- <index_col_name>`. |
| EA2.3 | NA | Table join operations should not be based on any assumed variable or column names |
| EA2.4 | NA | Use and demand an explicit class system for such input (for example, via the [`DM` package](https://github.com/krlmlr/dm)). |
| EA2.5 | NA | Ensure all individual tables follow the above standards for Index Columns |
| EA2.6 | NA | Routines should appropriately process vector data regardless of additional attributes |
| EA3.0 | NA | The algorithmic components of EDA Software should enable automated extraction and/or reporting of statistics as some sufficiently "meta" level (such as varia... |
| EA3.1 | NA | EDA software should enable standardised comparison of inputs, processes, models, or outputs which previous or reference implementations otherwise only enable... |
| EA4.0 | done | EDA Software should ensure all return results have types which are consistent with input types. |
| EA4.1 | NA | EDA Software should implement parameters to enable explicit control of numeric precision |
| EA4.2 | done | The primary routines of EDA Software should return objects for which default `print` and `plot` methods give sensible results. Default `summary` methods may ... |
| EA5.0 | NA | Graphical presentation in EDA software should be as accessible as possible or practicable. In particular, EDA software should consider accessibility in terms... |
| EA5.0a | NA | Typeface sizes, which should default to sizes which explicitly enhance accessibility |
| EA5.0b | NA | Default colour schemes, which should be carefully constructed to ensure accessibility. |
| EA5.1 | NA | Any explicit specifications of typefaces which override default values provided through other packages (including the `graphics` package) should consider acc... |
| EA5.2 | done | Screen-based output should never rely on default print formatting of `numeric` types, rather should also use some version of `round(., digits)`, `formatC`, `... |
| EA5.3 | NA | Column-based summary statistics should always indicate the `storage.mode`, `class`, or equivalent defining attribute of each column. |
| EA5.4 | NA | All visualisations should ensure values are rounded sensibly (for example, via `pretty()` function). |
| EA5.5 | NA | All visualisations should include units on all axes where such are specified or otherwise obtainable from input data or other routines. |
| EA5.6 | NA | Any packages which internally bundle libraries used for dynamic visualization and which are also bundled in other, pre-existing R packages, should explain th... |
| EA6.0 | done | Return values from all functions should be tested, including tests for the following characteristics: |
| EA6.0a | done | Classes and types of objects |
| EA6.0b | done | Dimensions of tabular objects |
| EA6.0c | done | Column names (or equivalent) of tabular objects |
| EA6.0d | done | Classes or types of all columns contained within `data.frame`-type tabular objects |
| EA6.0e | done | Values of single-valued objects; for `numeric` values either using `testthat::expect_equal()` or equivalent with a defined value for the `tolerance` paramete... |
| EA6.1 | NA | The properties of graphical output from EDA software should be explicitly tested, for example via the [`vdiffr` package](https://github.com/r-lib/vdiffr) or ... |

## Probability Distributions (PD) — 14 standards

| ID | Status | Standard |
|---|---|---|
| PD1.0 | done | Software should provide references justifying choice and usage of particular probability distributions. |
| PD2.0 | NA | Where possible, software should represent probability distributions using a package for general representation. |
| PD3.0 | NA | Manipulation of probability distributions should very generally be analytic, with numeric manipulations only implemented with clear justification (ideally in... |
| PD3.1 | NA | Operations on probability distributions should generally be contained within separate functions which themselves accept the names of the distributions as one... |
| PD3.2 | done | Use of optimisation routines to estimate parameters from probability distributions should explicitly specify and explain values of all parameters, including ... |
| PD3.3 | done | Return objects which include values generated from optimisation algorithms should include information on optimisation algorithm and performance, minimally in... |
| PD3.4 | NA | Use of routines to integrate probability distributions should explicitly document conditions under which integrals are expected to remain stable, and ideally... |
| PD3.5 | NA | Integration routines should only rely on discrete summation where such use can be justified (for example, through providing a literature reference), in which... |
| PD3.5a | NA | Use of discrete summation to approximate integrals must demonstrate that the Reimann sum has a finite limit (or, equivalently, must explicitly describe the c... |
| PD4.0 | done | The numeric outputs of probability distribution functions should be tested, not just output structures. These tests should generally be tests for numeric equ... |
| PD4.1 | done | Tests for numeric equality should compare the output of of probability distribution functions with the output of code which explicitly demonstrates how such ... |
| PD4.2 | done | All functions constructed in accordance with **PD2.1** - that is, which use a fixed distribution, and which name that distribution as an input parameter - sh... |
| PD4.3 | done | Tests of optimisation or integration algorithms should compare default results with results generated with alternative values for every parameter, including ... |
| PD4.4 | done | Tests of optimisation or integration algorithms should compare equivalent results generated with at least one alternative algorithm. |

## Dimensionality Reduction, Clustering & Unsupervised (UL) — 33 standards

| ID | Status | Standard |
|---|---|---|
| UL1.0 | done | Unsupervised Learning Software should explicitly document expected format (types or classes) for input data, including descriptions of types or classes which... |
| UL1.1 | done | Unsupervised Learning Software should provide distinct sub-routines to assert that all input data is of the expected form, and issue informative error messag... |
| UL1.2 | NA | Unsupervised learning which uses row or column names to label output objects should assert that input data have non-default row or column names, and issue an... |
| UL1.3 | NA | Unsupervised Learning Software should transfer all relevant aspects of input data, notably including row and column names, and potentially information from o... |
| UL1.3a | done | Where otherwise relevant information is not transferred, this should be explicitly documented. |
| UL1.4 | done | Unsupervised Learning Software should document any assumptions made with regard to input data; for example assumptions about distributional forms or location... |
| UL1.4a | done | Software which responds qualitatively differently to input data which has components on markedly different scales should explicitly document such differences... |
| UL1.4b | NA | Examples or other documentation should not use `scale()` or equivalent transformations without explaining why scale is applied, and explicitly illustrating a... |
| UL2.0 | NA | Routines likely to give unreliable or irreproducible results in response to violations of assumptions regarding input data (see UL1.4) should implement pre-p... |
| UL2.1 | done | Unsupervised Learning Software should document any transformations applied to input data, for example conversion of label-values to `factor`, and should prov... |
| UL2.2 | NA | Unsupervised Learning Software which accepts missing values in input data should implement explicit parameters controlling the processing of missing values, ... |
| UL2.3 | done | Unsupervised Learning Software should implement pre-processing routines to identify whether aspects of input data are perfectly collinear. |
| UL3.0 | NA | Algorithms which apply sequential labels to input data (such as clustering or partitioning algorithms) should ensure that the sequence follows decreasing gro... |
| UL3.1 | done | Dimensionality reduction or equivalent algorithms which label dimensions should ensure that that sequences of labels follows decreasing "importance" (for exa... |
| UL3.2 | NA | Unsupervised Learning Software for which input data does not generally include labels (such as `array`-like data with no row names) should provide an additio... |
| UL3.3 | NA | Where applicable, Unsupervised Learning Software should implement routines to predict the properties (such as numerical ordinates, or cluster memberships) of... |
| UL3.4 | done | Objects returned from Unsupervised Learning Software which labels, categorise, or partitions data into discrete groups should include, or provide immediate a... |
| UL4.0 | done | Unsupervised Learning Software should return some form of "model" object, generally through using or modifying existing class structures for model objects, o... |
| UL4.1 | NA | Unsupervised Learning Software may enable an ability to generate a model object without actually fitting values. This may be useful for controlling batch pro... |
| UL4.2 | done | The return object from Unsupervised Learning Software should include, or otherwise enable immediate extraction of, all parameters used to control the algorit... |
| UL4.3 | done | Model objects returned by Unsupervised Learning Software should implement or appropriately extend a default `print` method which provides an on-screen summar... |
| UL4.3a | NA | The default `print` method should always ensure only a restricted number of rows of any result matrices or equivalent are printed to the screen. |
| UL4.4 | NA | Unsupervised Learning Software should also implement `summary` methods for model objects which should summarise the primary statistics used in generating the... |
| UL6.0 | NA | Objects returned by Unsupervised Learning Software should have default `plot` methods, either through explicit implementation, extension of methods for exist... |
| UL6.1 | NA | Where the default `plot` method is **NOT** a generic `plot` method dispatched on the class of return objects (that is, through an S3-type `plot.<myclass>` fu... |
| UL6.2 | NA | Where default plot methods include labelling components of return objects (such as cluster labels), routines should ensure that labels are automatically plac... |
| UL7.0 | done | Inappropriate types of input data are rejected with expected error messages. |
| UL7.1 | NA | Tests should demonstrate that violations of assumed input properties yield unreliable or invalid outputs, and should clarify how such unreliability or invali... |
| UL7.2 | NA | Demonstrate that labels placed on output data follow decreasing group sizes (**UL3.0**) |
| UL7.3 | NA | Demonstrate that labels on input data are propagated to, or may be recovered from, output data. |
| UL7.4 | NA | Demonstrate that submission of new data to a previously fitted model can generate results more efficiently than initial model fitting. |
| UL7.5 | NA | Batch processing routines should be explicitly tested, commonly via extended tests (see **G4.10**--**G4.12**). |
| UL7.5a | NA | Tests of batch processing routines should demonstrate that equivalent results are obtained from direct (non-batch) processing. |

## Bayesian & Monte Carlo (BS) — 56 standards

| ID | Status | Standard |
|---|---|---|
| BS1.0 | done | Bayesian software which uses the term "hyperparameter" should explicitly clarify the meaning of that term in the context of that software. |
| BS1.1 | done | Descriptions of how to enter data, both in textual form and via code examples. Both of these should consider the simplest cases of single objects representin... |
| BS1.2 | done | Description of how to specify prior distributions, both in textual form describing the general principles of specifying prior distributions, along with more ... |
| BS1.2a | NA | The main package `README`, either as textual description or example code |
| BS1.2b | NA | At least one package vignette, both as general and applied textual descriptions, and example code |
| BS1.2c | done | Function-level documentation, preferably with code included in examples |
| BS1.3 | done | Description of all parameters which control the computational process (typically those determining aspects such as numbers and lengths of sampling processes,... |
| BS1.3a | NA | Bayesian Software should document, both in text and examples, how to use the output of previous simulations as starting points of subsequent simulations. |
| BS1.3b | done | Where applicable, Bayesian software should document, both in text and examples, how to use different sampling algorithms for a given model. |
| BS1.4 | NA | For Bayesian Software which implements or otherwise enables convergence checkers, documentation should explicitly describe and provide examples of use with a... |
| BS1.5 | NA | For Bayesian Software which implements or otherwise enables multiple convergence checkers, differences between these should be explicitly tested. |
| BS2.1 | done | Bayesian Software should implement pre-processing routines to ensure all input data is dimensionally commensurate, for example by ensuring commensurate lengt... |
| BS2.1a | NA | The effects of such routines should be tested. |
| BS2.2 | done | Ensure that all appropriate validation and pre-processing of distributional parameters are implemented as distinct pre-processing steps prior to submitting t... |
| BS2.3 | NA | Ensure that lengths of vectors of distributional parameters are checked, with no excess values silently discarded (unless such output is explicitly suppresse... |
| BS2.4 | NA | Ensure that lengths of vectors of distributional parameters are commensurate with expected model input (see example immediately below) |
| BS2.5 | done | Where possible, implement pre-processing checks to validate appropriateness of numeric values submitted for distributional parameters; for example, by ensuri... |
| BS2.6 | done | Check that values for computational parameters lie within plausible ranges. |
| BS2.7 | done | Enable starting values to be explicitly controlled via one or more input parameters, including multiple values for software which implements or enables multi... |
| BS2.8 | NA | Enable results of previous runs to be used as starting points for subsequent runs. |
| BS2.9 | done | Ensure each chain is started with a different seed by default. |
| BS2.10 | NA | Issue diagnostic messages when identical seeds are passed to distinct computational chains. |
| BS2.11 | NA | Software which accepts starting values as a vector should provide the parameter with a plural name: for example, "starting_values" and not "starting_value". |
| BS2.12 | done | Bayesian Software should implement at least one parameter controlling the verbosity of output, defaulting to verbose output of all appropriate messages, warn... |
| BS2.13 | done | Bayesian Software should enable suppression of messages and progress indicators, while retaining verbosity of warnings and errors. This should be tested. |
| BS2.14 | NA | Bayesian Software should enable suppression of warnings where appropriate. This should be tested. |
| BS2.15 | NA | Bayesian Software should explicitly enable errors to be caught, and appropriately processed either through conversion to warnings, or otherwise captured in r... |
| BS3.0 | done | Explicitly document assumptions made in regard to missing values; for example that data is assumed to contain no missing (`NA`, `Inf`) values, and that such ... |
| BS3.1 | done | Implement pre-processing routines to diagnose perfect collinearity, and provide appropriate diagnostic messages or warnings |
| BS3.2 | NA | Provide distinct routines for processing perfectly collinear data, potentially bypassing sampling algorithms |
| BS4.0 | done | Packages should document sampling algorithms (generally via literary citation, or reference to other software) |
| BS4.1 | NA | Packages should provide explicit comparisons with external samplers which demonstrate intended advantage of implementation (generally via tests, vignettes, o... |
| BS4.2 | done | Implement at least one means to validate posterior estimates. |
| BS4.3 | done | Implement or otherwise offer at least one type of convergence checker, and provide a documented reference for that implementation. |
| BS4.4 | NA | Enable computations to be stopped on convergence (although not necessarily by default). |
| BS4.5 | NA | Ensure that appropriate mechanisms are provided for models which do not converge. |
| BS4.6 | NA | Implement tests to confirm that results with convergence checker are statistically equivalent to results from equivalent fixed number of samples without conv... |
| BS4.7 | NA | Where convergence checkers are themselves parametrised, the effects of such parameters should also be tested. For threshold parameters, for example, lower va... |
| BS5.0 | done | Return values should include starting value(s) or seed(s), including values for each sequence where multiple sequences are included |
| BS5.1 | done | Return values should include appropriate metadata on types (or classes) and dimensions of input data |
| BS5.2 | done | Bayesian Software should either return the input function or prior distributional specification in the return object; or enable direct access to such via add... |
| BS5.3 | done | Bayesian Software should return convergence statistics or equivalent |
| BS5.4 | NA | Where multiple checkers are enabled, Bayesian Software should return details of convergence checker used |
| BS5.5 | NA | Appropriate diagnostic statistics to indicate absence of convergence should either be returned or immediately able to be accessed. |
| BS6.0 | NA | Software should implement a default `print` method for return objects |
| BS6.1 | NA | Software should implement a default `plot` method for return objects |
| BS6.2 | NA | Software should provide and document straightforward abilities to plot sequences of posterior samples, with burn-in periods clearly distinguished |
| BS6.3 | NA | Software should provide and document straightforward abilities to plot posterior distributional estimates |
| BS6.4 | NA | Software may provide `summary` methods for return objects |
| BS6.5 | NA | Software may provide abilities to plot both sequences of posterior samples and distributional estimates together in single graphic |
| BS7.0 | NA | Software should demonstrate and confirm recovery of parametric estimates of a prior distribution |
| BS7.1 | NA | Software should demonstrate and confirm recovery of a prior distribution in the absence of any additional data or information |
| BS7.2 | NA | Software should demonstrate and confirm recovery of a expected posterior distribution given a specified prior and some input data |
| BS7.3 | NA | Bayesian software should include tests which demonstrate and confirm the scaling of algorithmic efficiency with sizes of input data. |
| BS7.4 | NA | Bayesian software should implement tests which confirm that predicted or fitted values are on (approximately) the same scale as input values. |
| BS7.4a | NA | The implications of any assumptions on scales on input objects should be explicitly tested in this context; for example that the scales of inputs which do no... |

## Machine Learning (ML) — 90 standards

| ID | Status | Standard |
|---|---|---|
| ML1.0 | done | Documentation should make a clear conceptual distinction between training and test data (even where such may ultimately be confounded as described above.) |
| ML1.0a | done | Where these terms are ultimately eschewed, these should nevertheless be used in initial documentation, along with clear explanation of, and justification for... |
| ML1.1 | done | Absent clear justification for alternative design decisions, input data should be expected to be labelled "test", "training", and, where applicable, "validat... |
| ML1.1a | done | The presence and use of these labels should be explicitly confirmed via pre-processing steps (and tested in accordance with **ML7.0**, below). |
| ML1.1b | done | Matches to expected labels should be case-insensitive and based on partial matching such that, for example, "Test", "test", or "testing" should all suffice. |
| ML1.2 | done | Training and test data sets for ML software should be able to be input as a single, generally tabular, data object, with the training and test data distingui... |
| ML1.3 | done | Input data should be clearly partitioned between training and test data (for example, through having each passed as a distinct `list` item), or should enable... |
| ML1.4 | done | Training and test data sets, along with other necessary components such as validation data sets, should be stored in their own distinctly labelled sub-direct... |
| ML1.5 | done | ML software should implement a single function which summarises the contents of test and training (and other) data sets, minimally including counts of number... |
| ML1.6 | done | ML software which does not admit missing values, and which expects no missing values, should implement explicit pre-processing routines to identify whether d... |
| ML1.6a | done | Explain why missing values are not admitted. |
| ML1.6b | done | Provide explicit examples (in function documentation, vignettes, or both) for how missing values may be imputed, rather than simply discarded. |
| ML1.7 | done | ML software which admits missing values should clearly document how such values are processed. |
| ML1.7a | done | Where missing values are imputed, software should offer multiple user-defined ways to impute missing data. |
| ML1.7b | done | Where missing values are imputed, the precise imputation steps should also be explicitly documented, either in tests (see **ML7.2** below), function document... |
| ML1.8 | done | ML software should enable equal treatment of missing values for both training and test data, with optional user ability to control application to either one ... |
| ML2.0 | done | A dedicated function should enable pre-processing steps to be defined and parametrized. |
| ML2.0a | done | That function should return an object which can be directly submitted to a specified model (see section 3, below). |
| ML2.0b | done | Absent explicit justification otherwise, that return object should have a defined class minimally intended to implement a default `print` method which summar... |
| ML2.1 | done | ML software which uses broadcasting to reconcile dimensionally incommensurate input data should offer an ability to at least optionally record transformation... |
| ML2.2 | done | ML software which requires or relies upon numeric transformations of input data (such as change in mean values or variances) should allow optimal explicit sp... |
| ML2.2a | done | Where the parameters have default values, reasons for those particular defaults should be explicitly described. |
| ML2.2b | done | Any extended documentation (such as vignettes) which demonstrates the use of explicit values for numeric transformations should explicitly describe why parti... |
| ML2.3 | done | The values associated with all transformations should be recorded in the object returned by the function described in the preceding standard (**ML2.0**). |
| ML2.4 | done | Default values of all transformations should be explicitly documented, both in documentation of parameters where appropriate (such as for numeric transformat... |
| ML2.5 | done | ML software should provide options to bypass or otherwise switch off all default transformations. |
| ML2.6 | done | Where transformations are implemented via distinct functions, these should be exported to a package's namespace so they can be applied in other contexts. |
| ML2.7 | done | Where possible, documentation should be provided for how transformations may be reversed. For example, documentation may demonstrate how the values retained ... |
| ML3.0 | done | Model specification should be implemented as a distinct stage subsequent to specification of pre-processing routines (see Section 2, above) and prior to actu... |
| ML3.0a | done | A dedicated function should enable models to be specified without actually fitting or training them, or if this (**ML3**) and the following (**ML4**) stages ... |
| ML3.0b | done | That function should accept as input the objects produced by the previous Input Data Specification stage, and defined according to **ML2.0**, above. |
| ML3.0c | done | The function described above (**ML3.0a**) should return an object which can be directly trained as described in the following sub-section (**ML4**). |
| ML3.0d | done | That return object should have a defined class minimally intended to implement a default `print` method which summarises the model specification, including v... |
| ML3.1 | done | ML software should allow the use of both untrained models, specified through model parameters only, as well as pre-trained models. Use of the latter commonly... |
| ML3.2 | done | ML software should enable different models to be applied to the object specifying data inputs and transformations (see sub-sections 1--2, above) without need... |
| ML3.3 | done | Where ML software implements its own distinct classes of model objects, the properties and behaviours of those specific classes of objects should be explicit... |
| ML3.4 | done | Where training rates are used, ML software should provide explicit documentation both in all functions which use training rates, and in extended form such as... |
| ML3.4a | done | Unless explicitly justified otherwise, ML software should offer abilities to automatically determine appropriate or optimal training rates, either as distinc... |
| ML3.4b | done | ML software which provides default values for training rates should clearly document anticipated restrictions of validity of those default values; for exampl... |
| ML3.5 | done | Parameters controlling optimization algorithms should minimally include: |
| ML3.5a | done | Specification of the type of algorithm used to explore the search space (commonly, for example, some kind of gradient descent algorithm) |
| ML3.5b | done | The kind of loss function used to assess distance between model estimates and desired output. |
| ML3.6 | done | Unless explicitly justified otherwise (for example because ML software under consideration is an implementation of one specific algorithm), ML software should: |
| ML3.6a | done | Implement or otherwise permit usage of multiple ways of exploring search space |
| ML3.6b | done | Implement or otherwise permit usage of multiple loss functions. |
| ML3.7 | done | For ML software in which algorithms are coded in C++, user-controlled use of either CPUs or GPUs (on NVIDIA processors at least) should be implemented throug... |
| ML4.0 | done | ML software should generally implement a unified single-function interface to model training, able to receive as input a model specified according to all pre... |
| ML4.1 | done | ML software should at least optionally retain explicit information on paths taken as an optimizer advances towards minimal loss. Such information should mini... |
| ML4.1a | done | Specification of all model-internal parameters, or equivalent hashed representation. |
| ML4.1b | done | The value of the loss function at each point |
| ML4.1c | done | Information used to advance to next point, for example quantification of local gradient. |
| ML4.2 | done | The subsequent extraction of information retained according to the preceding standard should be explicitly documented, including through example code. |
| ML4.3 | done | All parameters controlling batch processing and associated terminology should be explicitly documented, and it should not, for example, be presumed that user... |
| ML4.4 | done | Explicit guidance should be provided on selection of appropriate values for parameter controlling batch processing, for example, on trade-offs between batch ... |
| ML4.5 | done | ML software may optionally include a function to estimate likely time to train a specified model, through estimating initial timings from a small sample of t... |
| ML4.6 | done | ML software should by default provide explicit information on the progress of batch jobs (even where those jobs may be implemented in parallel on GPUs). That... |
| ML4.7 | done | ML software should provide an ability to combine results from multiple re-sampling iterations using a single parameter specifying numbers of iterations. |
| ML4.8 | done | Absent any additional specification, re-sampling algorithms should by default partition data according to proportions of original test and training data. |
| ML4.8a | done | Re-sampling routines of ML software should nevertheless offer an ability to explicitly control or override such default proportions of test and training data. |
| ML5.0 | done | The result of applying the training processes described above should be contained within a single model object returned by the function defined according to ... |
| ML5.0a | done | That object should either have its own class, or extend some previously-defined class. |
| ML5.0b | done | That class should have a defined `print` method which summarises important aspects of the model object, including but not limited to summaries of input data ... |
| ML5.1 | done | As for the untrained model objects produced according to the above standards, and in particular as a direct extension of **ML3.3**, the properties and behavi... |
| ML5.2 | done | The structure and functionality of objects representing trained ML models should be thoroughly documented. In particular, |
| ML5.2a | done | Either all functionality extending from the class of model object should be explicitly documented, or a method for listing or otherwise accessing all associa... |
| ML5.2b | done | Documentation should include examples of how to save and re-load trained model objects for their re-use in accordance with **ML3.1**, above. |
| ML5.2c | done | Where general functions for saving or serializing objects, such as [`saveRDS`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/readRDS.html) are not ... |
| ML5.3 | done | Assessment of model performance should be implemented as one or more functions distinct from model training. |
| ML5.4 | done | Model performance should be able to be assessed according to a variety of metrics. |
| ML5.4a | done | All model performance metrics represented by functions internal to a package must be clearly and distinctly documented. |
| ML5.4b | done | It should be possible to submit custom metrics to a model assessment function, and the ability to do so should be clearly documented including through exampl... |
| ML6.0 | done | Descriptions of ML software should make explicit reference to a workflow which separates training and testing stages, and which clearly indicates a need for ... |
| ML6.1 | done | ML software intentionally designed to address only a restricted subset of the workflow described here should clearly document how it can be embedded within a... |
| ML6.1a | done | Such demonstrations should include and contrast embedding within a full workflow using at least two other packages to implement that workflow. |
| ML7.0 | done | Test should explicitly confirm partial and case-insensitive matching of "test", "train", and, where applicable, "validation" data. |
| ML7.1 | done | Tests should demonstrate effects of different numeric scaling of input data (see **ML2.2**). |
| ML7.2 | done | For software which imputes missing data, tests should compare internal imputation with explicit code which directly implements imputation steps (even where s... |
| ML7.3 | done | Where model objects are implemented as distinct classes, tests should explicitly compare the functionality of these classes with functionality of equivalent ... |
| ML7.3a | done | These tests should explicitly identify restrictions on the functionality of model objects in comparison with those of other packages. |
| ML7.3b | done | These tests should explicitly identify functional advantages and unique abilities of the model objects in comparison with those of other packages. |
| ML7.4 | done | ML software should explicit document the effects of different training rates, and in particular should demonstrate divergence from optima with inappropriate ... |
| ML7.5 | done | ML software which implements routines to determine optimal training rates (see **ML3.4**, above) should implement tests to confirm the optimality of resultan... |
| ML7.6 | done | ML software which implement independent training "epochs" should demonstrate in tests the effects of lesser versus greater numbers of epochs. |
| ML7.7 | done | ML software should explicitly test different optimization algorithms, even where software is intended to implement one specific algorithm. |
| ML7.8 | done | ML software should explicitly test different loss functions, even where software is intended to implement one specific measure of loss. |
| ML7.9 | done | Tests should explicitly compare all possible combinations in categorical differences in model architecture, such as different model architectures with same o... |
| ML7.9a | done | Such combinations will generally be formed from multiple categorical factors, for which explicit use of functions such as [`expand.grid()`](https://stat.ethz... |
| ML7.10 | done | The successful extraction of information on paths taken by optimizers (see **ML5.1**, above), should be tested, including testing the general properties, but... |
| ML7.11 | done | All performance metrics available for a given class of trained model should be thoroughly tested and compared. |
| ML7.11a | done | Tests which compare metrics should do so over a range of inputs (generally implying differently trained models) to demonstrate relative advantages and disadv... |
