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
#' @srrstatsNA {RE2.2} The estimators use complete-case selection; they
#'   do not offer separate missing-value handling for predictor versus
#'   response data.
#' @srrstatsNA {RE2.3} Centring / offsetting (z-scoring, zero-intercept)
#'   is left to the caller; no built-in centring parameter is provided.
#' @srrstatsNA {RE2.4b} Perfect collinearity between predictor and
#'   response is a degenerate case that surfaces as a fitting error
#'   rather than being separately pre-detected.
#' @srrstatsNA {RE4.1} Estimators always fit; there is no
#'   generate-object-without-fitting mode.
#' @srrstatsNA {RE4.7} The primary estimators are closed-form
#'   (OLS / IPW) with no iterative convergence; iterative backends report
#'   their own convergence diagnostics upstream.
#' @srrstatsNA {RE4.8} Response-variable metadata beyond the variable
#'   name is not attached to the compact result object.
#' @srrstatsNA {RE4.9} Per-observation modelled (fitted) response values
#'   are not returned by default; the result reports the estimate and its
#'   inference.
#' @srrstatsNA {RE4.13} Predictor-variable metadata is not attached to
#'   the result object.
#' @srrstatsNA {RE4.14} The DiD/causal estimators are not forecasting
#'   models; extrapolation / forecast errors do not apply (forecasting is
#'   handled by the time-series estimators).
#' @srrstatsNA {RE4.15} As above: no forecast errors to document for the
#'   regression estimators.
#' @srrstatsNA {RE4.16} The estimators do not model distinct responses
#'   per categorical group requiring submission of new group levels.
#' @srrstatsNA {RE4.18} A dedicated `summary` method beyond the rich
#'   `print` output is not implemented.
#' @srrstatsNA {RE5.0} Scaling of run time with data size is not tested;
#'   the tested property is statistical correctness.
#' @srrstatsNA {RE6.0} The compact result objects do not carry a default
#'   `plot` method; visualisation is provided by the separate figure
#'   exporters (`morie_otis_figures`, etc.).
#' @srrstatsNA {RE6.1} No default regression `plot` method, so its
#'   internals do not apply.
#' @srrstatsNA {RE6.2} As above, no default regression `plot` method.
#' @srrstatsNA {RE6.3} The regression estimators do not generate
#'   forecasts, so forecast-plotting does not apply.
#' @srrstatsNA {RE7.2} The compact result object does not retain input
#'   row / case names, so there is nothing to test for their retention.
#' @srrstatsNA {RE7.4} No forecast errors are produced by the regression
#'   estimators, so there are none to test.
#' @srrstatsNA {RE7.1a} Relative fitting speed on noiseless versus noisy
#'   data is a performance property that is not part of the test suite;
#'   correctness (exact recovery) is what is tested.
#'
#' @srrstatsNA {TS1.3} There is no single tsbox-style pre-processing
#'   routine; the heterogeneous point-process / volatility / survival
#'   models each validate their own domain-native input.
#' @srrstatsNA {TS1.4} No ts-class attributes are used, so there are none
#'   to preserve through pre-processing.
#' @srrstatsNA {TS1.7} `units`-package inputs are not supported (as with
#'   the general G2.11 position).
#' @srrstatsNA {TS2.0} The point-process models operate on irregular
#'   event times by construction; the regular-data / implicit-missing
#'   distinction does not apply.
#' @srrstatsNA {TS2.1} Missing-value handling options for regular series
#'   do not apply: the models require complete event-time / return series.
#' @srrstatsNA {TS2.1a} (see TS2.1)
#' @srrstatsNA {TS2.1b} (see TS2.1)
#' @srrstatsNA {TS2.1c} (see TS2.1)
#' @srrstatsNA {TS2.4} Stationarity is a documented model assumption; the
#'   software does not implement automatic stationarity checks or
#'   stationarity-inducing transforms.
#' @srrstatsNA {TS2.4a} (see TS2.4 — no automatic stationarity diagnostics)
#' @srrstatsNA {TS2.4b} (see TS2.4 — no automatic transforms)
#' @srrstatsNA {TS2.5} No auto-covariance matrix is returned, so there is
#'   no row/column ordering to tie to a time index.
#' @srrstatsNA {TS2.6} As above, no auto-covariance matrix and therefore
#'   no units to attach to one.
#' @srrstatsNA {TS3.0} The models estimate intensity / volatility /
#'   hazard; they do not produce multi-step-ahead forecasts, so
#'   forecast-error-versus-horizon behaviour does not apply.
#' @srrstatsNA {TS3.1} (no forecasting; see TS3.0)
#' @srrstatsNA {TS3.2} (no forecasting; see TS3.0)
#' @srrstatsNA {TS3.3} (no forecasting; see TS3.0)
#' @srrstatsNA {TS3.3a} (no forecasting; see TS3.0)
#' @srrstatsNA {TS3.3b} (no forecasting; see TS3.0)
#' @srrstatsNA {TS4.0a} No tsbox round-trip, as no ts-class is used.
#' @srrstatsNA {TS4.1} No units are carried on inputs, so none are
#'   returned.
#' @srrstatsNA {TS4.3} Return values carry no formal unit / time-scale
#'   attributes beyond the caller's own scale.
#' @srrstatsNA {TS4.4} No forecasting, so there are no transformation
#'   effects on forecasts to document.
#' @srrstatsNA {TS4.5} No forecasting / non-stationary back-transform path.
#' @srrstatsNA {TS4.5a} (see TS4.5)
#' @srrstatsNA {TS4.5b} (see TS4.5)
#' @srrstatsNA {TS4.5c} (see TS4.5)
#' @srrstatsNA {TS4.6} The models do not forecast, so no forecast
#'   distribution / moment object is returned.
#' @srrstatsNA {TS4.6a} (see TS4.6)
#' @srrstatsNA {TS4.6b} (see TS4.6)
#' @srrstatsNA {TS4.6c} (see TS4.6)
#' @srrstatsNA {TS4.7} No forecast values are produced, so there is
#'   nothing to distinguish from observed values.
#' @srrstatsNA {TS4.7a} (see TS4.7)
#' @srrstatsNA {TS4.7b} (see TS4.7)
#' @srrstatsNA {TS4.7c} (see TS4.7)
#' @srrstatsNA {TS5.0} The result objects carry no default temporal
#'   `plot` method; visualisation is via the separate figure exporters.
#' @srrstatsNA {TS5.1} (no default temporal plot method; see TS5.0)
#' @srrstatsNA {TS5.2} (see TS5.0)
#' @srrstatsNA {TS5.3} (see TS5.0)
#' @srrstatsNA {TS5.4} (see TS5.0)
#' @srrstatsNA {TS5.5} (see TS5.0)
#' @srrstatsNA {TS5.6} (see TS5.0)
#' @srrstatsNA {TS5.7} (see TS5.0)
#' @srrstatsNA {TS5.8} (see TS5.0)
#'
#' @srrstatsNA {SP2.0} For accessibility the spatial functions accept
#'   plain data.frames with documented coordinate / neighbourhood
#'   columns rather than requiring a dedicated spatial class as input;
#'   `sf` / `spdep` structures are constructed internally.
#' @srrstatsNA {SP2.0a} rmorie implements no new spatial input class, so
#'   there is no new-class conversion to document.
#' @srrstatsNA {SP2.2b} Round-trip translation tests into external
#'   spatial workflows are not provided; interoperability is via the
#'   wrapped spdep/sf/gstat calls themselves.
#' @srrstatsNA {SP2.3} Tests use bundled coordinate data rather than
#'   loading GDAL/sf raster/vector files.
#' @srrstatsNA {SP2.4} rmorie performs no coordinate reprojection, so
#'   PROJ6/WKT2 compliance is delegated entirely to `sf`.
#' @srrstatsNA {SP2.4a} (see SP2.4 — no CRS strings handled by rmorie)
#' @srrstatsNA {SP2.5} rmorie implements no spatial input class carrying
#'   CRS metadata; CRS handling is left to `sf`.
#' @srrstatsNA {SP2.5a} (see SP2.5 — no new spatial classes)
#' @srrstatsNA {SP2.8} There is no single spatial pre-processing routine;
#'   each method validates its own coordinate/neighbour input.
#' @srrstatsNA {SP2.9} No dedicated spatial metadata attributes are
#'   carried, so there are none to preserve through pre-processing.
#' @srrstatsNA {SP3.2} No routine samples input data by local spatial
#'   density.
#' @srrstatsNA {SP3.5} rmorie implements no spatial machine-learning
#'   models, so dimension-broadcasting concerns do not apply.
#' @srrstatsNA {SP3.6} (no spatial ML; sampling-procedure effects N/A)
#' @srrstatsNA {SP4.0a} Results are returned as analysis objects, not in
#'   the same class as the coordinate input.
#' @srrstatsNA {SP4.1} No units are carried on coordinate inputs, so none
#'   are returned.
#' @srrstatsNA {SP5.0} The spatial result objects carry no default
#'   `plot` method; mapping is provided by the separate figure
#'   exporters (`morie_tps_figures`, Moran heatmap helpers).
#' @srrstatsNA {SP5.1} (no default spatial plot method; see SP5.0)
#' @srrstatsNA {SP5.2} (see SP5.0)
#' @srrstatsNA {SP5.3} Interactive html visualisation is not implemented.
#' @srrstatsNA {SP6.0} rmorie implements no coordinate-transform routine,
#'   so coordinate-recovery tests do not apply.
#' @srrstatsNA {SP6.1} The methods are planar-only; there is no
#'   Cartesian-versus-curvilinear duality to test.
#' @srrstatsNA {SP6.1a} (planar-only; see SP6.1)
#' @srrstatsNA {SP6.1b} (planar-only; see SP6.1)
#' @srrstatsNA {SP6.2} Extreme-geographical-coordinate (polar/dateline)
#'   tests do not apply to the projected-neighbourhood methods.
#' @srrstatsNA {SP6.3} Neighbour construction delegates to spdep's tested
#'   knn / distance routines; rmorie does not re-test every neighbour
#'   definition exhaustively.
#' @srrstatsNA {SP6.4} (weighting schemes are spdep's; not exhaustively
#'   re-tested here; see SP6.3)
#' @srrstatsNA {SP6.5} Spatial clustering correctness is delegated to the
#'   underlying dbscan / scan-statistic implementations.
#' @srrstatsNA {SP6.6} rmorie implements no spatial machine-learning
#'   software, so its tests do not apply.
#'
#' @srrstatsNA {EA2.0} rmorie's exploratory layer summarises and tests
#'   data; it does not implement extensive table filter/join operations
#'   requiring an index-column system.
#' @srrstatsNA {EA2.1} (no index-column system; see EA2.0)
#' @srrstatsNA {EA2.2} (see EA2.0)
#' @srrstatsNA {EA2.2a} (see EA2.0)
#' @srrstatsNA {EA2.2b} (see EA2.0)
#' @srrstatsNA {EA2.3} No table joins are performed on assumed column
#'   names; there is no join layer.
#' @srrstatsNA {EA2.4} No multi-table `DM`-style class system is used.
#' @srrstatsNA {EA2.5} (no index-column tables; see EA2.0)
#' @srrstatsNA {EA2.6} Vector inputs are validated by the shared
#'   `.morie_check_*` helpers; there is no attribute-bearing table layer
#'   that this standard targets.
#' @srrstatsNA {EA3.0} rmorie does not automate meta-level extraction
#'   (e.g. automated variable/model selection reporting).
#' @srrstatsNA {EA3.1} rmorie provides no standardised cross-tool
#'   input/model/output comparison layer.
#' @srrstatsNA {EA4.1} No explicit numeric-precision control parameter is
#'   exposed; results are reported at full double precision and formatted
#'   for display only.
#' @srrstatsNA {EA5.0} Graphical accessibility standards target the
#'   package's own default plotting layer; rmorie's figures are produced
#'   by the separate exporters using ggplot2 defaults.
#' @srrstatsNA {EA5.0a} (see EA5.0)
#' @srrstatsNA {EA5.0b} (see EA5.0)
#' @srrstatsNA {EA5.1} rmorie does not override typeface specifications
#'   from the underlying graphics packages.
#' @srrstatsNA {EA5.3} Column summaries report values, not per-column
#'   storage.mode annotations, as they operate on validated numeric data.
#' @srrstatsNA {EA5.4} Visual rounding is left to the ggplot2 scales used
#'   by the figure exporters.
#' @srrstatsNA {EA5.5} Axis units are the caller's own; no unit metadata
#'   is carried to annotate axes automatically.
#' @srrstatsNA {EA5.6} rmorie bundles no dynamic-visualisation library.
#' @srrstatsNA {EA6.1} Graphical output is not tested via `vdiffr`; the
#'   figure exporters are smoke-tested for successful file creation, not
#'   pixel-level appearance.
#'
#' @srrstatsNA {PD2.0} Distributions are fitted and summarised directly
#'   rather than represented through a general distribution-object
#'   package (e.g. `distributional`).
#' @srrstatsNA {PD3.0} The heavy-tailed fits are numeric (Hill MLE); no
#'   analytic distribution algebra is performed.
#' @srrstatsNA {PD3.1} Operations are specific to the fitted models and
#'   are not organised as generic functions taking a distribution name.
#' @srrstatsNA {PD3.4} rmorie implements no probability-distribution
#'   integration routines.
#' @srrstatsNA {PD3.5} No integral is approximated by discrete summation.
#' @srrstatsNA {PD3.5a} (no discrete-summation integration; see PD3.5)
#' @noRd
NULL
