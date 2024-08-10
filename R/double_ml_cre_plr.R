#' @title Double machine learning for partially linear regression models for CRE and hybrid approaches.
#'
#' @description
#' Double machine learning (DML) for partially linear regression models for CRE and hybrid approaches.
#'
#' @format [R6::R6Class] object inheriting from [dml_cre].
#'
#' @family dml_cre
#' @details
#' Partially linear regression (PLR) models of form
#'
#' \eqn{(1) y_{it} = \theta d_{it} + l(x_{it},\overline{x}_i) + e_{it}}
#'
#' \eqn{(2) d_{it} = m(x_{it},\overline{x}_i,\overline{d}_i) +  \nu_{it}}
#'
#' where (1) is the outcome equation and (2) is the treatment equation.
#' @usage NULL
#'
#' @export
dml_cre_plr = R6Class("dml_cre_plr",
                      inherit = dml_cre, public = list(
                        #' @description
                        #' Creates a new instance of this R6 class.
                        #'
                        #' @param data (`dml_cre_data`) \cr
                        #' The `dml_cre_data` object providing the data and specifying the
                        #' variables of the causal model.
                        #'
                        #' @param ml_l ([`LearnerRegr`][mlr3::LearnerRegr],
                        #' [`Learner`][mlr3::Learner], `character(1)`) \cr
                        #' A learner of the class [`LearnerRegr`][mlr3::LearnerRegr], which is
                        #' available from [mlr3](https://mlr3.mlr-org.com/index.html) or its
                        #' extension packages [mlr3learners](https://mlr3learners.mlr-org.com/) or
                        #' [mlr3extralearners](https://mlr3extralearners.mlr-org.com/).
                        #' Alternatively, a [`Learner`][mlr3::Learner] object with public field
                        #' `task_type = "regr"` can be passed, for example of class
                        #' [`GraphLearner`][mlr3pipelines::GraphLearner]. The learner can possibly
                        #' be passed with specified parameters, for example
                        #' `lrn("regr.cv_glmnet", s = "lambda.min")`. \cr
                        #' `ml_l` refers to the nuisance function \eqn{l_0(X) = E[Y|X]}, where X is
                        #'  a matrix of covariates. If a `non-separable` model is chosen, the matrix includes
                        #'  the covariates and their means. If a `separable` model is chosen,
                        #'  the matrix includes the means of the covariates and treatment.
                        #'
                        #' @param ml_m ([`LearnerRegr`][mlr3::LearnerRegr],
                        #' [`LearnerClassif`][mlr3::LearnerClassif], [`Learner`][mlr3::Learner],
                        #' `character(1)`) \cr
                        #' A learner of the class [`LearnerRegr`][mlr3::LearnerRegr], which is
                        #' available from [mlr3](https://mlr3.mlr-org.com/index.html) or its
                        #' extension packages [mlr3learners](https://mlr3learners.mlr-org.com/) or
                        #' [mlr3extralearners](https://mlr3extralearners.mlr-org.com/).
                        #' For binary treatment variables, an object of the class
                        #' [`LearnerClassif`][mlr3::LearnerClassif] can be passed, for example
                        #' `lrn("classif.cv_glmnet", s = "lambda.min")`.
                        #' Alternatively, a [`Learner`][mlr3::Learner] object with public field
                        #' `task_type = "regr"` or `task_type = "classif"` can be passed,
                        #' respectively, for example of class
                        #' [`GraphLearner`][mlr3pipelines::GraphLearner]. \cr
                        #' `ml_m` refers to the nuisance function \eqn{m_0(X) = E[D|X]}, where X is
                        #'  a matrix of covariates. If a `'non-separable'` model is chosen, the matrix includes
                        #'  the covariates, their means, and the mean of the treatment. If a `'separable'`
                        #'  model is chosen, the matrix includes the means of the covariates and treatment.
                        #'
                        #' @param ml_g ([`LearnerRegr`][mlr3::LearnerRegr],
                        #' [`Learner`][mlr3::Learner], `character(1)`) \cr
                        #' A learner of the class [`LearnerRegr`][mlr3::LearnerRegr], which is
                        #' available from [mlr3](https://mlr3.mlr-org.com/index.html) or its
                        #' extension packages [mlr3learners](https://mlr3learners.mlr-org.com/) or
                        #' [mlr3extralearners](https://mlr3extralearners.mlr-org.com/).
                        #' Alternatively, a [`Learner`][mlr3::Learner] object with public field
                        #' `task_type = "regr"` can be passed, for example of class
                        #' [`GraphLearner`][mlr3pipelines::GraphLearner]. The learner can possibly
                        #' be passed with specified parameters, for example
                        #' `lrn("regr.cv_glmnet", s = "lambda.min")`. \cr
                        #' `ml_g` refers to the nuisance function \eqn{g_0(X) = E[Y - D\theta_0|X]}, where X is
                        #'  a matrix of covariates. If a `non-separable` model is chosen, the matrix includes
                        #'  the covariates and their means. If a `separable`
                        #'  model is chosen, the matrix includes the means of the covariates and treatment.
                        #' Note: The learner `ml_g` is only required for the score `'IV-type'`.
                        #' Optionally, it can be specified and estimated for callable scores.
                        #'
                        #' @param ml_lbar ([`LearnerRegr`][mlr3::LearnerRegr],
                        #' [`Learner`][mlr3::Learner], `character(1)`) \cr
                        #' A learner of the class [`LearnerRegr`][mlr3::LearnerRegr], which is
                        #' available from [mlr3](https://mlr3.mlr-org.com/index.html) or its
                        #' extension packages [mlr3learners](https://mlr3learners.mlr-org.com/) or
                        #' [mlr3extralearners](https://mlr3extralearners.mlr-org.com/).
                        #' Alternatively, a [`Learner`][mlr3::Learner] object with public field
                        #' `task_type = "regr"` can be passed, for example of class
                        #' [`GraphLearner`][mlr3pipelines::GraphLearner]. The learner can possibly
                        #' be passed with specified parameters, for example
                        #' `lrn("regr.cv_glmnet", s = "lambda.min")`. \cr
                        #' `ml_lbar` refers to the nuisance function \eqn{l_0(X) = E[Y|X]}, where X is
                        #'  a matrix of covariates that does not include the grand means of the
                        #'  covariates. Note: The learner `ml_mbar` is only required with a `separable` model.
                        #'
                        #' @param ml_mbar ([`LearnerRegr`][mlr3::LearnerRegr],
                        #' [`LearnerClassif`][mlr3::LearnerClassif], [`Learner`][mlr3::Learner],
                        #' `character(1)`) \cr
                        #' A learner of the class [`LearnerRegr`][mlr3::LearnerRegr], which is
                        #' available from [mlr3](https://mlr3.mlr-org.com/index.html) or its
                        #' extension packages [mlr3learners](https://mlr3learners.mlr-org.com/) or
                        #' [mlr3extralearners](https://mlr3extralearners.mlr-org.com/).
                        #' For binary treatment variables, an object of the class
                        #' [`LearnerClassif`][mlr3::LearnerClassif] can be passed, for example
                        #' `lrn("classif.cv_glmnet", s = "lambda.min")`.
                        #' Alternatively, a [`Learner`][mlr3::Learner] object with public field
                        #' `task_type = "regr"` or `task_type = "classif"` can be passed,
                        #' respectively, for example of class
                        #' [`GraphLearner`][mlr3pipelines::GraphLearner]. \cr
                        #' `ml_mbar` refers to the nuisance function \eqn{m_0(X) = E[D|X]}, where X is
                        #'  a matrix of covariates that does not include the grand means of the
                        #'  covariates and treatment. Note: The learner `ml_mbar` is only required
                        #'  with a `separable` model.
                        #'
                        #' @param ml_gbar ([`LearnerRegr`][mlr3::LearnerRegr],
                        #' [`Learner`][mlr3::Learner], `character(1)`) \cr
                        #' A learner of the class [`LearnerRegr`][mlr3::LearnerRegr], which is
                        #' available from [mlr3](https://mlr3.mlr-org.com/index.html) or its
                        #' extension packages [mlr3learners](https://mlr3learners.mlr-org.com/) or
                        #' [mlr3extralearners](https://mlr3extralearners.mlr-org.com/).
                        #' Alternatively, a [`Learner`][mlr3::Learner] object with public field
                        #' `task_type = "regr"` can be passed, for example of class
                        #' [`GraphLearner`][mlr3pipelines::GraphLearner]. The learner can possibly
                        #' be passed with specified parameters, for example
                        #' `lrn("regr.cv_glmnet", s = "lambda.min")`. \cr
                        #' `ml_g` refers to the nuisance function \eqn{g_0(X) = E[Y - D\theta_0|X]},
                        #' where X is a matrix of covariates that does not include the grand means of
                        #' the covariates.Note: The learner `ml_gbar` is only required for `'IV-type'`
                        #' scores and with a `separable` model.
                        #'
                        #' @param n_folds (`integer(1)`)\cr
                        #' Number of folds. Default is `5`.
                        #'
                        #' @param n_rep (`integer(1)`) \cr
                        #' Number of repetitions for the sample splitting. Default is `1`.
                        #'
                        #' @param score (`character(1)`) \cr
                        #' A `character(1)` (`"orth-PO"`, `"orth-IV"`, `"NO"`, or `"NO-IV"`).
                        #' `"orth-PO"` is Neyman orthogonal score with the partialling out formula.
                        #' `"orth-IV"` is Neyman orthogonal score with the instrumental variable formula.
                        #' `"NO"` is the non-orthogonal score.
                        #' `"NO-IV"` is  the non-orthogonal score with the instrumental variable formula.
                        #' Default is `"orth-PO"`.
                        #'
                        #' @param dml_procedure (`character(1)`) \cr
                        #' A `character(1)` (`"dml1"` or `"dml2"`) specifying the double machine
                        #' learning algorithm. Default is `"dml2"`.
                        #'
                        #' @param dml_approach (`character(1)`) \cr
                        #' A `character(1)` (`"cre"` or `"hybrid"`) specifying the double machine
                        #' learning algorithm. Default is `"cre"`.
                        #'
                        #' @param dml_type (`character(1)`) \cr
                        #' A `character(1)` (`"separable"` or `"non-separable"`).
                        #' Default is `"non-separable"`.
                        #'
                        #' @param dml_transform (`character(1)`) \cr
                        #' A `character(1)` (`"fd"` or `"wg"`) when `"dml_approach"` is `"hybrid"`.
                        #' Default is `"wg"`. Unused when `"dml_approach"` is `"cre"`.
                        #'
                        #' @param draw_sample_splitting (`logical(1)`) \cr
                        #' Indicates whether the sample splitting should be drawn during
                        #' initialization of the object. Default is `TRUE`.
                        #'
                        #' @param apply_cross_fitting (`logical(1)`) \cr
                        #' Indicates whether cross-fitting should be applied. Default is `TRUE`.
                        initialize = function(data,
                                              ml_l,
                                              ml_m,
                                              ml_g = NULL,
                                              ml_lbar = NULL,
                                              ml_mbar = NULL,
                                              ml_gbar = NULL,
                                              n_folds = 5,
                                              n_rep = 1,
                                              score = "orth-PO",
                                              dml_procedure = "dml2",
                                              dml_approach  = "cre",
                                              dml_type      = "non-separable",
                                              dml_transform = "wg",
                                              draw_sample_splitting = TRUE,
                                              apply_cross_fitting = TRUE) {

                          if (missing(ml_l)) {
                            if (!missing(ml_g)) {
                              warning(paste0(
                                "The argument ml_g was renamed to ml_l. ",
                                "Please adapt the argument name accordingly. ",
                                "ml_g is redirected to ml_l.\n",
                                "The redirection will be removed in a future version."),
                                call. = FALSE)
                              ml_l = ml_g
                              ml_g = NULL
                            }
                          }

                          super$initialize_double_ml(
                            data,
                            n_folds,
                            n_rep,
                            score,
                            dml_procedure,
                            dml_approach,
                            dml_type,
                            dml_transform,
                            draw_sample_splitting,
                            apply_cross_fitting)

                          private$check_data(self$data)
                          private$check_score(self$score)
                          private$check_approach_and_type(self$dml_approach,self$dml_type,self$dml_transform)

                          if(self$dml_type == "non-separable"){
                            ml_l = private$assert_learner(ml_l, "ml_l", Regr = TRUE, Classif = FALSE)
                            ml_m = private$assert_learner(ml_m, "ml_m", Regr = TRUE, Classif = TRUE)
                            #ml_lbar = NULL
                            #ml_lbar = NULL
                          }else if(self$dml_type == "non-separable"){
                            ml_l = private$assert_learner(ml_l, "ml_l", Regr = TRUE, Classif = FALSE)
                            ml_m = private$assert_learner(ml_m, "ml_m", Regr = TRUE, Classif = TRUE)
                            ml_lbar = private$assert_learner(ml_lbar, "ml_lbar", Regr = TRUE, Classif = FALSE)
                            ml_mbar = private$assert_learner(ml_mbar, "ml_mbar", Regr = TRUE, Classif = TRUE)
                          }

                          private$learner_ = list("ml_l" = ml_l,
                                                  "ml_m" = ml_m,
                                                  "ml_lbar" = ml_lbar,
                                                  "ml_mbar" = ml_mbar)

                          if (self$dml_type == "non-separable"){

                            if (!is.null(ml_g)){
                              assert(check_character(ml_g, max.len = 1),
                                     check_class(ml_g, "Learner"))
                              if ((self$score == "NO-IV") || (self$score == "orth-IV")) {
                                ml_g = private$assert_learner(ml_g, "ml_g",
                                                              Regr = TRUE, Classif = FALSE)
                                private$learner_[["ml_g"]] = ml_g
                              } else if ((self$score == "NO") || (self$score == "orth-PO")) {
                                warning(paste0(
                                  "A learner ml_g has been provided for ",
                                  "score = 'orth-PO' or 'NO' but will be ignored. ",
                                  "A learner ml_g is not required for estimation."))
                              }
                            }else if ((self$score == "NO-IV") || (self$score == "orth-IV")){
                              warning(paste0(
                                "For 'IV-type' scores, learners ml_l and ml_g ",
                                "should be specified. ",
                                "Set ml_g = ml_l$clone()."),
                                call. = FALSE)
                              ml_g = private$assert_learner(ml_l$clone(), "ml_g",
                                                            Regr = TRUE, Classif = FALSE)
                              private$learner_[["ml_g"]] = ml_g
                            }
                          }else if (self$dml_type == "separable"){
                            if (!is.null(ml_g) && !is.null(ml_gbar)) {
                              assert(check_character(ml_g, max.len = 1),
                                     check_class(ml_g, "Learner"))
                              assert(check_character(ml_gbar, max.len = 1),
                                     check_class(ml_gbar, "Learner"))

                              if ((self$score == "NO-IV") || (self$score == "orth-IV")) {
                                ml_g = private$assert_learner(ml_g, "ml_g",Regr = TRUE, Classif = FALSE)
                                ml_gbar = private$assert_learner(ml_gbar, "ml_gbar",Regr = TRUE, Classif = FALSE)
                                private$learner_[["ml_g"]] = ml_g
                                private$learner_[["ml_gbar"]] = ml_gbar
                              } else if ((self$score == "NO") || (self$score == "orth-PO")) {
                                warning(paste0(
                                  "A learner ml_g has been provided for ",
                                  "score = 'orth-PO' or 'NO' but will be ignored. ",
                                  "A learner ml_g and  ml_gbar is not required for estimation."))
                              }
                            }else if ((self$score == "NO-IV") || (self$score == "orth-IV")) {
                              warning(paste0(
                                "For 'IV-type' scores, learners ml_l and ml_g ",
                                "should be specified. ",
                                "Set ml_g = ml_l$clone(). Set ml_gbar = ml_lbar$clone()."),
                                call. = FALSE)
                              ml_g = private$assert_learner(ml_l$clone(), "ml_g",Regr = TRUE, Classif = FALSE)
                              ml_gbar = private$assert_learner(ml_lbar$clone(), "ml_gbar",Regr = TRUE, Classif = FALSE)
                              private$learner_[["ml_g"]] = ml_g
                              private$learner_[["ml_gbar"]] = ml_gbar
                            }
                          }
                          private$initialize_ml_nuisance_params()
                        },
                        #' @description
                        #' Set hyperparameters for the nuisance models of DML models with CRE.
                        #'
                        #' @param learner (`character(1)`) \cr
                        #' The nuisance model/learner (see method `params_names`).
                        #'
                        #' @param treat_var (`character(1)`) \cr
                        #' The treatment varaible (hyperparameters can be set treatment-variable
                        #' specific).
                        #'
                        #' @param params (named `list()`) \cr
                        #' A named `list()` with estimator parameters. Parameters are used for all
                        #' folds by default. Alternatively, parameters can be passed in a
                        #' fold-specific way if option  `fold_specific`is `TRUE`. In this case, the
                        #' outer list needs to be of length `n_rep` and the inner list of length
                        #' `n_folds`.
                        #'
                        #' @param set_fold_specific (`logical(1)`) \cr
                        #' Indicates if the parameters passed in `params_theta`  should be passed in
                        #' fold-specific way. Default is `FALSE`. If `TRUE`, the outer list needs
                        #' to be of length `n_rep` and the inner list of length `n_folds`.
                        #'
                        #' @return self
                        set_ml_nuisance_params = function(learner = NULL, treat_var = NULL,
                                                          params, set_fold_specific = FALSE) {
                          assert_character(learner, len = 1)
                          if (((self$score == "NO") || (self$score == "orth-PO")) &&
                              (learner == "ml_g")) {
                            warning(paste0(
                              "Learner ml_g was renamed to ml_l. ",
                              "Please adapt the argument learner accordingly. ",
                              "The provided parameters are set for ml_l. ",
                              "The redirection will be removed in a future version."),
                              call. = FALSE)
                            learner = "ml_l"
                          }
                          super$set_ml_nuisance_params(learner, treat_var,
                                                       params, set_fold_specific)
                        },
                        #' @description
                        #' Hyperparameter-tuning for DML models with CRE.
                        #'
                        #' The hyperparameter-tuning is performed using the tuning methods provided
                        #' in the [mlr3tuning](https://mlr3tuning.mlr-org.com/) package. For more
                        #' information on tuning in [mlr3](https://mlr3.mlr-org.com/), we refer to
                        #' the section on parameter tuning in the
                        #' [mlr3 book](https://mlr3book.mlr-org.com/optimization.html#tuning).
                        #'
                        #' @param param_set (named `list()`) \cr
                        #' A named `list` with a parameter grid for each nuisance model/learner
                        #' (see method `learner_names()`). The parameter grid must be an object of
                        #' class [ParamSet][paradox::ParamSet].
                        #'
                        #' @param tune_settings (named `list()`) \cr
                        #' A named `list()` with arguments passed to the hyperparameter-tuning with
                        #' [mlr3tuning](https://mlr3tuning.mlr-org.com/) to set up
                        #' [TuningInstance][mlr3tuning::TuningInstanceSingleCrit] objects.
                        #' `tune_settings` has entries
                        #' * `terminator` ([Terminator][bbotk::Terminator]) \cr
                        #' A [Terminator][bbotk::Terminator] object. Specification of `terminator`
                        #' is required to perform tuning.
                        #' * `algorithm` ([Tuner][mlr3tuning::Tuner] or `character(1)`) \cr
                        #' A [Tuner][mlr3tuning::Tuner] object (recommended) or key passed to the
                        #' respective dictionary to specify the tuning algorithm used in
                        #' [tnr()][mlr3tuning::tnr()]. `algorithm` is passed as an argument to
                        #' [tnr()][mlr3tuning::tnr()]. If `algorithm` is not specified by the users,
                        #' default is set to `"grid_search"`. If set to `"grid_search"`, then
                        #' additional argument `"resolution"` is required.
                        #' * `rsmp_tune` ([Resampling][mlr3::Resampling] or `character(1)`)\cr
                        #' A [Resampling][mlr3::Resampling] object (recommended) or option passed
                        #' to [rsmp()][mlr3::mlr_sugar] to initialize a
                        #' [Resampling][mlr3::Resampling] for parameter tuning in `mlr3`.
                        #' If not specified by the user, default is set to `"cv"`
                        #' (cross-validation).
                        #' * `n_folds_tune` (`integer(1)`, optional) \cr
                        #' If `rsmp_tune = "cv"`, number of folds used for cross-validation.
                        #' If not specified by the user, default is set to `5`.
                        #' * `measure` (`NULL`, named `list()`, optional) \cr
                        #' Named list containing the measures used for parameter tuning. Entries in
                        #' list must either be [Measure][mlr3::Measure] objects or keys to be
                        #' passed to passed to [msr()][mlr3::msr()]. The names of the entries must
                        #' match the learner names (see method `learner_names()`). If set to `NULL`,
                        #' default measures are used, i.e., `"regr.mse"` for continuous outcome
                        #' variables and `"classif.ce"` for binary outcomes.
                        #' * `resolution` (`character(1)`) \cr The key passed to the respective
                        #' dictionary to specify  the tuning algorithm used in
                        #' [tnr()][mlr3tuning::tnr()]. `resolution` is passed as an argument to
                        #' [tnr()][mlr3tuning::tnr()].
                        #'
                        #' @param tune_on_folds (`logical(1)`) \cr
                        #' Indicates whether the tuning should be done fold-specific or globally.
                        #' Default is `FALSE`.
                        #'
                        #' @return self
                        tune = function(param_set, tune_settings = list(
                          n_folds_tune = 5,
                          rsmp_tune = mlr3::rsmp("cv", folds = 5),
                          measure = NULL,
                          terminator = mlr3tuning::trm("evals", n_evals = 20),
                          algorithm = mlr3tuning::tnr("grid_search"),
                          resolution = 5),
                          tune_on_folds = FALSE) {

                          assert_list(param_set)
                          if ((self$score == "NO") || (self$score == "orth-PO")) {
                            if ((self$dml_type == "non-separable") && exists("ml_g", where = param_set)
                                && !exists("ml_l", where = param_set)) {
                              warning(paste0(
                                "Learner ml_g was renamed to ml_l. ",
                                "Please adapt the name in param_set accordingly. ",
                                "The provided param_set for ml_g is used for ml_l. ",
                                "The redirection will be removed in a future version."),
                                call. = FALSE)
                              names(param_set)[names(param_set) == "ml_g"] = "ml_l"
                            } else if ((self$dml_type == "separable") &&
                                       (exists("ml_g", where = param_set) || exists("ml_gbar", where = param_set)) &&
                                       (!exists("ml_l", where = param_set) || !exists("ml_lbar", where = param_set))) {
                              warning(paste0(
                                "Learner ml_g and ml_gbar was renamed to ml_l and ml_lbar, respectively. ",
                                "Please adapt the name in param_set accordingly. ",
                                "The provided param_set for ml_g is used for ml_l. ",
                                "The redirection will be removed in a future version."),
                                call. = FALSE)
                              names(param_set)[names(param_set) == "ml_g"] = "ml_l"
                              names(param_set)[names(param_set) == "ml_gbar"] = "ml_lbar"
                            }
                          }

                          assert_list(tune_settings)
                          if (test_names(names(tune_settings), must.include = "measure") && !is.null(tune_settings$measure)) {
                            assert_list(tune_settings$measure)
                            if (exists("ml_g", where = tune_settings$measure) && !exists("ml_l", where = tune_settings$measure)) {
                              warning(paste0(
                                "Learner ml_g was renamed to ml_l. ",
                                "Please adapt the name in tune_settings$measure accordingly. ",
                                "The provided tune_settings$measure for ml_g is used for ml_l. ",
                                "The redirection will be removed in a future version."),
                                call. = FALSE)
                              names(tune_settings$measure)[names(tune_settings$measure) == "ml_g"] = "ml_l"
                            }
                          }

                          super$tune(param_set, tune_settings, tune_on_folds)
                        }
                      ),
                      private = list(
                        n_nuisance = 2,
                        initialize_ml_nuisance_params = function() {
                          nuisance = vector("list", self$data$n_treat)
                          names(nuisance) = self$data$d_cols
                          if (self$dml_type == "non-separable"){
                            private$params_ = list("ml_l" = nuisance,
                                                   "ml_m"  = nuisance)
                            if (exists("ml_g", where = private$learner_)) {
                              private$params_[["ml_g"]] = nuisance
                            }
                          }else if (self$dml_type == "separable"){
                            private$params_ = list("ml_l"  = nuisance,
                                                   "ml_m"  = nuisance,
                                                   "ml_lbar" = nuisance,
                                                   "ml_mbar" = nuisance)
                            if (exists("ml_g" , where = private$learner_) &&
                                exists("ml_gbar", where = private$learner_)) {
                              private$params_[["ml_g"]] = nuisance
                              private$params_[["ml_gbar"]] = nuisance
                            }
                          }
                          invisible(self)
                        },

                        nuisance_est = function(smpls, ...) {

                          if(self$dml_type == "non-separable"){
                            m_hat = dml_cv_predict(self$learner$ml_m,
                                                   c(self$data$x_cols,self$data$xbar_cols,self$data$dbar_cols),
                                                   self$data$treat_col,
                                                   self$data$data_model,
                                                   nuisance_id = "nuis_m",
                                                   smpls = smpls,
                                                   est_params = self$get_params("ml_m"),
                                                   return_train_preds = FALSE,
                                                   task_type = private$task_type$ml_m,
                                                   fold_specific_params = private$fold_specific_params)

                            l_hat = dml_cv_predict(self$learner$ml_l,
                                                   c(self$data$x_cols,self$data$xbar_cols),
                                                   self$data$y_col,
                                                   self$data$data_model,
                                                   nuisance_id = "nuis_l",
                                                   smpls = smpls,
                                                   est_params = self$get_params("ml_l"),
                                                   return_train_preds = FALSE,
                                                   task_type = private$task_type$ml_l,
                                                   fold_specific_params = private$fold_specific_params)

                            d = self$data$data_model[[self$data$treat_col]]
                            y = self$data$data_model[[self$data$y_col]]

                            ##for IV-scores
                            g_hat = list(preds = NULL, targets = NULL,  models = NULL)
                            #gbar_hat = list(preds = NULL, models = NULL)
                            if (exists("ml_g", where = private$learner_)) {
                              if (self$score == "orth-IV") {
                                psi_theta_a = -(d - m_hat$preds) * (d - m_hat$preds)
                                psi_theta_b =  (d - m_hat$preds) * (y - l_hat$preds)
                              } else if (self$score == "NO-IV"){
                                psi_theta_a = -d * d
                                psi_theta_b =  d * (y - l_hat$preds)
                              }
                              theta_initial = -mean(psi_theta_b, na.rm = TRUE) / mean(psi_theta_a, na.rm = TRUE)

                              data_aux = data.table(self$data$data_model,
                                                    "y_minus_theta_d" = y - theta_initial * d)

                              g_hat = dml_cv_predict(self$learner$ml_g,
                                                     c(self$data$x_cols,self$data$xbar_cols),
                                                     "y_minus_theta_d",
                                                     data_aux,
                                                     nuisance_id = "nuis_g",
                                                     smpls = smpls,
                                                     est_params = self$get_params("ml_g"),
                                                     return_train_preds = FALSE,
                                                     task_type = private$task_type$ml_g,
                                                     fold_specific_params = private$fold_specific_params)
                            }

                            idx = self$data$data_model[[self$data$cluster_cols]]

                            # Transform the nuisance parameters and the target variables
                            if(self$dml_approach == "hybrid" && self$dml_transform == "wg"){

                              m_hat_raw = m_hat
                              l_hat_raw = l_hat
                              m_hat$pred   = wg_transformation(m_hat_raw$preds,idx)
                              l_hat$pred   = wg_transformation(l_hat_raw$preds,idx)
                              m_hat$target = wg_transformation(m_hat_raw$targets,idx)
                              l_hat$target = wg_transformation(l_hat_raw$targets,idx)
                              d = m_hat$target
                              y = l_hat$target


                              if (exists("ml_g", where = private$learner_)) {
                                g_hat$pred   = wg_transformation(g_hat_raw$preds,idx)
                                g_hat$target = wg_transformation(g_hat_raw$targets,idx)
                                y = g_hat$target
                              }

                              #print(paste0("m_hat$target = ", m_hat$target, "d = ", d, "m_hat_raw$target = ", m_hat_raw$target))

                            }else if(self$dml_approach == "hybrid" && self$dml_transform == "fd"){
                              m_hat_raw = m_hat
                              l_hat_raw = l_hat
                              m_hat$pred   = fd_transformation(m_hat_raw$preds,idx)
                              l_hat$pred   = fd_transformation(l_hat_raw$preds,idx)
                              m_hat$target = fd_transformation(m_hat_raw$targets,idx)
                              l_hat$target = fd_transformation(l_hat_raw$targets,idx)
                              d = m_hat$target
                              y = l_hat$target

                              if (exists("ml_g", where = private$learner_)) {
                                g_hat$pred   = fd_transformation(g_hat_raw$preds,idx)
                                g_hat$target = fd_transformation(g_hat_raw$targets,idx)
                                y = g_hat$target
                              }
                            }
                            res = private$panel_score_elements(y, d, ybar, dbar,
                                                               m_hat$preds, l_hat$preds, g_hat$preds)
                            res$preds = list(
                              "ml_l" = l_hat$preds,
                              "ml_m" = m_hat$preds,
                              "ml_g" = g_hat$preds)
                            res$targets = list(
                              "ml_l"    = l_hat$targets,
                              "ml_m"    = m_hat$targets,
                              "ml_g"    = g_hat$targets)
                            res$models = list(
                              "ml_l"    = l_hat$models,
                              "ml_m"    = m_hat$models,
                              "ml_g"    = g_hat$models)

                            #print(paste0("m_hat$preds in plr = ", m_hat$preds))


                          }else if(self$dml_type == "separable"){
                            mbar_hat = dml_cv_predict(self$learner$ml_mbar,  ##for BG estimation
                                                      self$data$xbar_cols,
                                                      self$data$dbar_cols,
                                                      self$data$data_model,
                                                      nuisance_id = "nuis_mbar",
                                                      smpls = smpls,
                                                      est_params = self$get_params("ml_mbar"),
                                                      return_train_preds = FALSE,
                                                      task_type = private$task_type$ml_mbar,
                                                      fold_specific_params = private$fold_specific_params)

                            m_hat = dml_cv_predict(self$learner$ml_m,       ##for CRE estimation
                                                   c(self$data$x_cols,self$data$xbar_cols),
                                                   self$data$treat_col,
                                                   self$data$data_model,
                                                   nuisance_id = "nuis_m",
                                                   smpls = smpls,
                                                   est_params = self$get_params("ml_m"),
                                                   return_train_preds = FALSE,
                                                   task_type = private$task_type$ml_m,
                                                   fold_specific_params = private$fold_specific_params)

                            lbar_hat = dml_cv_predict(self$learner$ml_lbar,  ##for bg estimation
                                                      self$data$xbar_cols,
                                                      self$data$ybar_col,
                                                      self$data$data_model,
                                                      nuisance_id = "nuis_lbar",
                                                      smpls = smpls,
                                                      est_params = self$get_params("ml_lbar"),
                                                      return_train_preds = FALSE,
                                                      task_type = private$task_type$ml_lbar,
                                                      fold_specific_params = private$fold_specific_params)

                            l_hat = dml_cv_predict(self$learner$ml_l,  ##for cre estimation
                                                   c(self$data$x_cols,self$data$xbar_cols),
                                                   self$data$y_col,
                                                   self$data$data_model,
                                                   nuisance_id = "nuis_l",
                                                   smpls = smpls,
                                                   est_params = self$get_params("ml_l"),
                                                   return_train_preds = FALSE,
                                                   task_type = private$task_type$ml_l,
                                                   fold_specific_params = private$fold_specific_params)


                            d = self$data$data_model[[self$data$treat_col]]
                            y = self$data$data_model[[self$data$y_col]]
                            dbar = self$data$data_model[[self$data$dbar_cols]]
                            ybar = self$data$data_model[[self$data$ybar_col]]

                            ##for IV-scores
                            g_hat = list(preds = NULL, targets = NULL,  models = NULL)
                            gbar_hat = list(preds = NULL, targets = NULL,  models = NULL)
                            if (exists("ml_g", where = private$learner_)) {
                              if (self$score == "orth-IV") {
                                psi_theta_a = -(d - m_hat$preds - mbar_hat$preds) * (d - m_hat$preds - mbar_hat$preds)
                                psi_theta_b =  (d - m_hat$preds - mbar_hat$preds) * (y - l_hat$preds - lbar_hat$preds)
                              } else if (self$score == "NO-IV"){
                                psi_theta_a = -d * d
                                psi_theta_b =  d * (y - l_hat$preds - lbar_hat$preds)
                              }
                              theta_initial = -mean(psi_theta_b, na.rm = TRUE) / mean(psi_theta_a, na.rm = TRUE)

                              data_aux = data.table(self$data$data_model,
                                                    "y_minus_theta_d" = y - theta_initial * d)

                              gbar_hat = dml_cv_predict(self$learner$ml_gbar,
                                                        self$data$x_cols,
                                                        "y_minus_theta_d",
                                                        data_aux,
                                                        nuisance_id = "nuis_gbar",
                                                        smpls = smpls,
                                                        est_params = self$get_params("ml_gbar"),
                                                        return_train_preds = FALSE,
                                                        task_type = private$task_type$ml_gbar,
                                                        fold_specific_params = private$fold_specific_params)

                              data_aux = data.table(self$data$data_model,
                                                    "y_minus_g0_hat" = y - gbar_hat$preds)

                              g_hat = dml_cv_predict(self$learner$ml_g,
                                                     self$data$xbar_cols,
                                                     "y_minus_g0_hat",
                                                     data_aux,
                                                     nuisance_id = "nuis_g",
                                                     smpls = smpls,
                                                     est_params = self$get_params("ml_g"),
                                                     return_train_preds = FALSE,
                                                     task_type = private$task_type$ml_g,
                                                     fold_specific_params = private$fold_specific_params)
                            }

                            idx = self$data$data_model[[self$data$cluster_cols]]

                            # Transform the nuisance parameters and the target variables
                            if(self$dml_transform == "wg"){

                              m_hat_raw = m_hat
                              l_hat_raw = l_hat

                              m_hat$pred   = wg_transformation(m_hat_raw$preds,idx)
                              l_hat$pred   = wg_transformation(l_hat_raw$preds,idx)

                              m_hat$target = wg_transformation(m_hat_raw$targets,idx)
                              l_hat$target = wg_transformation(l_hat_raw$targets,idx)

                              d = m_hat$target
                              y = l_hat$target

                              if (exists("ml_g", where = private$learner_)) {
                                g_hat$pred   = wg_transformation(g_hat_raw$preds,idx)
                                g_hat$target = wg_transformation(g_hat_raw$targets,idx)
                                y = g_hat$target
                              }
                            }else if(self$dml_transform == "fd"){
                              m_hat = fd_transformation(m_hat_raw,idx)
                              l_hat = fd_transformation(l_hat_raw,idx)
                              d = fd_transformation(d_raw,idx)
                              y = fd_transformation(y_raw,idx)
                              if (exists("ml_g", where = private$learner_)) {
                                g_hat_raw = g_hat$preds
                                g_hat = fd_transformation(g_hat_raw,idx)
                              }
                            }

                            res = private$panel_score_elements(y, d, ybar, dbar,
                                                               m_hat$preds, l_hat$preds, g_hat$preds,
                                                               mbar_hat$preds, lbar_hat$preds, gbar_hat$preds)

                            res$preds = list(
                              "ml_l" = l_hat$preds,
                              "ml_m" = m_hat$preds,
                              "ml_g" = g_hat$preds,
                              "ml_lbar" = lbar_hat$preds,
                              "ml_mbar" = mbar_hat$preds,
                              "ml_gbar" = gbar_hat$preds)
                            res$targets = list(
                              "ml_l"    = l_hat$targets,
                              "ml_m"    = m_hat$targets,
                              "ml_g"    = g_hat$targets,
                              "ml_lbar" = lbar_hat$targets,
                              "ml_mbar" = mbar_hat$targets,
                              "ml_gbar" = gbar_hat$targets)
                            res$models = list(
                              "ml_l"    = l_hat$models,
                              "ml_m"    = m_hat$models,
                              "ml_g"    = g_hat$models,
                              "ml_lbar" = lbar_hat$models,
                              "ml_mbar" = mbar_hat$models,
                              "ml_gbar" = gbar_hat$models)
                          }
                          return(res)
                        },

                        ## Estimate causal parameters (pi_hat, theta_hat)
                        panel_score_elements = function(y, d, ybar, dbar,
                                                        m_hat, l_hat,  g_hat,
                                                        mbar_hat, lbar_hat, gbar_hat,
                                                        smpls){

                          if(self$dml_type == "non-separable"){
                            # Residuals
                            u_hat  = y - l_hat
                            v_hat  = d - m_hat
                            ug_hat = y - g_hat

                            # Calculate scores
                            if (self$score == "orth-PO") {
                              # orthogonal score
                              psi_theta_a = -1*v_hat * v_hat
                              psi_theta_b =    v_hat * u_hat
                              res_y = u_hat
                              res_d = v_hat

                            } else if (self$score == "orth-IV") {
                              # orthogonal score
                              psi_theta_a = -1*v_hat * d
                              psi_theta_b =    v_hat * ug_hat
                              res_y = ug_hat
                              res_d = d

                            } else if (self$score == "NO"){
                              # non-orthogonal score
                              psi_theta_a = -1*d * d
                              psi_theta_b =    d * u_hat
                              res_y = u_hat
                              res_d = d

                            } else if (self$score == "NO-IV"){
                              psi_theta_a = -1*d * d
                              psi_theta_b =    d * ug_hat
                              res_y = ug_hat
                              res_d = d
                            }

                            theta_hat = -mean(psi_theta_b, na.rm = TRUE) / mean(psi_theta_a, na.rm = TRUE)

                            # Calculate Model RMSE
                            if (self$score == "orth-PO") {
                              model_mse = (u_hat - v_hat*theta_hat)^2
                            } else if (self$score == "orth-IV") {
                              model_mse = (ug_hat - d *theta_hat)^2  ##check if correct
                            } else if (self$score == "NO"){
                              model_mse = (u_hat - d *theta_hat)^2
                            } else if (self$score == "NO-IV"){
                              model_mse = (ug_hat - d *theta_hat)^2
                            }

                            #print(paste0("model_rmse in double_ml_cre_plr.R: ", model_rmse))

                            res = list(theta_hat  = theta_hat,
                                       model_mse = model_mse,
                                       res_y = res_y,
                                       res_d = res_d)
                            psis = list(psi_theta_a = psi_theta_a,
                                        psi_theta_b = psi_theta_b)
                            #print(paste0("res$res_y: ", res$res_y, "; ", "res$res_d: ", res$res_d))

                            return(c(res,psis))

                          }else if(self$dml_type == "separable"){
                            pi_old = 0
                            theta_old = 0

                            iterations = 0
                            ErrorTolerance = 1e-15
                            MaxIterations = 1000
                            ContinueCondition = TRUE

                            while(ContinueCondition){

                              v_hat = d - m_hat
                              vbar_hat = dbar - mbar_hat

                              psi_theta_a = -1*v_hat*v_hat
                              psi_theta_b = v_hat*(y - l_hat - vbar_hat*pi_old)

                              theta_new = -mean(psi_theta_b, na.rm = TRUE) / mean(psi_theta_a, na.rm = TRUE)

                              #BG estimator for d_i
                              psi_bg_a = -1*vbar_hat*vbar_hat
                              psi_bg_b =    vbar_hat*(ybar-lbar_hat)

                              theta_bg = -mean(psi_bg_b, na.rm = TRUE) / mean(psi_bg_a, na.rm = TRUE)

                              #Note that CRE estimator for d_i is: pi_new = theta_bg - theta_wg
                              pi_new =  theta_bg - theta_new

                              # 4. Evaluate convergence
                              ContinueCondition = (abs(pi_old-pi_new)>ErrorTolerance &
                                                     abs(theta_new-theta_old)>ErrorTolerance &
                                                     iterations < MaxIterations)

                              if (ContinueCondition){
                                # For next iteration
                                pi_old     = pi_new
                                theta_old  = theta_new
                                iterations = iterations+1

                              } else if (ContinueCondition == FALSE & iterations < MaxIterations)  {

                                print(paste0("Convergence of estimation algorithm at initeration no.",iterations))

                                theta_hat  = theta_new
                                res = list(theta_hat = theta_hat)
                                psis = list(psi_theta_a = psi_theta_a,
                                            psi_theta_b = psi_theta_b)
                                return(c(res,psis))

                              } else {
                                stop(print(paste0("Convergence of estimation algorithm NOT achieved. no. iterations = ",iterations)))
                              }
                            }
                          }
                        },

                        nuisance_tuning = function(smpls, param_set, tune_settings,
                                                   tune_on_folds, ...) {

                          if (!tune_on_folds) {
                            data_tune_list = list(self$data$data_model)
                          } else {
                            data_tune_list = lapply(smpls$train_ids, function(x) {
                              extract_training_data(self$data$data_model, x)
                            })
                          }

                          if(self$dml_type == "non-separable"){

                            tuning_result_m = dml_tune(self$learner$ml_m,
                                                       c(self$data$x_cols,self$data$xbar_cols,self$data$dbar_cols),
                                                       self$data$treat_col,
                                                       data_tune_list,
                                                       nuisance_id = "nuis_m",
                                                       param_set$ml_m,
                                                       tune_settings,
                                                       tune_settings$measure$ml_m,
                                                       private$task_type$ml_m)
#print(paste0("data_tune_list: ", data_tune_list))
#print(paste0("tuning_result_m: ", tuning_result_m))

                            tuning_result_l = dml_tune(self$learner$ml_l,
                                                       c(self$data$x_cols,self$data$xbar_cols),
                                                       self$data$y_col,
                                                       data_tune_list,
                                                       nuisance_id = "nuis_l",
                                                       param_set$ml_l,
                                                       tune_settings,
                                                       tune_settings$measure$ml_l,
                                                       private$task_type$ml_l)

                            if (exists("ml_g", where = private$learner_)) {
                              if (tune_on_folds) {
                                params_l = tuning_result_l$params
                                params_m = tuning_result_m$params
                              } else {
                                params_l = tuning_result_l$params[[1]]
                                params_m = tuning_result_m$params[[1]]
                              }

                              l_hat = dml_cv_predict(self$learner$ml_l,
                                                     c(self$data$x_cols,self$data$xbar_cols),
                                                     self$data$y_col,
                                                     self$data$data_model,
                                                     nuisance_id = "nuis_l",
                                                     smpls = smpls,
                                                     est_params = params_l,
                                                     return_train_preds = FALSE,
                                                     task_type = private$task_type$ml_l,
                                                     fold_specific_params = private$fold_specific_params)

                              ##Only for orth-IV
                              if (self$score == "orth-IV"){
                                m_hat = dml_cv_predict(self$learner$ml_m,
                                                       c(self$data$x_cols,self$data$xbar_cols,self$data$dbar_cols),
                                                       self$data$treat_col,
                                                       self$data$data_model,
                                                       nuisance_id = "nuis_m",
                                                       smpls = smpls,
                                                       est_params = params_m,
                                                       return_train_preds = FALSE,
                                                       task_type = private$task_type$ml_m,
                                                       fold_specific_params = private$fold_specific_params)
                              }

                              d = self$data$data_model[[self$data$treat_col]]
                              y = self$data$data_model[[self$data$y_col]]

                              ##give two options
                              if (self$score == "orth-IV") {
                                # orthogonal score
                                psi_theta_a = -(d - m_hat) * (d - m_hat)
                                psi_theta_b =  (d - m_hat) * (y - l_hat)

                              } else if (self$score == "NO-IV"){
                                psi_theta_a = -d * d
                                psi_theta_b =  d * (y - l_hat)
                              }

                              theta_initial = -mean(psi_theta_b, na.rm = TRUE) / mean(psi_theta_a, na.rm = TRUE)

                              data_aux = data.table(self$data$data_model,
                                                    "y_minus_theta_d" = y - theta_initial * d)

                              if (!tune_on_folds) {
                                data_aux_tune_list = list(data_aux)
                              } else {
                                data_aux_tune_list = lapply(smpls$train_ids, function(x) {
                                  extract_training_data(data_aux, x)
                                })
                              }

                              tuning_result_g = dml_tune(self$learner$ml_g,
                                                         c(self$data$x_cols,self$data$xbar_cols),
                                                         "y_minus_theta_d",
                                                         data_aux_tune_list,
                                                         nuisance_id = "nuis_g",
                                                         param_set$ml_g, tune_settings,
                                                         tune_settings$measure$ml_g,
                                                         private$task_type$ml_g)

                              tuning_result = list(
                                "ml_l" = list(tuning_result_l, params = tuning_result_l$params),
                                "ml_m" = list(tuning_result_m, params = tuning_result_m$params),
                                "ml_g" = list(tuning_result_g, params = tuning_result_g$params))

                            }else {
                              tuning_result = list(
                                "ml_l" = list(tuning_result_l, params = tuning_result_l$params),
                                "ml_m" = list(tuning_result_m, params = tuning_result_m$params))
                            }
                          } else if(self$dml_type == "separable"){

                            d = self$data$data_model[[self$data$treat_col]]
                            y = self$data$data_model[[self$data$y_col]]
                            dbar = self$data$data_model[[self$data$dbar_cols]]
                            ybar = self$data$data_model[[self$data$ybar_col]]

                            tuning_result_mbar = dml_tune(self$learner$ml_mbar,
                                                          self$data$xbar_cols,
                                                          self$data$dbar_cols,
                                                          data_tune_list,
                                                          nuisance_id = "nuis_mbar",
                                                          param_set$ml_mbar,
                                                          tune_settings,
                                                          tune_settings$measure$ml_mbar,
                                                          private$task_type$ml_mbar)

                            tuning_result_m = dml_tune(self$learner$ml_m,
                                                       c(self$data$x_cols,self$data$xbar_cols),
                                                       self$data$treat_col,
                                                       data_tune_list,
                                                       nuisance_id = "nuis_m",
                                                       param_set$ml_m,
                                                       tune_settings,
                                                       tune_settings$measure$ml_m,
                                                       private$task_type$ml_m)

                            tuning_result_lbar = dml_tune(self$learner$ml_lbar,
                                                          self$data$xbar_cols,
                                                          self$data$ybar_col,
                                                          data_tune_list,
                                                          nuisance_id = "nuis_lbar",
                                                          param_set$ml_lbar,
                                                          tune_settings,
                                                          tune_settings$measure$ml_lbar,
                                                          private$task_type$ml_lbar)

                            tuning_result_l = dml_tune(self$learner$ml_l,
                                                       c(self$data$x_cols,self$data$xbar_cols),
                                                       self$data$y_col,
                                                       data_tune_list,
                                                       nuisance_id = "nuis_l",
                                                       param_set$ml_l,
                                                       tune_settings,
                                                       tune_settings$measure$ml_l,
                                                       private$task_type$ml_l)


                            if (exists("ml_g", where = private$learner_) &&
                                exists("ml_gbar", where = private$learner_)) {
                              if (tune_on_folds) {
                                params_l = tuning_result_l$params
                                params_m = tuning_result_m$params
                                params_l0 = tuning_result_lbar$params
                                params_m0 = tuning_result_mbar$params
                              } else {
                                params_l = tuning_result_l$params[[1]]
                                params_m = tuning_result_m$params[[1]]
                                params_l0 = tuning_result_lbar$params[[1]]
                                params_m0 = tuning_result_mbar$params[[1]]
                              }

                              lbar_hat = dml_cv_predict(self$learner$ml_lbar,
                                                        self$data$x_cols,
                                                        self$data$y_col,
                                                        self$data$data_model,
                                                        nuisance_id = "nuis_lbar",
                                                        smpls = smpls,
                                                        est_params = self$get_params("ml_lbar"),
                                                        return_train_preds = FALSE,
                                                        task_type = private$task_type$ml_lbar,
                                                        fold_specific_params = private$fold_specific_params)

                              data_aux = data.table(self$data$data_model,
                                                    "y_minus_l0_hat" = y - lbar_hat)

                              l_hat = dml_cv_predict(self$learner$ml_l,
                                                     self$data$xbar_cols,
                                                     "y_minus_l0_hat",
                                                     data_aux,
                                                     nuisance_id = "nuis_l",
                                                     smpls = smpls,
                                                     est_params = self$get_params("ml_l"),
                                                     return_train_preds = FALSE,
                                                     task_type = private$task_type$ml_l,
                                                     fold_specific_params = private$fold_specific_params)

                              ##Only for orth-IV
                              if (self$score == "orth-IV"){

                                mbar_hat = dml_cv_predict(self$learner$ml_mbar,
                                                          self$data$x_cols,
                                                          self$data$treat_col,
                                                          self$data$data_model,
                                                          nuisance_id = "nuis_mbar",
                                                          smpls = smpls,
                                                          est_params = self$get_params("ml_mbar"),
                                                          return_train_preds = FALSE,
                                                          task_type = private$task_type$ml_mbar,
                                                          fold_specific_params = private$fold_specific_params)

                                data_aux = data.table(self$data$data_model,
                                                      "d_minus_m0_hat" = d - mbar_hat)

                                m_hat = dml_cv_predict(self$learner$ml_m,
                                                       c(self$data$xbar_cols,self$data$dbar_cols),
                                                       "d_minus_m0_hat",
                                                       data_aux,
                                                       nuisance_id = "nuis_m",
                                                       smpls = smpls,
                                                       est_params = self$get_params("ml_m"),
                                                       return_train_preds = FALSE,
                                                       task_type = private$task_type$ml_m,
                                                       fold_specific_params = private$fold_specific_params)
                              }

                              ##give two options
                              if (self$score == "orth-IV") {
                                # orthogonal score
                                psi_theta_a = -(d - m_hat) * (d - m_hat)
                                psi_theta_b =  (d - m_hat) * (y - l_hat)

                              } else if (self$score == "NO-IV"){
                                psi_theta_a = -d * d
                                psi_theta_b =  d * (y - l_hat)
                              }

                              theta_initial = -mean(psi_theta_b, na.rm = TRUE) / mean(psi_theta_a, na.rm = TRUE)

                              data_aux = data.table(self$data$data_model,
                                                    "y_minus_theta_d" = y - theta_initial * d)

                              if (!tune_on_folds) {
                                data_aux_tune_list = list(data_aux)
                              } else {
                                data_aux_tune_list = lapply(smpls$train_ids, function(x) {
                                  extract_training_data(data_aux, x)
                                })
                              }

                              tuning_result_g0 = dml_tune(self$learner$ml_gbar,
                                                          self$data$x_cols,
                                                          "y_minus_theta_d",
                                                          data_aux_tune_list,
                                                          nuisance_id = "nuis_gbar",
                                                          param_set$ml_gbar, tune_settings,
                                                          tune_settings$measure$ml_gbar,
                                                          private$task_type$ml_gbar)

                              data_aux_tuned = data.table(self$data$data_model,
                                                          "y_minus_tuned_g0" = y - tuning_result_g0)

                              if (!tune_on_folds) {
                                data_aux_tune_list = list(data_aux_tuned)
                              } else {
                                data_aux_tune_list = lapply(smpls$train_ids, function(x) {
                                  extract_training_data(data_aux_tuned, x)
                                })
                              }

                              tuning_result_g = dml_tune(self$learner$ml_g,
                                                         self$data$xbar_cols,
                                                         "y_minus_tuned_g0",
                                                         data_aux_tune_list,
                                                         nuisance_id = "nuis_g",
                                                         param_set$ml_g, tune_settings,
                                                         tune_settings$measure$ml_g,
                                                         private$task_type$ml_g)

                              tuning_result = list(
                                "ml_l" = list(tuning_result_l, params = tuning_result_l$params),
                                "ml_m" = list(tuning_result_m, params = tuning_result_m$params),
                                "ml_g" = list(tuning_result_g, params = tuning_result_g$params),
                                "ml_lbar" = list(tuning_result_lbar, params = tuning_result_lbar$params),
                                "ml_mbar" = list(tuning_result_mbar, params = tuning_result_mbar$params),
                                "ml_gbar" = list(tuning_result_gbar, params = tuning_result_gbar$params))

                            }else {
                              tuning_result = list(
                                "ml_l" = list(tuning_result_l, params = tuning_result_l$params),
                                "ml_m" = list(tuning_result_m, params = tuning_result_m$params),
                                "ml_lbar" = list(tuning_result_lbar, params = tuning_result_lbar$params),
                                "ml_mbar" = list(tuning_result_mbar, params = tuning_result_mbar$params))
                            }
                          }
                          return(tuning_result)
                        },
                        check_score = function(score) {
                          assert(check_character(score))
                          if (is.character(score)) {
                            valid_score = c("orth-PO", "orth-IV", "NO", "NO-IV")
                            assertChoice(score, valid_score)
                          }
                          return()
                        },
                        check_approach_and_type = function(approach, type, transform) {
                          assert(check_character(approach))
                          assert(check_character(type))
                          assert(check_character(transform))
                          if (is.character(approach)){
                            valid_approach = c("cre", "hybrid")
                            assertChoice(approach, valid_approach)
                            if (approach == "hybrid"){
                              if (is.character(transform)){
                                valid_transform = c("wg", "fd")
                                assertChoice(transform, valid_transform)
                              }
                            }
                          }
                          if (is.character(type)){
                            valid_type = c("non-separable", "separable")
                            assertChoice(type, valid_type)
                          }
                          return()
                        },
                        check_data = function(obj_dml_data) {
                          return()
                        }
                      )
)
