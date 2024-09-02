#' @title Double machine learning for partially linear IV regression models
#'
#' @description
#' Double machine learning for partially linear IV regression models.
#'
#' @format [R6::R6Class] object inheriting from [dml_approx].
#'
#' @family dml_approx
#'
#' @details
#' Partially linear IV regression (PLIV) models take the form
#'
#' \eqn{Y - D\theta_0 = g_0(X) + \zeta},
#'
#' \eqn{Z = m_0(X) + V},
#'
#' with \eqn{E[\zeta|Z,X]=0} and \eqn{E[V|X] = 0}. \eqn{Y} is the outcome variable variable, \eqn{D} is the policy variable of interest and \eqn{Z} denotes one or multiple instrumental variables. The high-dimensional vector \eqn{X = (X_1, \ldots, X_p)} consists of other confounding covariates, and \eqn{\zeta} and \eqn{V} are stochastic errors.
#'
#' @usage NULL
#'
#' @export
dml_approx_pliv = R6Class("dml_approx_pliv",
  inherit = dml_approx,
  active = list(
    #' @field partialX (`logical(1)`)  \cr
    #' Indicates whether covariates \eqn{X} should be partialled out.
    partialX = function(value) {
      if (missing(value)) {
        return(private$partialX_)
      } else {
        stop("can't set field partialX")
      }
    },

    #' @field partialZ (`logical(1)`) \cr
    #' Indicates whether instruments \eqn{Z} should be partialled out.
    partialZ = function(value) {
      if (missing(value)) {
        return(private$partialZ_)
      } else {
        stop("can't set field partialZ")
      }
    }),

  public = list(
    #' @description
    #' Creates a new instance of this R6 class.
    #'
    #' @param data (`dml_approx_data`) \cr
    #' The `dml_approx_data` object providing the data and specifying the variables
    #' of the causal model.
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
    #' `lrn("regr.cv_glmnet", s = "lambda.min")`.  \cr
    #' `ml_l` refers to the nuisance function \eqn{l_0(X) = E[Y|X]}.
    #'
    #' @param ml_m ([`LearnerRegr`][mlr3::LearnerRegr],
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
    #' `ml_m` refers to the nuisance function \eqn{m_0(X) = E[Z|X]}.
    #'
    #' @param ml_r ([`LearnerRegr`][mlr3::LearnerRegr],
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
    #' `ml_r` refers to the nuisance function \eqn{r_0(X) = E[D|X]}.
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
    #' `ml_g` refers to the nuisance function \eqn{g_0(X) = E[Y - D\theta_0|X]}.
    #' Note: The learner `ml_g` is only required for the score `'IV-type'`.
    #' Optionally, it can be specified and estimated for callable scores.
    #'
    #' @param partialX (`logical(1)`)  \cr
    #' Indicates whether covariates \eqn{X} should be partialled out.
    #' Default is `TRUE`.
    #'
    #' @param partialZ (`logical(1)`) \cr
    #' Indicates whether instruments \eqn{Z} should be partialled out.
    #' Default is `FALSE`.
    #'
    #' @param n_folds (`integer(1)`)\cr
    #' Number of folds. Default is `5`.
    #'
    #' @param n_rep (`integer(1)`) \cr
    #' Number of repetitions for the sample splitting. Default is `1`.
    #'
    #' @param score (`character(1)`, `function()`) \cr
    #' A `character(1)` (`"partialling out"` or `"IV-type"`) or a `function()`
    #' specifying the score function.
    #' If a `function()` is provided, it must be of the form
    #' `function(y, z, d, l_hat, m_hat, r_hat, g_hat, smpls)` and
    #' the returned output must be a named `list()` with elements
    #' `psi_theta_a` and `psi_theta_b`. Default is `"partialling out"`.
    #'
    #' @param dml_procedure (`character(1)`) \cr
    #' A `character(1)` (`"dml1"` or `"dml2"`) specifying the double machine
    #' learning algorithm. Default is `"dml2"`.
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
      ml_r,
      ml_g = NULL,
      partialX = TRUE,
      partialZ = FALSE,
      n_folds = 5,
      n_rep = 1,
      score = "partialling out",
      dml_procedure = "dml2",
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
        draw_sample_splitting,
        apply_cross_fitting)

      private$check_data(self$data)
      assert_logical(partialX, len = 1)
      assert_logical(partialZ, len = 1)
      private$partialX_ = partialX
      private$partialZ_ = partialZ
      private$check_score(self$score)

      if (!self$partialX & self$partialZ) {
        ml_r = private$assert_learner(ml_r, "ml_r", Regr = TRUE, Classif = FALSE)
        private$learner_ = list("ml_r" = ml_r)
      } else {
        ml_l = private$assert_learner(ml_l, "ml_l", Regr = TRUE, Classif = FALSE)
        ml_m = private$assert_learner(ml_m, "ml_m", Regr = TRUE, Classif = FALSE)
        ml_r = private$assert_learner(ml_r, "ml_r", Regr = TRUE, Classif = FALSE)
        private$learner_ = list("ml_l" = ml_l,
                                "ml_m" = ml_m,
                                "ml_r" = ml_r)
        if (!is.null(ml_g)) {
          assert(
            check_character(ml_g, max.len = 1),
            check_class(ml_g, "Learner"))
          if ((is.character(self$score) && (self$score == "IV-type")) ||
            is.function(self$score)) {
            ml_g = private$assert_learner(ml_g, "ml_g",
              Regr = TRUE, Classif = FALSE)
            private$learner_[["ml_g"]] = ml_g
          } else if (is.character(self$score) &&
            (self$score == "partialling out")) {
            warning(paste0(
              "A learner ml_g has been provided for ",
              "score = 'partialling out' but will be ignored. ",
              "A learner ml_g is not required for estimation."))
          }
        } else if (is.character(self$score) && (self$score == "IV-type")) {
          stop(paste(
            "For score = 'IV-type', learners",
            "ml_l, ml_m, ml_r and ml_g need to be specified."))
        }
      }
      private$initialize_ml_nuisance_params()
    },
    #' @description
    #' Set hyperparameters for the nuisance models of dml_approx models.
    #'
    #' Note that in the current implementation, either all parameters have to
    #' be set globally or all parameters have to be provided fold-specific.
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
    #' Indicates if the parameters passed in `params` should be passed in
    #' fold-specific way. Default is `FALSE`. If `TRUE`, the outer list needs
    #' to be of length `n_rep` and the inner list of length `n_folds`.
    #' Note that in the current implementation, either all parameters have to
    #' be set globally or all parameters have to be provided fold-specific.
    #'
    #' @return self
    set_ml_nuisance_params = function(learner = NULL, treat_var = NULL,
                                      params, set_fold_specific = FALSE) {
      assert_character(learner, len = 1)
      if (is.character(self$score) && (self$score == "partialling out") &&
        (learner == "ml_g")) {
        warning(paste0(
          "Learner ml_g was renamed to ml_l. ",
          "Please adapt the argument learner accordingly. ",
          "The provided parameters are set for ml_l. ",
          "The redirection will be removed in a future version."),
        call. = FALSE)
        learner = "ml_l"
      }
      super$set_ml_nuisance_params(
        learner, treat_var, params,
        set_fold_specific)
    },
    #' @description
    #' Hyperparameter-tuning for dml_approx models.
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
      algorithm = mlr3tuning::tnr("grid_search"), resolution = 5),
    tune_on_folds = FALSE) {

      assert_list(param_set)
      if (is.character(self$score) && (self$score == "partialling out")) {
        if (exists("ml_g", where = param_set) && !exists("ml_l", where = param_set)) {
          warning(paste0(
            "Learner ml_g was renamed to ml_l. ",
            "Please adapt the name in param_set accordingly. ",
            "The provided param_set for ml_g is used for ml_l. ",
            "The redirection will be removed in a future version."),
          call. = FALSE)
          names(param_set)[names(param_set) == "ml_g"] = "ml_l"
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
    partialX_ = NULL,
    partialZ_ = NULL,
    n_nuisance = 3,
    i_instr = NULL,
    initialize_ml_nuisance_params = function() {
      if ((self$partialX && !self$partialZ) && (self$data$n_instr > 1)) {
        param_names = c("ml_l", "ml_r", paste0("ml_m_", self$data$z_cols))
      } else {
        param_names = names(private$learner_)
      }
      nuisance = vector("list", self$data$n_treat)
      names(nuisance) = self$data$d_cols

      private$params_ = rep(list(nuisance), length(param_names))
      names(private$params_) = param_names
      invisible(self)
    },
    nuisance_est = function(smpls, ...) {
      if (self$partialX & !self$partialZ) {
        res = private$nuisance_est_partialX(smpls, ...)

      } else if (!self$partialX & self$partialZ) {
        res = private$nuisance_est_partialZ(smpls, ...)

      } else if (self$partialX & self$partialZ) {
        res = private$nuisance_est_partialXZ(smpls, ...)
      }
      return(res)
    },

    nuisance_est_partialX = function(smpls, ...) {

      l_hat = dml_cv_predict(self$learner$ml_l,
        self$data$x_cols,
        self$data$y_col,
        self$data$data_model,
        nuisance_id = "nuis_l",
        smpls = smpls,
        est_params = self$get_params("ml_l"),
        return_train_preds = FALSE,
        task_type = private$task_type$ml_l,
        fold_specific_params = private$fold_specific_params)

      preds   = list('ml_l' = l_hat$preds)
      targets = list('ml_l' = l_hat$targets)
      models  = list('ml_l' = l_hat$models)

      #print(paste0("preds[['ml_l']]: ", preds))

      r_hat = dml_cv_predict(self$learner$ml_r,
        self$data$x_cols,
        self$data$treat_col,
        self$data$data_model,
        nuisance_id = "nuis_r",
        smpls = smpls,
        est_params = self$get_params("ml_r"),
        return_train_preds = FALSE,
        task_type = private$task_type$ml_r,
        fold_specific_params = private$fold_specific_params)

      preds[['ml_r']]   = r_hat$preds
      targets[['ml_r']] = r_hat$targets
      models[['ml_r']]  = r_hat$models

      if (self$data$n_instr == 1) {
        m_hat = dml_cv_predict(self$learner$ml_m,
          self$data$x_cols,
          self$data$z_cols,
          self$data$data_model,
          nuisance_id = "nuis_m",
          smpls = smpls,
          est_params = self$get_params("ml_m"),
          return_train_preds = FALSE,
          task_type = private$task_type$ml_m,
          fold_specific_params = private$fold_specific_params)
        z = self$data$data_model[[self$data$z_cols]]

        preds[['ml_m']]   = m_hat$preds
        targets[['ml_m']] = m_hat$targets
        models[['ml_m']]  = m_hat$models

      } else {
         m_hat = list(preds = matrix(NA, nrow = self$data$n_obs, ncol = self$data$n_instr),
                      targets =  matrix(NA, nrow = self$data$n_obs, ncol = self$data$n_instr),
                      models  = NULL)

         for (i_instr in 1:self$data$n_instr){

           print(paste0("Instrumental variable: ", self$data$z_cols[[i_instr]], "; instrument no.: ", i_instr))

           res_cv_predict  = dml_cv_predict(self$learner$ml_m,
                                  self$data$x_cols,
                                  self$data$z_cols[[i_instr]],
                                  self$data$data_model,
                                  nuisance_id = "nuis_m",
                                  smpls = smpls,
                                  est_params = self$get_params(paste0("ml_m_", self$data$z_cols[[i_instr]])),
                                  return_train_preds = FALSE,
                                  task_type = private$task_type$ml_m,
                                  fold_specific_params = private$fold_specific_params)

           m_hat$preds[,i_instr]   = res_cv_predict$preds
           m_hat$targets[,i_instr] = res_cv_predict$targets
           #m_hat$models[i_instr]   = res_cv_predict$models

           m_lrn = paste0("ml_m_", self$data$z_cols[[i_instr]])
           preds[[m_lrn]]   = res_cv_predict$preds
           targets[[m_lrn]] = res_cv_predict$targets
           #models[[m_lrn]]  = res_cv_predict$models
        }
        z = self$data$data_model[, self$data$z_cols, with = FALSE]
      }


      d = self$data$data_model[[self$data$treat_col]]
      y = self$data$data_model[[self$data$y_col]]

      g_hat = list(preds = NULL, targets = NULL,  models = NULL)
      if (exists("ml_g", where = private$learner_)) {
        # get an initial estimate for theta using the partialling out score
        psi_theta_a = -(d - r_hat$preds) * (z - m_hat$preds)
        psi_theta_b =  (z - m_hat$preds) * (y - l_hat$preds)
        theta_initial = -mean(psi_theta_b, na.rm = TRUE) / mean(psi_theta_a, na.rm = TRUE)

        data_aux = data.table(self$data$data_model,
                              "y_minus_theta_d" = y - theta_initial * d)

        g_hat = dml_cv_predict(self$learner$ml_g,
                              self$data$x_cols,
                              "y_minus_theta_d",
                              data_aux,
                              nuisance_id = "nuis_g",
                              smpls = smpls,
                              est_params = self$get_params("ml_g"),
                              return_train_preds = FALSE,
                              task_type = private$task_type$ml_g,
                              fold_specific_params = private$fold_specific_params)
      }

      res = private$panel_score_elements(y, z, d,
                      l_hat$preds, m_hat$preds, r_hat$preds, g_hat$preds, smpls)

      res$preds = preds
      res$targets = targets
      res$models = models
      return(res)
    },
    panel_score_elements = function(y, z, d,
                              l_hat, m_hat, r_hat, g_hat, smpls){
      # Residuals
      u_hat = y - l_hat
      w_hat = d - r_hat
      v_hat = z - m_hat

      # Calculate scores
      if (self$data$n_instr == 1) {
        if (self$score == "partialling out") {
          psi_theta_a = -w_hat * v_hat
          psi_theta_b =  v_hat * u_hat
          res_y = u_hat
          res_d = w_hat
          res_z = v_hat

        } else if (self$score == "IV-type") {
          psi_theta_a = -d * v_hat
          psi_theta_b =  v_hat * (y - g_hat)
          res_y = y - g_hat
          res_d = d
          res_z = v_hat
        }

        theta_hat = -mean(psi_theta_b, na.rm = TRUE) / mean(psi_theta_a, na.rm = TRUE)

        # Calculate Model RMSE
        if (self$score == "partialling out") {
          model_mse = (y - l_hat - w_hat*theta_hat)^2
        } else if (self$score == "IV-type") {
          model_mse = (y - g_hat - d *theta_hat)^2
        }
        res = list(theta_hat  = theta_hat,
                   model_mse = model_mse,
                   res_y = res_y,
                   res_d = res_d,
                   res_z = res_z)
        psis = list(psi_theta_a = psi_theta_a,
                    psi_theta_b = psi_theta_b)
        return(c(res,psis))

      } else {
        stopifnot(self$apply_cross_fitting)
        # Projection: r_hat from projection on m_hat
        data_aux = data.table(w_hat, v_hat)
        task_r_tilde = initiate_task("nuis_r_tilde", data_aux,
          target = "w_hat",
          select_cols = c(self$data$z_cols), "regr")
        #print(nuis_r_tilde)

        # equivalent to ml_r_tilde = lrn("regr.lm")
        ml_r_tilde = LearnerRegrLM$new()
        resampling_r_tilde = rsmp("insample")$instantiate(task_r_tilde)
        r_r_tilde = resample(task_r_tilde, ml_r_tilde, resampling_r_tilde,
          store_models = TRUE)
        r_hat_tilde = as.data.table(r_r_tilde$prediction())$response

        if (self$score == "partialling out") {
          psi_theta_a = -w_hat * r_hat_tilde
          psi_theta_b =  r_hat_tilde * u_hat
          res_y = u_hat
          res_d = w_hat
          res_z = r_hat_tilde
          theta_hat = -mean(psi_theta_b, na.rm = TRUE) / mean(psi_theta_a, na.rm = TRUE)
          model_mse = (u_hat - w_hat*theta_hat)^2
        }
        res = list(theta_hat = theta_hat,
                   model_mse = model_mse,
                   res_y = res_y,
                   res_d = res_d,
                   res_z = res_z)
        psis = list(psi_theta_a = psi_theta_a,
                    psi_theta_b = psi_theta_b)
        return(c(res,psis))
      }
    },
    nuisance_est_partialXZ = function(smpls, ...) {

      l_hat = dml_cv_predict(self$learner$ml_l,
        self$data$x_cols,
        self$data$y_col,
        self$data$data_model,
        nuisance_id = "nuis_l",
        smpls = smpls,
        est_params = self$get_params("ml_l"),
        return_train_preds = FALSE,
        task_type = private$task_type$ml_l,
        fold_specific_params = private$fold_specific_params)

       m_hat = dml_cv_predict(self$learner$ml_m,
                 c(self$data$x_cols,self$data$z_cols),
                 self$data$treat_col,
                 self$data$data_model,
                 nuisance_id = "nuis_m",
                 smpls = smpls,
                 est_params = self$get_params("ml_m"),
                 return_train_preds = TRUE,
                 task_type = private$task_type$ml_m,
                 fold_specific_params = private$fold_specific_params)
      data_aux_list = lapply(m_hat$train_preds, function(x) {
        setnafill(data.table(self$data$data_model, "m_hat_on_train" = x),
                  fill = -9999.99) # mlr3 does not allow NA's (values are not used)
      })
      #print(paste0("m_hat$train_preds: ", m_hat$train_preds))
      # TRY TO REMOVE NAs

      m_hat_tilde = dml_cv_predict(self$learner$ml_r,
                 self$data$x_cols,
                 "m_hat_on_train",
                 data_aux_list,
                 nuisance_id = "nuis_r",
                 smpls = smpls,
                 est_params = self$get_params("ml_r"),
                 return_train_preds = FALSE,
                 task_type = private$task_type$ml_r,
                 fold_specific_params = private$fold_specific_params)
      #print(paste0("m_hat_tilde$train_preds: ", m_hat_tilde$train_preds))

      d = self$data$data_model[[self$data$treat_col]]
      y = self$data$data_model[[self$data$y_col]]

      u_hat = y - l_hat$preds
      w_hat = d - m_hat_tilde$preds
      v_hat = m_hat$preds - m_hat_tilde$preds

      if (self$score == "partialling out") {
        psi_theta_a = -w_hat * v_hat
        psi_theta_b =  v_hat * u_hat
        res_y = u_hat
        res_d = w_hat
        res_z = v_hat
        theta_hat = -mean(psi_theta_b, na.rm = TRUE) / mean(psi_theta_a, na.rm = TRUE)
        model_mse = (u_hat - w_hat*theta_hat)^2
      }

      res = list(theta_hat  = theta_hat,
                 model_mse = model_mse,
                 res_y = res_y,
                 res_d = res_d,
                 res_z = res_z,
                 psi_theta_a = psi_theta_a,
                 psi_theta_b = psi_theta_b)

      res$preds = list(
        "ml_l"  = l_hat$preds,
        "ml_m"  = m_hat$preds,
        "ml_r"  = m_hat_tilde$preds)
      res$targets = list(
        "ml_l"    = l_hat$targets,
        "ml_m"    = m_hat$targets,
        "ml_r"    = m_hat_tilde$targets)
      res$models = list(
        "ml_l"   = l_hat$models,
        "ml_m"   = m_hat$models,
        "ml_r"   = m_hat_tilde$models)

      return(res)
    },

    nuisance_est_partialZ = function(smpls, ...) {

      # nuisance r
      r_hat = dml_cv_predict(self$learner$ml_r,
        c(self$data$x_cols,self$data$z_cols),
        self$data$treat_col,
        self$data$data_model,
        nuisance_id = "nuis_r",
        smpls = smpls,
        est_params = self$get_params("ml_r"),
        return_train_preds = FALSE,
        task_type = private$task_type$ml_r,
        fold_specific_params = private$fold_specific_params)

      d = self$data$data_model[[self$data$treat_col]]
      y = self$data$data_model[[self$data$y_col]]

      if (is.character(self$score)) {
        if (self$score == "partialling out") {
          psi_theta_a = -r_hat$preds * d
          psi_theta_b = r_hat$preds * y
          res_y = y
          res_d = d
          res_z = r_hat$preds
          theta_hat = -mean(psi_theta_b, na.rm = TRUE) / mean(psi_theta_a, na.rm = TRUE)
          model_mse = (y - d*theta_hat)^2
        }

      } else if (is.function(self$score)) {
        stop(paste(
          "Callable score not implemented for dml_approx_pliv",
          "with partialX=FALSE and partialZ=TRUE."))
      }

      res = list(theta_hat  = theta_hat,
                 model_mse = model_mse,
                 res_y = res_y,
                 res_d = res_d,
                 res_z = res_z,
                 psi_theta_a = psi_theta_a,
                 psi_theta_b = psi_theta_b)

      res$preds   = list("ml_r" = r_hat$preds)
      res$targets = list("ml_r" = r_hat$targets)
      res$models  = list("ml_r" = r_hat$models)

      return(res)
    },
    nuisance_tuning = function(smpls, param_set, tune_settings,
      tune_on_folds, ...) {
      if (self$partialX & !self$partialZ) {
        res = private$nuisance_tuning_partialX(
          smpls, param_set,
          tune_settings,
          tune_on_folds, ...)

      } else if (!self$partialX & self$partialZ) {
        res = private$nuisance_tuning_partialZ(
          smpls, param_set,
          tune_settings,
          tune_on_folds, ...)

      } else if (self$partialX & self$partialZ) {
        res = private$nuisance_tuning_partialXZ(
          smpls, param_set,
          tune_settings,
          tune_on_folds, ...)
      }

      return(res)
    },

    nuisance_tuning_partialX = function(smpls, param_set,
      tune_settings, tune_on_folds, ...) {

      if (!tune_on_folds) {
        data_tune_list = list(self$data$data_model)
      } else {
        data_tune_list = lapply(
          smpls$train_ids,
          function(x) extract_training_data(self$data$data_model, x))
      }

      tuning_result_l = dml_tune(self$learner$ml_l,
        self$data$x_cols,
        self$data$y_col, data_tune_list,
        nuisance_id = "nuis_l",
        param_set$ml_l, tune_settings,
        tune_settings$measure$ml_l,
        private$task_type$ml_l)

      tuning_result_r = dml_tune(self$learner$ml_r,
        self$data$x_cols,
        self$data$treat_col, data_tune_list,
        nuisance_id = "nuis_r",
        param_set$ml_r, tune_settings,
        tune_settings$measure$ml_r,
        private$task_type$ml_r)

      if (self$data$n_instr == 1) {
        tuning_result_m = dml_tune(self$learner$ml_m,
          self$data$x_cols,
          self$data$z_cols, data_tune_list,
          nuisance_id = "nuis_m",
          param_set$ml_m, tune_settings,
          tune_settings$measure$ml_m,
          private$task_type$ml_m)

        if (exists("ml_g", where = private$learner_)) {
          if (tune_on_folds) {
            params_l = tuning_result_l$params
            params_r = tuning_result_r$params
            params_m = tuning_result_m$params
          } else {
            params_l = tuning_result_l$params[[1]]
            params_r = tuning_result_r$params[[1]]
            params_m = tuning_result_m$params[[1]]
          }
          l_hat = dml_cv_predict(self$learner$ml_l,
            self$data$x_cols,
            self$data$y_col,
            self$data$data_model,
            nuisance_id = "nuis_l",
            smpls = smpls,
            est_params = params_l,
            return_train_preds = FALSE,
            task_type = private$task_type$ml_l,
            fold_specific_params = private$fold_specific_params)

          r_hat = dml_cv_predict(self$learner$ml_r,
            self$data$x_cols,
            self$data$treat_col,
            self$data$data_model,
            nuisance_id = "nuis_r",
            smpls = smpls,
            est_params = params_r,
            return_train_preds = FALSE,
            task_type = private$task_type$ml_r,
            fold_specific_params = private$fold_specific_params)

          m_hat = dml_cv_predict(self$learner$ml_m,
            self$data$x_cols,
            self$data$treat_col,
            self$data$data_model,
            nuisance_id = "nuis_m",
            smpls = smpls,
            est_params = params_m,
            return_train_preds = FALSE,
            task_type = private$task_type$ml_m,
            fold_specific_params = private$fold_specific_params)

          d = self$data$data_model[[self$data$treat_col]]
          y = self$data$data_model[[self$data$y_col]]
          z = self$data$data_model[[self$data$z_cols]]

          psi_theta_a = -(d - r_hat) * (z - m_hat)
          psi_theta_b =  (z - m_hat) * (y - l_hat)
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
            self$data$x_cols,
            "y_minus_theta_d", data_aux_tune_list,
            nuisance_id = "nuis_g",
            param_set$ml_g, tune_settings,
            tune_settings$measure$ml_g,
            private$task_type$ml_g)
          tuning_result = list(
            "ml_l" = list(tuning_result_l,params = tuning_result_l$params),
            "ml_m" = list(tuning_result_m,params = tuning_result_m$params),
            "ml_r" = list(tuning_result_r,params = tuning_result_r$params),
            "ml_g" = list(tuning_result_g,params = tuning_result_g$params))
        } else {
          tuning_result = list(
            "ml_l" = list(tuning_result_l,params = tuning_result_l$params),
            "ml_m" = list(tuning_result_m,params = tuning_result_m$params),
            "ml_r" = list(tuning_result_r,params = tuning_result_r$params))
        }

      } else {
        tuning_result = vector("list", length = self$data$n_instr + 2)
        names(tuning_result) = c(
          "ml_l", "ml_r",
          paste0("ml_m_", self$data$z_cols))
        tuning_result[["ml_l"]] = list(tuning_result_l,
          params = tuning_result_l$params)
        tuning_result[["ml_r"]] = list(tuning_result_r,
          params = tuning_result_r$params)

        tuning_result_m = vector("list", length = self$data$n_instr)
        names(tuning_result_m) = self$data$z_cols

        for (i_instr in 1:self$data$n_instr) {
          this_z = self$data$z_cols[i_instr]
          tuning_result_this_z = dml_tune(self$learner$ml_m,
            self$data$x_cols,
            this_z, data_tune_list,
            nuisance_id = paste0("nuis_m_", this_z),
            param_set$ml_m, tune_settings,
            tune_settings$measure$ml_m,
            private$task_type$ml_m)
          tuning_result[[paste0("ml_m_", this_z)]] = list(tuning_result_this_z,
            params = tuning_result_this_z$params)
        }
      }
      return(tuning_result)
    },

    nuisance_tuning_partialXZ = function(smpls, param_set,
      tune_settings, tune_on_folds, ...) {

      if (!tune_on_folds) {
        data_tune_list = list(self$data$data_model)
      } else {
        data_tune_list = lapply(
          smpls$train_ids,
          function(x) extract_training_data(self$data$data_model, x))
      }

      tuning_result_l = dml_tune(self$learner$ml_l,
        self$data$x_cols,
        self$data$y_col, data_tune_list,
        nuisance_id = "nuis_l",
        param_set$ml_l, tune_settings,
        tune_settings$measure$ml_l,
        private$task_type$ml_l)

      tuning_result_m = dml_tune(self$learner$ml_m,
        c(self$data$x_cols, self$data$z_cols),
        self$data$treat_col, data_tune_list,
        nuisance_id = "nuis_m",
        param_set$ml_m, tune_settings,
        tune_settings$measure$ml_m,
        private$task_type$ml_m)

      m_params = tuning_result_m$params
      ml_m = lapply(m_params, function(x) {
        initiate_learner(self$learner$ml_m,
          private$task_type$ml_m,
          params = x,
          return_train_preds = TRUE)
      })
      task_m = lapply(data_tune_list, function(x) {
        initiate_task("nuis_m", x,
          target = self$data$treat_col,
          select_cols = c(self$data$x_cols, self$data$z_cols),
          private$task_type$ml_m)
      })
      resampling_m_on_train = lapply(
        task_m,
        function(x) rsmp("insample")$instantiate(x))
      r_m_on_train = lapply(
        seq_len(length(data_tune_list)),
        function(x) {
          resample(task_m[[x]], ml_m[[x]],
            resampling_m_on_train[[x]],
            store_models = TRUE)
        })
      m_hat_on_train = extract_prediction(r_m_on_train,
        private$task_type$ml_m,
        self$data$n_obs,
        return_train_preds = TRUE)
      data_aux_list = lapply(seq_len(length(data_tune_list)), function(x) {
        data.table(data_tune_list[[x]], "m_hat_on_train" = m_hat_on_train[[x]])
      })

      tuning_result_r = dml_tune(self$learner$ml_r,
        self$data$x_cols,
        "m_hat_on_train", data_aux_list,
        nuisance_id = "nuis_r",
        param_set$ml_r, tune_settings,
        tune_settings$measure$ml_r,
        private$task_type$ml_r)

      tuning_result = list(
        "ml_l" = list(tuning_result_l, params = tuning_result_l$params),
        "ml_m" = list(tuning_result_m, params = tuning_result_m$params),
        "ml_r" = list(tuning_result_r, params = tuning_result_r$params))
      return(tuning_result)
    },

    nuisance_tuning_partialZ = function(smpls, param_set,
      tune_settings, tune_on_folds, ...) {
      if (!tune_on_folds) {
        data_tune_list = list(self$data$data_model)
      } else {
        data_tune_list = lapply(
          smpls$train_ids,
          function(x) extract_training_data(self$data$data_model, x))
      }

      tuning_result_r = dml_tune(self$learner$ml_r,
        c(self$data$x_cols,self$data$z_cols),
        self$data$treat_col, data_tune_list,
        nuisance_id = "nuis_r",
        param_set$ml_r, tune_settings,
        tune_settings$measure$ml_r,
        private$task_type$ml_r)

      tuning_result = list("ml_r" = list(tuning_result_r, params = tuning_result_r$params))
      return(tuning_result)
    },
    check_score = function(score) {
      assert(
        check_character(score),
        check_class(score, "function"))
      if (is.character(score)) {
        if ((self$partialX && !self$partialZ) && (self$data$n_instr == 1)) {
          valid_score = c("partialling out", "IV-type")
        } else {
          valid_score = c("partialling out")
        }
        assertChoice(score, valid_score)
      }
      return()
    },
    check_data = function(obj_dml_data) {
      if (obj_dml_data$n_instr == 0) {
        stop(paste(
          "Incompatible data.\n",
          "At least one variable must be set as instrumental variable.\n",
          "To fit a partially linear regression model without instrumental",
          "variable(s) use dml_approxPLR instead of dml_approx_pliv."))
      }
      return()
    }
  )
)

# Initializer for partialX
dml_approx_pliv.partialX = function(data,
  ml_l,
  ml_m,
  ml_r,
  ml_g = NULL,
  n_folds = 5,
  n_rep = 1,
  score = "partialling out",
  dml_procedure = "dml2",
  draw_sample_splitting = TRUE,
  apply_cross_fitting = TRUE) {

  obj = dml_approx_pliv$new(
    data = data,
    ml_l = ml_l,
    ml_m = ml_m,
    ml_r = ml_r,
    ml_g = ml_g,
    partialX = TRUE,
    partialZ = FALSE,
    n_folds = n_folds,
    n_rep = n_rep,
    score = score,
    dml_procedure = dml_procedure,
    draw_sample_splitting = draw_sample_splitting,
    apply_cross_fitting = apply_cross_fitting)

  return(obj)
}

# Initializer for partialZ
dml_approx_pliv.partialZ = function(data,
  ml_r,
  n_folds = 5,
  n_rep = 1,
  score = "partialling out",
  dml_procedure = "dml2",
  draw_sample_splitting = TRUE,
  apply_cross_fitting = TRUE) {

  obj = dml_approx_pliv$new(
    data = data,
    ml_l = NULL,
    ml_m = NULL,
    ml_r = ml_r,
    ml_g = NULL,
    partialX = FALSE,
    partialZ = TRUE,
    n_folds = n_folds,
    n_rep = n_rep,
    score = score,
    dml_procedure = dml_procedure,
    draw_sample_splitting = draw_sample_splitting,
    apply_cross_fitting = apply_cross_fitting)

  return(obj)
}

# Initializer for partialXZ
dml_approx_pliv.partialXZ = function(data,
  ml_l,
  ml_m,
  ml_r,
  n_folds = 5,
  n_rep = 1,
  score = "partialling out",
  dml_procedure = "dml2",
  draw_sample_splitting = TRUE,
  apply_cross_fitting = TRUE) {

  obj = dml_approx_pliv$new(
    data = data,
    ml_l = ml_l,
    ml_m = ml_m,
    ml_r = ml_r,
    ml_g = NULL,
    partialX = TRUE,
    partialZ = TRUE,
    n_folds = n_folds,
    n_rep = n_rep,
    score = score,
    dml_procedure = dml_procedure,
    draw_sample_splitting = draw_sample_splitting,
    apply_cross_fitting = apply_cross_fitting)

  return(obj)
}
