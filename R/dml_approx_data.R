#' @title Set up for data with two cluster variables
#'
#' @description
#' Double machine learning (DML) data-backend for data with cluster variables.
#' `dml_approx_data` construct a synthetic data set suitable for panel data analysis.
#'
#' `dml_approx_data` objects can be initialized from a
#' [data.table][data.table::data.table()]. Alternatively `dml_approx` provides
#' functions to initialize from a collection of `matrix` objects or
#' a `data.frame`. The following functions can be used to create a new
#' instance of `dml_approx_data`.
#' * `dml_approx_data$new()` for initialization from a `data.table`.
#' * [dml_approx_data_from_data_frame()] for initialization from a `data.frame`.
#' @export
dml_approx_data = R6Class("dml_approx_data",
                              active = list(
                                #' @field all_variables (`character()`)\cr
                                #' All variables available in the dataset.
                                all_variables = function(value) {
                                  if (missing(value)) {
                                    return(names(self$data))
                                  } else {
                                    stop("can't set field all_variables")
                                  }
                                },

                                #' @field d_cols (`character()`)\cr
                                #' The treatment variable(s).
                                d_cols = function(value) {
                                  if (missing(value)) {
                                    return(private$d_cols_)
                                  } else {
                                    d_cols = value # to get more meaningful assert error messages
                                    reset_value = !is.null(self$data_model)
                                    assert_character(d_cols, unique = TRUE)
                                    assert_subset(d_cols, self$all_variables)
                                    private$d_cols_ = d_cols
                                    if (reset_value) {
                                      private$check_disjoint_sets()
                                      self$set_data_model(self$d_cols[1])
                                    }
                                  }
                                },

                                #' @field data ([`data.table`][data.table::data.table()])\cr
                                #' Data object.
                                data = function(value) {
                                  if (missing(value)) {
                                    return(private$data_)
                                  } else {
                                    stop("can't set field data")
                                  }
                                },

                                #' @field data_model ([`data.table`][data.table::data.table()])\cr
                                #' Internal data object that implements the causal model as specified by
                                #' the user via `y_col`, `d_cols`, `x_cols`,  and `z_cols`.
                                data_model = function(value) {
                                  if (missing(value)) {
                                    return(private$data_model_)
                                  } else {
                                    stop("can't set field data_model")
                                  }
                                },

                                #' @field n_instr (`NULL`, `integer(1)`) \cr
                                #' The number of instruments.
                                n_instr = function(value) {
                                  if (missing(value)) {
                                    return(length(self$z_cols))
                                  } else {
                                    stop("can't set field n_instr")
                                  }
                                },

                                #' @field n_obs (`integer(1)`) \cr
                                #' The number of observations.
                                n_obs = function(value) {
                                  if (missing(value)) {
                                    return(dim(self$data)[1])
                                  } else {
                                    stop("can't set field n_obs")
                                  }
                                },

                                #' @field n_treat (`integer(1)`) \cr
                                #' The number of treatment variables.
                                n_treat = function(value) {
                                  if (missing(value)) {
                                    return(length(self$d_cols))
                                  } else {
                                    stop("can't set field n_treat")
                                  }
                                },

                                #' @field treat_col (`character(1)`) \cr
                                #' "Active" treatment variable in the multiple-treatment case.
                                treat_col = function(value) {
                                  if (missing(value)) {
                                    return(private$treat_col_)
                                  } else {
                                    stop("can't set field treat_col")
                                  }
                                },

                                #' @field x_cols (`character()`) \cr
                                #' The covariates.
                                x_cols = function(value) {
                                  if (missing(value)) {
                                    return(private$x_cols_)
                                    #stop("Specify x_cols")
                                  } else {
                                    x_cols = value # to get more meaningful assert error messages
                                    reset_value = !is.null(self$data_model)
                                    if (!is.null(x_cols)) {
                                      assert_character(x_cols, unique = TRUE)
                                    }

                                    if (!is.null(x_cols)) {
                                      assert_subset(x_cols, self$all_variables)
                                      private$x_cols_ = x_cols
                                    }
                                    if (reset_value) {
                                      private$check_disjoint_sets()
                                      self$set_data_model(self$d_cols[1])
                                    }
                                  }
                                },

                                #' @field y_col (`character(1)`) \cr
                                #' The outcome variable.
                                y_col = function(value) {
                                  if (missing(value)) {
                                    return(private$y_col_)
                                  } else {
                                    y_col = value # to get more meaningful assert error messages
                                    reset_value = !is.null(self$data_model)
                                    assert_character(y_col, len = 1)
                                    assert_subset(y_col, self$all_variables)
                                    private$y_col_ = y_col
                                    if (reset_value) {
                                      private$check_disjoint_sets()
                                      self$set_data_model(self$d_cols[1])
                                    }
                                  }
                                },

                                #' @field z_cols (`NULL`, `character()`) \cr
                                #' The instrumental variables. Default is `NULL`.
                                z_cols = function(value) {
                                  if (missing(value)) {
                                    return(private$z_cols_)
                                  } else {
                                    z_cols = value # to get more meaningful assert error messages
                                    reset_value = !is.null(self$data_model)
                                    if (!is.null(z_cols)) {
                                      assert_character(z_cols, unique = TRUE)
                                    }
                                    assert_subset(z_cols, self$all_variables)
                                    private$z_cols_ = z_cols
                                    if (reset_value) {
                                      private$check_disjoint_sets()
                                      self$set_data_model(self$d_cols[1])
                                    }
                                  }
                                },

                                #' @field cluster_cols (`character()`)\cr
                                #' The cluster variable(s).
                                cluster_cols = function(value) {
                                  if (missing(value)) {
                                    return(private$cluster_cols_)
                                  } else {
                                    cluster_cols = value
                                    reset_value = !is.null(self$data_model)
                                    assert_character(cluster_cols, unique = TRUE)
                                    assert_subset(cluster_cols, self$all_variables)
                                    private$cluster_cols_ = cluster_cols
                                    if (reset_value) {
                                      private$check_disjoint_sets()
                                      self$set_data_model(self$d_cols[1])
                                    }
                                  }
                                },
                                #' @field n_cluster_vars (`integer(1)`) \cr
                                #' The number of cluster variables.
                                n_cluster_vars = function(value) {
                                  if (missing(value)) {
                                    return(length(self$cluster_cols))
                                  } else {
                                    stop("can't set field n_cluster_vars")
                                  }
                                }
                              ),
                              public = list(
                                #' @description
                                #' Creates a new instance of this [R6][R6::R6Class] class.
                                #'
                                #' @param data ([`data.table`][data.table::data.table()], `data.frame()`)\cr
                                #' Data object.
                                #'
                                #' @param y_col (`character(1)`) \cr
                                #' The outcome variable.
                                #'
                                #' @param d_cols (`character(1)`) \cr
                                #' The treatment variable.
                                #'
                                #' @param cluster_cols (`character()`) \cr
                                #' The cluster variable(s).
                                #'
                                #' @param x_cols (`character()`) \cr
                                #'
                                #' @param z_cols (`NULL`, `character()`) \cr
                                #' The instrumental variables. Default is `NULL`.
                                initialize = function(data = NULL,
                                                      x_cols = NULL,
                                                      y_col = NULL,
                                                      d_cols = NULL,
                                                      z_cols = NULL,
                                                      cluster_cols = NULL)
                                  {

                                  # we need to set cluster_cols (needs _data) before call to the super class
                                  # initialize because of the x_cols active binding

                                  if (all(class(data) == "data.frame")) {
                                    data = data.table(data)
                                  }
                                  assert_class(data, "data.table")
                                  assert_character(names(data), unique = TRUE)

                                  private$data_ = data

                                  self$cluster_cols = cluster_cols

                                  self$y_col  = y_col
                                  self$d_cols = d_cols
                                  self$z_cols = z_cols
                                  self$x_cols = x_cols
                                  private$check_disjoint_sets()

                                  # by default, we initialize to the first treatment variable
                                  self$set_data_model(d_cols[1])

                                  invisible(self)
                                },

                                #' @description
                                #' Print `dml_approx_data` objects.
                                print = function() {
                                  header = "================= DML-FE Object ==================\n"
                                  data_info = paste0(
                                    "Outcome variable: ", self$y_col, "\n",
                                    "Treatment variable(s): ", paste0(self$d_cols, collapse = ", "), "\n",
                                    "Cluster variable(s): ", paste0(self$cluster_cols, collapse = ", "),
                                    "\n",
                                    "Covariates: ", paste0(self$x_cols, collapse = ", "), "\n",
                                    "Instrument(s): ", paste0(self$z_cols, collapse = ", "), "\n",
                                    "No. Observations: ", self$n_obs, "\n")
                                  cat(header, "\n",
                                      "\n------------------ Data summary ------------------\n",
                                      data_info,
                                      sep = "")

                                  invisible(self)
                                },

                                #' @description
                                #' Setter function for `data_model`. The function implements the causal model
                                #' as specified by the user via `y_col`, `d_cols`, `x_cols`, `z_cols` and
                                #' `cluster_cols` and assigns the role for the treatment variables in the
                                #' multiple-treatment case.
                                #' @param treatment_var (`character()`)\cr
                                #' Active treatment variable that will be set to `treat_col`.
                                set_data_model = function(treatment_var) {

                                  assert_character(treatment_var, max.len = 1)
                                  assert_subset(treatment_var, self$d_cols)

                                  private$treat_col_ = treatment_var

                                  if (self$n_treat > 1) {
                                    stop("Specify one treatment variable at a time.")
                                  }

                                  #super$set_data_model(treatment_var) #############

                                  # add the cluster_cols to the data_model_
                                  col_indx = c(
                                    self$x_cols,
                                    self$y_col,
                                    self$treat_col,
                                    self$xbar_cols,
                                    self$z_cols,
                                    self$cluster_cols)
                                  private$data_model_ = self$data[, col_indx, with = FALSE]
                                  stopifnot(nrow(self$data) == nrow(self$data_model))

                                  invisible(self)
                                }
                              ),
                              private = list(
                                data_ = NULL,
                                x_cols_ = NULL,
                                y_col_ = NULL,
                                d_cols_ = NULL,
                                z_cols_ = NULL,
                                treat_col_ = NULL,
                                cluster_cols_ = NULL,
                                data_model_ = NULL,
                                check_disjoint_sets = function() {

                                 # super$check_disjoint_sets()

                                  y_col     = self$y_col
                                  x_cols    = self$x_cols
                                  d_cols    = self$d_cols

                                  cluster_cols = self$cluster_cols


                                  if (y_col %in% x_cols) {
                                    stop(paste(
                                      y_col,
                                      "cannot be set as outcome variable 'y_col' and",
                                      "covariate in 'x_cols'."))
                                  }
                                  if (y_col %in% d_cols) {
                                    stop(paste(
                                      y_col,
                                      "cannot be set as outcome variable 'y_col' and",
                                      "treatment variable in 'd_cols'."))
                                  }
                                  if (y_col %in% cluster_cols) {
                                    stop(paste(
                                      y_col,
                                      "cannot be set as outcome variable 'y_col' and",
                                      "cluster variable in 'cluster_cols'."))
                                  }
                                  if (any(d_cols %in% x_cols)) {
                                    stop(paste(
                                      "At least one variable/column is set as treatment",
                                      "variable ('d_cols') and as a covariate ('x_cols').",
                                      "Consider using parameter 'use_other_treat_as_covariate'."))
                                  }
                                  if (any(d_cols %in% cluster_cols)) {
                                    stop(paste(
                                      "At least one variable/column is set as treatment",
                                      "variable ('d_cols') and as a cluster variable ('cluster_cols')."))
                                  }
                                  if (any(cbind(x_cols) %in% cluster_cols)) {
                                    stop(paste(
                                      "At least one variable/column is set as covariate ('x_cols')",
                                      "and as a cluster variable ('cluster_cols')."))
                                  }
                                  if (!is.null(self$z_cols)) {
                                    z_cols = self$z_cols

                                    if (y_col %in% z_cols) {
                                      stop(paste(
                                        y_col,
                                        "cannot be set as outcome variable 'y_col' and",
                                        "instrumental variable in 'z_cols'."))
                                    }
                                    if (any(z_cols %in% d_cols)) {
                                      stop(paste(
                                        "At least one variable/column is set as treatment",
                                        "variable ('d_cols') and instrumental variable in 'z_cols'."))
                                    }
                                    if (any(z_cols %in% x_cols)) {
                                      stop(paste(
                                        "At least one variable/column is set as covariate ('x_cols')",
                                        "and instrumental variable in 'z_cols'."))
                                    }
                                    if (any(z_cols %in% cluster_cols)) {
                                      stop(paste(
                                        "At least one variable/column is set as instrumental variable",
                                        "('z_cols') and as a cluster variable ('cluster_cols')."))
                                    }
                                  }
                                }
                              )
)

#' @title Wrapper for Double machine learning data-backend initialization from
#' data.frame.
#'
#' @description
#' Initalization of DoubleMLData from `data.frame`.
#'
#' @param df (`data.frame()`)\cr
#' Data object.
#'
#' @param y_col (`character(1)`) \cr
#' The outcome variable.
#'
#' @param d_cols (`character()`) \cr
#' The treatment variable(s).
#'
#' @param x_cols (`character()`) \cr
#' The covariates.
#'
#' @param z_cols (`NULL`, `character()`) \cr
#' The instrumental variables. Default is `NULL`.
#'
#' @param cluster_cols (`NULL`, `character()`) \cr
#' The cluster variables. Default is `NULL`.
#'
#' @return Creates a new instance of class `dml_approx_data`.
#'
#' @export
dml_approx_data_from_data_frame = function(df,
                               x_cols = NULL, y_col = NULL, d_cols = NULL,
                               z_cols = NULL, cluster_cols = NULL)
  {
  if (is.null(cluster_cols)) {
    stop(print("Specify at least one (`cluster_vars`)."))
  } else {
    data = dml_approx_data$new(df,
                           x_cols = x_cols,
                           y_col = y_col,
                           d_cols = d_cols,
                           z_cols = z_cols,
                           cluster_cols = cluster_cols)
  }
  return(data)
}

