# xtdml

**Double Machine Learning for Static Panel Models with Fixed Effects**

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/xtdml)](https://CRAN.R-project.org/package=xtdml)
[![License: GPL v2](https://img.shields.io/badge/License-GPL_v2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html)
[![R >= 3.5.0](https://img.shields.io/badge/R-%3E%3D%203.5.0-blue.svg)](https://www.r-project.org/)
<!-- badges: end -->

The `xtdml` package implements **double machine learning (DML)** for static partially linear regression models for **panel data with fixed effects**, following the methodology of Clarke and Polselli (2026), [*The Econometrics Journal*](https://academic.oup.com/ectj/advance-article/doi/10.1093/ectj/utaf011/8120202).

It is built on top of the [`DoubleML`](https://docs.doubleml.org/r/stable/) package (Bach et al., 2024) and uses the [`mlr3`](https://mlr3.mlr-org.com/) ecosystem for nuisance-function estimation, keeping the same R6-style API and similar notation.

## The Partially Linear Panel Regression Model

`xtdml` estimates the structural (causal) parameter from panel data models of the form

$$
Y_{it} = \theta_0\, D_{it} + g_0(X_{it}) + \alpha_i + U_{it},
$$

$$
D_{it} = m_0(X_{it}) + \gamma_i + V_{it},
$$

where

- $Y_{it}$ is the outcome, $D_{it}$ the treatment, $X_{it}$ the (possibly high-dimensional) covariates;
- $\theta_0$ is the structural (causal) parameter to **estimate**;
- $g_0$ and $m_0$ are (possibly nonlinear) nuisance functions to **learn** from the data using one of the panel-data approaches described below;
- $\alpha_i$ and $\gamma_i$ are unobserved individual heterogeneities, potentially correlated with the covariates;
- $U_{it}$ and $V_{it}$ are disturbances.

### What the user controls

The user can run the estimator directly on a panel `data.frame`; *no manual* data preparation is required. The two main choices are:

1. **Panel-data approach:**  `approach = c("fd-exact", "wg-approx", "cre")`, default `"fd-exact"`. `xtdml` transforms the data internally according to the selected approach, following Clarke and Polselli (2025).
2. **Covariate transformation:**  `transformX = c("no", "minmax", "poly")`, default `"no"`.
   - `"no"` leaves $X$ untransformed — recommended for tree-based learners.
   - `"minmax"` applies min–max normalization $x' = (x - x_{\min})/(x_{\max} - x_{\min})$ — recommended for neural networks.
   - `"poly"` adds polynomials up to order three and pairwise/three-way interactions — recommended for Lasso.

> [!NOTE]
> Two **orthogonal scores** are available via the `score` argument of `xtdml_plr$new()`:
> `"orth-PO"` (partialling-out, requires `ml_l` and `ml_m`) and `"orth-IV"` (IV-type orthogonal score, additionally requires `ml_g`).

> [!IMPORTANT]
> **Changes in recent versions**
> - **New `plot()` and `predict()` methods** on `xtdml_plr` objects for diagnostics and obtaining fitted values / residuals.
> - The package has been renamed from `XTDML` to `xtdml`.
> - Since version 0.1.7, `xtdml_data_from_data_frame()` has received major updates and various bug fixes.
> - `tune_settings` have been updated for compatibility with `mlr3tuning (>= 1.5.0)` and `bbotk (>= 1.8.0)`.
> - The **hybrid approach** is no longer available, in line with the published version of the article.
> - Earlier versions allowed for **endogenous treatment**; that option has been removed from `xtdml`. IV estimation and weak-IV tests for panel DML are being developed in a companion package `xtivdml` in [https://github.com/POLSEAN/xtivdml](https://github.com/POLSEAN/xtivdml/tree/main).

## Installation

Install the stable version from CRAN:

```r
install.packages("xtdml")
```

Or install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("POLSEAN/xtdml")
```

## Quick start

A minimal end-to-end example using gradient boosting (`xgboost`) as the nuisance learner, looping over the three panel approaches and both orthogonal scores:

```r
library(xtdml)
library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(paradox)

# Silence mlr3 / bbotk chatter
lgr::get_logger("mlr3")$set_threshold("warn")
lgr::get_logger("bbotk")$set_threshold("warn")

# 1. Simulate panel data
set.seed(1234)
df <- make_plpr_data(n_obs = 1000, t_per = 10, dim_x = 20,
                     theta = 0.5, rho = 0.8)

x_cols     <- paste0("X", 1:20)
approaches <- c("fd-exact", "cre", "wg-approx")
scores     <- c("orth-PO", "orth-IV")

for (approach in approaches) {

  # 2. Build the xtdml data backend
  obj_data <- xtdml_data_from_data_frame(
    df,
    x_cols     = x_cols,
    y_col      = "y",
    d_cols     = "d",
    panel_id   = "id",
    time_id    = "time",
    approach   = approach,
    transformX = "no"
  )

  # 3. Learners
  learner <- lrn("regr.xgboost", nrounds = 100)
  ml_l    <- learner$clone()
  ml_m    <- learner$clone()

  for (score in scores) {
    if (score == "orth-PO") {

     # 4. Set DML estimation environment
      xtdml_obj <- xtdml_plr$new(obj_data,
                                 ml_l = ml_l, ml_m = ml_m,
                                 n_folds = 5, score = score)
      param_grid <- list(
        "ml_l" = ps(max_depth = p_int(2, 10), lambda = p_dbl(0, 2)),
        "ml_m" = ps(max_depth = p_int(2, 10), lambda = p_dbl(0, 2))
      )
    } else {  # "orth-IV"
      ml_g <- learner$clone()
      xtdml_obj <- xtdml_plr$new(obj_data,
                                 ml_l = ml_l, ml_m = ml_m, ml_g = ml_g,
                                 n_folds = 5, score = score)
      param_grid <- list(
        "ml_l" = ps(max_depth = p_int(2, 10), lambda = p_dbl(0, 2)),
        "ml_m" = ps(max_depth = p_int(2, 10), lambda = p_dbl(0, 2)),
        "ml_g" = ps(max_depth = p_int(2, 10), lambda = p_dbl(0, 2))
      )
    }

    # 5. Tune and fit
     tune_settings = list(n_folds_tune = 5, 
                  rsmp_tune  = mlr3::rsmp("cv", folds = 5),
                  terminator = mlr3tuning::trm("evals", n_evals = 10),
                  tuner      = tnr("grid_search", resolution = 10))

    xtdml_obj$tune(param_set = param_grid, tune_settings = tune_settings)
    xtdml_obj$fit()

   # 6. Print estimated objects 
    xtdml_obj$print()
    xtdml_obj$summary()
  }
}
```

### Inspecting the results

After fitting, the main quantities are available as fields and methods on the `xtdml_plr` object:

| Field / method | Meaning |
|---|---|
| `coef_theta` | Point estimate of the structural parameter $\theta_0$ |
| `se_theta` | Standard error of $\hat\theta$ |
| `pval_theta` | Two-sided $p$-value |
| `confint()` | Confidence interval (default 95%) |
| `model_rmse` | Overall model RMSE |
| `rmses` | RMSE of each nuisance learner (`ml_l`, `ml_m`, `ml_g`) |
| `params` | Tuned hyperparameters |
| `get_panel_info()` | **New.** Stored panel data information (`n_obs`, `n_subjects`, `n_groups`) |
| `predict()` | **New.** Computes predicted outcomes for specified values of the treatment variable, based on the estimated causal parameter. The predictions use the residualized outcome and treatment variables obtained from the DML estimation. This method is most useful for counterfactual analysis or diagnostic purposes, rather than for evaluating predictive performance in the usual machine learning sense.|
| `plot()` | **New.** Produces diagnostic graphs for each nuisance parameter (`ml_l` and `ml_m` for `score = "orth-PO"`;  `ml_g` and `ml_m` for `score = "orth-IV"`). Each graph displays four plots: residuals versus fitted values (top-left), residuals versus targets (top-right), fitted values versus targets (bottom-left), and a normal Q-Q plot of the residuals (bottom-right). This tool allows to assess the quality of the learning tasks of the nuisance parameters visually. |

```r
# After xtdml_obj$fit():

# Diagnostic plots (residuals, nuisance fits, theta-hat distribution)
xtdml_obj$plot()

# to customise the plots:
xtdml_obj$plot(title = "Lasso DML diagnostic plots", ask = FALSE, col = "steelblue", cex = 0.7)

# Fitted values and residuals from the DML fit
predict = xtdml_obj$predict(d = c(0, 1.5, 2))
head(predict)

```

## Main functions

| Function | Purpose |
|---|---|
| `xtdml_plr` | R6 class implementing DML for partially linear panel models with fixed effects. |
| `xtdml_data` | Constructor for the data backend used by the estimator. |
| `xtdml_data_from_data_frame()` | Wrapper that builds an `xtdml_data` object directly from a `data.frame`. |
| `make_plpr_data()` | Simulates panel data from a partially linear regression model with fixed effects as DGP3 in Clarke and Polselli (2026). |

Full documentation is available via `?xtdml_plr`, `?xtdml_data_from_data_frame`, etc.

## Dependencies

`xtdml` requires `R (>= 3.5.0)` and imports `R6 (>= 2.4.1)`, `data.table (>= 1.12.8)`, `mlr3 (>= 1.3.0)`, `mlr3tuning (>= 1.5.0)`, `mlr3learners (>= 0.13.0)`, `mlr3misc (>= 0.19.0)`, `mvtnorm`, `clusterGeneration`, `readstata13`, `magrittr`, `dplyr`, `MLmetrics`, `checkmate`, and `rlang`. Suggested: `rpart`, `mlr3pipelines`, `bbotk (>= 1.8.0)`.

## Citation

If you use `xtdml` in academic work, please cite both the methodological paper and the package:

```bibtex
@article{clarke2026double,
  title={Double machine learning for static panel models with fixed effects},
  author={Clarke, Paul S and Polselli, Annalivia},
  journal={Econometrics Journal},
  volume={29},
  number={1},
  pages={69--86},
  year={2026},
  publisher={Oxford University Press}
}

@article{polselli2025xtdml,
  title={xtdml: Double Machine Learning Estimation to Static Panel Data Models with Fixed Effects in R},
  author={Polselli, Annalivia},
  journal={arXiv preprint arXiv:2512.15965},
  year={2025}
}

@manual{polselli2026xtdml,
  title  = {xtdml: Double Machine Learning for Static Panel Models with Fixed Effects},
  author = {Polselli, Annalivia},
  year   = {2026},
  note   = {R package version 0.1.12},
  url    = {https://CRAN.R-project.org/package=xtdml}
}
```

You can also generate an up-to-date entry from R with `citation("xtdml")`.

## References

- Bach, P., Chernozhukov, V., Kurz, M. S., Spindler, M., and Klaassen, S. (2024). DoubleML — An Object-Oriented Implementation of Double Machine Learning in R. *Journal of Statistical Software*, 108(3), 1–56. [doi:10.18637/jss.v108.i03](https://doi.org/10.18637/jss.v108.i03)
- Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen, C., Newey, W., and Robins, J. (2018). Double/debiased machine learning for treatment and structural parameters. *The Econometrics Journal*, 21(1), C1–C68.
- Clarke S. P., and Polselli A., (2026). Double machine learning for static panel models with fixed effects, The Econometrics Journal, Volume 29, Issue 1, January 2026, Pages 69–86. <https://doi.org/10.1093/ectj/utaf011>
- Polselli, A. (2025). `xtdml`: Double Machine Learning Estimation to Static Panel Data Models with Fixed Effects in R, (2025). <https://arxiv.org/abs/2512.15965> 
- Polselli, A. (2026). **xtdml**: Double Machine Learning for Static Panel Models with Fixed Effects. R package version 0.1.12. <https://CRAN.R-project.org/package=xtdml>

