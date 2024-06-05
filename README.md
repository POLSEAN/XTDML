# XTDML
The `XTDML` package implements double machine learning (DML) for static partially linear regression (PLR) models with fixed effects as in [Clarke and Polselli (2023)](https://arxiv.org/abs/2312.08174). The package buids on `DoubleML` package by Chernozhukov et al. (2018) and uses the `mlr3` ecosystem.

The package allows for the choice of three approaches for handling the unobserved individual heterogeneity:
  1. Mundlak (1978)'s device or **correlated random effects** (CRE).
  2. The **approximation approach** requires that the user *transforms* the data with the within-group (wg) or first-difference  (fd)  transformation *beforehand*.
  3. The **hybrid approach** uses *original data* and requires that the user specifies the transformation (wg or fd; default ```model = "wg"```).

> [!WARNING]
> Modifications to the `DoubleML` package will be soon available 

## Installing the package from GitHub
The current version can be installed via devtools:
```
library(devtools)
install_github("POLSEAN/XTDML")
```
## Sample code
Below we report an illustrative example of the use of the code with the proposed approaches. Simulation data is generated following DGP3 in [Clarke and Polselli (2023)](https://arxiv.org/abs/2312.08174) and is uploaded in the folder `/data`.

```
# load sample of simulated data 
df <- read.csv("https://raw.githubusercontent.com/POLSEAN/XTDML/main/data/dgp4_cre_short.csv")
```

### Example for CRE
```
# set up data
x_cols <- paste0("x", 1:30)
xbar_cols <- paste0("m_x", 1:30)

# set up data for DML procedure
obj_dml_data = dml_cre_data_from_data_frame(df,
                            x_cols = x_cols,  y_col = "y", d_cols = "d",
                            xbar_cols = xbar_cols, dbar_cols = "m_d",                                                 
                            cluster_cols = "id")

# lasso w/t dictionary
learner = lrn("regr.cv_glmnet", s="lambda.min")
ml_m = learner$clone()
ml_l = learner$clone()

ml_mbar = learner$clone()
ml_lbar = learner$clone()

# estimation with CRE with non-separable model
dml_obj = dml_cre_plr$new(obj_dml_data, ml_l = ml_l, ml_m = ml_m,
                          ml_lbar = ml_lbar, ml_mbar = ml_mbar,
                          score="orth-PO", model = "non-separable")
dml_obj$fit()
dml_obj$print()
```

### Example for approximation approach
```
# load data
df = read.csv("https://raw.githubusercontent.com/POLSEAN/XTDML/main/data/dgp4_cre_short.csv")

# _________________________________________________________________________________________ #
## IMPORTANT: TRANSFORM DATA BEFOREHAND! (example with wg transformation)
# _________________________________________________________________________________________ #
# below the code for within-group transformation
X = paste0("x", 1:30)
y = df$y
d = df$d

data$y = df$y
data$d = df$d

# grand-mean
df_gm = data %>%
  mutate(across(c(X, y, d), ~ mean(.x)))
gmX_list = as.list(select(df_gm[1,], starts_with(c("X", "y", "d"))))

# indivifual mean
df_m = df %>%
  group_by(id) %>%
  mutate(across(c(X, y, d), ~  mean(.x)))

mX_list = as.list(select(df_m, starts_with(c("X", "y", "d"))))
mX_list = mX_list[-1]

# within-group transformation
df_dm = data %>%
  mutate(across(all_of(names(gmX_list)), ~ .x - mX_list[[cur_column()]] + gmX_list[[cur_column()]]))

df_dm <- add_column(df_dm,  id, time, .before = 1)
df2 = as.data.frame(df_dm)
# _________________________________________________________________________________________ #

# set up DML procedure
obj_dml_data = dml_approx_data_from_data_frame(df2,
                            x_cols = x_cols,  y_col = "y", d_cols = "d",
                            cluster_cols = "id")

# lasso w/t dictionary
learner = lrn("regr.cv_glmnet", s="lambda.min")
ml_m = learner$clone()
ml_l = learner$clone()

# estimation with within-group transformation
dml_obj = dml_approx_plr$new(obj_dml_data, ml_l = ml_l, ml_m = ml_m)

dml_obj$fit()
dml_obj$print()
```

### Example for hybrid approach
```
# load data
df = read.csv("https://raw.githubusercontent.com/POLSEAN/XTDML/main/data/dgp4_cre_short.csv")

# set up data
x_cols <- paste0("x", 1:30)
xbar_cols <- paste0("m_x", 1:30)

# set up data for DML procedure
obj_dml_data = dml_hybrid_data_from_data_frame(df,
                            x_cols = x_cols,  y_col = "y", d_cols = "d",
                            xbar_cols = xbar_cols, dbar_cols = "m_d",                                                 
                            cluster_cols = "id")

# lasso w/t dictionary
learner = lrn("regr.cv_glmnet", s="lambda.min")
ml_m = learner$clone()
ml_l = learner$clone()

ml_mbar = learner$clone()
ml_lbar = learner$clone()

# estimation with within-group transformation
dml_obj = dml_hybrid_plr$new(obj_dml_data, ml_l = ml_l, ml_m = ml_m,
                          ml_lbar = ml_lbar, ml_mbar = ml_mbar,
                          score="orth-PO", model = "wg")
dml_obj$fit()
dml_obj$print()
```
## Hyperparameter tuning
Example of code for CRE to tune regression random forest (RF) as in Table 1 in [Clarke and Polselli (2023)](https://arxiv.org/abs/2312.08174).

```
# set up data
x_cols <- paste0("x", 1:30)
xbar_cols <- paste0("m_x", 1:30)

# set up data for DML procedure
obj_dml_data = dml_cre_data_from_data_frame(df,
                         x_cols = x_cols,  y_col = "y", d_cols = "d",
                         xbar_cols = xbar_cols, dbar_cols = "m_d",
                         cluster_cols = "id")

vars = c(x_cols,xbar_cols)
p = length(vars)
p1 = length(xbar_cols)

learner = lrn("regr.ranger", mtry = p, importance = "impurity")
ml_m = learner$clone()
ml_l = learner$clone()

learner2 = lrn("regr.ranger", mtry = p1, importance = "impurity")
ml_mbar = learner2$clone()
ml_lbar = learner2$clone()


dml_obj = dml_cre_plr$new(obj_dml_data, ml_l = ml_l, ml_m = ml_m,
                              ml_lbar = ml_lbar, ml_mbar = ml_mbar,
                              score="orth-PO", model = "non-separable")

## Hyperparameter tuning for RF
n.max = ceiling(n/2)
param_grid = list(
  "ml_l" = paradox::ParamSet$new(list(
    paradox::ParamInt$new("num.trees", lower = 5, upper = 100),
    paradox::ParamInt$new("min.node.size", lower = 5, upper = n.max),
    paradox::ParamInt$new("max.depth", lower = 1, upper = 10))),
  "ml_m" = paradox::ParamSet$new(list(
    paradox::ParamInt$new("num.trees", lower = 5, upper = 100),
    paradox::ParamInt$new("min.node.size", lower = 5, upper = n.max),
    paradox::ParamInt$new("max.depth", lower = 1, upper = 10))),
  "ml_lbar" = paradox::ParamSet$new(list(
    paradox::ParamInt$new("num.trees", lower = 5, upper = 100),
    paradox::ParamInt$new("min.node.size", lower = 5, upper = n.max),
    paradox::ParamInt$new("max.depth", lower = 1, upper = 10))),
  "ml_mbar" = paradox::ParamSet$new(list(
    paradox::ParamInt$new("num.trees", lower = 5, upper = 100),
    paradox::ParamInt$new("min.node.size", lower = 5, upper = n.max),
    paradox::ParamInt$new("max.depth", lower = 1, upper = 10))))

# Tuning settings
tune_settings = list(
  terminator = mlr3tuning::trm("evals", n_evals = 5),
  algorithm = mlr3tuning::tnr("grid_search", resolution = 5)
)
# Start tuning
dml_obj$tune(param_set = param_grid, tune_settings = tune_settings)
print(dml_obj$params)
print(dml_obj$tune_settings$measure)

# Fit DML
dml_obj$fit()
dml_obj$print()
```

## References
1. Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen, C., Newey, W., and Robins, J. (2018). Double/debiased machine learning for treatment and structural parameters. *The Econometrics Journal*, 21(1):C1–C68.
2. Clarke, P. and Polselli, A. (2023). Double machine learning for static panel models with fixed effects. *arXiv preprint arXiv:2312.08174*.
3. Mundlak, Y. (1978). On the pooling of time series and cross section data. *Econometrica*, pages 69–85.
