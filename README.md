## Double Machine Learning for panel data models with fixed effects
Machine Learning (ML) algorithms are powerful data-driven tools for approximating highdimensional
or non-linear nuisance functions which are useful in practice because the true
functional form of the predictors is ex-ante unknown. In this paper, we develop estimators
of policy interventions from panel data which allow for non-linear effects of the confounding
regressors, and investigate the performance of these estimators using three well-known ML
algorithms, specifically, LASSO, classification and regression trees, and random forests. We
use Double Machine Learning (DML) (Chernozhukov et al., 2018) for the estimation of causal
effects of homogeneous treatments with unobserved individual heterogeneity (fixed effects)
and no unobserved confounding by extending Robinson (1988)’s partially linear regression
model. We develop three alternative approaches for handling unobserved individual heterogeneity
based on extending the within-group estimator, first-difference estimator, and correlated
random effect estimator (Mundlak, 1978) for non-linear models. Using Monte Carlo simulations,
we find that conventional least squares estimators can perform well even if the data generating
process is non-linear, but there are substantial performance gains in terms of bias reduction under
a process where the true effect of the regressors is non-linear and discontinuous. However,
for the same scenarios, we also find – despite extensive hyperparameter tuning – inference to
be problematic for both tree-based learners because these lead to highly non-normal estimator
distributions and the estimator variance being severely under-estimated. This contradicts
the performance of trees in other circumstances and requires further investigation. Finally, we
provide an illustrative example of DML for observational panel data showing the impact of the
introduction of the national minimum wage in the UK.

## R package
`XTDML' implements DML for panel data models with fixed effects.
