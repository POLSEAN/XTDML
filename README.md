# XTDML
This package implements double machine learning (DML) for static partial linear regression (PLR) model with fixed effects in [Clarke and Polselli (2023)](https://arxiv.org/abs/2312.08174).

The package allows for the choice of three approaches for handling unobserved individual heterogeneity:
1. Correlated random effect estimator (Mundlak, 1978)
2. Approximation approach that requires to transform the variables (i.e, time-demeaning or first-differencing)
3. Exact or hybrid approach that uses Mundlak's device to learn the nuisance parameters and than transforms the variables and nuisance predictions


