# XTDML
The `XTDML` package implements double machine learning (DML) for static partially linear regression (PLR) models with fixed effects as in [Clarke and Polselli (2023)](https://arxiv.org/abs/2312.08174).
The package buids on 'DoubleML' by Chernozhukov et al. (2018).

The package allows for the choice of three approaches for handling unobserved individual heterogeneity:
1. Mundlak (1978)'s device or correlated random effects (CRE)
2. Approximation approach that requires to transform the variables (i.e, time-demeaning or first-differencing)
3. Hybrid approach that uses Mundlak's device to learn the nuisance parameters and than transforms the variables and nuisance predictions

The current version can be installed via devtools:
```
library(devtools)
install_github("POLSEAN/XTDML")
```

Simulation data generated following DGP3 in [Clarke and Polselli (2023)](https://arxiv.org/abs/2312.08174) can be found in the folder /data.

## Sample code
```
dta = read.csv("data/dgp4_cre_short.csv")
# set up data
x_cols <- paste0("x", 1:30)
xbar_cols <- paste0("m_x", 1:30)
```
### Example for CRE

### Example for Approximation

### Example for Hybrid

## References
Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen, C., Newey, W., and Robins, J. (2018). Double/debiased machine learning for treatment and structural parameters. *The Econometrics Journal*, 21(1):C1–C68.
Clarke, P. and Polselli, A. (2023). Double machine learning for static panel models with fixed effects. *arXiv preprint arXiv:2312.08174*.
Mundlak, Y. (1978). On the pooling of time series and cross section data. *Econometrica*, pages 69–85.
