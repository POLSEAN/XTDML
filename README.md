# XTDML
The `XTDML` package implements double machine learning (DML) for static partially linear regression (PLR) models with fixed effects as in [Clarke and Polselli (2023)](https://arxiv.org/abs/2312.08174). The package heavily builds on `DoubleML` package by Bach et al. (2022) using the `mlr3` ecosystem.

The package allows for the choice of three approaches for handling the unobserved individual heterogeneity:
  1. Mundlak (1978)'s device or **correlated random effects** (CRE).
  2. The **approximation approach** requires that the user *transforms* the data with the within-group (wg) or first-difference  (fd)  transformation *beforehand*.
  3. The **hybrid approach** uses *original data* and requires that the user specifies the transformation (wg or fd; default ```model = "wg"```).

> [!WARNING]
> New version of `XTDML` package contains Model RMSE and RMSE of the nuisance parameters. We are currently working on `XTDML` for IV estimation (soon available), where tests for weak IV will be made available.

## Installing the package from GitHub
The current version can be installed via devtools:
```
library(devtools)
install_github("POLSEAN/XTDML")
```
## Sample code
Sample code is provided in the folder `./examples`

1. `01_xtdml_for_cre.ipjnb` shows how to use the CRE approach in DML
2. `02_xtdml_for_wg_approx.ipjnb` shows how to use the WG (approximation) approach in DML
3. `03_xtdml_for_fd_exact.ipjnb` shows how to use the FD (exact) approach in DML

## References
Bach, P., Chernozhukov, V., Kurz, M. S., Spindler, M. and Klaassen, S. (2024), DoubleML - An Object-Oriented Implementation of Double Machine Learning in R, *Journal of Statistical Software*, 108(3): 1-56, doi:10.18637/jss.v108.i03, arXiv:2103.09603.

Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen, C., Newey, W., and Robins, J. (2018). Double/debiased machine learning for treatment and structural parameters. *The Econometrics Journal*, 21(1):C1–C68.

Clarke, P. and Polselli, A. (2023). Double machine learning for static panel models with fixed effects. *arXiv preprint arXiv:2312.08174*. (forthcoming at *The Econometrics Journal*)

Mundlak, Y. (1978). On the pooling of time series and cross section data. *Econometrica*, pages 69–85.


