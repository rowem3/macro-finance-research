# ECN 481 — Banking Deserts (Senior Thesis)

Probit regression analysis examining what demographic and socioeconomic factors predict whether a U.S. census tract qualifies as a "banking desert" (no physical bank branch within a defined radius).

**Data:** FDIC branch data merged with 2019 ACS Census estimates (~74,000 tracts)

**Methods:**
- Baseline probit with classical standard errors
- State fixed-effects probit
- Cluster-robust standard errors by state (sandwich estimator)
- Average marginal effects via `margins`
- McFadden pseudo-R² for model comparison

**Key regressors:** broadband access, HDI, racial composition, population density

**Packages:** `tidycensus`, `sf`, `margins`, `sandwich`, `pscl`
