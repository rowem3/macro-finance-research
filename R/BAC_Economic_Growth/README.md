# BAC @ MC 2025 — Economic Growth Panel Regression

Panel data regression on OECD country-level data combined with Heritage Foundation Economic Freedom Index scores to model GDP growth.

**Data:** OECD macroeconomic indicators + Heritage Foundation freedom scores, year-over-year transformed

**Methods:**
- Fixed Effects (within) model
- Random Effects (GLS) model
- Hausman test to select between FE and RE specifications

**Key regressors:** migration, FDI, labor force participation, trade freedom, fiscal health, monetary freedom, government spending

**Packages:** `plm`, `car`, `dplyr`, `readr`
