# ============================================================
#  ECN 481  •  Banking-Deserts Probit Analysis  •  All-in-One
#  (Baseline model + State Fixed Effects + Cluster-Robust SEs)
# ============================================================



# --------------------------- Libraries -----------------------
required_pkgs <- c(
  "tidycensus", "sf", "dplyr", "stringr", "readr",
  "corrplot", "car", "margins", "lmtest", "sandwich", "pscl"
)
new_pkgs <- setdiff(required_pkgs, installed.packages()[, "Package"])
if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)

library(tidycensus)
library(sf)            # geometry handling
library(dplyr)
library(stringr)
library(readr)
library(corrplot)
library(car)           # VIF
library(margins)       # marginal effects
library(lmtest)        # coeftest, bptest
library(sandwich)      # vcovHC / vcovCL
library(pscl)          # pR2()

# ------------------------ Census API Key ---------------------
# Run **once** per machine. Skip if already installed.
# census_api_key("YOUR-KEY-HERE", install = TRUE, overwrite = TRUE)
# readRenviron("~/.Renviron")   # load key in current session

# ------------------------- Load Data -------------------------
Combined_banking_data <- read_csv(
  "Data.R/Cleaned_Banking_Deserts_Data_File_2019.csv",
  col_types = cols(GEOID = col_character())
)

# Zero-pad GEOID to 11 characters
Combined_banking_data <- Combined_banking_data |>
  mutate(GEOID = str_pad(GEOID, 11, "left", "0"))

# --------------- Fetch & Merge Census Pop Density ------------
states <- unique(substr(Combined_banking_data$GEOID, 1, 2))

tract_pop_list <- lapply(states, \(st) {
  get_acs(
    geography = "tract",
    variables = c(pop = "B01003_001"),
    year      = 2019,
    state     = st,
    geometry  = TRUE,
    cache     = TRUE
  ) |>
    mutate(
      area_km2    = as.numeric(st_area(geometry)) / 1e6,
      pop_density = estimate / area_km2
    ) |>
    st_drop_geometry() |>
    select(GEOID, pop_density)
})

pop_density_df <- bind_rows(tract_pop_list)

Combined_banking_data <- Combined_banking_data |>
  left_join(pop_density_df, by = "GEOID")

# ---------------- Population Density Descriptives ----------------
cat("\n--- Population Density Summary ---\n")
pop_stats <- Combined_banking_data |>
  summarize(
    Mean    = mean(pop_density, na.rm = TRUE),
    Std_Dev = sd(pop_density,   na.rm = TRUE),
    Median  = median(pop_density, na.rm = TRUE),
    Min     = min(pop_density,   na.rm = TRUE),
    Max     = max(pop_density,   na.rm = TRUE)
  )
print(pop_stats)

# ============================================================
#  ECN 481  •  Banking-Deserts Probit Analysis  •  All-in-One
#  (Baseline model + State Fixed Effects + Cluster-Robust SEs)
# ============================================================



# --------------------------- Libraries -----------------------
required_pkgs <- c(
  "tidycensus", "sf", "dplyr", "stringr", "readr",
  "corrplot", "car", "margins", "lmtest", "sandwich", "pscl"
)
new_pkgs <- setdiff(required_pkgs, installed.packages()[, "Package"])
if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)

library(tidycensus)
library(sf)            # geometry handling
library(dplyr)
library(stringr)
library(readr)
library(corrplot)
library(car)           # VIF
library(margins)       # marginal effects
library(lmtest)        # coeftest, bptest
library(sandwich)      # vcovHC / vcovCL
library(pscl)          # pR2()

# ------------------------ Census API Key ---------------------
# Run **once** per machine. Skip if already installed.
# census_api_key("YOUR-KEY-HERE", install = TRUE, overwrite = TRUE)
# readRenviron("~/.Renviron")   # load key in current session

# ------------------------- Load Data -------------------------
Combined_banking_data <- read_csv(
  "Data.R/Cleaned_Banking_Deserts_Data_File_2019.csv",
  col_types = cols(GEOID = col_character())
)

# Zero-pad GEOID to 11 characters
Combined_banking_data <- Combined_banking_data |>
  mutate(GEOID = str_pad(GEOID, 11, "left", "0"))

# --------------- Fetch & Merge Census Pop Density ------------
states <- unique(substr(Combined_banking_data$GEOID, 1, 2))

tract_pop_list <- lapply(states, \(st) {
  get_acs(
    geography = "tract",
    variables = c(pop = "B01003_001"),
    year      = 2019,
    state     = st,
    geometry  = TRUE,
    cache     = TRUE
  ) |>
    mutate(
      area_km2    = as.numeric(st_area(geometry)) / 1e6,
      pop_density = estimate / area_km2
    ) |>
    st_drop_geometry() |>
    select(GEOID, pop_density)
})

pop_density_df <- bind_rows(tract_pop_list)

Combined_banking_data <- Combined_banking_data |>
  left_join(pop_density_df, by = "GEOID")

# ---------------- Population Density Descriptives ----------------
cat("\n--- Population Density Summary ---\n")
pop_stats <- Combined_banking_data |>
  summarize(
    Mean    = mean(pop_density, na.rm = TRUE),
    Std_Dev = sd(pop_density,   na.rm = TRUE),
    Median  = median(pop_density, na.rm = TRUE),
    Min     = min(pop_density,   na.rm = TRUE),
    Max     = max(pop_density,   na.rm = TRUE)
  )
print(pop_stats)


# ---------------- Baseline Probit (classical SEs) ------------

# --------------------- Specify Regressors --------------------
regressors <- c(
  "Pct_no_broadband", 
  "unadjusted_hdi",
  "Pct_hisp",
  "Pct_black_nh",
  "Pct_aian_nh",
  "Pct_asian_nh",
  "Pct_2more_nh",
  "pop_density"
)

formula_base <- reformulate(regressors, response = "D_Banking_Desert")

# ---------------- Baseline Probit (classical SEs) ------------
baseline_probit <- glm(
  formula_base,
  family = binomial(link = "probit"),
  data   = Combined_banking_data
)

cat("\n--- Baseline Probit Summary ---\n")
print(summary(baseline_probit))

# ------------- Multicollinearity Diagnostics -----------------
cat("\n--- VIF (baseline) ---\n")
print(vif(baseline_probit))

# --- Heteroskedasticity tests --------------------------------
cat("\n--- Heteroskedasticity tests ---\n")
print(bptest(baseline_probit))      # Breusch-Pagan
print(
  bptest(
    baseline_probit,
    ~ fitted(baseline_probit) + I(fitted(baseline_probit)^2)
  )                                  # White
)                                    # <- this was missing!

# ------------- State Fixed-Effects Probit --------------------
tformula_fe <- update(formula_base, . ~ . + factor(State_FIPS))

state_fe_probit <- glm(
  tformula_fe,
  family = binomial(link = "probit"),
  data   = Combined_banking_data
)

# ------------- Cluster-robust SEs by state -------------------
se_state <- vcovCL(state_fe_probit,
                   cluster = Combined_banking_data$State_FIPS)
cat("\n--- State FE Probit (cluster-robust SEs) ---\n")
print(coeftest(state_fe_probit, se_state))

# ------------- McFadden Pseudo-R² comparison -----------------
mcf_baseline <- pR2(baseline_probit)["McFadden"]
mcf_state_fe <- pR2(state_fe_probit)["McFadden"]
cat("\n--- McFadden Pseudo-R² ---\n")
cat(sprintf("Baseline : %.3f\n", mcf_baseline))
cat(sprintf("State FE : %.3f\n", mcf_state_fe))

# ------------------------------------------------------------
# Average Marginal Effects – State-FE probit (cluster robust)
# ------------------------------------------------------------
library(margins)

vars <- c(
  "Pct_no_broadband", 
  "unadjusted_hdi",
  "Pct_hisp",
  "Pct_black_nh",
  "Pct_aian_nh",
  "Pct_asian_nh",
  "Pct_2more_nh",
  "pop_density"
)

# Calculate AMEs with state-clustered VCOV
mfx_fe <- margins(
  state_fe_probit,
  variables = vars,
  vcov      = se_state,            # <- cluster-robust vcov from earlier
  data      = Combined_banking_data
)

summary(mfx_fe)

