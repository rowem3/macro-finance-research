# ============================================================
#  ECN 481  •  State-FE Probit Model  •  Cluster-Robust SEs
# ============================================================

# --------------------------- Libraries -----------------------
library(readr)
library(dplyr)
library(lmtest)      # coeftest()
library(sandwich)    # vcovCL()
library(margins)     # marginal effects
library(corrplot)
library(car)

# ------------------------- Load data -------------------------
Combined_banking_data <- read_csv("Data.R/Cleaned_Banking_Deserts_Data_File_2019.csv")

# --------------------- Probit with state FE ------------------
state_fe_probit <- glm(
  D_Banking_Desert ~
    Pop + Med_hh_inc + D_Rural + D_Suburban +
    Pct_no_broadband + Pct_older + Pct_disab + unadjusted_hdi +
    D_Hispanic + D_Black + D_American_Indian_Alaska_Native +
    D_Asian + D_No_Predominant_Race +
    factor(State_FIPS),                    # ← state dummies
  family = binomial("probit"),
  data   = Combined_banking_data
)

# ----------------- State-clustered SEs -----------------------
vc_FE <- vcovCL(state_fe_probit, cluster = Combined_banking_data$State_FIPS)

cat("\n=== Coefficient table (state-clustered SEs) ===\n")
print(coeftest(state_fe_probit, vcov = vc_FE), digits = 4)

# ------------- Cluster-robust marginal effects ---------------
cat("\n=== Average marginal effect of HDI (clustered SE) ===\n")
print(summary(
  margins(state_fe_probit,
          variables = "unadjusted_hdi",
          vcov      = vc_FE)
), digits = 4)

# ------------- Optional: multicollinearity re-check ----------
# (Same as script 1, optional because FE dummies don't affect VIF of tract vars)

# Null model WITH state dummies only
null_fe <- glm(D_Banking_Desert ~ factor(State_FIPS),
               family = binomial("probit"),
               data   = Combined_banking_data)

# McFadden R² for the FE spec
mcfadden_r2_fe <- 1 - (logLik(state_fe_probit) / logLik(null_fe))
print(mcfadden_r2_fe)

# Intercept-only null
null_int <- glm(D_Banking_Desert ~ 1,
                family = binomial("probit"),
                data   = Combined_banking_data)

R2_FE_vs_int <- 1 - (logLik(state_fe_probit) / logLik(null_int))
print(R2_FE_vs_int)      # typically ≈ 0.18–0.22

# assuming state_fe_probit  +  vc_FE already created
library(margins)

# Pick the variables you want in the slide
vars <- c("unadjusted_hdi",
          "Pct_no_broadband",
          "D_American_Indian_Alaska_Native",
          "D_Suburban",
          "D_Rural")

ame_fe <- margins(state_fe_probit,
                  variables = vars,
                  vcov      = vc_FE)   # <- clustered SEs

print(summary(ame_fe), digits = 4)


