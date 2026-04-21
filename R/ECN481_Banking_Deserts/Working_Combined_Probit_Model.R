# ============================================================
#  ECN 481  •  Baseline Probit Model  •  Classical SEs
# ============================================================

# --------------------------- Libraries -----------------------
library(readr)
library(dplyr)
library(corrplot)
library(car)
library(margins)

# ------------------------- Load data -------------------------
Combined_banking_data <- read_csv("Data.R/Cleaned_Banking_Deserts_Data_File_2019.csv")

# --------------------- Probit regression ---------------------
baseline_probit <- glm(
  D_Banking_Desert ~
    Pop + 
    D_Rural + 
    D_Suburban +
    Pct_no_broadband + 
    Pct_older + 
    Pct_disab + 
    unadjusted_hdi +
    Pct_hisp + 
    Pct_black_nh + 
    Pct_aian_nh +
    Pct_asian_nh + 
    Pct_2more_nh,
  family = binomial("probit"),
  data   = Combined_banking_data
)
summary(baseline_probit)

# ----------------- Multicollinearity check -------------------
corr_matrix <- cor(
  Combined_banking_data %>% 
    select(Pop, Med_hh_inc, Pct_no_broadband, Pct_no_cpu,
           Pct_older, Pct_disab, unadjusted_hdi,
           Pct_hisp, Pct_black_nh, Pct_aian_nh,
           Pct_asian_nh, Pct_oth_nh, Pct_2more_nh),
  use = "complete.obs"
)
corrplot(corr_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

vif_lm <- lm(
  D_Banking_Desert ~
    Pop + 
    D_Rural + 
    D_Suburban +
    Pct_no_broadband + 
    Pct_older + 
    Pct_disab + 
    unadjusted_hdi +
    Pct_hisp + 
    Pct_black_nh + 
    Pct_aian_nh +
    Pct_asian_nh + 
    Pct_2more_nh,
  data = Combined_banking_data
)
print(vif(vif_lm))

# ---------------- Marginal-effects analysis ------------------
print(summary(margins(baseline_probit)))

# ---------------- Model-fit statistic (McFadden R²) ----------
null_mod <- glm(D_Banking_Desert ~ 1,
                family = binomial("probit"),
                data   = Combined_banking_data)
mcfadden_r2 <- 1 - (logLik(baseline_probit) / logLik(null_mod))
print(mcfadden_r2)

# ------------------------------------------------------------
# Heteroskedasticity tests for Baseline Probit (via LPM surrogate)
# ------------------------------------------------------------
library(lmtest)      # bptest()
library(sandwich)    # vcovHC()

# 1.  Fit a Linear-Probability Model with the same RHS
lpm_baseline <- lm(
  D_Banking_Desert ~
    Pop + Med_hh_inc + D_Rural + D_Suburban +
    Pct_no_broadband + Pct_older + Pct_disab + unadjusted_hdi +
    D_Hispanic + D_Black + D_American_Indian_Alaska_Native +
    D_Asian + D_No_Predominant_Race,
  data = Combined_banking_data
)

# 2.  Breusch–Pagan test  (null = homoskedastic)
bp_raw      <- bptest(lpm_baseline)                     # classical
bp_student  <- bptest(lpm_baseline, studentize = TRUE)  # studentized

print(bp_raw); print(bp_student)

# 3.  White test (heteroskedasticity of unknown form)
white <- bptest(lpm_baseline, ~ fitted(lpm_baseline) + I(fitted(lpm_baseline)^2))
print(white)

