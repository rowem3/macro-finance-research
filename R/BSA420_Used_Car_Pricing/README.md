# BSA 420 — Used Car Price Prediction

Built and compared regression models to predict used car prices using the Toyota Corolla dataset.

**Models:**
- Linear regression with stepwise variable selection
- k-Nearest Neighbors (KNN) with 10-fold cross-validation and k tuned from 1–15

**Preprocessing:** min-max normalization on numeric features, 50/50 train-test split

**Evaluation:** RMSE and MAE on held-out test set; RMSE vs. k plot to visualize the bias-variance tradeoff

**Packages:** `caret`, `rsample`, `Metrics`, `ggplot2`
