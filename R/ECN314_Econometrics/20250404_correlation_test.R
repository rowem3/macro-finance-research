#Initial Correlation Test

# Load necessary packages
if (!requireNamespace("stargazer", quietly = TRUE)) {
  install.packages("stargazer")
}
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
if (!requireNamespace("broom", quietly = TRUE)) {
  install.packages("broom")
}
if (!requireNamespace("corrplot", quietly = TRUE)) {
  install.packages("corrplot")
}
if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
  install.packages("RColorBrewer")
}
library(stargazer)
library(readxl)
library(broom)
library(corrplot)
library(RColorBrewer)

#Import data
library(readr)
Banking_Deserts_Dashboard_Data_File_2019 <- read_csv("~/R Projects/ECN 481 Thesis.R/Data.R/Banking_Deserts_Dashboard_Data_File_2019.csv")
View(Banking_Deserts_Dashboard_Data_File_2019)
s