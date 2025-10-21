
# AdvancedRHT2025LabBonus

<!-- badges: start -->
[![R-CMD-check](https://github.com/Cl4ryty/AdvancedRHT2025LabBonus/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Cl4ryty/AdvancedRHT2025LabBonus/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of AdvancedRHT2025LabBonus is to create a linear regression package implemented using matrix computations for Advanced R Programming (732A94) at Linköping University.

## Installation

You can install the development version of AdvancedRHT2025LabBonus from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("Cl4ryty/AdvancedRHT2025LabBonus")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(AdvancedRHT2025LabBonus)
data(iris)

# Fit linear regression model
mod <- linreg(Petal.Length ~ Species, data = iris)
print(mod)

# Detailed statistical summary
summary(mod)

# Extract components
coef(mod)      # Coefficients
resid(mod)     # Residuals  
pred(mod)      # Fitted values

# Create diagnostic plots
plot(mod)

# Use numerically stable QR decomposition method
mod_qr <- linreg(Petal.Length ~ Species, data = iris, method = "qr")
summary(mod_qr)

# Multiple predictors
mod_multi <- linreg(Petal.Length ~ Petal.Width + Sepal.Length, data = iris)
summary(mod_multi)

```

