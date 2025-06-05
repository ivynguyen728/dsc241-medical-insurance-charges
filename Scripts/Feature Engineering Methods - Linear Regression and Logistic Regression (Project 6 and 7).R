library(car)

# Load data
insurance = read.csv("Data/insurance.csv")
str(insurance)
summary(insurance)

# Turn the categorical and ordinal numeric variables into factors
insurance$sex = as.factor(insurance$sex)
insurance$children = as.factor(insurance$children)
insurance$smoker = as.factor(insurance$smoker)
insurance$region = as.factor(insurance$region)

# Fix diagnostic issues
# `charges` is right-skewed
insurance$log_charges = log(insurance$charges)
# relationship between `bmi` and `charges` is non-linear
insurance$center_bmi = scale(insurance$bmi, center = TRUE, scale = FALSE)
insurance$squared_bmi = insurance$center_bmi^2

# HW4: OLD MODEL
old_model <- lm(charges ~ age + sex + bmi + children + smoker + region,
                data = insurance)
summary(old_model)

# Diagnostics
par(mfrow = c(2, 2))
plot(old_model)
vif(old_model) # Check multicollinearity

# NEW MODEL
new_model <- lm(log_charges ~ age + sex + center_bmi + squared_bmi + children + smoker + region,
                data = insurance)
summary(new_model)

# Diagnostics
par(mfrow = c(2, 2))
plot(new_model)
vif(new_model) # Check multicollinearity

# COMPARISON
# Adjusted R-squared
cat("Adjusted R-squared (old):", summary(old_model)$adj.r.squared, "\n")
cat("Adjusted R-squared (new):", summary(new_model)$adj.r.squared, "\n")

# Variance of coefficients
cat("\nCoefficient variances (old):\n")
print(diag(vcov(old_model)))

cat("\nCoefficient variances (new):\n")
print(diag(vcov(new_model)))

### Homework 7
# Use variable selection to choose a reduced model that offers a reasonable 
# balance between predictive ability and interpretation
summary(new_model)

library(caret)
train_control <- trainControl(method = 'cv', number = 10)
new_model.cv <- train(log_charges ~ age + sex + center_bmi + squared_bmi + children + smoker + region,
                      data = insurance,
                      method = 'lm',
                      trControl = train_control)
print(new_model.cv)

n = nrow(insurance)
# Stepwise search with AIC
step(lm(log_charges ~ 1, data = insurance),
     scope = log_charges ~ age + sex + center_bmi + squared_bmi + children + smoker + region,
     direction = 'forward') $ call
step(new_model, direction="backward") $ call
step(new_model, direction="both") $ call

# stepwise search with BIC
step(lm(log_charges ~ 1, data = insurance),
     scope = log_charges ~ age + sex + center_bmi + squared_bmi + children + smoker + region,
     direction = 'forward') $ call
step(new_model, direction="backward", k=log(n)) $ call
step(new_model, direction="both", k=log(n)) $ call

# Binarize the continuous outcome by setting an appropriate threshold
threshold = median(insurance$log_charges)
insurance$high_cost = ifelse(insurance$log_charges > threshold, 1, 0)

# Fit a new model with the same predictors to the binarized outcome using logistic regression.
logreg_model <- glm(high_cost ~ age + sex + center_bmi + squared_bmi + children + smoker + region,
                    data = insurance,
                    family = binomial(link = "logit"))
summary(logreg_model)
residualPlots(logreg_model)

# Include standard errors and an interpretation of the coefficients.
exp(coef(logreg_model))
