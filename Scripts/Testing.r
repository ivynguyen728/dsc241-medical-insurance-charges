ins_df = read.csv("Data/insurance.csv")
lm_model = lm(charges ~ age + bmi + children, data = ins_df)
par(mfrow = c(2, 2))  # optional: layout in a grid
plot(lm_model, which = 1)  # Residuals vs Fitted
plot(lm_model, which = 2)  # Normal Q-Q
plot(lm_model, which = 3)  # Scale-Location
plot(lm_model, which = 5)  # Residuals vs Leverage