library(tidyverse)

df <- read.csv("DatasetPro12.0.csv")

new_df <- df[, 1:9]

df$Sex <- as.factor(df$Sex)

for (i in 10:1076) {
  model <- lm(df[[i]] ~ Age + Sex, data = df)
  
  residuals <- residuals(model)
  
  new_df[[names(df)[i]]] <- residuals
  
  if (names(df)[i] == 'rh_caudalanteriorcingulate_volume') {
    
    plot(fitted(model), residuals, 
         main = paste("Residuals vs Fitted for", names(df)[i]), 
         xlab = "Fitted Values", 
         ylab = "Residuals", 
         pch = 19, col = "black")
    
    # Add horizontal line at 0
    abline(h = 0, col = "red", lty = 2)
    
    qqnorm(residuals, main = paste("Q-Q Plot for", names(df)[i]))
    qqline(residuals, col = "red", lty = 2)
    
    plot(density(residuals), 
         main = paste("Density Plot for", names(df)[i]), 
         xlab = "Residuals", 
         col = "blue")
    break
  }
}
