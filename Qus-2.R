library(readxl)
library(dplyr)
library(ggplot2)
file_path <- "D:/5th semester/MAT 4509 (Statistics)/Lecture/United Airlines Aircraft Operating Statistics- Cost Per Block Hour (Unadjusted).xls"
data <- read_excel(file_path)
head(data)
data_subset <- data %>% select(`Salaries and Wages`, `Pilot Training`, `Benefits and Payroll Taxes`, 
                               `Per Diem/Personnel`, `Maintenance`, `Aircraft Ownership`, `Indices`, 
                               `Daily Utilization per Aircraft`)
names(data_subset) <- c("Salaries_Wages", "Pilot_Training", "Benefits_Payroll", "Per_Diem", 
                        "Maintenance", "Aircraft_Ownership", "Indices", "Daily_Utilization")

model <- lm(Salaries_Wages ~ Pilot_Training + Benefits_Payroll + Per_Diem + 
              Maintenance + Aircraft_Ownership + Indices + Daily_Utilization, data = data_subset)

summary(model)

cat("R-squared:", summary(model)$r.squared)

cat("Standard Error of Estimate:", summary(model)$sigma)

cor_matrix <- cor(data_subset)
print(cor_matrix)

cat("F-statistic:", summary(model)$fstatistic[1])
cat("p-value for F-test:", pf(summary(model)$fstatistic[1], 
                              summary(model)$fstatistic[2], 
                              summary(model)$fstatistic[3], lower.tail = FALSE))

cat("t-values and p-values for each coefficient:\n")
summary(model)$coefficients[, c("t value", "Pr(>|t|)")]

significant_vars <- summary(model)$coefficients[, "Pr(>|t|)"] < 0.05
significant_vars <- names(significant_vars[significant_vars == TRUE])
significant_formula <- as.formula(paste("Salaries_Wages ~", paste(significant_vars[-1], collapse = " + ")))
revised_model <- lm(significant_formula, data = data_subset)
summary(revised_model)

cat("New regression equation:\n")
cat(paste("Salaries_Wages =", round(coef(revised_model)[1], 2), "+", 
          paste(round(coef(revised_model)[-1], 2), "*", names(coef(revised_model))[-1], collapse = " + ")))
