install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
library(readxl)
library(dplyr)

data <- read_excel("D:/5th semester/MAT 4509 (Statistics)/Lecture/United Airlines Aircraft Operating Statistics- Cost Per Block Hour (Unadjusted).xls")

data <- as.data.frame(t(data))

head(data)
colnames(data)

colnames(data) <- data[1, ]
data <- data[-1, ] 


n <- 21 
sample_data <- data %>% sample_n(n)


colnames(sample_data)
salary_min <- min(as.numeric(sample_data$`COLUMN_NAME`), na.rm = TRUE)
salary_max <- max(as.numeric(sample_data$`COLUMN_NAME`), na.rm = TRUE)
salary_breaks <- seq(salary_min, salary_max, by = 5000)
salary_freq <- table(cut(as.numeric(sample_data$`COLUMN_NAME`), breaks = salary_breaks, include.lowest = TRUE))

print(salary_freq)

mean_salary <- mean(as.numeric(sample_data$`COLUMN_NAME`), na.rm = TRUE)
median_salary <- median(as.numeric(sample_data$`COLUMN_NAME`), na.rm = TRUE)
mode_salary <- as.numeric(names(sort(table(as.numeric(sample_data$`COLUMN_NAME`)), decreasing = TRUE)[1]))
sd_salary <- sd(as.numeric(sample_data$`COLUMN_NAME`), na.rm = TRUE)
variance_salary <- var(as.numeric(sample_data$`COLUMN_NAME`), na.rm = TRUE)
quartiles_salary <- quantile(as.numeric(sample_data$`COLUMN_NAME`), probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
decile_salary <- quantile(as.numeric(sample_data$`COLUMN_NAME`), probs = 0.9, na.rm = TRUE)
percentile_salary <- quantile(as.numeric(sample_data$`COLUMN_NAME`), probs = 0.1, na.rm = TRUE)
range_salary <- range(as.numeric(sample_data$`COLUMN_NAME`), na.rm = TRUE)

cat("Mean:", mean_salary, "\n")
cat("Median:", median_salary, "\n")
cat("Mode:", mode_salary, "\n")
cat("Standard Deviation:", sd_salary, "\n")
cat("Variance:", variance_salary, "\n")
cat("Quartiles:", quartiles_salary, "\n")
cat("9th Decile:", decile_salary, "\n")
cat("10th Percentile:", percentile_salary, "\n")
cat("Range:", range_salary, "\n")
