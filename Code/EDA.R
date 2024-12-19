library(caret)
library(corrplot)
library(dplyr)
library(tidyr)

# Read data
census_data <- read.csv("Data/census_data.csv", header=TRUE, na.strings=c("","NA"))

# Drop ID column
df <- subset(census_data, select = -c(1))
str(df)

# Percent missing ----------------------------------------------
pct_missing <- function(data) {
  # Calculate the total number of observations
  total_obs <- nrow(data)
  
  # Calculate the number of missing values in each column
  missing_count <- colSums(is.na(data))
  
  # Calculate the percentage of missing values in each column
  percent_missing <- (missing_count / total_obs) *100
  
  # Create a data frame to store the results
  missing_data <- data.frame(Column = names(data), Percent_Missing = percent_missing)
  
  # Return the data frame
  return(missing_data)
}

percent_missing <- pct_missing(df)
print(percent_missing)

# Drop columns with missing values over 90% of the time
over_90 <- percent_missing$Percent_Missing > 90
drop_missing <- percent_missing[over_90, ]

# Columns to drop
drop_missing$Column

# Variance ---------------------------------------------------- 
near_zero_variance <- function(data) {

    near_zero_vars <- nearZeroVar(data, names = TRUE, saveMetrics = TRUE)
    
    # Create a data frame to store the results
    variance_data <- data.frame(Column = names(data), ZeroVariance = near_zero_vars$zeroVar, NearZeroVariance = near_zero_vars$nzv)
    
    return(variance_data)
}

near_zero_vars <- near_zero_variance(df)
print(near_zero_vars)

# Drop columns with zero variance
zero_var <- near_zero_vars$ZeroVariance == TRUE
drop_variance <- near_zero_vars[zero_var, ]

# Columns to drop
drop_variance$Column

# Correlation matrix ----------------------------------------
correlation_matrix <- function(data) {
  # Subset numeric columns
  numeric_data <- data[, sapply(data, is.numeric)]
  selected_df <- numeric_data %>% select(age, employment_stat, wage_per_hour, working_week_per_year, industry_code, occupation_code, total_employed)
  
  # Calculate correlation matrix
  corr_matrix <- cor(selected_df)
  
  # Create correlation plot
  corrplot(corr_matrix, method = "color", tl.col= "black", addCoef.col = "black",col=colorRampPalette(c("brown4", "red3", "pink", "red2", "brown4"))(7) , is.corr = FALSE, tl.cex = 1.2,  number.cex = 1.1)
}

correlation_matrix(df)


# Plots ---------------------------------------------------
ggplot(df, aes(x = is_labor_union)) +
  geom_bar(stat = "count", fill = "cyan3") +
  labs(title = "Count of is_labor_union", x = "is_labor_union", y = "Count")


# Weight of Evidence -------------------------------------
library(Information)

# Drop columns from above analysis
woe_df <- subset(df, select = -c(education_institute, unemployment_reason, 
                                       under_18_family, veterans_admin_questionnaire, 
                                       old_residence_reg, old_residence_state))

# Response to numeric
woe_df$is_labor_union <- ifelse(woe_df$is_labor_union == 'Same', 1, 0)

# Create Info Tables
IV <- create_infotables(data=woe_df, y="is_labor_union")

# Plot WOE charts
names <- IV[["Summary"]][["Variable"]]
plots <- list()
for (i in 1:length(names)){
  plots[[i]] <- plot_infotables(IV, names[i])
}

# Showing the top 10 variables
plots[1:10]

