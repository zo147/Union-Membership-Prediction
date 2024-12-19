numeric_cols <- df[sapply(df, is.numeric)]
head(numeric_cols)
cor(numeric_cols)

# Explore outliers
hist(df$age, xlab = "Age", main = "Age Frequency") # skewed slightly right, might want to group into bands for higher age groups.
summary(df$age) 
table(df$age)
df$age_grp <- ifelse(df$age <18, "Under 18", ifelse(df$age <25, "18-24", ifelse(df$age <30, "25-29", 
      ifelse(df$age <40, "30-39", ifelse(df$age <50, "40-49", ifelse(df$age <60, "50-59", 
      ifelse(df$age <70, "60-69", "70+")))))))
table(df$age_grp) # lowest group frequency is 195. Use age_grp variable

hist(df$employment_stat, xlab = "Employment Stat", main = "Employment Stat Frequency")
table(df$employment_stat) # this is a categorical factor
df$employment_stat <- as.factor(df$employment_stat)

hist(df$wage_per_hour, xlab = "Wage per Hour", main = "Wage per Hour Frequency") # examine for outliers
summary(df$wage_per_hour) # Minimum 0 and Maximum 9999, manipulate?
length(which(df$wage_per_hour > 2000)) # consider dropping or grouping due to large number of outliers (~537)
df$wage_grps <- ifelse(df$wage_per_hour == 0, "0", ifelse(df$wage_per_hour < 1000, "1-999", 
      ifelse(df$wage_per_hour < 2000, "1000-1999","2000+")))
table(df$wage_grps) # lowest group frequency is 643. Use wage_grps variable.

hist(df$working_week_per_year, xlab = "Working Week per Year", main = "Working Week per Year Frequency")
summary(df$working_week_per_year)
table(df$working_week_per_year) # decent amount of observations in most weeks

hist(df$industry_code, xlab = "Industry Code", main = "Industry Code Frequency")
summary(df$industry_code)
table(df$industry_code) # lowest number of observations in a group is 9 (code 20), rest are 25+
df$industry_code <- as.factor(df$industry_code)

hist(df$occupation_code, xlab = "Occupation Code", main = "Occupation Code Frequency") # look at highest value
summary(df$occupation_code)
table(df$occupation_code) # lowest number of observations in a group is 7 (code 20), rest are 19+
df$occupation_code <- as.factor(df$occupation_code)

hist(df$total_employed, xlab = "Total Employed", main = "Total Employed Frequency") # some are 0?
summary(df$total_employed)
table(df$total_employed) # group of 0 has 384 observations

hist(df$gains, xlab = "Gains", main = "Gains Frequency") # examine for outliers
summary(df$gains)
table(df$gains) # recommend dropping. Due to large amount of 0.


hist(df$losses, xlab = "Losses", main = "Losses Frequency") # examine for outliers
summary(df$losses)
table(df$losses) # recommend dropping. Due to large amount of 0. 

hist(df$stocks_status, xlab = "Stock Status", main = "Stock Status Frequency") # examine for outliers
summary(df$stocks_status)
table(df$stocks_status) # recommend dropping or grouping into bins
df$stock_status_grp <- ifelse(df$stocks_status == 0, "0", ifelse(df$stocks_status < 1000, "1-999", "1000+"))
table(df$stock_status_grp) # not sure how useful this is.

hist(df$mig_year, xlab = "Mig Year", main = "Mig Year Frequency") # examine for outliers
summary(df$mig_year)
table(df$mig_year) # categorical 94 or 95, fine to use.
df$mig_year <- as.factor(df$mig_year)

hist(df$importance_of_record, xlab = "Importance of Record", main = "Importance of Record Frequency") # examine for outliers
summary(df$importance_of_record)
table(df$importance_of_record) # don't use or split into bins.
df$importance_grp <- ifelse(df$importance_of_record <1000, "<1000", ifelse(df$importance_of_record <2000, "1000-1999",
      ifelse(df$importance_of_record <3000, "2000-2999", "3000+")))
table(df$importance_grp) # use this grouped variable.


## Summary:

# Use age_grp instead of age, use wage_grps instead of wage_per_hour, 
# use importance_grp instead of importance_of_record.

# Turned Employment Stat into factor. Turned industry code into a factor. Turned occupation code into a factor.
# Turned Mig Year into factor.

# Recommend dropping gains and losses. Possible stocks_status, or use stock_status_grp.
