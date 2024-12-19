library(caret)
library(dplyr)
library(stringr)
library(glmnet)
library(tidyr)
library(tidyverse)
library(MASS)
library(ggplot2)

set.seed(1)

#Read data - replace empty string and " ?" with NA 
census_data <- read.csv("./Data/census_data.csv", header=TRUE, na.strings=c(c("", ' ?'),"NA"))

#Clean Up-------------------------------------------------------------------------------
#Drop columns discovered in EDA.R
drop <- match(c("ID", "education_institute", "under_18_family", "veterans_admin_questionnaire", 
        "old_residence_reg", "old_residence_state", "unemployment_reason"), names(census_data))
df <- subset(census_data, select = -drop)

#Remove whitespace from column data
df <- df %>%
  mutate(across(where(is.character), str_trim))

#convert non-numeric columns to factors
cols = c("age", "employment_stat", "wage_per_hour", "working_week_per_year", "industry_code", 
         "occupation_code", "total_employed", "gains", "losses", "stocks_status", "mig_year", "importance_of_record")
df[,!names(df) %in% cols] <- lapply(df[,!names(df) %in% cols], as.factor)

#Drop Columns with Na proportion >20%
df <- df %>%
  select_if(~mean(is.na(.)) < .2)

#Impute missing values w/ simple Mode imputation for factor items
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
df <- df %>%
  mutate_if(is.factor, funs(replace(., is.na(.), mode(na.omit(.)))))

#Proportions of NA data after cleanup
df %>%
  mutate(across(everything(), is.na)) %>%
  arrange(across(everything())) %>%
  mutate(row = row_number()) %>%
  pivot_longer(!row, names_to="column", values_to="missing") %>%
  ggplot() +
  geom_tile(aes(row, column, fill=missing))

#Variable Selection--------------------------------------------------------------------------------------------
#LASSO - separate into feature and target set
y <- df$is_labor_union
X <- data.matrix(df[, c("is_labor_union")])


#Create cv sets at 10 folds, models, and coefficients and select for 20 variables 
lasso_cv <- cv.glmnet(X, y, alpha=1, nfolds=10, family="binomial", type.measure = "mse", dfmax=20)
lasso_model <- glmnet(X, y, alpha=1, nlambda=100, family = "binomial", dfmax=20)
lasso_coef <- coef(lasso_model, s=lasso_cv$lambda.min, family="binomial")

#Plot model and its minimum lambda found
plot(lasso_model, xvar="lambda", lwd=2)
abline(v=log(lasso_cv$lambda.min), col='black', lty=2, lwd=2)


#Model Coefficients
lasso_coef
coefs = as.data.frame(as.matrix(lasso_coef))
zeros <- coefs %>%
  filter(s1 == 0)
coefs <- coefs %>% 
  filter(s1 != 0)

#Features not selected with threshold 20
zeros
#Features selected with threshold 20
coefs
