"Determine if a relationship exists between missing data in the training set."

#Load libraries and Prep Data -------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

#Read data
census <- read.csv("./Data/census_data.csv", header=TRUE, na.strings=c("","NA"))

#Drop ID column
df <- subset(census, select = -c(1))

#Visualizations for missingness within our data -------------------------------------------------------------------------
#Across Entire Dataset
df %>%
  mutate(across(everything(), is.na)) %>%
  arrange(across(everything())) %>%
  mutate(row = row_number()) %>%
  pivot_longer(!row, names_to="column", values_to="missing") %>%
  ggplot() +
  geom_tile(aes(row, column, fill=missing)) + ggtitle("Missing Observation Occurances") + xlab("Observation") + ylab("Variable")

#Analyze relationship between specific instances ------------------------------------------------------------------------
#Determine relationship between residence categories (highly related)
df %>%
  count(across(c(old_residence_state, old_residence_reg, residence_1_year_ago), is.na))

#Look at relationship between residence 1 year ago and migration to the sunbelt (highly inverse)
df %>%
  count(across(c(residence_1_year_ago, migration_prev_sunbelt), is.na))


  


