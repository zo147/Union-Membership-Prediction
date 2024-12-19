# Replace 'education' (16 levels) with 'education_grp': low_ed, mid_ed, high_ed
df <- df %>%
  mutate(education_grp = case_when(
    education %in% c("Less than 1st grade", "1st 2nd 3rd or 4th grade", "5th or 6th grade", "7th and 8th grade")
    ~ "low_ed",
    education %in% c("9th grade", "10th grade", "11th grade", "High school graduate",  " 12th grade no diploma" ) 
    ~ "mid_ed",
    TRUE ~ "high_ed"))
df$education_grp <- factor(df$education_grp, levels = c("high_ed", "mid_ed", "low_ed"))

# Replace 'class' (4 levels) with 'govt_employee': yes or no
df <- df %>%
  mutate(govt_employee = 
           ifelse(class %in% c("Federal government", "Local government", "State government"), 
                  "yes", "no"))
df$govt_employee <- factor(df$govt_employee, levels = c("yes", "no"))

# Replace 'marital_status' (7 levels) with 'is_married': yes or no
# Include "married- spouse absent' with 'is_married: no'. 
df <- df %>%
  mutate(is_married = 
           ifelse(marital_status %in% c("Married-civilian spouse present", "Married-A F spouse present,
                                        "), "yes", "no"))
df$is_married <- factor(df$is_married, levels = c("yes", "no"))

# Replace 'is_hispanic' (10 levels) with 'is_hispanic_grp': yes, no, or unknown
df <- df %>%
  mutate(is_hispanic_grp = case_when(
    is_hispanic %in% c("Mexican-American", "Mexican (Mexicano)", "Chicano", 
                       "Central or South American", "Puerto Rican", "Other Spanish", "Cuban") ~ "yes",
    is_hispanic %in% c("All other") ~ "no",
    is_hispanic %in% c("NA", "Do not know") ~ "unknown"))
df$is_hispanic_grp <- factor(df$is_hispanic_grp, levels = c("yes", "no", "unknown"))

# Replace 'employment_commitment' (7 levels) with 'work_hours': full-time, part-time, or unemployed
df <- df %>%
  mutate(work_hours =
           ifelse(employment_commitment %in% c("Full-time schedules", 
                                               "Children or Armed Forces"),
           "full-time",
           ifelse(employment_commitment %in% c("PT for non-econ reasons usually FT", 
                                               "PT for econ reasons usually FT", 
                                               "Unemployed full-time"),
           "part-time", "unemployed")))
df$work_hours <- factor(df$work_hours, levels = c("full-time", "part-time", "unemployed"))

# Replace 'tax_status' (6 levels) with 'tax_status_grp': single, joint, head-of-household, nonfiler, or unknown
df <- df %>%
  mutate(tax_status_grp = 
           ifelse(tax_status %in% c("Joint both under 65", 
                                    "Joint one under 65 & one 65+", 
                                    "Joint both 65+"),
                  "joint",
                  ifelse(tax_status == "Single",
                         "single",
                         ifelse(tax_status == "Head of household",
                                "head-of-household",
                                ifelse(tax_status == "Nonfiler",
                                       "nonfiler", "unknown")))))
df$tax_status_grp <- factor(df$tax_status_grp, levels = 
                              c("single", "joint", "head-of-household", "nonfiler", "unknown"))
         
# Replace 'citizenship' (5 levels) with 'citizenship_grp': citizen, non-citizen, or naturalized citizen
df <- df %>%
  mutate(citizenship_grp = case_when(
    citizenship %in% c("Native", "Native- Born in Puerto Rico or U S Outlying", 
                       "Native- Born abroad of American Parent(s)") ~ "citizen",
    citizenship == "Foreign born- Not a citizen of U S " ~ "non-citizen",
    citizenship == "Foreign born- U S citizen by naturalization" ~ "naturalized citizen")) 
df$citizenship_grp <- factor(df$citizenship_grp, levels = 
                              c("citizen", "non-citizen", "naturalized citizen"))

# For 'is_labor_union, replace "Same" with "Yes"
df <- df %>%
  mutate(is_labor_union = factor(
    ifelse(is_labor_union == "Same", "Yes", is_labor_union),
    levels = c("yes", "no") ))

# Drop from df: 'age', 'wage_per_hour', 'importance_of_record', 'stocks_status', 'working_week_per_year' 
# 'education', 'class', 'marital_status', 'is_hispanic', 'employment_commitment', 'tax_status', 
# 'citizenship'
df = subset(df, select = -c(age, wage_per_hour, importance_of_record, stocks_status, working_week_per_year,
                            education, class, marital_status, is_hispanic, employment_commitment,
                            tax_status, citizenship))

# Instead use: 'age_grp', 'wage_grp', 'importance_grp', 'stock_status_grp', 'working_weeks_grp'
# 'education_grp, 'govt_employee', 'is_married', 'is_hispanic_grp', 'work_hours', 'tax_status_grp',
# 'citizenship_grp'

# drop 'gains' and 'losses' per above recommendation (large amount of 0's)
df = subset(df, select = -c(gains, losses))