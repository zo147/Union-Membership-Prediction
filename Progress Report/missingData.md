# Missing Data in Census Data

We will examine the missing data within our dataset and discover if any relevant patterns exist as well as if certain variables should be ommitted entirely.
We start by importing our needed libraries, reading our data housed in this this repository, and dropping the ID column.

```r
library(dplyr)
library(tidyr)
library(ggplot2)

census <- read.csv("./Data/census_data.csv", header=TRUE, na.strings=c("","NA"))

df <- subset(census, select = -c(1))
```

From this we can create an initial visualization of our data, showing what is missing and seeing if there are any overt patterns.

```r
df %>%
  mutate(across(everything(), is.na)) %>%
  arrange(across(everything())) %>%
  mutate(row = row_number()) %>%
  pivot_longer(!row, names_to="column", values_to="missing") %>%
  ggplot() +
  geom_tile(aes(row, column, fill=missing))
```

![Missing Data](/Visualizations/missingDataAcrossColumns.png)

## Observations
From the above image we notice several things worth investigating. Such as,

- Missing data for for old residence state and old residence region tend to be missing together
- Whether someone lived in their residence 1 year ago is inversely correlated with whether they have previously migrated to the sunbelt
- There are no unemployment reasons, as for someone to be in a labor union they must be employed
- Education institute is frequently missing 
- Family under 18 is frequently missing

For some of these, the reasons can be seen from a cursory glance at the data:

* education_institute captures whether someone has gone to high school, college/university, or none. This information is already captured with the variable education, and this information is superfluous as a result.
* Similarly to the above, under_18_family is primarily captured with the household_summary variable.

From here, we will look to examine our remaining question regarding missing data, and take a closer look at our residence information and our seemingly inverse correlation between
sunbelt migration and permanency of residence.

Beginning with our residence information:
```r
df %>%
  count(across(c(old_residence_state, old_residence_reg, residence_1_year_ago), is.na))
```

| old_residence_state | old_residence_reg | residence_1_year_ago | n |
| ------------------- | ----------------- | -------------------- | - |
| FALSE | FALSE | FALSE| 1646 |
| TRUE | TRUE | FALSE | 8340 |
| TRUE | TRUE | TRUE | 10093 |

From the above we can see there is a high correlation between missing data for all our aged residence categories, but most prominently for old residence state and region.
This would makese sense, inuitively, as it is unlikely region could be pinpointed if state information was unavailable.

Looking at whether an observation was within their current residence 1 year ago and their prior migration status to the sunbelt"
| old_residence_state | old_residence_reg | n |
| ------------------- | ----------------- | - |
| FALSE | FALSE | 1646 |
| FALSE | TRUE | 8340 |
| TRUE | FALSE | 10093 |

Once again, the relationship is supported by our table view, in which only a small proportion (~8.2%) of both responses are not missing. Otherwise, when one is missing the other is not.
Several conclusions may be drawn from this, and it would be potentially worth regressing the two to determine if this correlation has a signficant relationship.
