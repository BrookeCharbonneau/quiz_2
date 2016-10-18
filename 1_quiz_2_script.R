## Creating analytic data file

library(tidyverse)
library(apaTables)

raw_data <- read_csv(file="raw_data.csv")

str(raw_data)
# View(raw_data)

raw_data$sex <- as.factor(raw_data$sex)
levels(raw_data$sex) <- list("Male"=1, "Female"=2)

sex <- select(raw_data, sex)
neg_affect_items <- select(raw_data, afraid, angry, anxious, ashamed)
pos_affect_items <- select(raw_data, delighted, elated, enthusiastic, excited)
Neuroticism <- select(raw_data, Neuroticism)
Extraversion <- select(raw_data, Extraversion)

psych::describe(neg_affect_items)
is_bad_value <- neg_affect_items<0 | neg_affect_items>3
neg_affect_items[is_bad_value] <- NA
# View(neg_affect_items)

psych::describe(pos_affect_items)
is_bad_value <- pos_affect_items<0 | pos_affect_items>3
pos_affect_items[is_bad_value] <- NA
# View(pos_affect_items)

is_bad_value <- Neuroticism<0 | Neuroticism>24
Neuroticism[is_bad_value] <- NA

is_bad_value <- Extraversion<0 | Extraversion>24
Extraversion[is_bad_value] <- NA

psych::describe(Neuroticism)
psych::describe(Extraversion)

## To obtain scale scores:
pos_affect <- psych::alpha(as.data.frame(pos_affect_items),check.keys=FALSE)$scores
neg_affect <- psych::alpha(as.data.frame(neg_affect_items),check.keys=FALSE)$scores

analytic_data <- cbind(sex,pos_affect,neg_affect,Neuroticism, Extraversion)

# View(analytic_data)

write_csv(analytic_data,path="analytic_data.csv")

str(analytic_data)

analytic_data

# View(analytic_data)