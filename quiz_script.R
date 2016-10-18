## Creating analytic data file

## Load libraries

library(tidyverse)
library(apaTables)

##Load data
bfi_data<-psych::bfi

str(bfi_data)
# View(bfi_data)

categorical_variables <- select(bfi_data, gender, education, age)

categorical_variables$gender <- as.factor(categorical_variables$gender)
levels(categorical_variables$gender) <- list("Male"=1, "Female"=2)


str(categorical_variables)
## View(categorical_variables)

agreeableness_items <- select(bfi_data, A1, A2, A3, A4, A5)
extraversion_items <- select(bfi_data, E1, E2, E3, E4, E5)
neuroticism_items <- select(bfi_data, N1, N2, N3, N4, N5)

# Reverse key
agreeableness_items <- mutate(agreeableness_items,A1=6-A1)
extraversion_items <- mutate(extraversion_items,E1=6-E1)
extraversion_items <- mutate(extraversion_items,E2=6-E2)

## View(extraversion_items)
## View(agreeableness_items)

## To obtain scale scores:
agreeableness <- psych::alpha(as.data.frame(agreeableness_items),check.keys=FALSE)$scores
extraversion <- psych::alpha(as.data.frame(extraversion_items),check.keys=FALSE)$scores
neuroticism <- psych::alpha(as.data.frame(neuroticism_items),check.keys=FALSE)$scores

analytic_data <- cbind(agreeableness, extraversion, neuroticism, categorical_variables)

# View(analytic_data)

write_csv(analytic_data,path="analytic_data.csv")

str(analytic_data)

analytic_data

# View(analytic_data)

## Assignment part 2

## Create data sets without gender
analytic_data_part2 <- select(analytic_data, -gender)
## View(analytic_data_part2)

## Create 1st correlation table
apa.cor.table(analytic_data_part2,filename="Table1.doc",table.number=1)

## Corr Table Men 40+
analytic_data_male40 <- filter(analytic_data, gender=="Male")
analytic_data_male40 <- analytic_data %>% filter(age>40) %>% select(-gender)
## View(analytic_data_male40)
apa.cor.table(analytic_data_male40,filename="Table2.doc",table.number=2)

## Save data sets
write_csv(analytic_data_male40,path="analytic_data_male40.csv")
write_csv(analytic_data_part2,path="analytic_data_part2.csv")

## Create Agree-Extra Scatter Plot
my.scatter <- qplot(x=agreeableness,y=extraversion,data=analytic_data_male40)
my.scatter <- my.scatter + geom_smooth(method = "lm", se = FALSE, color='black')
my.scatter <- my.scatter + labs(title="Agreeableness and Extraversion for Men Over the Age of 40",
                                x="Agreeableness",y="Extraversion")
my.scatter <- my.scatter + theme_classic()
my.scatter <- my.scatter + theme(axis.line.x = element_line(colour = 'black',size=0.5, linetype='solid'),
                                 axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

print(my.scatter)
ggsave(filename="Figure1.pdf", plot=my.scatter, width=6,height=6, units="in")

