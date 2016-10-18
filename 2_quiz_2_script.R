library (tidyverse)
library (apaTables)

## Create 3 filtered data sets
analytic_data.male <- filter(analytic_data, sex=="Male")
analytic_data_male <- select(analytic_data.male, pos_affect, neg_affect, Neuroticism, Extraversion)
analytic_data.female <- filter(analytic_data, sex=="Female")
analytic_data_female <- select(analytic_data.female, pos_affect,neg_affect, Neuroticism, Extraversion)
# View(analytic_data_male)
# View(analytic_data_female)

## Save data sets
write_csv(analytic_data_male,path="analytic_data_male.csv")
write_csv(analytic_data_female,path="analytic_data_female.csv")

## Create 3 correlation tables
apa.cor.table(analytic_data,filename="Table_1_Overall.doc",table.number=1)
apa.cor.table(analytic_data_male,filename="Table_2_Male.doc",table.number=2)
apa.cor.table(analytic_data_female,filename="Table_3_Female.doc",table.number=3)

## Create 3 correlation figures
psych::pairs.panels(as.data.frame(analytic_data),lem=TRUE)
psych::pairs.panels(as.data.frame(analytic_data_male),lem=TRUE)
psych::pairs.panels(as.data.frame(analytic_data_female),lem=TRUE)

## Create Neuroticism Histogram
my.hist.neur <- ggplot(analytic_data_female,aes(Neuroticism))
my.hist.neur <- my.hist.neur + geom_histogram(aes(y=..count..), binwidth = 1, fill="black",color="black")
my.hist.neur <- my.hist.neur + labs(title="Female Neuroticism Histogram",x="Neuroticism Level",y="Frequency")
my.hist.neur <- my.hist.neur + coord_cartesian(xlim=c(0,25), ylim = c(0,175))
my.hist.neur <- my.hist.neur + theme_classic()
my.hist.neur <- my.hist.neur + theme(axis.line.x = element_line(colour = 'black',size=0.5, linetype='solid'),
                                     axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
my.hist.neur <- my.hist.neur + scale_y_continuous(breaks = seq(0,175,by=25), expand = c(0,0))
print(my.hist.neur)
ggsave(filename="Figure_4_Neuroticism_Histogram_Female.tiff", plot=my.hist.neur, width=6,height=6, units="in")

## Create Neg_affect Histogram
my.hist.neg <- ggplot(analytic_data_female,aes(neg_affect))
my.hist.neg <- my.hist.neg + geom_histogram(aes(y=..count..), binwidth = .25, fill="black",color="black")
my.hist.neg <- my.hist.neg + labs(title="Female Negative Affect Histogram",x="Negative Affect Level",y="Frequency")
my.hist.neg <- my.hist.neg + theme_classic()
my.hist.neg <- my.hist.neg + theme(axis.line.x = element_line(colour = 'black',size=0.5, linetype='solid'),
                                   axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
my.hist.neg <- my.hist.neg + scale_x_continuous(breaks = seq(0,3.5,by=.5))
my.hist.neg <- my.hist.neg + scale_y_continuous( expand = c(0,0))
print(my.hist.neg)
ggsave(filename="Figure_5_Neuroticism_Histogram_Female.tiff", plot=my.hist.neg, width=6,height=6, units="in")

## Create NA-Neur Scatter Plot
my.scatter <- qplot(x=neg_affect,y=Neuroticism,data=analytic_data_female)
my.scatter <- my.scatter + geom_smooth(method = "lm", se = FALSE, color='black')
my.scatter <- my.scatter + labs(title="Female Negative Affect Against Neuroticism",x="Negative Affect",y="Neuroticism")
my.scatter <- my.scatter + theme_classic()
my.scatter <- my.scatter + theme(axis.line.x = element_line(colour = 'black',size=0.5, linetype='solid'),
                                 axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

print(my.scatter)
ggsave(filename="Figure_6_NA_Neuroticism_Scatter.tiff", plot=my.scatter, width=6,height=6, units="in")
