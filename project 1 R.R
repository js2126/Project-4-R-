data_a <- read.csv('data_1.csv')

print(colnames(data_a))
nrow(data_a) #28053 rows
ncol(data_a) #23 columns
data_a <- data_a[data_a$Please.indicate.the.currency=="GBP",]
print(unique(data_a$What.country.do.you.work.in.))
data_a <- data_a[data_a$What.country.do.you.work.in. %in% c("United Kingdom","UK","Scotland","England","Uk","Great Britain","Northern Ireland","England/UK","England, UK.","Britain","United Kingdom (England)","United Kingdom.","United kingdom","U.K.","United Kindom","England, UK","uk","UK (Northern Ireland)","UK for U.S. company","Remote","United Kingdomk","united kingdom","Wales (United Kingdom)","England, Gb","U.K. (northern England)","U.K","England, United Kingdom","Englang","Wales","UK (England)","UK, remote","Scotland, UK","Unites kingdom","england","Wales, UK","Wales (UK)","Northern Ireland, United Kingdom","UK, but for globally fully remote company","ENGLAND"),]




#install.packages('ggplot2')
#install.packages('tidyr')
library('ggplot2')
library('tidyr')
library('scales')
library('plyr')

age_data <- data_a["How.old.are.you."]
names <- unique(age_data)
names <- names[1:7,]
print(names)

a <- table(age_data)
a #figure 1

barplot(a) #most respondents are between 25-44
#figure 2


#What is the median salary by age excluding bonuses?
ag_in <- data_a[,c("How.old.are.you.","What.is.your.annual.salary...You.ll.indicate.the.currency.in.a.later.question..If.you.are.part.time.or.hourly..please.enter.an.annualized.equivalent....what.you.would.earn.if.you.worked.the.job.40.hours.a.week..52.weeks.a.year..","Please.indicate.the.currency")]
colnames(ag_in) <- c("Age", "Salary","Currency")
print(ag_in)

ag_fa <- ag_in
ag_fa$Age <- as.factor(ag_fa$Age)
class(ag_fa$Age)
class(ag_fa$Salary) #character change to numeric
ag_fa$Salary <- gsub("[,]","",ag_fa$Salary)
sum(is.na(ag_fa$Salary))

ag_fa$Salary <- as.numeric(ag_fa$Salary)
class(ag_fa$Salary)

str(ag_fa)

options(scipen = 999)

ag_med <- ddply(ag_fa, .(Age), summarize, med=median(Salary))
print(ag_med)

ag_plt <- ggplot(ag_fa, aes(x=Age,y=Salary)) +
  geom_boxplot() +
  geom_text(ag_med, mapping=aes(x=Age,y=med, label=round(med,2)), size = 3, vjust=-1.5,colour="Blue") +
  labs(y="Salary (GBP)",x="Age range") +
  ggtitle("Salary by Age Range")
ag_plt #figure 3
#the salaries (excluding bonus) of respondents are well above the UK average salary

#What is the race of the respondents?
race <- data_a["What.is.your.race...Choose.all.that.apply.."]
colnames(race) <- c("Race")

race_table <- table(race)
print(race_table) #vast majority are white

#conclusion:
# Most of the respondents were white, between 25-34 years old and earned well above the median UK salary

