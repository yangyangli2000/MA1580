# Setup RStudio environment:
setwd("C:\\Users\\liyan\\Desktop\\MA1580\\Assessment 3\\YangYang_Li_Assessment_3")
# Libraries
#~~~~~~~~~~
library(dplyr)
library(ggplot2)
library(gridExtra)

# Import data
bank = read.csv(file = "Data 8/bank-full.csv",header = T, sep = ";" )

# Preparation
summary(bank)
str(bank)
dim(bank)
is.na(bank)

# Remove unwanted data
new_bank = bank %>% select(-c(day,month,pdays,poutcome))
sum(bank$balance<0)
filtered_bank = new_bank %>% filter(balance > 0) %>% filter(balance < 10000)
 
# Subsampling:
grouped_bank = filtered_bank %>% group_by(education) 
summarise(grouped_bank, n.fren = n())
sub_bank = grouped_bank %>% sample_frac(size = 0.1, replace = F)

# Transform variables
rscl_01 = function(x)(x-min(x))/(max(x)-min(x))
rscl_bank = sub_bank %>% mutate(rscl_balance = rscl_01(balance))

# Explore and Visualise the 
bank_yes = rscl_bank %>% filter(y == "yes")
bank_no = rscl_bank %>% filter(y == "no")

# Balance histogram
ggyesBalance = ggplot(bank_yes, aes(balance)) + geom_histogram(col = "black",fill ="red") + 
  labs(title = "Term Deposits Yes by Balance", x="Balance", y="Number of Yes")  +  xlim(0,3000) 
ggnoBalance = ggplot(bank_no, aes(balance)) + geom_histogram(col = "black",fill = "blue") + 
  labs(title = "Term Deposits No by Balance", x="Balance", y="Number of No")+  xlim(0,3000) 
grid.arrange(ggyesBalance, ggnoBalance, ncol = 2)

# Duration Boxplot
ggplot(rscl_bank, aes(duration,y)) +geom_boxplot(aes(col = y)) + xlim(0,1000) +
  labs(title = "Box pot of Yes and no by Duration", x="Duration", y="Term deposit chance")
# Job vs Duration
ggplot(rscl_bank, aes(job)) + geom_bar(aes(size = duration, fill = job )) + 
  labs(title = "Job vs Duration", y="Duration", x="Job") + coord_flip()
# education vs balance
ggplot(rscl_bank, aes(education, balance)) + geom_jitter(aes(col = education)) + 
  labs(title = "Education vs Balance", y="Balance", x="Education")

# Maritial distubtion
ggplot(rscl_bank, aes(y,marital)) + geom_jitter(aes(col = y, shape = y )) + 
  labs(title = "Term Deposits by maritial", y="Maritial", x="Density")

# Previous Boxplot
ggplot(rscl_bank, aes(previous,y)) +geom_boxplot(aes(col = y)) + xlim(0,5) +
  labs(title = "Box pot of Yes and no by Previous", x="Previous", y="Term deposit chance")




# Age histogram
ggyesAge = ggplot(bank_yes, aes(age)) + geom_histogram(col = "black",fill ="red") + 
  labs(title = "Term Deposits Yes by Age", x="Age", y="Number of Yes")
ggnoAge = ggplot(bank_no, aes(age)) + geom_histogram(col = "black",fill = "blue") + 
  labs(title = "Term Deposits No by Age", x="Age", y="Number of No")
grid.arrange(ggyesAge, ggnoAge, ncol = 2)
ggplot(rscl_bank,aes(age))+geom_bar(aes(fill=y)) +
  labs(title = "Bar plot of Yes and No by Age", x="Age", y="Number of No")
# This chart makes me think that age probably doesn't have much to do with our predicted value

# Education distubtion
ggplot(rscl_bank, aes(y, education)) + geom_jitter(aes(col = y, shape = y )) + 
  labs(title = "Term Deposits by education", y="Education", x="Density")
ggplot(rscl_bank,aes(y))+geom_bar(aes(fill=education))
ggplot(rscl_bank,aes(education))+geom_bar(aes(fill=y)) + labs(title = "Term Deposits by education", x="Education", y="Frequency")
#This again doesn't look all that meaninful. Relatively speaking, the "yes's" and the "No's" seem to line up
#relatively evenly across educational levels

# Job distubtion
ggplot(rscl_bank, aes(y, job)) + geom_jitter(aes(col = y, shape = y )) + 
  labs(title = "Term Deposits by Job", y="Job", x="Density")
#Here again the relative pattern appears about the same across job types. So job type may not be a good predictor of Y

# Maritial distubtion
ggplot(rscl_bank, aes(y,marital)) + geom_jitter(aes(col = y, shape = y )) + 
  labs(title = "Term Deposits by maritial", y="Maritial", x="Density")
#No obvious relationship... again

# Default distubtion
ggplot(rscl_bank, aes(y, default)) + geom_jitter(aes(col = y, shape = y )) + 
  labs(title = "Term Deposits by default", y="Default", x="Density")
#Maybe there is something here, but it doesn't look like a home-run

# Balance histogram
ggyesBalance = ggplot(bank_yes, aes(balance)) + geom_histogram(col = "black",fill ="red") + 
  labs(title = "Term Deposits Yes by Balance", x="Balance", y="Number of Yes")  +  xlim(0,3000) 
ggnoBalance = ggplot(bank_no, aes(balance)) + geom_histogram(col = "black",fill = "blue") + 
  labs(title = "Term Deposits No by Balance", x="Balance", y="Number of No")+  xlim(0,3000) 
grid.arrange(ggyesBalance, ggnoBalance, ncol = 2)
ggplot(rscl_bank,aes(rscl_balance))+geom_bar(aes(col=y)) +
  labs(title = "Bar plot of Yes and No by Balance", x="Balance", y="Frequency")
#Yeah, even term deposits doesn't seem to have much effect on the predicted

# Housing distubtion
ggplot(rscl_bank, aes(y, housing)) + geom_jitter(aes(col = y, shape = y )) + 
  labs(title = "Term Deposits by housing", y="Housing", x="Density")
#That doesn't look strong either

# Loan distubtion
ggplot(rscl_bank, aes(y, loan)) + geom_jitter(aes(col = y, shape = y )) + 
  labs(title = "Term Deposits by loan", y="Loan", x="Density")
# or that....

# Contact distubtion
ggplot(rscl_bank, aes(y, contact)) + geom_jitter(aes(col = y, shape = y )) + 
  labs(title = "Term Deposits by contact", y="Contact", x="Density")
# or that :(

# Duration
ggyesDuration = ggplot(bank_yes, aes(duration)) + geom_histogram(col = "red",fill ="red", binwidth = 10) + 
  labs(title = "Term Deposits Yes by Duration", x="Duration", y="Number of Yes")
ggnoDuration = ggplot(bank_no, aes(duration)) + geom_histogram(col = "blue",fill = "blue", binwidth = 10) + 
  labs(title = "Term Deposits No by Duration", x="Duration", y="Number of No")
grid.arrange(ggyesDuration, ggnoDuration, ncol =2)



ggplot(rscl_bank, aes(balance, y)) +geom_boxplot(aes(col = y))+ xlim(0,2500) +
  labs(title = "Box pot of Yes and no by Balance", x="Balance", y="Term deposit chance") 

#finally, something interesting. Unfortunately this doesn't really 
#help anyone to know, obviously, the longer the prospect is on the call the higher the probability that they will say yes...

# Campaign
ggyesCampaign = ggplot(bank_yes, aes(campaign)) + geom_histogram(col = "red",fill ="red",binwidth = 5) + 
  labs(title = "Term Deposits Yes by number of campaigns", x="Campaigns", y="Number of Yes")+ xlim(0,20) 
ggnoCampaign = ggplot(bank_no, aes(campaign)) + geom_histogram(col = "blue",fill = "blue",binwidth = 5) +
  labs(title = "Term Deposits No by number of campaigns", x="Campaigns", y="Number of No") + xlim(0,20)
grid.arrange(ggyesCampaign, ggnoCampaign, ncol =2)

# Previous
ggyesPrevious = ggplot(bank_yes, aes(previous)) + geom_histogram(col = "red",fill ="red",binwidth = 2) + 
  labs(title = "Term Deposits Yes by number of previous", x="Previous", y="Number of Yes") + xlim(0,15)
ggnoPrevious = ggplot(bank_no, aes(previous)) + geom_histogram(col = "blue",fill = "blue",binwidth = 2) + 
  labs(title = "Term Deposits No by number of previous", x="Previous", y="Number of No") + xlim(0,15)
grid.arrange(ggyesPrevious, ggnoPrevious)
#This is also somewhat interesting, as the first graph has a much longer "tail"







ggplot(bank,aes(y))+geom_bar(aes(fill=job))
ggplot(samp_bank2, aes(y,job)) + geom_jitter(aes(color = y))
ggplot(samp_bank2, aes(y,balance)) + geom_boxplot()
ggplot(samp_bank2) + geom_histogram(mapping = aes(education))
ggplot(sub_bank, aes(balance)) + geom_histogram(col = "black",fill ="red") + 
  labs(title = "Term Deposits Yes by balance", x="", y="Number of Yes")
ggplot(rscl_bank, aes(rscl_balance)) + geom_histogram(col = "black",fill = "blue")
ggplot(bank, aes(age,y)) + geom_boxplot()
ggplot(sub_bank, aes(education,y)) + geom_jitter(aes(color = y))


his.c1 = ggplot(data = cut1)
his.c1 = his.c1 + geom_histogram(mapping = aes(price), col = "black", fill = "red", bins = 100) + theme_bw()

his.c2 = ggplot(data = cut2)
his.c2 = his.c2 + geom_histogram(mapping = aes(price), col = "black", fill = "red", bins = 100) + theme_bw()

his.c3 = ggplot(data = cut3)
his.c3 = his.c3 + geom_histogram(mapping = aes(price), col = "black", fill = "red", bins = 100) + theme_bw()

gridExtra::grid.arrange(his.c1,his.c2,his.c3,ncol = 3)


