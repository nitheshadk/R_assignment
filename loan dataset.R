library(readr)
library(ggplot2)
library(tidyr)
library(plyr)
library(dplyr)
library(lattice)
library(tidyverse)
library(Hmisc)


df <- read_csv("loan_test.csv")
View(df)
summary(df)
colnames(df)
n_distinct(df$Area)
count(df$Area)
df$income<-(df$Applicant_Income+df$Coapplicant_Income)

#Histogram
histogram(~Applicant_Income, data=df, breaks = 100)
histogram(~Coapplicant_Income, data=df, breaks = 50)
histogram(~Loan_Amount, data=df, breaks = 50)
histogram(~Loan_Amount|Married,data = df,breaks = 100)
histogram(~Loan_Amount|Education,data = df,breaks = 100)
histogram(~Loan_Amount|Self_Employed,data = df,breaks = 100)
histogram(~Loan_Amount|Credit_History,data = df,breaks = 100)
histogram(~Loan_Amount|Area,data = df,breaks = 50)

histogram(~income,data = df,breaks = 100)
histogram(~income|Married,data = df,breaks = 100)
histogram(~income|Education,data = df,breaks = 100)
histogram(~income|Self_Employed,data = df,breaks = 100)
histogram(~income|Credit_History,data = df,breaks = 100)
histogram(~income|Area,data = df,breaks = 50)

#subset
urban=subset(df, Area=='Urban')
semiurban=subset(df, Area=='Semiurban')
rural=subset(df, Area=='Rural')
female=subset(df,Gender=="Female")
male=subset(df,Gender=='male')

summary(urban)
count(df$Education)
count(df$Gender)
count(df$Credit_History)
range(female$income)
count(df$Term)

#bar chart
df_income <- gather(df, key ='income1', value = "value", 'income','Loan_Amount')

ggplot(data = df_income, aes(x = Gender, y = value, fill = income1)) +
  geom_bar(stat = "identity", position = position_dodge())+theme(axis.text.x = element_text(angle = 90))

ggplot(data = df_income, aes(x =Education, y = value, fill = income1)) +
  geom_bar(stat = "identity", position = position_dodge())+theme(axis.text.x = element_text(angle = 90))

ggplot(data = df_income, aes(x =Area, y = value, fill = income1)) +
  geom_bar(stat = "identity", position = position_dodge())+theme(axis.text.x = element_text(angle = 90))

ggplot(data = df_income, aes(x =Self_Employed, y = value, fill = income1)) +
  geom_bar(stat = "identity", position = position_dodge())+theme(axis.text.x = element_text(angle = 90))

ggplot(data = df_income, aes(x =Married, y = value, fill = income1)) +
  geom_bar(stat = "identity", position = position_dodge())+theme(axis.text.x = element_text(angle = 90))

ggplot(data = df_income, aes(x =Credit_History, y = value, fill = income1)) +
  geom_bar(stat = "identity", position = position_dodge())+theme(axis.text.x = element_text(angle = 90))

#boxplot
library(ggplot2)
boxplot(Loan_Amount~Area,data = df,title="loan vs area")
boxplot(Loan_Amount~Education,data = df,title="loan vs Education")
boxplot(Loan_Amount~Dependents,data = df,title="loan vs area")
boxplot(Loan_Amount~df$Married,data = df,title="loan vs area")
boxplot(Loan_Amount~df$Gender,data = df,title="loan vs area")

#scatterplot

plot(df$Loan_Amount,df$Applicant_Income)
plot(df$Loan_Amount,df$Coapplicant_Income)
plot(x=df$Loan_Amount,y=df$income)
