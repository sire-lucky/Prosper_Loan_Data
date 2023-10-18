
#installing packages and opening libraries
#install.packages("AgroR")
library(AgroR)
library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)
library(skimr)
library(here)
library(janitor)
library(ggrepel)
library(lubridate)
library(ggpubr)
library(tidyverse)

#importing .csv files into Global environment
Loan <- read.csv("C:/Users/DELL/Documents/prosperLoanData.csv")

#taking a look/preview at the data sets
View(Loan)
head(Loan)
str(Loan)
colnames(Loan)
names(Loan)

#checking the numbers of user
n_unique(Loan)

#cleaning and checking for duplicate and blank space
clean_names(Loan)
sum(duplicated(Loan))

is.null(Loan)
sum(is.na(Loan))

#extracting specific columns of interest
loan1 <- Loan %>% select(MemberKey, Term, BorrowerRate, ProsperScore, BorrowerState, 
                         Occupation, EmploymentStatus, EmploymentStatusDuration, IsBorrowerHomeowner, 
                         CreditScoreRangeLower, CreditScoreRangeUpper, IncomeRange, StatedMonthlyIncome, 
                         LoanOriginalAmount, LoanOriginationDate, MonthlyLoanPayment)

#checking the output of the newly created data(loan1) for blank space and n/a
View(loan1)
names(loan1)
n_unique(loan1)
sum(is.na(loan1))

#removing duplicates and N/A
loan1 <- loan1 %>% 
  distinct() %>% 
  drop_na()

#confirming that n/a have been dropped
sum(is.na(loan1))

#renaming columns to ensure same format
loan1 <- rename_with(loan1, tolower)

#transforming loanoriginationdate to date object 
loan1$loanoriginationdate <- ymd_hms(loan1$loanoriginationdate)

#loan1$loandate <- ymd_hms(loan1$loanoriginationdate) to create new column

#extracting month and year from loanoriginationdate column
loan1$month <- month(loan1$loanoriginationdate)

loan1$year <- year(loan1$loanoriginationdate)

#Converting column "incomerange", "month", "year" into ordered(factor)categorical types
loan1$incomerange <- factor(loan1$incomerange)

loan1$month <- factor(loan1$month, ordered = TRUE, levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                      labels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug",
                                 "sep", "oct", "nov", "dec"))

loan1$year <- factor(loan1$year, ordered = TRUE)

str(loan1)

##SHARE
#DATA VISUALIZATION

#using a bar-chart to show the demography of Borrower's
ggplot(loan1, aes(x = borrowerstate, fill = "red")) + 
  geom_bar() +
  labs(title = "Borrower's state",
       x = "state", y = "count") +
  theme_minimal() +
  theme(plot.title = element_text(size = 22, color = "#42a0ab", face = "bold"),
        plot.subtitle = element_text(size = 14, color = "grey20", face = "bold"),
        plot.caption = element_text(size = 8, color = "grey35"),
        axis.title.x = element_text(size = 15, color = "#42a0ab", face = "bold"),
        axis.title.y = element_text(size = 15, color = "#42a0ab", face = "bold"),
        axis.text.x = element_text(angle = 90, size = 10, colour = "grey20", face = "bold"),
        axis.text.y = element_blank(), legend.position = "none")
#*****"CA" California has the highest number of borrowers*******#

#What month is loan most issued?
ggplot(loan1, aes(x = month)) + 
  geom_bar() +
  labs(title = "Month Distribution",
       x = "Month") +
  theme_minimal() +
  theme(plot.title = element_text(size = 22, color = "#42a0ab", face = "bold"),
        plot.subtitle = element_text(size = 14, color = "grey20", face = "bold"),
        plot.caption = element_text(size = 8, color = "grey35"),
        axis.title.x = element_text(size = 15, color = "#42a0ab", face = "bold"),
        axis.title.y = element_text(size = 15, color = "#42a0ab", face = "bold"),
        axis.text.x = element_text(colour = "grey20", face = "bold"))
#*******month with the highest number of borrowers is 1(jan) == January*

#What year has the highest number of borrowers?
ggplot(loan1, aes(x = year, color ="red")) + 
  geom_bar() +
  labs(title = "Year Distribution",
       x = "Year") +
  theme_minimal() +
  theme(plot.title = element_text(size = 22, color = "#42a0ab", face = "bold"),
        plot.subtitle = element_text(size = 14, color = "grey20", face = "bold"),
        plot.caption = element_text(size = 8, color = "grey35"),
        axis.title.x = element_text(size = 15, color = "#42a0ab", face = "bold"),
        axis.title.y = element_text(size = 15, color = "#42a0ab", face = "bold"),
        axis.text.x = element_text(colour = "grey20", face = "bold"),
        axis.text.y = element_blank(), legend.position = "none")
#*******2013 is the year with the highest number of borrowers*

#What is the Loan Amount distribution?
ggplot(loan1, aes(x = loanoriginalamount)) + 
  geom_histogram(binwidth = 1000) +
  labs(title = "Loan Amount Distribution",
       x = "loan amount($)", y = "count")+
  theme_minimal() +
  theme(plot.title = element_text(size = 22, color = "#42a0ab", face = "bold"),
        plot.subtitle = element_text(size = 14, color = "grey20", face = "bold"),
        plot.caption = element_text(size = 8, color = "grey35"),
        axis.title.x = element_text(size = 15, color = "#42a0ab", face = "bold"),
        axis.title.y = element_text(size = 15, color = "#42a0ab", face = "bold"),
        axis.text.x = element_text(colour = "grey20", face = "bold"))
#**majority of borrowers took loans of less than 5k. However, the peak values are 5k, 10k, 15k, 20k, and 25k*


#What is the length of employment distribution?
#using a bar-chart to show the distribution of Employment Duration
ggplot(loan1, aes(x = employmentstatusduration, color = "red")) + 
  geom_histogram(binwidth = 30) +
  labs(title = "Employment Duration Distribution",
                 x = "employment duration(month)", y = "count") +
  theme_minimal() +
  theme(plot.title = element_text(size = 22, color = "#42a0ab", face = "bold"),
        plot.subtitle = element_text(size = 14, color = "grey20", face = "bold"),
        plot.caption = element_text(size = 8, color = "grey35"),
        axis.title.x = element_text(size = 15, color = "#42a0ab", face = "bold"),
        axis.title.y = element_text(size = 15, color = "#42a0ab", face = "bold"),
        axis.text.x = element_text(colour = "grey20", face = "bold"))
#*******majority of the borrowers had worked for less than 100 months*


#What is the loan payment distribution?
ggplot(loan1, aes(x = monthlyloanpayment, color = "red")) + 
  geom_histogram(bins = 20) +
  labs(title = "Payment Distribution",
       x = "payment amount($)", y = "count") +
  theme_minimal() +
  theme(plot.title = element_text(size = 22, color = "#42a0ab", face = "bold"),
        plot.subtitle = element_text(size = 14, color = "grey20", face = "bold"),
        plot.caption = element_text(size = 8, color = "grey35"),
        axis.title.x = element_text(size = 15, color = "#42a0ab", face = "bold"),
        axis.title.y = element_text(size = 15, color = "#42a0ab", face = "bold"),
        axis.text.x = element_text(colour = "grey20", face = "bold"))
#*******majority of the borrowers pay at least 200 dollars monthly, and the highest loan payment is 1250k*


#distribution of upper and lower credit score range
ggarrange(
  ggplot(data = loan1, mapping = aes(x = creditscorerangelower, color="red")) + 
    geom_histogram(binwidth = 20) +
    labs(title = "Lower credit score range", x = "creditscorerangelower", y = "count") +
    theme(panel.background = element_blank(), 
          plot.title = element_text(size = 14, color = "#42a0ab", face = "bold"),
          axis.title.x = element_text(size = 15, color = "#42a0ab", face = "bold"),
          axis.title.y = element_text(size = 15, color = "#42a0ab", face = "bold"),
          axis.text.x = element_text(colour = "grey20", face = "bold")),
  ggplot(data = loan1, mapping = aes(x = creditscorerangeupper, color="red")) + 
    geom_histogram(binwidth = 20) +
    labs(title = "Upper credit score range", x = "creditscorerangeupper", y = "count") +
    theme(panel.background = element_blank(), 
          plot.title = element_text(size = 14, color = "#42a0ab", face = "bold"),
          axis.title.x = element_text(size = 15, color = "#42a0ab", face = "bold"),
          axis.title.y = element_text(size = 15, color = "#42a0ab", face = "bold"),
          axis.text.x = element_text(colour = "grey20", face = "bold")))
#*******similar no different BTW Credit Score Range Lower and Credit Score Range Upper*


#what is the duration of the loan(A histogram-chart showing the distribution of Term)
ggplot(loan1, aes(x = term, fill = "red")) + 
  geom_histogram(bins = 30) +
  labs(title = "Loan Duration",
       x = "Term(month)", y = "count") +
  theme_minimal() +
  theme(plot.title = element_text(size = 22, color = "#42a0ab", face = "bold"),
        plot.caption = element_text(size = 8, color = "grey35"),
        axis.title.x = element_text(size = 15, color = "#42a0ab", face = "bold"),
        axis.title.y = element_text(size = 15, color = "#42a0ab", face = "bold"),
        axis.text.x = element_text(colour = "grey20", face = "bold"),
        axis.text.y = element_blank(), legend.position = "none")
#*******length of time(Term) to pay back the loans for the majority of the borrowers is thirty-six months*


#What is the occupation of majority of the borrower
ggplot(loan1, aes(x = occupation)) + 
  geom_bar() +
  labs(title = "Borrower's Occupation",
       x = "Occupation", y = "count") +
  theme(axis.text.x = element_text(angle = 90))
#*******The paramount occupation of the borrowers is termed other*#


#Plotting two categorical variables(EmploymentStatus and IncomeRange) simultaneously
ggarrange(
  ggplot(data = loan1, mapping = aes(x = incomerange)) + 
    geom_bar() +
    labs(title = "Income(monthly)", x = "incomerange", y = "count") +
    theme_minimal() +
    theme(plot.title = element_text(size = 22, color = "#42a0ab", face = "bold"),
          plot.subtitle = element_text(size = 14, color = "grey20", face = "bold"),
          plot.caption = element_text(size = 8, color = "grey35"),
          axis.title.x = element_text(size = 15, color = "#42a0ab", face = "bold"),
          axis.title.y = element_text(size = 15, color = "#42a0ab", face = "bold"),
          axis.text.x = element_text(angle = 90, size = 10, colour = "grey20", face = "bold"),
          axis.text.y = element_blank(), legend.position = "none"),
  ggplot(data = loan1, mapping = aes(x = employmentstatus)) + 
    geom_bar() +
    labs(title = "Employment Status", x = "employmentstatus", y = "count") +
    theme_minimal() +
    theme(plot.title = element_text(size = 22, color = "#42a0ab", face = "bold"),
          plot.subtitle = element_text(size = 14, color = "grey20", face = "bold"),
          plot.caption = element_text(size = 8, color = "grey35"),
          axis.title.x = element_text(size = 15, color = "#42a0ab", face = "bold"),
          axis.title.y = element_text(size = 15, color = "#42a0ab", face = "bold"),
          axis.text.x = element_text(angle = 90, size = 10, colour = "grey20", face = "bold"),
          axis.text.y = element_blank(), legend.position = "none"))
#*******The majority of the borrowers have an IncomeRange of 50k - 74.999k*
#*******While the EmploymentStatus of most of the borrowers is Employed*


#correlation between Loan vs Monthly Income
loan1 %>% 
  select(statedmonthlyincome, loanoriginalamount) %>% 
  summary()

attach(loan1)
plot_cor(statedmonthlyincome, loanoriginalamount,
         method = "pearson",
         ylab = "Loan Amount", xlab = "Monthly Income",
         theme = theme_classic(), pointsize = 2,
         shape = 20, fill = "black", color = "red",
         axis.size = 12, ic = TRUE, title = "correlation Btw Loan & Monthly Income")

ggplot(data = loan1, mapping = aes(x = statedmonthlyincome, y = loanoriginalamount)) +
  geom_point() +
  labs(title = "correlation Btw Loan & Monthly Income", x = "Monthly Income", y = "Loan Amount") +
  theme(panel.background = element_blank(), plot.title = element_text(size = 14))
#*******The majority of the borrowers earning around 10k accessed a loan amount way above their monthly Income*


#correlation between LoanOriginalAmount vs MonthlyLoanPayment
attach(loan1)
plot_cor(monthlyloanpayment, loanoriginalamount,
         method = "pearson",
         ylab = "loan amount", xlab = "Monthly Income",
         theme = theme_classic(), pointsize = 2,
         shape = 20, fill = "black", color = "blue",
         axis.size = 12, ic = TRUE, title = "correlation Btw LoanOriginalAmount & MonthlyLoanPayment")
#*******the borrowers are committed and consistent in loan payment,*
#*******a strong positive relationship between Loan Amount and Monthly Loan Payment*


#correlation between Loan Amount and IncomeRange
ggplot(data = loan1, mapping = aes(x = incomerange, y = loanoriginalamount)) +
  geom_col(stat="identity", position = "dodge") +
  labs(title = "correlation Btw Loan & Incomerange", x = "Incomerange", y = "Loan Amount") +
  theme(panel.background = element_blank(), plot.title = element_text(size = 14))
#*******borrowers with Incomerange of 100k+ got access to a higher amount of loan*
#**The higher your IncomeRange the higher the loan amount granted*

# investigate the relationship between two ordered(factor)categorical variables
ggarrange(
  ggplot(data = loan1, mapping = aes(x = incomerange, fill = isborrowerhomeowner)) +
    geom_bar() +
    labs(title = "correlation Btw incomerange & 
      isborrowerhomeowner", x = "Incomerange", y = "isborrowerhomeowner")+
    theme(axis.text.x = element_text(angle = 90),
          panel.background = element_blank(), plot.title = element_text(size = 14)),
  ggplot(data = loan1, mapping = aes(x = employmentstatus, fill = isborrowerhomeowner)) +
    geom_bar() +
    labs(title = "EmploymentStatus &
IsBorrowerHomeowner", x = "employmentstatus", y = "isborrowerhomeowner") +
    theme(axis.text.x = element_text(angle = 90),
          panel.background = element_blank(), plot.title = element_text(size = 14)))
#*******majority of the borrowers are employed and they own their homes*
#*******while most of them earn 50k-74.999*


#correlation between Loan Amount and occupation
ggplot(data = loan1, mapping = aes(x = occupation, y = loanoriginalamount)) +
  geom_histogram(stat="identity", position = "dodge") +
  labs(title = "correlation Btw Loan Amount & occupation", x = "occupation", y = "Loan Amount") +
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_blank(), plot.title = element_text(size = 14))


#what is the monthly income distribution?
ggplot(loan1, aes(x = statedmonthlyincome, y = 1:nrow(loan1))) +
  geom_bar(stat = "identity", position = "dodge")

