library(dplyr)
library(readr)
library(magrittr)
library(reshape2)

Online_Retail_data <- read.csv("C:/Users/Kruthi Tatavarthy/Documents/Masters in Business Analytics/Sem-1/Business Analytics/Online_Retail.csv")

head(Online_Retail_data)

#Question 1: 

attach(Online_Retail_data)

glimpse(Online_Retail_data)

dim(Online_Retail_data)

summary(Online_Retail_data)

# 1 mutate, n(), filter 

count_countries <- Online_Retail_data %>% group_by(Country) %>% summarise(country_txn = n())
total_txns <- nrow(Online_Retail_data) #sum(count_countries$country_txn)
percent <- round(100*(count_countries$country_txn/total_txns), digits=2)
print(subset(cbind(count_countries,percent),percent > 1))

#Question 2:


Online_Retail_data1 <- Online_Retail_data %>% mutate(TransactionValue = Quantity * UnitPrice)
summarise(Online_Retail_data1)
glimpse(Online_Retail_data1)

#Question 3: 


attach(Online_Retail_data1)
new_dataframe <- summarise(group_by(Online_Retail_data1, Online_Retail_data1$Country), sum(TransactionValue))
Transaction_data <- filter(new_dataframe, new_dataframe$`sum(TransactionValue)` >130000)
glimpse(Transaction_data)

#Question 4:

Temp=strptime(Online_Retail_data1$InvoiceDate,format='%m/%d/%Y %H:%M',tz='GMT')
head(Temp)
Online_Retail_data1$New_Invoice_Date <- as.Date(Temp)
Online_Retail_data1$New_Invoice_Date[20000]-Online_Retail_data1$New_Invoice_Date[10]
Online_Retail_data1$Invoice_Day_Week = weekdays(Online_Retail_data1$New_Invoice_Date)
Online_Retail_data1$New_Invoice_Hour = as.numeric(format(Temp, "%H"))
Online_Retail_data1$New_Invoice_Month = as.numeric(format(Temp, "%m"))

# a) Show the percentage of transactions (by numbers) by days of the week 

a_no<-summarise(group_by(Online_Retail_data1,Invoice_Day_Week),Transaction_Value=n_distinct(InvoiceNo))
a_no1<-mutate(a_no, transaction_percent=(Transaction_Value/sum(Transaction_Value))*100)
a_no1


# b) Show the percentage of transactions (by transaction volume) by days of the week

b_no <- summarise(group_by(Online_Retail_data1,Invoice_Day_Week),Transaction_Volume=sum(TransactionValue))
b_no1 <- mutate(b_no,percentage=(Transaction_Volume/sum(Transaction_Volume))*100)
b_no1

# c) Show the percentage of transactions (by transaction volume) by month of the year

c_no<-summarise(group_by(Online_Retail_data1,New_Invoice_Month),Transaction_Volume=sum(TransactionValue))
c_no1<-mutate(c_no,percentage=(Transaction_Volume/sum(Transaction_Volume))*100)
c_no1


#d)What was the date with the highest number of transactions from Australia? 

Online_Retail_data1 %>% filter(Country == 'Australia') %>% group_by(New_Invoice_Date) %>% summarise(max=max(TransactionValue))



# Question 5: 

germany_dataset <- subset(Online_Retail_data1$TransactionValue, Online_Retail_data1$Country == "Germany")
hist(germany_dataset, xlim=c(-400, 600), breaks = 150, xlab="Txns Of Germany", main="Germany")

# Question 6:

Online_Retail_data2 <- na.omit(Online_Retail_data1)
res <- summarise(group_by(Online_Retail_data2,CustomerID), sum2= sum(TransactionValue))
res[which.max(res$sum2),]
#14646 has highest number of transactions 279489
data1 <- as.data.frame(table(Online_Retail_data1$CustomerID))
data1[which.max(data1$Freq),]
#17841 custId has highest nof txns

# Question 7:

colMeans(is.na(Online_Retail_data)*100)

# Question 8: 

intial_result <- Online_Retail_data %>% filter(is.na(CustomerID)) %>% group_by(Country)
glimpse(intial_result$Country)
summary(intial_result$Country)

# Question 9: 

visit <- Online_Retail_data1 %>%
  group_by(InvoiceNo, CustomerID, Country, New_Invoice_Date, New_Invoice_Month, New_Invoice_Hour, Invoice_Day_Week) %>%
  summarise(orderVal = sum(TransactionValue)) %>%
  mutate(recent = Sys.Date() - New_Invoice_Date) %>%
  ungroup()
visit$recent <- as.character(visit$recent)
visit$recentDays <- sapply(visit$recent, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][1]})
visit$recentDays <- as.integer(visit$recentDays)
head(visit, n = 5)
attach(visit)

custVisit <- Online_Retail_data1 %>%
  group_by(CustomerID, Country) %>%
  summarise(orders = n_distinct(InvoiceNo), revenue = sum(TransactionValue), 
            mostDay = names(which.max(table(Invoice_Day_Week))), mostHour = names(which.max(table(New_Invoice_Hour))),
            recency = min(recentDays))%>%
  ungroup()

head(custVisit)

# Question 10: 
france_dataframe <- (filter(Online_Retail_data1,Online_Retail_data1$Country=="France"))
cancelled_df <- nrow(subset(france_dataframe, Online_Retail_data1$TransactionValue < 0))
cancelled_df
not_cancelled_df <- nrow(Online_Retail_data1) - cancelled_df
(cancelled_df/not_cancelled_df)


