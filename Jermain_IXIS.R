library(dplyr)
library(ggplot2)
library(openxlsx)

### Read in Data
setwd('C:/Users/nwjer/Desktop/job materials/IXIS_Assessment')
addstocart = read.csv('DataAnalyst_Ecom_data_addsToCart.csv')
sesscounts = read.csv('DataAnalyst_Ecom_data_sessionCounts.csv')

### get acquainted with tables
head(addstocart)
head(sesscounts)


###################### data cleaning ######################

### are there any nas
sum(apply(sesscounts, 2, is.nan))
sum(apply(addstocart, 2, is.nan))
sum(apply(sesscounts, 2, is.infinite))

### I dont think you can have sessions of 0 and transactions or QTY
sesscounts %>% filter(transactions>sessions) # there are very few like this and there is no way to determine reasonable replacement for sessions
sesscounts %>% filter(transactions>QTY)%>%head(10) # seems odd that QTY could be less than txns, but maybe returns could cause this

### check that every month has a reasonable number of days and no partial overlap between years
sesscounts %>% group_by(month) %>% summarize(days = n_distinct(mydate)) # looks complete and mutually exclusive




####################### create table 1 #################################
### get month from dates
sesscounts$month = as.integer(format(as.Date(sesscounts$dim_date, '%m/%d/%y'), '%m'))
sesscounts$mydate = as.Date(sesscounts$dim_date, '%m/%d/%y')

### how dispersed are transactions across device types?
tottxns = sum(sesscounts$transactions)
sesscounts%>%group_by(dim_deviceCategory)%>%summarize(perc_txns = sum(transactions)*100/tottxns)

### how are sessions distributed across device types?
totsessions = sum(sesscounts$sessions)
sesscounts%>%group_by(dim_deviceCategory)%>%summarize(perc_sess = sum(sessions)*100/totsessions)


## group by device and month then sum KPIs and add conversion rates
table1 = sesscounts %>% group_by(month,  dim_deviceCategory) %>%
  summarize(mindate = min(mydate), sessions = sum(sessions),txns = sum(transactions),QTY= sum(QTY)) %>%
  mutate(ECR = txns/sessions, QTYpersession = QTY/sessions)%>%arrange(month)

## How does ECR vary by device overall?
sesscounts%>%group_by(dim_deviceCategory)%>%summarize(sess = sum(sessions), txns=sum(transactions))%>%
  mutate(ECR = txns/sess)


## Plot transactions overall 
table1 %>% group_by(mindate)%>% summarize(txns = sum(txns))%>%ggplot(aes(x = mindate, y = txns)) +
  geom_line(size=1.5)+theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(color="black", size=15), axis.title.x=element_blank(),
        axis.text=element_text(color="black", size=12))+ylab('Transactions')

################# Plot each kpi over time by device #######################
# ECR
table1 %>% ggplot(aes(x = mindate, y = ECR, color = dim_deviceCategory, group = dim_deviceCategory)) +
  geom_line(size=1.5)+theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
       axis.title = element_text(color="black", size=15), axis.title.x=element_blank(),
       axis.text=element_text(color="black", size=12))+
  labs(colour = "Device Type")

# Quantity per session
table1 %>% ggplot(aes(x = mindate, y = QTYpersession, color = dim_deviceCategory, group = dim_deviceCategory)) +
  geom_line(size=1.5)+theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(color="black", size=15), axis.title.x=element_blank(),
        axis.text=element_text(color="black", size=12))+
  labs(colour = "Device Type")+ylab('Quantity Per Session')

  
# Sessions
table1 %>% ggplot(aes(x = mindate, y = sessions/1000, color = dim_deviceCategory, group = dim_deviceCategory)) +
  geom_line(size=1.5)+theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(color="black", size=15), axis.title.x=element_blank(),
        axis.text=element_text(color="black", size=12))+ylab('Sessions (thousands)')+
  labs(colour = "Device Type")

# Transactions
table1 %>% ggplot(aes(x = mindate, y = txns, color = dim_deviceCategory, group = dim_deviceCategory)) +
  geom_line(size=1.5)+theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(color="black", size=15), axis.title.x=element_blank(),
        axis.text=element_text(color="black", size=12))+
  labs(colour = "Device Type")+ylab('Transactions')


# Did QTY per txn change?
table1 %>% mutate(QTYperTxn = QTY/txns) %>% ggplot(aes(x = mindate, y = QTYperTxn, color = dim_deviceCategory, group = dim_deviceCategory)) +
  geom_line(size=1.5)+theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(color="black", size=15), axis.title.x=element_blank(),
        axis.text=element_text(color="black", size=12))+
  labs(colour = "Device Type")+ylab('QTYperTxn')
# not by very much




#################### create table 2 ###################################
# extract year from date for join
sesscounts$year = as.integer(format(as.Date(sesscounts$dim_date, '%m/%d/%y'), '%Y'))
head(sesscounts)

# check that latest month has full number of days
length(unique(sesscounts[which(sesscounts$month==6 & sesscounts$year==2013),]$dim_date)) #looks good

## Group by month and year, sum KPIs, join addstocart, and add some rate variables
table2 = sesscounts %>% group_by(month, year) %>% summarise(sessions = sum(sessions),
                                                   transactions = sum(transactions),
                                                   QTY = sum(QTY)) %>%
  left_join(addstocart, by=c("year"="dim_year", "month"="dim_month")) %>%
  mutate(addspersession = addsToCart/sessions,
         QTYperadd = QTY/addsToCart,
         QTYpertxn = QTY/transactions,
         QTYpersession = QTY/sessions,
         ECR = transactions/sessions) %>% arrange(year, month) %>% filter(month>=5 & year==2013)

# transform the dataframe and calculate differences
table2 = t(table2)
colnames(table2) = c('May_2013', 'June_2013')
table2 = as.data.frame(table2)
table2$Absolute_Difference = table2$June_2013 - table2$May_2013
table2$Percent_Difference = ((table2$June_2013 - table2$May_2013)*100)/table2$May_2013
table2$KPI = rownames(table2)
table2 = subset(table2, table2$KPI!='month' & table2$KPI!='year')
table2
######### addstocart declined, investigate more ###################3
table3 = sesscounts %>% group_by(month, year) %>% summarise(sessions = sum(sessions),
                                                            transactions = sum(transactions),
                                                            QTY = sum(QTY),
                                                            mindate = min(mydate)) %>%
  left_join(addstocart, by=c("year"="dim_year", "month"="dim_month")) %>%
  mutate(addspersession = addsToCart/sessions,
         QTYperadd = QTY/addsToCart)

# plot the decline in adds to cart
table3 %>%ggplot(aes(x = mindate, y = addspersession)) +
  geom_line(size=1.5)+theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(color="black", size=15), axis.title.x=element_blank(),
        axis.text=element_text(color="black", size=12))+ylab('Adds Per Session')

# plot the change in quantity purchased to cart adds
table3 %>%ggplot(aes(x = mindate, y = QTYperadd)) +
  geom_line(size=1.5)+theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title = element_text(color="black", size=15), axis.title.x=element_blank(),
        axis.text=element_text(color="black", size=12))+ylab('Quantity Bought per Cart-Add')


############ calculate potential sales benefit from increasing mobile conversion rates ###############
sesscounts%>%group_by(dim_deviceCategory)%>%summarize(sess = sum(sessions), txns=sum(transactions))%>%
  mutate(ECR = txns/sess)
## how many additional transactions would the client get by increasing mobile conversion to equal that of desktop
(3630483*.034)-42872 # a lot
# how many additional total transaction caused by this action
((sum(sesscounts$transactions)-42872+(3630483*.034))-sum(sesscounts$transactions))/sum(sesscounts$transactions)



################### Save tables as xlsx file #####################
wb = createWorkbook()
addWorksheet(wb, sheetName = "Month by Device Aggregation" )
writeData(wb, sheet = "Month by Device Aggregation", table1)
addWorksheet(wb, sheetName = "Recent Month Over Month" )
writeData(wb, sheet = "Recent Month Over Month", table2)
saveWorkbook(wb, file = "N_Jermain_IXIS_Assessment.xlsx", overwrite = TRUE)
