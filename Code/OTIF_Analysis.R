#Main Key Influencers

##### LOAD LIBRARIES #####

library(data.table)
library(RODBC)
library(yaml)
library(ggplot2)
library(forecast)
library(h2o)
library(mlr)
library(randomForest)
library(rTools)
library(e1071) #Check Key Influencers
library(class)
library(DataExplorer)
library(radiant)
library(corrplot)

##### LOAD FUNCTIONS #####
source('Code/functions.R')

##### READ DATA #####

OTIF19 <- data.table(read.csv(file = "SourceData/OTIF_2019cs.csv"))
OTIF18 <- data.table(read.csv(file = "SourceData/OTIF_2018cs.csv"))

##### MERGE DATA #####
OTIF <- rbind(OTIF18, OTIF19)

##### GET A SUMMARY OF DATA #####
summary(OTIF)

OTIF_data <- OTIF
OTIF_data[, .N, by=OTIF..]
OTIF_data$OTIF.. <- factor(OTIF_data$OTIF..)

#Save data to excel to be analized.
#write.csv(x = OTIF_data, file = "OTIF.csv")


##### FILTER COLUMNS #####

#Select Columns to Analize
namesCol <- c('OTIF..','OTIF.Scope','Doc.Type','SoldTo.Industry.L1','Plant'
              ,'SoldTo.Sales.Area','SoldTo.ReportingGroup','Order.Doc'
              ,'Order.Item','Sales.Org.Code','SoldTo.Profitability.Segment'
              ,'Request.Qty.M3','Request.Qty.UoM','Early..','Grid.Level.1'
              ,'Production.OTIF..','Product.Allocation.Qty.on.MAD..M3'
              ,'SoldTo','ShipTo.Country','Shipping.Condition'
              ,'GFFTT','Material','Request.Date','Created.Date','X1st.Confirmed.Date'
              ,'Unload.Date.OT','Delay.Reason','Delivery.Priority','OTIF'
              ,'Material.TopReference','Material.TopFinish','Material.Quality'
              ,'Order.DeliveryBlock','Doc.PackingType','Time.Year','Month...'
              ,'Time.Week','Week...','Date.Type','Date.ReadyPlanning'
              ,'Avg.Planning.Time','Order.Status'
              )

#Filter Dataset to get only the necessary columns
OTIF_dataC <- OTIF_data[, ..namesCol] #41 Columns
length(unique(colnames(OTIF_dataC))) ## 41
length(namesCol) ## 41

##### SIMPLE EDA #####
plot_missing(OTIF_dataC)

##### Change Column names to a friendly names #####
newColNames <- c( 'OTIF.Bin','OTIF.Scope'
                 ,'Doc.Type','SoldTo.Industry.L1'
                 ,'Plant','SoldTo.Sales.Area'
                 ,'SoldTo.ReportingGroup','Order.Doc'
                 ,'Order.Item','Sales.Org.Code'
                 ,'SoldTo.Profitability.Segment','Request.Qty.M3'
                 ,'Request.Qty.UoM','Early.Bin'
                 ,'Grid.Level.1','Production.OTIF.Bin'
                 ,'Product.Allocation.Qty.on.MAD..M3','SoldTo'
                 ,'ShipTo.Country','Shipping.Condition'
                 ,'GFFTT','Material'
                 ,'Request.Date','Created.Date'
                 ,'X1st.Confirmed.Date','Unload.Date.OT'
                 ,'Delay.Reason','Delivery.Priority'
                 ,'OTIF','Material.TopReference'
                 ,'Material.TopFinish','Material.Quality'
                 ,'Order.DeliveryBlock','Doc.PackingType'
                 ,'Year','Month'
                 ,'Time.Week','Week'
                 ,'Date.Type','Date.ReadyPlanning'
                 ,'Avg.Planning.Time','Order.Status'  
                 )

setnames(OTIF_dataC, names(OTIF_dataC), newColNames)

#Remove Order.status = Removed
OTIF_dataC <- OTIF_dataC[ (!Order.Status %in% c('Removed', 'Blocked'))]

##### Normalize Data #####
#Sold to Industry
unique(OTIF_dataC$SoldTo.Industry.L1)
OTIF_dataC$SoldTo.Industry.L1 <- ifelse(grepl("TRADE", OTIF_dataC$SoldTo.Industry.L1), "TRADE", "INDUSTRY")
#INDUSTRY 216250
#TRADE 207551

##### Normalize Delay Reason #####
OTIF_dataC <- Mapping(OTIF_dataC, GetDelayReason(), "Delay.Reason", "MaterialGroup5", "DelayReason")


##### Check Columns #####
OTIF_dataC[, .N, by = OTIF.Bin] # 3 distinct (563 blanks) X
OTIF_dataC[, .N, by = OTIF.Scope] # 2 distinct (Certo)
OTIF_dataC[, .N, by = Doc.Type] # 8 distinct (Certo)
OTIF_dataC[, .N, by = SoldTo.Industry.L1] # 2 distinct (Certo)
OTIF_dataC[, .N, by = Plant]# 39 distinct Certo
OTIF_dataC[, .N, by = SoldTo.Sales.Area] # 23 (26568 Blanks, devo classificar como Others?)
OTIF_dataC[, .N, by = SoldTo.ReportingGroup] #2466 Certo
OTIF_dataC[, .N, by = Order.Doc] #156558 disitnct Certo
OTIF_dataC[, .N, by = Order.Item] # 130 distinct Certo
OTIF_dataC[, .N, by = Sales.Org.Code] #22 distinct Certo
OTIF_dataC[, .N, by = SoldTo.Profitability.Segment] # 4 distinct Certo
OTIF_dataC[, .N, by = Request.Qty.M3] # 1356 distinct certo
OTIF_dataC[, .N, by = Request.Qty.UoM] # 3854 distinct certo
OTIF_dataC[, .N, by = Early.Bin] # 3 (558 blanks - Devo apagar estes registos?)
OTIF_dataC[, .N, by = Grid.Level.1] # 23 distincts certo
OTIF_dataC[, .N, by = Production.OTIF.Bin] #3 (154363 blanks - Devo apagar estes registos?) 
OTIF_dataC[, .N, by = Product.Allocation.Qty.on.MAD..M3] # 4307 distinct
OTIF_dataC[, .N, by = SoldTo] #3209 distinct
OTIF_dataC[, .N, by = ShipTo.Country] # 84 distinct. Certo
OTIF_dataC[, .N, by = Shipping.Condition] # 15 distinct Certo
OTIF_dataC[, .N, by = GFFTT] # 418 distinct Certo
OTIF_dataC[, .N, by = Material] # 26219 distinct
OTIF_dataC[, .N, by = Request.Date] #599 distinct
OTIF_dataC[, .N, by = Created.Date] #476 distinct 
OTIF_dataC[, .N, by = X1st.Confirmed.Date] # 500 distinct
OTIF_dataC[, .N, by = Unload.Date.OT] # 580 distinct 
OTIF_dataC[, .N, by = Delivery.Priority] #9 distinct
OTIF_dataC[, .N, by = OTIF] # 6 distincts (5 null and 558 NA)
OTIF_dataC[, .N, by = Material.TopReference] #1507 distinct
OTIF_dataC[, .N, by = Material.TopFinish] #162 distinct
OTIF_dataC[, .N, by = Material.Quality] #19 distinct
OTIF_dataC[, .N, by = Order.DeliveryBlock] # 3 distinct
OTIF_dataC[, .N, by = Doc.PackingType] # 483 distinct
OTIF_dataC[, .N, by = Year] #2 distinct  certo
OTIF_dataC[, .N, by = Month] # 12 distinct certo
OTIF_dataC[, .N, by = Time.Week] # 52 distinct certo
OTIF_dataC[, .N, by = Week] # 52 certo
OTIF_dataC[, .N, by = Date.Type] # 3 certo
OTIF_dataC[, .N, by = Date.ReadyPlanning] # 234 distinct certo 
OTIF_dataC[, .N, by = Avg.Planning.Time] #1023 distinct (212130 NA and 172737 0)
OTIF_dataC[, .N, by = DelayReason] #6  distinct Certo
OTIF_dataC[, .N, by = Order.Status]

str(OTIF_dataC)
#Working with dates
#Put in date format all data columns
OTIF_dataC$Request.Date        <- as.Date(OTIF_dataC$Request.Date,        format= "%d/%m/%Y")
OTIF_dataC$Created.Date        <- as.Date(OTIF_dataC$Created.Date,        format= "%d/%m/%Y")
OTIF_dataC$X1st.Confirmed.Date <- as.Date(OTIF_dataC$X1st.Confirmed.Date, format= "%d/%m/%Y")
OTIF_dataC$Unload.Date.OT      <- as.Date(OTIF_dataC$Unload.Date.OT,      format= "%d/%m/%Y")
OTIF_dataC$Date.ReadyPlanning  <- as.Date(OTIF_dataC$Date.ReadyPlanning,  format= "%d/%m/%Y")


#Put week days (Monday, Tuesday, etc)
OTIF_dataC$Request.Date_WeekDay        <- weekdays(OTIF_dataC$Request.Date       )
OTIF_dataC$Created.Date_WeekDay        <- weekdays(OTIF_dataC$Created.Date       )
OTIF_dataC$X1st.Confirmed.Date_WeekDay <- weekdays(OTIF_dataC$X1st.Confirmed.Date)
OTIF_dataC$Unload.Date.OT_WeekDay      <- weekdays(OTIF_dataC$Unload.Date.OT     ) #27995 NA's
OTIF_dataC$Date.ReadyPlanning_WeekDay  <- weekdays(OTIF_dataC$Date.ReadyPlanning ) #212130 NA's


head(OTIF_dataC)
#Days of between dates
#Calculate Work days
OTIF_dataC$Request.Unload_BusinessDays       <- Nweekdays(a =OTIF_dataC$Unload.Date.OT      , b = OTIF_dataC$Request.Date)
OTIF_dataC$Created.Unload_BusinessDays       <- Nweekdays(a =OTIF_dataC$Created.Date        , b = OTIF_dataC$Request.Date)
OTIF_dataC$X1stConfirmed.Unload_BusinessDays <- Nweekdays(a =OTIF_dataC$X1st.Confirmed.Date , b = OTIF_dataC$Request.Date)
OTIF_dataC$ReadyPlanning.Unload_BusinessDays <- Nweekdays(a =OTIF_dataC$Date.ReadyPlanning  , b = OTIF_dataC$Request.Date)


#Test
#write.csv(OTIF_dataC, file = "OTIF2PBI.csv")


#Put nulls as NA
is.null(OTIF_dataC$OTIF.Bin)
str(OTIF_dataC)

#Replace NA from OTIF_BIN
OTIF_dataC[OTIF.Bin == '0.0%']$OTIF.Bin          <-  as.factor(0)
OTIF_dataC[OTIF.Bin == '100.0%']$OTIF.Bin        <-  as.factor(1)
OTIF_dataC[!(OTIF.Bin %in% c('1','0'))]$OTIF.Bin <-  as.factor('NA')

OTIF_dataC[Early.Bin == '0.0%']$Early.Bin          <-  as.factor(0)
OTIF_dataC[Early.Bin == '100.0%']$Early.Bin        <-  as.factor(1)
OTIF_dataC[!(Early.Bin %in% c('1','0'))]$Early.Bin <-  as.factor('NA')

OTIF_dataC[Production.OTIF.Bin == '0.0%']$Production.OTIF.Bin          <-  as.factor(0)
OTIF_dataC[Production.OTIF.Bin == '100.0%']$Production.OTIF.Bin        <-  as.factor(1)
OTIF_dataC[!(Production.OTIF.Bin %in% c('1','0'))]$Production.OTIF.Bin <-  as.factor('NA')

OTIF_dataC[!(OTIF %in% c('IF','OTIF','NOTIF','OT'))]$OTIF          <-  as.factor('NA')



OTIF_dataC[, .N, by = Request.Date_WeekDay] # 7 distinct Certo
OTIF_dataC[, .N, by = Created.Date_WeekDay] # 7 distinct Certo
OTIF_dataC[, .N, by = X1st.Confirmed.Date_WeekDay] # 7 distinct Certo
OTIF_dataC[, .N, by = Unload.Date.OT_WeekDay] # 8 distinct (27995 <NA>)
OTIF_dataC[, .N, by = Date.ReadyPlanning_WeekDay] # 8 distinct (212130 <NA>)
OTIF_dataC[, .N, by = Request.Unload_BusinessDays] # 159 distinct
OTIF_dataC[, .N, by = Created.Unload_BusinessDays] #237 distinct
OTIF_dataC[, .N, by = X1stConfirmed.Unload_BusinessDays] # 208 distinct
OTIF_dataC[, .N, by = ReadyPlanning.Unload_BusinessDays] # 166 distinct


#Check for null values
any(is.na(OTIF_dataC$OTIF.Bin))
any(is.na(OTIF_dataC$OTIF.Scope))
any(is.na(OTIF_dataC$Doc.Type))
any(is.na(OTIF_dataC$SoldTo.Industry.L1))
any(is.na(OTIF_dataC$Plant))
any(is.na(OTIF_dataC$SoldTo.Sales.Area)) #Há blanks
any(is.na(OTIF_dataC$SoldTo.ReportingGroup)) #Há blanks
any(is.na(OTIF_dataC$Order.Doc))
any(is.na(OTIF_dataC$Order.Item))
any(is.na(OTIF_dataC$Sales.Org.Code))
any(is.na(OTIF_dataC$SoldTo.Profitability.Segment))
any(is.na(OTIF_dataC$Request.Qty.M3))
any(is.na(OTIF_dataC$Request.Qty.UoM))
any(is.na(OTIF_dataC$Early.Bin))
any(is.na(OTIF_dataC$Grid.Level.1))
any(is.na(OTIF_dataC$Production.OTIF.Bin))
any(is.na(OTIF_dataC$Product.Allocation.Qty.on.MAD..M3))
any(is.na(OTIF_dataC$SoldTo))
any(is.na(OTIF_dataC$ShipTo.Country))
any(is.na(OTIF_dataC$Shipping.Condition))
any(is.na(OTIF_dataC$GFFTT))
any(is.na(OTIF_dataC$Material))
any(is.na(OTIF_dataC$Request.Date))
any(is.na(OTIF_dataC$Created.Date))
any(is.na(OTIF_dataC$X1st.Confirmed.Date))
any(is.na(OTIF_dataC$Unload.Date.OT)) # TRUE
any(is.na(OTIF_dataC$Delivery.Priority))
any(is.na(OTIF_dataC$OTIF))
any(is.na(OTIF_dataC$Material.TopReference))
any(is.na(OTIF_dataC$Material.TopFinish))
any(is.na(OTIF_dataC$Material.Quality))
any(is.na(OTIF_dataC$Order.DeliveryBlock))
any(is.na(OTIF_dataC$Doc.PackingType)) # 21011 Blanks
any(is.na(OTIF_dataC$Year))
any(is.na(OTIF_dataC$Month))
any(is.na(OTIF_dataC$Time.Week))
any(is.na(OTIF_dataC$Week))
any(is.na(OTIF_dataC$Date.Type))
any(is.na(OTIF_dataC$Date.ReadyPlanning)) # TRUE
any(is.na(OTIF_dataC$Avg.Planning.Time)) #True
any(is.na(OTIF_dataC$DelayReason))


#set Blanks factors from all factor columns to NA
indx <- which(sapply(OTIF_dataC, is.factor))
for (j in indx) set(OTIF_dataC, i = grep("^$|^ $", OTIF_dataC[[j]]), j = j, value = NA_integer_) 

#Set Blanks characters from all characters columns to NA
indx2 <- which(sapply(OTIF_dataC, is.character)) 
for (j in indx2) set(OTIF_dataC, i = grep("^$|^ $", OTIF_dataC[[j]]), j = j, value = NA_character_)


OTIF_dataC[is.na(SoldTo.Sales.Area)]$SoldTo.Sales.Area         <- as.factor('NA')
OTIF_dataC[is.na(SoldTo.ReportingGroup)]$SoldTo.ReportingGroup <- as.factor('NA')
OTIF_dataC[is.na(Doc.PackingType)]$Doc.PackingType             <- as.factor('NA')
OTIF_dataC[is.na(Date.ReadyPlanning)]$Date.ReadyPlanning       <- as.factor('NA')

#For all blank unload dates, put x1st confirmed date

#Change columns type

str(OTIF_dataC)


write.csv(OTIF_dataC, file = "OTIF2PBI.csv")














#Possible algorithms for classification
#Naive Bayes Classifier:
e1071::naiveBayes(OTIF.Bin ~., data = OTIF_dataC)
#Key-Nearest Neighbours
class::knn(train, test, cl, k = 1, l=0, prob = FALSE, use.all = TRUE)
#Logistic Regression
stats::glm(OTIF.Bin ~., family = binomial(link = 'logit'), data = OTIF_dataC)
#Support Vector Machines
e1071::svm(formula, data=NULL,...)