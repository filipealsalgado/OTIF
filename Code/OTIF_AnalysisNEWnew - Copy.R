#Main Key Influencers

########################### LOAD LIBRARIES########################### 

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
library(varhandle)

########################### LOAD FUNCTIONS########################### 
source('Code/functions.R')

########################### READ DATA ########################### 

OTIF19 <- data.table(read.csv(file = "SourceData/OTIF_2019cs.csv"))
OTIF18 <- data.table(read.csv(file = "SourceData/OTIF_2018cs.csv"))

########################### MERGE DATA ########################### 
OTIF <- rbind(OTIF18, OTIF19)

########################### GET A SUMMARY OF DATA ########################### 
summary(OTIF)

OTIF_data <- OTIF
OTIF_data[, .N, by=OTIF..]
OTIF_data$OTIF.. <- factor(OTIF_data$OTIF..)


########################### FILTER COLUMNS########################### 

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

########################### EXPLORATORY DATA ANALYSIS ########################### 
plot_missing(OTIF_dataC)

##### Change Column names to a friendlier names #####
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

##### Normalize Delay Reason #####
OTIF_dataC <- Mapping(OTIF_dataC, GetDelayReason(), "Delay.Reason", "MaterialGroup5", "DelayReason")

# There are NA values
any(is.na(OTIF_dataC))


#Replace NA from OTIF_BIN
OTIF_dataC[OTIF.Bin == '0.0%']$OTIF.Bin          <-  as.factor(0)
OTIF_dataC[OTIF.Bin == '100.0%']$OTIF.Bin        <-  as.factor(1)
OTIF_dataC <- OTIF_dataC[OTIF.Bin %in% c('1','0')]

OTIF_dataC[Early.Bin == '0.0%']$Early.Bin          <-  as.factor(0)
OTIF_dataC[Early.Bin == '100.0%']$Early.Bin        <-  as.factor(1)
OTIF_dataC <- OTIF_dataC[Early.Bin %in% c('1','0')]

#Select only two OTIFS
OTIF_dataC[Production.OTIF.Bin == '0.0%']$Production.OTIF.Bin          <-  as.factor(0)
OTIF_dataC[Production.OTIF.Bin == '100.0%']$Production.OTIF.Bin        <-  as.factor(1)
OTIF_dataC <- OTIF_dataC[Production.OTIF.Bin %in% c('1','0')]

#Select only the necessary OTIFs
OTIF_dataC <- OTIF_dataC[OTIF %in% c('IF','OTIF','NOTIF','OT')]


#####################################################################################

#OTIF.Bin CERTO
OTIF_dataC[, .N, by = OTIF.Bin] #2 Factors
any(is.na(OTIF_dataC$OTIF.Bin))
nrow(OTIF_dataC[OTIF.Bin==''])
class(OTIF_dataC$OTIF.Bin) #FACTOR


#OTIF.Scope CERTO
GetStrSummary(OTIF_dataC, "OTIF.Scope") #Certo 2 
class(OTIF_dataC$OTIF.Scope) #FACTOR

#Doc.Type CERTO
OTIF_dataC[, .N, by = Doc.Type] #7 Factors
any(is.na(OTIF_dataC$Doc.Type)) # FALSE
nrow(OTIF_dataC[Doc.Type=='']) #0
class(OTIF_dataC$Doc.Type) #FACTOR

#SoldTo.Industry.L1
OTIF_dataC[, .N, by = SoldTo.Industry.L1] #2 Factors
any(is.na(OTIF_dataC$SoldTo.Industry.L1)) # FALSE
nrow(OTIF_dataC[SoldTo.Industry.L1=='']) #0
class(OTIF_dataC$SoldTo.Industry.L1) #Character

#Plant
OTIF_dataC[, .N, by = Plant] #37 Factors
any(is.na(OTIF_dataC$Plant)) # FALSE
nrow(OTIF_dataC[Plant=='']) #0
class(OTIF_dataC$Plant) #Factor

#SoldTo.Sales.Area
OTIF_dataC[, .N, by = SoldTo.Sales.Area] #22 Factors
any(is.na(OTIF_dataC$SoldTo.Sales.Area)) # FALSE
nrow(OTIF_dataC[SoldTo.Sales.Area=='']) #0
class(OTIF_dataC$SoldTo.Sales.Area) #Factor

OTIF_dataC[!(SoldTo.Sales.Area %in% SoldArea)]$SoldTo.Sales.Area <- 'Other'

#SoldTo.ReportingGroup
OTIF_dataC[, .N, by = SoldTo.ReportingGroup] #2062 Factors
any(is.na(OTIF_dataC$SoldTo.ReportingGroup)) # FALSE
nrow(OTIF_dataC[SoldTo.ReportingGroup=='']) #0
class(OTIF_dataC$SoldTo.ReportingGroup) #Character

OTIF_dataC$SoldTo.ReportingGroup <- as.character(OTIF_dataC$SoldTo.ReportingGroup)

#Order.Doc
OTIF_dataC[, .N, by = Order.Doc] #98140 Factors
any(is.na(OTIF_dataC$Order.Doc)) # FALSE
nrow(OTIF_dataC[Order.Doc=='']) #0
class(OTIF_dataC$Order.Doc) #character

OTIF_dataC$Order.Doc <- as.character(OTIF_dataC$Order.Doc)

#Order.Item
OTIF_dataC[, .N, by = Order.Item] #108 Factors
any(is.na(OTIF_dataC$Order.Item)) # FALSE
nrow(OTIF_dataC[Order.Item=='']) #0
class(OTIF_dataC$Order.Item) #factor

OTIF_dataC$Order.Item <- as.factor(OTIF_dataC$Order.Item)

#Sales.Org.Code
OTIF_dataC[, .N, by = Sales.Org.Code] #22 Factors
any(is.na(OTIF_dataC$Sales.Org.Code)) # FALSE
nrow(OTIF_dataC[Sales.Org.Code=='']) #0
class(OTIF_dataC$Sales.Org.Code) #factor

OTIF_dataC$Sales.Org.Code <- as.factor(OTIF_dataC$Sales.Org.Code)

#SoldTo.Profitability.Segment
OTIF_dataC[order(SoldTo.Profitability.Segment), .N, by = SoldTo.Profitability.Segment] #4 Factors
any(is.na(OTIF_dataC$SoldTo.Profitability.Segment)) # FALSE
nrow(OTIF_dataC[SoldTo.Profitability.Segment=='']) #0
class(OTIF_dataC$SoldTo.Profitability.Segment) #factor

#Request.Qty.M3
OTIF_dataC[order(Request.Qty.M3), .N, by = Request.Qty.M3] #1111 
any(is.na(OTIF_dataC$Request.Qty.M3)) # FALSE
nrow(OTIF_dataC[Request.Qty.M3=='']) #0
class(OTIF_dataC$Request.Qty.M3) #numeric

OTIF_dataC$Request.Qty.M3 <- as.numeric(gsub(",","",unfactor(OTIF_dataC$Request.Qty.M3))) #funciona pq são todos positivos

#Request.Qty.UoM
OTIF_dataC[order(Request.Qty.UoM), .N, by = Request.Qty.UoM] #3110
any(is.na(OTIF_dataC$Request.Qty.UoM)) # FALSE
nrow(OTIF_dataC[Request.Qty.UoM=='']) #0
class(OTIF_dataC$Request.Qty.UoM) #numeric

OTIF_dataC$Request.Qty.UoM <- as.numeric(gsub(",","",unfactor(OTIF_dataC$Request.Qty.UoM))) #funciona pq são todos positivos e inteiros

#Early.Bin
OTIF_dataC[order(Early.Bin), .N, by = Early.Bin] #2
any(is.na(OTIF_dataC$Early.Bin)) # FALSE
nrow(OTIF_dataC[Early.Bin=='']) #0
class(OTIF_dataC$Early.Bin) #factor

#Grid.Level.1
OTIF_dataC[order(Grid.Level.1), .N, by = Grid.Level.1] #23
any(is.na(OTIF_dataC$Grid.Level.1)) # FALSE
nrow(OTIF_dataC[Grid.Level.1=='']) #0
class(OTIF_dataC$Grid.Level.1) #factor

#Production.OTIF.Bin
OTIF_dataC[order(Production.OTIF.Bin), .N, by = Production.OTIF.Bin] #2
any(is.na(OTIF_dataC$Production.OTIF.Bin)) # FALSE
nrow(OTIF_dataC[Production.OTIF.Bin=='']) #0
class(OTIF_dataC$Production.OTIF.Bin) #factor

#Product.Allocation.Qty.on.MAD..M3
OTIF_dataC[order(Product.Allocation.Qty.on.MAD..M3), .N, by = Product.Allocation.Qty.on.MAD..M3] #4281
any(is.na(OTIF_dataC$Product.Allocation.Qty.on.MAD..M3)) # FALSE
nrow(OTIF_dataC[Product.Allocation.Qty.on.MAD..M3=='']) #0
class(OTIF_dataC$Product.Allocation.Qty.on.MAD..M3) #numeric

OTIF_dataC$Product.Allocation.Qty.on.MAD..M3 <- as.numeric(gsub(",","",unfactor(OTIF_dataC$Product.Allocation.Qty.on.MAD..M3)))

#SoldTo
OTIF_dataC[order(SoldTo), .N, by = SoldTo] #2758
any(is.na(OTIF_dataC$SoldTo)) # FALSE
nrow(OTIF_dataC[SoldTo=='']) #0
class(OTIF_dataC$SoldTo) #character ###DEVO MUDAT PARA FACTOR OU CHARACTER

OTIF_dataC$SoldTo <- as.character(unfactor(OTIF_dataC$SoldTo))

#ShipTo.Country
OTIF_dataC[order(ShipTo.Country), .N, by = ShipTo.Country] #77
any(is.na(OTIF_dataC$ShipTo.Country)) # FALSE
nrow(OTIF_dataC[ShipTo.Country=='']) #0
class(OTIF_dataC$ShipTo.Country) #factor

#Shipping.Condition
OTIF_dataC[order(Shipping.Condition), .N, by = Shipping.Condition] #15
any(is.na(OTIF_dataC$Shipping.Condition)) # FALSE
nrow(OTIF_dataC[Shipping.Condition=='']) #0
class(OTIF_dataC$Shipping.Condition) #factor

#GFFTT
OTIF_dataC[order(GFFTT), .N, by = GFFTT] #15
any(is.na(OTIF_dataC$GFFTT)) # FALSE
nrow(OTIF_dataC[GFFTT=='']) #0
class(OTIF_dataC$GFFTT) 

OTIF_dataC$GFFTT <- unfactor(OTIF_dataC$GFFTT)

#factor - as character

#Material
OTIF_dataC[order(Material), .N, by = Material] #20829
any(is.na(OTIF_dataC$Material)) # FALSE
nrow(OTIF_dataC[Material=='']) #0
class(OTIF_dataC$Material) #character

OTIF_dataC$Material <- as.character(OTIF_dataC$Material)

#Request.Date
OTIF_dataC[order(Request.Date), .N, by = Request.Date] #377
any(is.na(OTIF_dataC$Request.Date)) # FALSE
nrow(OTIF_dataC[Request.Date=='']) #0
class(OTIF_dataC$Request.Date) #factor - will be classified as date

OTIF_dataC$Request.Date        <- as.Date(OTIF_dataC$Request.Date,        format= "%d/%m/%Y")

#Created.Date
OTIF_dataC[order(Created.Date), .N, by = Created.Date] #264
any(is.na(OTIF_dataC$Created.Date)) # FALSE
nrow(OTIF_dataC[Created.Date=='']) #0
class(OTIF_dataC$Created.Date) #factor - will be classified as date

OTIF_dataC$Created.Date        <- as.Date(OTIF_dataC$Created.Date,        format= "%d/%m/%Y")

#X1st.Confirmed.Date
OTIF_dataC[order(X1st.Confirmed.Date), .N, by = X1st.Confirmed.Date] #349
any(is.na(OTIF_dataC$X1st.Confirmed.Date)) # FALSE
nrow(OTIF_dataC[X1st.Confirmed.Date=='']) #0
class(OTIF_dataC$X1st.Confirmed.Date) #factor - will be classified as date

OTIF_dataC$X1st.Confirmed.Date <- as.Date(OTIF_dataC$X1st.Confirmed.Date, format= "%d/%m/%Y")

#Unload.Date.OT
OTIF_dataC[order(Unload.Date.OT), .N, by = Unload.Date.OT] #382
any(is.na(OTIF_dataC$Unload.Date.OT)) # FALSE
nrow(OTIF_dataC[Unload.Date.OT=='']) #3770 1.4 % dos dados totais
class(OTIF_dataC$Unload.Date.OT) #factor - will be classified as date

OTIF_dataC <- OTIF_dataC[!(Unload.Date.OT =='')] #Delete data with no Unload Date

OTIF_dataC$Unload.Date.OT      <- as.Date(OTIF_dataC$Unload.Date.OT,      format= "%d/%m/%Y")

#Date.ReadyPlanning
OTIF_dataC[order(Date.ReadyPlanning), .N, by = Date.ReadyPlanning] #232
any(is.na(OTIF_dataC$Date.ReadyPlanning)) # FALSE
nrow(OTIF_dataC[Date.ReadyPlanning=='']) #56412 22 % dos dados totais
class(OTIF_dataC$Date.ReadyPlanning) #factor - will be classified as date

OTIF_dataC$Date.ReadyPlanning <- as.Date(OTIF_dataC$Date.ReadyPlanning,      format= "%d/%m/%Y")
OTIF_dataC[is.na(Date.ReadyPlanning)]$Date.ReadyPlanning <- as.Date("2000-01-01")

#Delivery.Priority
OTIF_dataC[order(Delivery.Priority), .N, by = Delivery.Priority] #9
any(is.na(OTIF_dataC$Delivery.Priority)) # FALSE
nrow(OTIF_dataC[Delivery.Priority=='']) #0
class(OTIF_dataC$Delivery.Priority) #factor

#OTIF
OTIF_dataC[order(OTIF), .N, by = OTIF] #4
any(is.na(OTIF_dataC$OTIF)) # FALSE
nrow(OTIF_dataC[OTIF=='']) #0
class(OTIF_dataC$OTIF) #factor

#Material.TopReference
OTIF_dataC[, .N, by = Material.TopReference] #1364
any(is.na(OTIF_dataC$Material.TopReference)) # FALSE
nrow(OTIF_dataC[Material.TopReference=='']) #0
class(OTIF_dataC$Material.TopReference) #factor - Também posso por como Character

#Material.TopFinish
OTIF_dataC[, .N, by = Material.TopFinish] #148
any(is.na(OTIF_dataC$Material.TopReference)) # FALSE
nrow(OTIF_dataC[Material.TopReference=='']) #0
class(OTIF_dataC$Material.TopReference) #factor - Também posso por como Character

#Material.Quality
OTIF_dataC[, .N, by = Material.Quality] #17
any(is.na(OTIF_dataC$Material.Quality)) # FALSE
nrow(OTIF_dataC[Material.Quality=='']) #0
class(OTIF_dataC$Material.Quality) #factor - Também posso por como Character

#Order.DeliveryBlock
OTIF_dataC[, .N, by = Order.DeliveryBlock] #2
any(is.na(OTIF_dataC$Order.DeliveryBlock)) # FALSE
nrow(OTIF_dataC[Order.DeliveryBlock=='']) #0
class(OTIF_dataC$Order.DeliveryBlock) #factor

#Doc.PackingType
OTIF_dataC[, .N, by = Doc.PackingType] #452
any(is.na(OTIF_dataC$Doc.PackingType)) # FALSE
nrow(OTIF_dataC[Doc.PackingType=='']) #0
class(OTIF_dataC$Doc.PackingType) #factor -Tbm posso por como Character

#Year
OTIF_dataC[, .N, by = Year] #2
any(is.na(OTIF_dataC$Year)) # FALSE
nrow(OTIF_dataC[Year=='']) #0
class(OTIF_dataC$Year) #factor

OTIF_dataC$Year <- as.factor(OTIF_dataC$Year)

#Month
OTIF_dataC[, .N, by = Month] #12
any(is.na(OTIF_dataC$Month)) # FALSE
nrow(OTIF_dataC[Month=='']) #0
class(OTIF_dataC$Month) #factor

OTIF_dataC$Month <- as.factor(OTIF_dataC$Month)

#Time.Week
OTIF_dataC[, .N, by = Time.Week] #51
any(is.na(OTIF_dataC$Time.Week)) # FALSE
nrow(OTIF_dataC[Time.Week=='']) #0
class(OTIF_dataC$Time.Week) #factor

OTIF_dataC$Time.Week <- as.factor(OTIF_dataC$Time.Week)

#Week
OTIF_dataC[, .N, by = Week] #51
any(is.na(OTIF_dataC$Week)) # FALSE
nrow(OTIF_dataC[Week=='']) #0
class(OTIF_dataC$Week) #factor

OTIF_dataC$Week <- as.factor(OTIF_dataC$Week)

#Date.Type
OTIF_dataC[, .N, by = Date.Type] #3
any(is.na(OTIF_dataC$Date.Type)) # FALSE
nrow(OTIF_dataC[Date.Type=='']) #0
class(OTIF_dataC$Date.Type) #factor

#Avg.Planning.Time
OTIF_dataC[, .N, by = Avg.Planning.Time] #996
any(is.na(OTIF_dataC$Avg.Planning.Time)) # TRUE
nrow(OTIF_dataC[Avg.Planning.Time=='']) #0
class(OTIF_dataC$Avg.Planning.Time) #numeric

OTIF_dataC[is.na(Avg.Planning.Time)]$Avg.Planning.Time <- 9999 #Avg Planing is 9999 for NA. Há alguns valores negativos

#DelayReason
OTIF_dataC[, .N, by = DelayReason] #6  distinct 
any(is.na(OTIF_dataC$DelayReason)) # False
nrow(OTIF_dataC[DelayReason=='']) #0
class(OTIF_dataC$DelayReason) #factor


#Order.Status
OTIF_dataC[, .N, by = Order.Status] #2  distinct 
any(is.na(OTIF_dataC$Order.Status)) # False
nrow(OTIF_dataC[Order.Status=='']) #0
class(OTIF_dataC$Order.Status) #factor

# There are NA values
any(is.na(OTIF_dataC)) # FALSE
plot_missing(OTIF_dataC) #There are no missing values

###################################### WORKING WITH DATES ###############################################

#Put week days (Monday, Tuesday, etc)
OTIF_dataC$Request.Date_WeekDay        <- weekdays(OTIF_dataC$Request.Date       )
OTIF_dataC$Created.Date_WeekDay        <- weekdays(OTIF_dataC$Created.Date       )
OTIF_dataC$X1st.Confirmed.Date_WeekDay <- weekdays(OTIF_dataC$X1st.Confirmed.Date)
OTIF_dataC$Unload.Date.OT_WeekDay      <- weekdays(OTIF_dataC$Unload.Date.OT     ) 
OTIF_dataC$Date.ReadyPlanning_WeekDay  <- weekdays(OTIF_dataC$Date.ReadyPlanning ) 

#Calculate business days' difference between dates
OTIF_dataC$Request.Unload_BusinessDays       <- Nweekdays(a =OTIF_dataC$Unload.Date.OT      , b = OTIF_dataC$Request.Date)
OTIF_dataC$Created.Unload_BusinessDays       <- Nweekdays(a =OTIF_dataC$Created.Date        , b = OTIF_dataC$Request.Date)
OTIF_dataC$X1stConfirmed.Unload_BusinessDays <- Nweekdays(a =OTIF_dataC$X1st.Confirmed.Date , b = OTIF_dataC$Request.Date)
OTIF_dataC$ReadyPlanning.Unload_BusinessDays <- Nweekdays(a =OTIF_dataC$Date.ReadyPlanning  , b = OTIF_dataC$Request.Date)

OTIF_dataC[, .N, by = Request.Date_WeekDay] #7
OTIF_dataC[, .N, by = Created.Date_WeekDay] #7
OTIF_dataC[, .N, by = X1st.Confirmed.Date_WeekDay] #7
OTIF_dataC[, .N, by = Unload.Date.OT_WeekDay] # 7
OTIF_dataC[, .N, by = Date.ReadyPlanning_WeekDay] # 7
OTIF_dataC[, .N, by = Request.Unload_BusinessDays] # 159 distinct
OTIF_dataC[, .N, by = Created.Unload_BusinessDays] #237 distinct
OTIF_dataC[, .N, by = X1stConfirmed.Unload_BusinessDays] # 208 distinct
OTIF_dataC[, .N, by = ReadyPlanning.Unload_BusinessDays] # 166 distinct


#####GGFFTT ######
OTIF_dataC$Group  <- as.factor(substr(OTIF_dataC$GFFTT, start = 0, 1))
OTIF_dataC$Family <- as.factor(substr(OTIF_dataC$GFFTT, start = 2, 3))
OTIF_dataC$Type   <- as.factor(substr(OTIF_dataC$GFFTT, start = 4, 5))

#Create data backup
DATA <- OTIF_dataC

###### Drop Unused Factors
levels(droplevels(DATA$OTIF.Bin))
levels(droplevels(DATA$OTIF.Scope))
levels(droplevels(DATA$Doc.Type))
levels(droplevels(DATA$Plant)) #X
levels(droplevels(DATA$SoldTo.Sales.Area))
levels(droplevels(DATA$Order.Item))
levels(droplevels(DATA$Sales.Org.Code))
levels(droplevels(DATA$SoldTo.Profitability.Segment))
levels(droplevels(DATA$Request.Qty.M3))
levels(droplevels(DATA$Early.Bin))
levels(droplevels(DATA$Grid.Level.1))
levels(droplevels(DATA$Production.OTIF.Bin))
levels(droplevels(DATA$ShipTo.Country))
levels(droplevels(DATA$Shipping.Condition))
levels(droplevels(DATA$GFFTT))


#DATA[, .N, by= DelayReason]
#head(DATA)

featuresToAnalyze <- c("OTIF.Bin", "OTIF.Scope","Doc.Type","SoldTo.Industry.L1","Plant","SoldTo.Profitability.Segment","Request.Qty.M3","Request.Qty.UoM","Early.Bin"
                       ,"Grid.Level.1","Production.OTIF.Bin","Product.Allocation.Qty.on.MAD..M3","Shipping.Condition","Delivery.Priority","OTIF","Material.Quality"
                       ,"Order.DeliveryBlock","Year","Month","Time.Week","Week","Date.Type","Avg.Planning.Time","Order.Status","DelayReason","Request.Date_WeekDay"
                       ,"Created.Date_WeekDay","X1st.Confirmed.Date_WeekDay","Unload.Date.OT_WeekDay","Date.ReadyPlanning_WeekDay","Request.Unload_BusinessDays"
                       ,"Created.Unload_BusinessDays","X1stConfirmed.Unload_BusinessDays","ReadyPlanning.Unload_BusinessDays","Group","Family","Type")

maybe <- c("Sales.Org.Code","GFFTT")
not <- c("SoldTo.Sales.Area","SoldTo.ReportingGroup","Order.Doc","Order.Item","SoldTo","ShipTo.Country","Material","Request.Date","Created.Date","X1st.Confirmed.Date"
         ,"Unload.Date.OT","Material.TopReference","Material.TopFinish","Doc.PackingType","Date.ReadyPlanning")


###### BUILD MODEL ######
# Boruta: Decide if a variable is important or not
library(Boruta)
boruta_output <- Boruta(OTIF.Bin ~ ., data=na.omit(DATA[, featuresToAnalyze, with=F]), doTrace=2) 

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  
