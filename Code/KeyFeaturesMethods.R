#http://r-statistics.co/Variable-Selection-and-Importance-With-R.html

##### RANDOM FOREST METHOD #####

inputData <- read.csv("SourceData/ozone1.csv", stringsAsFactors=F)

#Random forest can be very effective to find a set of predictors that best explains the variance in the response variable.

library(party)
cf1 <- cforest(OTIF.Bin ~ . , data= OTIF100, control=cforest_unbiased(mtry=2,ntree=50)) 

varimp(cf1) # get variable importance, based on mean decrease in accuracy

varimp(cf1, conditional=TRUE)  # conditional=True, adjusts for correlations between predictors

varimpAUC(cf1)  # more robust towards class imbalance.


##### RELATIVE IMPORTANCE ######
#Using calc.relimp {relaimpo}, the relative importance of variables fed into a lm model can be determined as a relative percentage.
library(relaimpo)
lmMod <- lm(OTIF.Bin ~ . , data = OTIF100)  # fit lm() model
relImportance <- calc.relimp(lmMod, type = "lmg", rela = TRUE)  # calculate relative importance scaled to 100
sort(relImportance$lmg, decreasing=TRUE)  # relative importance



##### MARS #####
library(earth)
marsModel <- earth(ozone_reading ~ ., data=inputData) # build model
ev <- evimp (marsModel) # estimate variable importance
plot(ev)


##### STEP-WISE REGRESSION ##### FOR LARGE NUMBER OF PREDICTORS
base.mod <- lm(ozone_reading ~ 1 , data= inputData)  # base intercept only model
all.mod <- lm(ozone_reading ~ . , data= inputData) # full model with all predictors
stepMod <- step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = "both", trace = 0, steps = 1000)  # perform step-wise algorithm
shortlistedVars <- names(unlist(stepMod[[1]])) # get the shortlisted variable.
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"]  # remove intercept 
print(shortlistedVars) 


##### BORUTA #####
#The ‘Boruta’ method can be used to decide if a variable is important or not.

library(Boruta)
# Decide if a variable is important or not using Boruta
boruta_output <- Boruta(ozone_reading ~ ., data=na.omit(inputData), doTrace=2)  # perform Boruta search
# Confirmed 10 attributes: Humidity, Inversion_base_height, Inversion_temperature, Month, Pressure_gradient and 5 more.
# Rejected 3 attributes: Day_of_month, Day_of_week, Wind_speed.
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  # plot variable importance



###### Information value and Weight of evidence ###### 
#The InformationValue package provides convenient functions to compute weights of evidence and information value for categorical variables.
library(devtools)
install_github("selva86/InformationValue")

library(InformationValue)
inputData <- read.csv("http://rstatistics.net/wp-content/uploads/2015/09/adult.csv")
head(inputData)


factor_vars <- c ("WORKCLASS", "EDUCATION", "MARITALSTATUS", "OCCUPATION", "RELATIONSHIP", "RACE", "SEX", "NATIVECOUNTRY")  # get all categorical variables
all_iv <- data.frame(VARS=factor_vars, IV=numeric(length(factor_vars)), STRENGTH=character(length(factor_vars)), stringsAsFactors = F)  # init output dataframe
for (factor_var in factor_vars){
  all_iv[all_iv$VARS == factor_var, "IV"] <- InformationValue::IV(X=inputData[, factor_var], Y=inputData$ABOVE50K)
  all_iv[all_iv$VARS == factor_var, "STRENGTH"] <- attr(InformationValue::IV(X=inputData[, factor_var], Y=inputData$ABOVE50K), "howgood")
}

all_iv <- all_iv[order(-all_iv$IV), ]  # sort

for(factor_var in factor_vars){
  inputData[[factor_var]] <- WOE(X=inputData[, factor_var], Y=inputData$ABOVE50K)
}






##### Apply Information value and Weight of evidence to OTIF
OTIF100 <-head(OTIF_dataC, n =100000)
str(OTIF_dataC)
factor_vars <- c ("OTIF.Scope", "Doc.Type","SoldTo.Industry.L1", "Plant", "SoldTo.Sales.Area", "SoldTo.Profitability.Segment", "Early.Bin", "Grid.Level.1","Production.OTIF.Bin"
                  ,"Production.OTIF.Bin","ShipTo.Country","Shipping.Condition","Delivery.Priority","OTIF","Material.TopFinish","Material.TopReference","Material.Quality"
                  ,"Order.DeliveryBlock","Date.Type", "DelayReason","Date.ReadyPlanning_WeekDay","Created.Unload_BusinessDays", "X1stConfirmed.Unload_BusinessDays","ReadyPlanning.Unload_BusinessDays")  # 
all_iv <- data.frame(VARS=factor_vars, IV=numeric(length(factor_vars)), STRENGTH=character(length(factor_vars)), stringsAsFactors = F)  # init output dataframe

for (factor_var in factor_vars){
  all_iv[all_iv$VARS == factor_var, "IV"] <- InformationValue::IV(X=OTIF_dataC[, ..factor_var], Y=OTIF_dataC$OTIF.Bin)
  all_iv[all_iv$VARS == factor_var, "STRENGTH"] <- attr(InformationValue::IV(X=OTIF_dataC[, ..factor_var], Y=OTIF_dataC$OTIF.Bin), "howgood")
}


all_iv <- all_iv[order(-all_iv$IV), ] 

for(factor_var in factor_vars){
  OTIF_dataC[[factor_var]] <- WOE(X=OTIF_dataC[, ..factor_var], Y=OTIF_dataC$OTIF.Bin)
}
