##### FUNCTIONS

NormalizeIndustry <- function(){
  if(grepl('TRADE', OTIF_dataC$SoldTo.Industry.L1)==TRUE){
    OTIF_dataC$SoldTo.Industry.L1 <- 'TRADE'
  }else{ 
    OTIF_dataC$SoldTo.Industry.L1 <- 'INDUSTRY'
  }
  return(OTIF_dataC$SoldTo.Industry.L1)
}



Mapping<- function(dt1, dt2, dt1.key, dt2.key, dt2.output){ 
  
   if(dt1.key==dt2.key) {   
      
     names(dt2)[which(names(dt2)==dt2.key)]=paste0("new.",dt2.key) 
     dt2.key = paste0("new.",dt2.key) 
      
   } 
    
   if(sum(names(dt1)==dt2.output)!=0) { 
      
     names(dt2)[which(names(dt2)==dt2.output)]=paste0("new.",dt2.output) 
     dt2.output = paste0("new.",dt2.output) 
      
   } 
    
    
   dt2 <-  unique(dt2[,c(dt2.output,dt2.key), with=F]) 
    
   dt1 <- merge(dt1, dt2, by.x=dt1.key, by.y=dt2.key, all.x = T) 
   unmatched  <- dt1[ is.na(get(dt2.output)), .N, by=dt1.key] 
    
   if(length(unmatched$N)>0) { 
     cat(paste0("\nUnmatech:\n",paste(unmatched[[dt1.key]], unmatched$N, sep = " -> ", collapse="\n"))) 
     stop("Missing dimensions") 
   } 
    
   dt1[[dt1.key]] = NULL 
    
   dt1 
    
} 


GetDelayReason <- function(){
  DelayReasonAggr <- data.table(read.csv(file = "Normalization/DelayReasonAggr.csv"))
  setnames(DelayReasonAggr, 'MaterialGroup5Aggr','DelayReason')
  return(DelayReasonAggr)
}







Nweekdays <- Vectorize(
  function(a, b) 
  {
    #This function return the total number of business days between two dates
    ifelse(a < b, 
           return(sum(!weekdays(seq(a, b, "days")) %in% c("Saturday", "Sunday"), na.rm = TRUE) - 1), 
           return(sum(!weekdays(seq(b, a, "days")) %in% c("Saturday", "Sunday"), na.rm = TRUE) - 1))
  })



GetStrSummary <- function(DT, Column){
  print("Uniques: ")
  print(DT[, .N, by = Column]) #2 Factors
  anyNA <- any(is.na(DT$Column))
  anyBlank <- nrow(data.table(DT)[Column==''])
  print(paste0("Any NA value: " , anyNA))
  print(paste0("Any Blank value: " , anyBlank))
}


MaterialGroup5 <- c('MaterialGroup5'
                  ,'Not Assigned'
                  ,'CR - Lack of Credit'
                  ,'IC - Customer Request'
                  ,'CE - Pick Up by the Customer'
                  ,'AP - Delay of Planning/Production'
                  ,'AD - Anticipation Delivery'
                  ,'X6 - goods not supplied-warehouse'
                  ,'XK - Availability'
                  ,'CG - Composition of Load'
                  ,'IP Lack of Impregnated Paper'
                  ,'QL - Quality Problem'
                  ,'RP - Lack of Dry Paper'
                  ,'CN - Waits Order of Load'
                  ,'SP - Downtime production'
                  ,'ES - Stock strategy'
                  ,'XX - Order entry error'
                  ,'XH - load features (weight,size...)'
                  ,'R - Transportation'
                  ,'EC - Partial Delivery'
                  ,'FM - Lack of Raw Materials'
                  ,'LP Lack of particle boards'
                  ,'XR - Commercial Priority'
                  ,'complete delivery AP'
                  ,'X1 - NOT LOADED-LOADING CAPACITY'
                  ,'AA - Delay on Pricing Approval'
                  ,'##X9 - LACK OF SPACE-LOAD PREPARATION')

MaterialGroup5Aggr <- c('MaterialGroup5Aggr'
                        ,'Others Not Classified'
                        ,'Credit'
                        ,'Customer'
                        ,'Customer'
                        ,'Planning/Production'
                        ,'Planning/Production'
                        ,'Planning/Production'
                        ,'Planning/Production'
                        ,'Transport'
                        ,'Planning/Production'
                        ,'Planning/Production'
                        ,'Planning/Production'
                        ,'Transport'
                        ,'Planning/Production'
                        ,'Planning/Production'
                        ,'Others'
                        ,'Transport'
                        ,'Transport'
                        ,'Transport'
                        ,'Planning/Production'
                        ,'Planning/Production'
                        ,'Planning/Production'
                        ,'Transport'
                        ,'Planning/Production'
                        ,'Others'
                        ,'Planning/Production')

SoldArea          <- c( 'Trade'
                       ,'KZ Natal'
                       ,'Spain'
                       ,'Portugal'
                       ,'France'
                       ,'KeyAccount'
                       ,'Global Acc'
                       ,'Export'
                       ,'Morocco'
                       ,'UK & IRL'
                       ,'SAN'
                       ,'EastEurope'
                       ,'Industry'
                       ,'Gauteng'
                       ,'Freestate'
                       ,'EST Cape'
                       ,'Other'
                       ,'SA Suisse'
                       ,'Inter-comp'
                       ,'WoodForce'
                       ,'WST Cape'
                       ,'Germany')

