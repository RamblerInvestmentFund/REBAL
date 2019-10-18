'''
Author: 7Leven
Purpose: Portfolio Optimization & Risk Analytics

'''

library("sqldf")
library("dplyr")

################# READING sETTINGS #########################

setwd("C:\\Users\\patdj\\Documents\\RIF\\GUI\\")
settings <- read.csv("PORT_TEST.csv")

settings.directory <- settings$Directory[1]
settings.risk_av <- settings$Risk.Aversion[1]
settings.start <- settings$Start.Time[1]
settings.end <- settings$End.Time[1]
settings.period <- settings$Period[1]
settings.ind_min <- settings$Ind.Min[1]
settings.ind_max <- settings$Ind.Max[1]
settings.tickers <- settings$Ticker
settings.groups <- unique(settings$Group)

raw_data <- settings[,1:7]

################# GENERATING WEIGHTS ######################
ticker_query <- 'SELECT "Group", "Group.Type",  Number AS gSTART, Number AS gEND, "Group.Min", "Group.Max", "Weight" FROM raw_data WHERE "Group" = "-"
                UNION
                SELECT "Group", "Group.Type", MIN(Number) AS gSTART, MAX(Number) AS gEND, "Group.Min", "Group.Max", "Weight" FROM raw_data WHERE "Group" != "-" GROUP BY "Group", "Group.Type"
                '
ext_weights <- sqldf(ticker_query)

weight_sub <- function(type, minmax, start, end, weight){
  template.box.min <- "minW[1:2]=3"
  template.box.max <- "maxW[1:2]=3"
  template.sum.min <- "minsumW[1:2]=3"
  template.sum.max <- "maxsumW[1:2]=3"
  if(type == "box" || type == "-"){
    if(minmax == "min"){temp <- template.box.min}
    else{temp <- template.box.max}
  }
  if(type == "sum"){
    if(minmax == "min"){temp <- template.sum.min}
    else{temp <- template.sum.max}
  }
  return(gsub("3",weight,gsub("2",end,gsub("1", start, temp))))
}

#very close, just gotta fix this. 
gen_weights <- function(data, weights){
  for(i in 1:nrow(data)){
    if(data[i,][2] == '-'){
      flat.min <- weight_sub(data[i,][2], "min", data[i,][3], data[i,][4], data[i,][7])
      flat.max <- weight_sub(data[i,][2], "max", data[i,][3], data[i,][4], data[i,][7])
      print(c(flat.min, flat.max))
      #weights <- c(weights, flat.min, flat.max)
    }
    if(data[i,][2] == 'box'){
      if(data[i,][5]!= "-"){
        box.min <- weight_sub(data[i,][2], "min", data[i,][3], data[i,][4], data[i,][5])
        weights <- c(weights, box.min)
      }
      if(data[i,][6]!= "-") {
        box.max <- weight_sub(data[i,][2], "max", data[i,][3], data[i,][4], data[i,][6])
        weights <- c(weights, box.max)  
      }
    }
    if(data[i,][2] == 'sum'){
      if(data[i,][5] != "-"){
        sum.min <- weight_sub(data[i,][2], "min", data[i,][3], data[i,][4], data[i,][5])
        weights <- c(weights, sum.min)
      }
      if(data[i,][6] != "-"){
        sum.max <- weight_sub(data[i,][2], "max", data[i,][3], data[i,][4], data[i,][6])
        weights <- c(weights, sum.max)
      }
    }
  }
  return(weights)
}

gen_weights(ext_weights, c())

