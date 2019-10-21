'
Author: 7Leven
Purpose: Portfolio Optimization & Risk Analytics

'

library("sqldf")
library("dplyr")
library("data.table")

################# READING sETTINGS #########################

setwd("C:\\Users\\patdj\\Documents\\RIF\\GUI\\v1")
settings <- read.csv("RIF_PORT.csv")

settings.directory <- as.character(settings$Directory[1])
settings.risk_av <- settings$Risk.Aversion[1]
settings.start <- gsub("/", "-",as.character(settings$Start.Time[1]))
settings.end <-  gsub("/", "-", as.character(settings$End.Time[1]))
settings.period <- as.character(settings$Period[1])
settings.ind_min <- settings$Ind.Min[1]
settings.ind_max <- settings$Ind.Max[1]
settings.tickers <- as.character(settings$Ticker)
settings.groups <- as.character(unique(settings$Group))

raw_data <- settings[,1:7]

################# GENERATING WEIGHTS ######################
# This is a SQL query that helps format and extract the weights from 
# the excel document. 
ticker_query <- '
                SELECT "Group", "Group.Type",  Number AS gSTART, Number AS gEND, "Group.Min", "Group.Max", "Weight" FROM raw_data WHERE "Group" = "-"
                UNION
                SELECT "Group", "Group.Type", MIN(Number) AS gSTART, MAX(Number) AS gEND, "Group.Min", "Group.Max", "Weight" FROM raw_data WHERE "Group" != "-" GROUP BY "Group", "Group.Type"
                '
settings.ext_weights <- sqldf(ticker_query) # running sql query and extracting weights

# Input:  type - Constraint Type (Box, Sum, Flat) - read fPortfolio documentation for specifics
#         minmax - lower or upper constraint designation
#         start - which security # does the group start at
#         end   - which security # does the group end at
#         weight - group/flat weight
# Output: outputs a formated string that is input into the optimization tool. 
# Description:
# Basically this function does all the formatting that once needed to be done by hand. 
# You input which security #'s need to be grouped, the group weight, and the type of constraint
# and the function will output the string needed to be input into the model to do what you want.
# This script outputs one at a time. We will loop through all the groups and input them into this function later.
weight_sub <- function(type, minmax, start, end, weight){
  template.box.min <- "minW[!:@]=#"
  template.box.max <- "maxW[!:@]=#"
  template.sum.min <- "minsumW[!:@]=#"
  template.sum.max <- "maxsumW[!:@]=#"
  if(type == "box" || type == "-"){
    if(minmax == "min"){temp <- template.box.min}
    else{temp <- template.box.max}
  }
  if(type == "sum"){
    if(minmax == "min"){temp <- template.sum.min}
    else{temp <- template.sum.max}
  }
  return(gsub("#",weight,gsub("@",end,gsub("!", start, temp))))
}

#Inputs: data - basically we input a data frame of groups/flat weights for the function to iterate through
#        weights - if you have a list of prewritten weights you can input them here and this function 
#                   will append the newly generated weights onto your pre-created ones. 
#                   if you don't have any prewritten weights, just input c(). 
gen_weights <- function(data, weights=c()){
  for(i in 1:nrow(data)){
    if(data[i,2] == '-'){
      flat.min <- weight_sub(data[i,2], "min", data[i,3], data[i,4], data[i,7])
      flat.max <- weight_sub(data[i,2], "max", data[i,3], data[i,4], data[i,7])
      weights <- c(weights, flat.min, flat.max)
    }
    else if(data[i,2] == 'box'){
      if(data[i,5]!= "-"){
        box.min <- weight_sub(data[i,2], "min", data[i,3], data[i,4], data[i,5])
        weights <- c(weights, box.min)
      }
      if(data[i,6]!= "-") {
        box.max <- weight_sub(data[i,2], "max", data[i,3], data[i,4], data[i,6])
        weights <- c(weights, box.max)
      }
    }
    else if(data[i,2] == 'sum'){
      if(data[i,5] != "-"){
        sum.min <- weight_sub(data[i,2], "min", data[i,3], data[i,4], data[i,5])
        weights <- c(weights, sum.min)
      }
      if(data[i,6] != "-"){
        sum.max <- weight_sub(data[i,2], "max", data[i,3], data[i,4], data[i,6])
        weights <- c(weights, sum.max)
      }
    }
  }
  allMin <- weight_sub("box", "min", 1, length(settings.tickers), settings.ind_min)
  allMax <- weight_sub("box", "max", 1, length(settings.tickers), settings.ind_max)
  weights <- c(weights, allMin, allMax)
  return(weights)
}

settings.weights <- gen_weights(settings.ext_weights)
