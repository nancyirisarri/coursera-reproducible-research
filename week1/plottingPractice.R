#Make a plot that answers the question: what is the relationship between mean 
#covered charges (Average.Covered.Charges) and mean total payments 
#(Average.Total.Payments) in New York?

library(dplyr)

loadAndReadData <- function(fileName) {
  read.csv(fileName, header=TRUE)
}

makePlot1 <- function(data) {
  dataNy <- data %>% filter(Provider.State == "NY")
  
  pdf(file="plot1.pdf")

  smoothScatter(log10(dataNy$Average.Covered.Charges), log10(dataNy$Average.Total.Payments), 
              xlab="Log of Mean Covered Charges ($)", ylab="Log of Mean Total Payments ($)", 
              main="Relationship between Charges and Payments in New York")

  dev.off()
}

#Make a plot (possibly multi-panel) that answers the question: how does the relationship 
#between mean covered charges (Average.Covered.Charges) and mean total payments 
#(Average.Total.Payments) vary by medical condition (DRG.Definition) and the state 
#in which care was received (Provider.State)?

makeDataPlot2 <- function(data) {
  # By medical condition (DRG.Definition) 
  # mean covered charges (Average.Covered.Charges) and mean total payments (Average.Total.Payments).
  dataDrg <- data %>% group_by(DRG.Definition) %>% summarize(
    charges = sum(Average.Covered.Charges), payments = sum(Average.Total.Payments))
  
  # By the state in which care was received (Provider.State) 
  # mean covered charges (Average.Covered.Charges) and mean total payments (Average.Total.Payments)
  dataState <- data %>% group_by(Provider.State) %>% summarize(
    charges = sum(Average.Covered.Charges), payments = sum(Average.Total.Payments))
  
  return(list(dataDrg, dataState))
}
  
makePlot2 <- function(data) {
  dataDrg <- data[[1]]
  
  dataState <- data[[2]]
  
  pdf(file="plot2.pdf")
  
  par(mfrow = c(2, 2), mar = c(6, 6, 6, 6), oma = c(0, 0, 2, 0))
  
  with(dataDrg, {
    hist(charges, xlab="Average Covered Charges ($)", ylab="Frequency", 
         main="Average Covered Charges")
    
    hist(payments, xlab="Average Total Payments ($)", ylab="Frequency", 
         main="Average Total Payments")
    
    mtext("Variation by Medical Condition", outer=TRUE)
  })
  
  with(dataState, {
    hist(charges, xlab="Average Covered Charges ($)", ylab="Frequency", 
         main="Average Covered Charges")

    hist(payments, xlab="Average Total Payments ($)", ylab="Frequency", 
         main="Average Total Payments")
    
    mtext("Variation by State of Care", side=3, line=-21, outer=TRUE)
  })
  
  dev.off()
}

doAll <- function() {
  data <- loadAndReadData("_e143dff6e844c7af8da2a4e71d7c054d_payments.csv")
  
  makePlot1(data)
  
  dataPlot2 <- makeDataPlot2(data)
  
  makePlot2(dataPlot2)
}