fitness.bootsratp.function <- function(inputdata,BootNo){
  # Function performs bootsrapping
  # Inputs:
  #   nputData - data frame
  #   BootNo - integer - number of resampling
  # Output:
  #   bootResults - matrix - contains the y-intercept and the slopes of the 
  
  # Input checks
  if(!is.data.frame(inputdata) || BootNo < 0){
    stop("Invalid arguments")
  }
  
  set.seed(180024815)
  
  bootResults <- array(dim = c(BootNo, 5))
  for(i in 1:BootNo){
    # resample our data with replacement
    bootData <- inputdata[sample(1:31, 100, replace = T),]
    # fit the model under this alternative reality
    bootLM <- lm(Oxygen ~ Age + Weight + RunTime + RunPulse, data = bootData)
    # store the coefs
    bootResults[i, ] <- coef(bootLM)
  }
  bootResults
}

bootResult <- fitness.bootsratp.function(fitness, 1500)