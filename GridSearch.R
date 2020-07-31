## Grid Search
#In our grid search we examine how minibatch and dataset sizes affect performance of algorithms.
#Dataset sizes we test are full dataset (N=1), 10% of dataset (N=0.1) and 1% respectively (N=0.01).
#Minibatch sizes we test are relative to the data input, that are 1% of the data input (m=0.01) and 0.1% respectively (m=0.001).
#We examine this on SGLD, SGLD-CV, SGHMC, SGHMC-CV.
#This results in 2*3*4=24 combinations. We run three repetitions.

#Before running below lines, please do the following:
#Open update.R again, uncomment line 158 ("times <<- ..."), and rerun that script.
#Then continue here below.

#do this after inital loading to store 10k away for later testing 
testInd = sample(nrow(dataset$X), 10^4) 
testset = list( "X" = dataset$X[testInd,], "y" = dataset$y[testInd] ) 
dataset = list( "X" = dataset$X[-testInd,], "y" = dataset$y[-testInd] )

#store all results of the grid search in "testresults" (~700MB)
testresults=list()

#repeat grid search three times to create average results later
for (i in 1:3){
  
  #iterate over three different sizes of data input
  for (datasetsize in c(1,0.1,0.01)){
    
    N <- floor(datasetsize * length(dataset$y))
    sampleInd = sample(nrow(dataset$X), N) 
    datasample = list( "X" = dataset$X[sampleInd,], "y" = dataset$y[sampleInd] ) 
    
    # iterate over two different minibatchsizes per dataset size 
    for (minibatchsize in c(0.01, 0.001)){
      
      # run algorithms
      
      #### SGLD ####
      name <- paste("sgldRep",i,",m",minibatchsize,",N",datasetsize, sep = "")
      times <- c()
      starttime <- proc.time()
      tmp <- sgld( logLik, dataset = datasample, params, stepsize = 2e-5, logPrior = logPrior, minibatchSize = minibatchsize, nIters = 40000)
      testresults[[name]] <- tmp
      testresults[[name]]$times <- times
      
      #### SGLDCV ####
      name <- paste("sgldcvRep",i,",m",minibatchsize,",N",datasetsize, sep = "")
      times <- c()
      starttime <- proc.time()
      tmp <- sgldcv( logLik, dataset = datasample, params, stepsize = 5e-6, optStepsize = 5e-6, logPrior = logPrior, minibatchSize = minibatchsize, nIters = 40000)
      testresults[[name]] <- tmp
      testresults[[name]]$times <- times
      
      #### SGHMC ####
      
      name <- paste("sghmcRep",i,",m",minibatchsize,",N",datasetsize, sep = "")
      times <- c()
      starttime <- proc.time()
      tmp <- sghmc(logLik, dataset = datasample, params, stepsize = 5e-7, logPrior = logPrior, minibatchSize = minibatchsize, nIters = 8000)
      testresults[[name]] <- tmp
      testresults[[name]]$times <- times
      
      #### SGHMCCV ####
      
      name <- paste("sghmccvRep",i,",m",minibatchsize,",N",datasetsize, sep = "")
      times <- c()
      starttime <- proc.time()
      tmp <- sghmccv(logLik, dataset = datasample, params, stepsize = 1e-6, optStepsize = 1e-6, logPrior = logPrior, minibatchSize = minibatchsize, nIters = 8000)
      testresults[[name]] <- tmp
      testresults[[name]]$times <- times
    }
  }
}

#Now we have the object "testresults" with results for all 24 combinations, three times each.
#It will be used in PlottingForReport.R , please proceed to that file. 