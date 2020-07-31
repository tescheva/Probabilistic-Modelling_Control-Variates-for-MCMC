#### introduction ####
#Model example after:
#Baker, Jack, et al. "sgmcmc: An r package for stochastic gradient markov chain monte carlo." Journal of Statistical Software, 91 (3).
#Logistic Regression for tree cover type classification

#This script shows our preliminary analysis, where we manually ran 6 algorithms,
#i.e. SGLD and SGLD-CV with minibatch sizes of 100, 500 and 1000. 
#The results led us to the idea of a grid search (see Grid Search.R).

#Before running below, all relevant (modified) functions of the R package need
#to be loaded manually: data.R, sgld.R, setup.R, dynamics.R, update.R, storage.R
#Also make sure the right tensorflow package is installed and loaded, e.g. by
#install_tensorflow(version = "1.13.1")

#### data ####
covertype = getDataset("covertype")
X = covertype[,2:ncol(covertype)] #predictors
y = covertype[,1] #response
dataset = list( "X" = X, "y" = y )

#### params ####
d = ncol(dataset$X) #observations are split along first axis (rows=obs)
params = list( "bias" = 0, "beta" = matrix( rep( 0, d ), nrow = d ) ) #bias=intercept, beta=coefficients, initialized to 0

#### logLikelihood function ####
logLik = function(params, dataset) { 
  yEst = 1 / (1 + tf$exp( - tf$squeeze(params$bias + tf$matmul( 
    dataset$X, params$beta)))) 
  logLik = tf$reduce_sum(dataset$y * tf$log(yEst) + 
                           (1 - dataset$y) * tf$log(1 - yEst)) 
  return(logLik) 
}

#### logPriorDensity function ####
logPrior = function(params) { 
  logPrior = - (tf$reduce_sum(tf$abs(params$beta)) + tf$reduce_sum(tf$abs(params$bias))) 
  return(logPrior) 
}

#### stepsize ####
stepsize = list("beta" = 2e-5, "bias" = 2e-5) #for step-size tuning, we are taking these ideal ones

#### computation ####
#run code under "computation" three times each for sgld and sgld-cv, with m=100,500,1000

## Running the SGLD algorithm
#adjust "output_sgld_m100" & "minibatchSize" -> m500, m1000
output_sgld_m100 = sgld( logLik, dataset, params, stepsize, logPrior = logPrior, 
                    minibatchSize = 100, nIters = 5000, seed = 13 ) #variations minibatch size: 100,500,1000

## Running the SGLD-CV algorithm
#adjust "output_sgldcv_m100" & "minibatchSize" -> m500, m1000
set.seed(13) 
testInd = sample(nrow(dataset$X), 10^4) 
testset = list( "X" = dataset$X[testInd,], "y" = dataset$y[testInd] ) 
dataset = list( "X" = dataset$X[-testInd,], "y" = dataset$y[-testInd] )
output_sgldcv_m100 = sgldcv( logLik, dataset, params, 5e-6, 5e-6, logPrior = logPrior, 
                        minibatchSize = 100, nIters = 5000, seed = 13 ) #variations minibatch size: 100,500,1000
output_sgld_m100$beta = output_sgld_m100$beta[-c(1:1000),,] 
output_sgld_m100$bias = output_sgld_m100$bias[-c(1:1000)]

## Compute logLoss
#adjust 2x "output_sgld_m100" -> m500, m1000 -> sgldcv
#adjust 3x "logLoss_sgld_m100" -> m500, m1000 -> sgldcv
iterations = seq(from = 1, to = 5000, by = 10) 
logLoss_sgldcv_m100 = rep(0, length(iterations)) 
for ( iter in 1:length(iterations) ) { 
  j = iterations[iter] 
  beta0_j = output_sgldcv_m100$bias[j] 
  beta_j = output_sgldcv_m100$beta[j,,] 
  for ( i in 1:length(testset$y) ) { 
    piCurr = 1 / (1 + exp(- beta0_j - sum(testset$X[i,] * beta_j))) 
    y_i = testset$y[i] 
    logLossCurr = - (y_i * log(piCurr) + (1 - y_i) * log(1 - piCurr)) 
    logLoss_sgldcv_m100[iter] = logLoss_sgldcv_m100[iter] + 1 / length(testset$y) * logLossCurr 
  } 
} 

#### SGLD vs SGLD-CV, with different minibatch sizes m, 5000 iterations, N1 ####
#Figure 1 in report

#Plot logLoss
library(ggplot2)

#gather results from above
plotFrame = data.frame("Iteration" = iterations, 
                       "logLoss_sgldcv_m100" = logLoss_sgldcv_m100, 
                       "logLoss_sgld_m100" =logLoss_sgld_m100,
                       "logLoss_sgldcv_m500" = logLoss_sgldcv_m500, 
                       "logLoss_sgld_m500" =logLoss_sgld_m500,
                       "logLoss_sgldcv_m1000" = logLoss_sgldcv_m1000, 
                       "logLoss_sgld_m1000" =logLoss_sgld_m1000)

cols = c("SGLD, m=100"="red2","SGLD-CV, m=100"="red4",
         "SGLD, m=500"="gold","SGLD-CV, m=500"="gold3",
         "SGLD, m=1000"="green3","SGLD-CV, m=1000"="darkgreen")

sgldVSsgldcv <- ggplot(plotFrame, aes(x = Iteration)) + 
  geom_line(aes(y=logLoss_sgld_m100, color = "SGLD, m=100"), size=0.3) +
  geom_line(aes(y=logLoss_sgld_m500, color = "SGLD, m=500"), size=0.3) +
  geom_line(aes(y=logLoss_sgld_m1000, color = "SGLD, m=1000"), size=0.3) +
  geom_line(aes(y=logLoss_sgldcv_m100, color = "SGLD-CV, m=100"), size=0.7) +
  geom_line(aes(y=logLoss_sgldcv_m500, color = "SGLD-CV, m=500"), size=0.7) +
  geom_line(aes(y=logLoss_sgldcv_m1000, color = "SGLD-CV, m=1000"), size=0.7) +
  ylab("Log loss of test set")+
  ylim(0.52,0.59)+
  scale_colour_manual(name="Algorithms",values=cols, guide = guide_legend(ncol=2,fill = NULL,colour = NULL, labels=c("SGLD, m=100","SGLD, m=500","SGLD, m=1000","SGLD-CV, m=100","SGLD-CV, m=500","SGLD-CV, m=1000"))) + 
  scale_fill_manual(name="Bar",values=cols, guide="none")

ggsave("sgldVSsgldcv.png",path = "C:/Users/evate/OneDrive/Dokumente/Uni/Probabilistic Modelling/Project/Replication/Plots",width=10,height=5)


#After this preliminary analysis, we created a grid search, see GridSearch.R