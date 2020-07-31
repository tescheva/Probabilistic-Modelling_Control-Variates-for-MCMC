# PLOTTING OF ALL RELEVANT RESULTS
# Implemented in results section in the report

# Plots are based on the "testresults" object created in GridSearch.R

#load required packages
library(ggplot2)
library(grid)
library(gridExtra)
theme_set(theme_minimal())

#### Compute logLoss ####
#come back to this for every plot & compute logLoss for relevant algorithm results

#adjust logLoss_sgldRep1_m0.001_N1 to the relevant one (3x)
#adjust logLossTimes_sgldRep1_m0.001_N1 to the relevant one(2x)
#adjust testresults$`sgldRep1,m0.001,N1` to the relevant one(3x)
#adjust seq(to=40000) to 8000 for Hamiltonian algorithms

iterations = seq(from = 1, to = 40000, by = 10) 
logLoss_sgldRep1_m0.001_N1 = rep(0, length(iterations)) 
logLossTimes_sgldRep1_m0.001_N1 = rep(0, length(iterations)) 
for ( iter in 1:length(iterations) ) { 
  j = iterations[iter] 
  logLossTimes_sgldRep1_m0.001_N1[iter] = testresults$`sgldRep1,m0.001,N1`$times[j]
  beta0_j = testresults$`sgldRep1,m0.001,N1`$bias[j,] 
  beta_j = testresults$`sgldRep1,m0.001,N1`$beta[j,,] 
  for ( i in 1:length(testset$y) ) { 
    piCurr = 1 / (1 + exp(- beta0_j - sum(testset$X[i,] * beta_j))) 
    y_i = testset$y[i] 
    logLossCurr = - (y_i * log(piCurr) + (1 - y_i) * log(1 - piCurr)) 
    logLoss_sgldRep1_m0.001_N1[iter] = logLoss_sgldRep1_m0.001_N1[iter] + 1 / length(testset$y) * logLossCurr 
  } 
} 

#PLOTTING:

#### Sgldcv for all 6 combis, SGLDCV, Rep1 ####
#Figure 2 in the report

##create a plot for each combi:

# m0.01N1
plotFrame = data.frame("Iteration" = iterations, 
                       "logLoss" = logLoss_sgldcvRep1_m0.01_N1, 
                       "time" = logLossTimes_sgldcvRep1_m0.01_N1 * -1)
SgldcvRep1m0.01N1 <- ggplot(plotFrame, aes(x = time, y = logLoss)) +
  geom_line(color = "maroon") +
  ylab(NULL) +
  xlab(NULL) +
  labs(subtitle="SGLD-CV with m=0.01 and N=1",title="A")+
  theme(plot.subtitle = element_text(hjust=0.5))

# m0.01N0.1
plotFrame = data.frame("Iteration" = iterations, 
                       "logLoss" = logLoss_sgldcvRep1_m0.01_N0.1, 
                       "time" = logLossTimes_sgldcvRep1_m0.01_N0.1 * -1)
SgldcvRep1m0.01N0.1 <- ggplot(plotFrame, aes(x = time, y = logLoss)) +
  geom_line(color = "maroon") +
  ylab(NULL) +
  xlab(NULL) +
  labs(subtitle="SGLD-CV with m=0.01 and N=0.1",title="B")+
  theme(plot.subtitle = element_text(hjust=0.5))

# 0.01N0.01
plotFrame = data.frame("Iteration" = iterations, 
                       "logLoss" = logLoss_sgldcvRep1_m0.01_N0.01, 
                       "time" = logLossTimes_sgldcvRep1_m0.01_N0.01 * -1)
SgldcvRep1m0.01N0.01 <- ggplot(plotFrame, aes(x = time, y = logLoss)) +
  geom_line(color = "maroon") +
  ylab(NULL) +
  xlab(NULL) +
  labs(subtitle="SGLD-CV with m=0.01 and N=0.01",title="C")+
  theme(plot.subtitle = element_text(hjust=0.5))

# m0.001N1
plotFrame = data.frame("Iteration" = iterations, 
                       "logLoss" = logLoss_sgldcvRep1_m0.001_N1, 
                       "time" = logLossTimes_sgldcvRep1_m0.001_N1 * -1)
SgldcvRep1m0.001N1 <- ggplot(plotFrame, aes(x = time, y = logLoss)) +
  geom_line(color = "maroon") +
  ylab(NULL) +
  xlab(NULL) +
  labs(subtitle="SGLD-CV with m=0.001 and N=1",title="D")+
  theme(plot.subtitle = element_text(hjust=0.5))

# m0.001N0.1
plotFrame = data.frame("Iteration" = iterations, 
                       "logLoss" = logLoss_sgldcvRep1_m0.001_N0.1, 
                       "time" = logLossTimes_sgldcvRep1_m0.001_N0.1 * -1)
SgldcvRep1m0.001N0.1 <- ggplot(plotFrame, aes(x = time, y = logLoss)) +
  geom_line(color = "maroon") +
  ylab(NULL) +
  xlab(NULL) +
  labs(subtitle="SGLD-CV with m=0.001 and N=0.1",title="E")+
  theme(plot.subtitle = element_text(hjust=0.5))

# m0.001N0.01 
plotFrame = data.frame("Iteration" = iterations, 
                       "logLoss" = logLoss_sgldcvRep1_m0.001_N0.01, 
                       "time" = logLossTimes_sgldcvRep1_m0.001_N0.01 * -1)
SgldcvRep1m0.001N0.01 <- ggplot(plotFrame, aes(x = time, y = logLoss)) +
  geom_line(color = "maroon") +
  ylab(NULL) +
  xlab(NULL) +
  labs(subtitle="SGLD-CV with m=0.001 and N=0.01",title="F")+
  theme(plot.subtitle = element_text(hjust=0.5))

#gather in one plot:
grid <- grid.arrange(SgldcvRep1m0.01N1, SgldcvRep1m0.01N0.1, SgldcvRep1m0.01N0.01, 
                     SgldcvRep1m0.001N1, SgldcvRep1m0.001N0.1, SgldcvRep1m0.001N0.01, nrow=2)
grid <- annotate_figure(grid,left=text_grob("Log loss of test set",size=14,rot = 90), 
                        bottom=text_grob("Time elapsed in seconds",size=14))

ggsave("grid_SGLDCV_Rep1_6combis.jpg", plot= grid, scale=1.5, width=9, height=5,
       path = "C:/Users/evate/OneDrive/Dokumente/Uni/Probabilistic Modelling/Project/Replication/Plots")



#### Three repetitions, fix m0.01 and N0.1, on SGLDCV ####
#Figure 3 in the report

## layered in one plot:
plotFrame = data.frame("Iteration" = iterations, "logLoss3" = logLoss_sgldcvRep3_m0.01_N0.1, 
                       "logLoss2" = logLoss_sgldcvRep2_m0.01_N0.1,
                       "logLoss1" = logLoss_sgldcvRep1_m0.01_N0.1,
                       "time" = logLossTimes_sgldcvRep3_m0.01_N0.1 * -1) 

plot <- ggplot(plotFrame, aes(x = time)) + 
  geom_line(aes(y=logLoss3), color = "orange") +
  geom_line(aes(y=logLoss2), color = "darkblue") +
  geom_line(aes(y=logLoss1), color = "maroon") +
  ylab("Log loss of test set") +
  xlab("Time elapsed in seconds")

ggsave("grid_SGLDCV_3Reps_m001_N01_layeredx.jpg", plot = plot, width=10, height=4, scale=0.9,
       path = "C:/Users/evate/OneDrive/Dokumente/Uni/Probabilistic Modelling/Project/Replication/Plots")



#### Langevin vs Hamiltonian with CV vs no CV ####
#Figure 4 in the report

plotFrame_sgldcv_1_0001 = data.frame("Iteration" = iterations, "logLoss" = logLoss, "time" = logLossTimes * -1) 

cols = c("SGHMC"="darkred","SGHMC-CV"="firebrick2","SGLD"="lightblue","SGLD-CV"="royalblue")
ggplot() + 
  ylim(0.518,0.54) +
  geom_line(data= plotFrame_sghmc_1_0001, aes(x = time, y = logLoss, color = "SGHMC"), alpha = 0.9, size=0.3) +
  geom_line(data= plotFrame_sghmccv_1_0001, aes(x = time, y = logLoss, color = "SGHMC-CV"), alpha = 0.9, size =1) +  
  geom_line(data= plotFrame_sgld_1_0001, aes(x = time, y = logLoss, color = "SGLD"), alpha = 0.7, size=0.3) + 
  geom_line(data= plotFrame_sgldcv_1_0001, aes(x = time, y = logLoss, color = "SGLD-CV"), alpha = 0.7, size=1) + 
  ylab("Log loss of test set") + xlab("Time elapsed in seconds")+
  #labs(title="Comparison of four algorithms: SGLD(-CV), SGHMC(-CV)",subtitle="Under same fixed parameters: m=0.001, N=1")+
  scale_colour_manual(name="Algorithms",values=cols, guide = guide_legend(fill = NULL,colour = NULL)) + 
  scale_fill_manual(name="Bar",values=cols, guide="none")
ggsave("4Algoss.png",path = "C:/Users/evate/OneDrive/Dokumente/Uni/Probabilistic Modelling/Project/Replication/Plots",scale=0.9,width=10,height=5)



#### Probability distribution of parameter values for one variable, m and N fixed ####
#Figure 5 in the report

# create separate objects for each algorithm:
testa <- as.data.frame(testresults$`sgldRep1,m0.01,N1`$beta[,,1])
testb <- as.data.frame(testresults$`sgldcvRep1,m0.01,N1`$beta[,,1])
testc <- as.data.frame(testresults$`sghmcRep1,m0.01,N1`$beta[,,1])
testd <- as.data.frame(testresults$`sghmccvRep1,m0.01,N1`$beta[,,1])

# create plots for each algorithm:
a <- ggplot(testa, aes(x=testa[,2]))+
  geom_density(fill="maroon")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("SGLD")+
  theme(plot.title = element_text(hjust=0.5))
b <- ggplot(testb, aes(x=testb[,2]))+
  geom_density(fill="maroon")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("SGLD-CV")+
  theme(plot.title = element_text(hjust=0.5))
c <- ggplot(testc, aes(x=testc[,2]))+
  geom_density(fill="maroon")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("SGHMC")+
  theme(plot.title = element_text(hjust=0.5))
d <- ggplot(testd, aes(x=testd[,2]))+
  geom_density(fill="maroon")+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("SGHMC-CV")+
  theme(plot.title = element_text(hjust=0.5))

# gather in one plot:
hist_Rep1m001N1_beta2 <- grid.arrange(a,c,b,d,nrow=2)
hist_Rep1m001N1_beta2 <- annotate_figure(hist_Rep1m001N1_beta2,left="Probability", bottom="Parameter values")

ggsave("hist_Rep1m001N1_beta2x.jpg", plot = hist_Rep1m001N1_beta2, scale=1.2, width=9, height=5,
       path = "C:/Users/evate/OneDrive/Dokumente/Uni/Probabilistic Modelling/Project/Replication/Plots")




