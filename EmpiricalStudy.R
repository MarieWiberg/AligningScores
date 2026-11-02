#-------------------------------------------------
# Empirical Study Aligning Test Scores
# 2026
# This is the code used. Please note that the data is 
# not possible to share. Data transfer only possible
# through Betula steering group.
#-------------------------------------------------

# Set working directory
setwd("C:\...")
# Read in the data (your own) for the NEC design
library(haven)
T1T2_S1S3_mrAE <- read_sav("T1T2_S1S3_NEC.sav")
dfT1T2         <- as.data.frame(T1T2_S1S3_mrAE)

# Read in the data (your own) for the SG design 
library(haven)
T1T2_missremoveSPTB12_241011 <- read_sav("T1T2_SG.s") 

# Histogram ---------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)

long_data <- T1T2_missremoveSPTB12_241011 %>%
  pivot_longer(
    cols = c(T1_sptb, T2_sptb),
    names_to = "Time",
    values_to = "Scores"
  )
    
    ggplot(long_data, aes(x = Scores, fill = Time)) +
      geom_histogram(
        binwidth = 1,
        boundary = 0,
        closed = "left",
        position = "dodge"
      ) +
      scale_x_continuous(breaks = 0:16, limits = c(0, 16)) +
      
      scale_fill_manual(
        values = c("steelblue", "firebrick"),
        labels = c("T1", "T2"),
        name = "Times"
      ) +
      
      labs(
        x = "Scores",
        y = "Frequency",
        fill = "Times"
      ) +
      theme_minimal()

    
    library(dplyr)
    
    data <- T1T2_missremoveSPTB12_241011 %>%
      mutate(
        Change = T2_sptb-T1_sptb
      )

    library(ggplot2)
    
    ggplot(data, aes(x = Change)) +
      geom_histogram(
        binwidth = 1,
        boundary = 0,
        closed = "left",
        fill = "steelblue",
        color = "black"
      ) +
      scale_x_continuous(
        breaks = -10:10,
        limits = c(-10, 10)
      ) +
      labs(
        x = "Change scores",
        y = "Frequency"
      ) +
      theme_minimal()
    
#-------------------------------------------------
T1T2mRsptb   <- T1T2_missremoveSPTB12_241011
dfT1T2mRsptb <- as.data.frame(T1T2mRsptb)

# EG design

library(haven) 
T1T2_S1_EG <- read_sav("T1T2_S1_4080.sav")
T1T2_S3_EG <- read_sav("T1T2_S3_4080.sav")

dfT1T2eg_S1  <- as.data.frame(T1T2_S1_EG)
dfT1T2eg_S3  <- as.data.frame(T1T2_S3_EG)

#--------------------------------------------------------
# Examine the data with equate
#--------------------------------------------------------

library(equate)

# EG design
T1_sptb <- dfT1T2eg_S1$T1_sptb
T2_sptb <- dfT1T2eg_S3$T2_sptb

SPTB1 <- freqtab(x=T1_sptb,scales=0:16) # EG
SPTB2 <- freqtab(x=T2_sptb,scales=0:16) # EG

sp1 <- as.data.frame(SPTB1)
sp2 <- as.data.frame(SPTB2)

spc1 <- sp1$count
spc2 <- sp2$count

plot(0:16, spc1, type = "l", xlim=c(0,16),col="black",ylab = "Frequency", xlab = "Score")
lines(0:16, spc2, lty=5, col="blue")
legend("topleft",lty=c(1,2), col=c("black","blue"),
       c("SPTB1","SPTB2"))

# SG design
T1_sptb_sg <- T1T2mRsptb$T1_sptb
T2_sptb_sg <- T1T2mRsptb$T2_sptb

SPTB21 <- freqtab(cbind(T2_sptb_sg,T1_sptb_sg),scales=list(0:16,0:16),design="sg") # SG
plot(SPTB21)

# ------------------------------
# Perform equating 
#-------------------------------
EG <- equate(SPTB2,SPTB1,type = "equi",design="EG") # EG
SG <- equate(SPTB21,type = "equi",design="SG")      # SG
EG$concordance 
SG$concordance  

#------------------------------
# GKE 
#------------------------------
library(kequate)

T1_sptb <-  dfT1T2eg_S1$T1_sptb    
T2_sptb <-  dfT1T2eg_S3$T2_sptb

T1_sptbS <- dfT1T2mRsptb$T1_sptb 
T2_sptbS <- dfT1T2mRsptb$T2_sptb

# EG
SPTB1k <- kefreq(T1_sptb,0:16) # EG
SPTB2k <- kefreq(T2_sptb,0:16) # EG

gSPTB1k   <- glm(frequency~I(X)+I(X^2)+I(X^3)+I(X^4), family="poisson",data=SPTB1k,x=TRUE)
#gSPTB1ka  <- glm(frequency~I(X)+I(X^2)+I(X^3)+I(X^4)+I(X^5), family="poisson",data=SPTB1k,x=TRUE)
#gSPTB1kb  <- glm(frequency~I(X)+I(X^2)+I(X^3)+I(X^4)+I(X^5)+I(X^6), family="poisson",data=SPTB1k,x=TRUE)
#gSPTB1kc  <- glm(frequency~I(X)+I(X^2)+I(X^3), family="poisson",data=SPTB1k,x=TRUE)
gSPTB2k   <- glm(frequency~I(X)+I(X^2)+I(X^3)+I(X^4), family="poisson",data=SPTB2k,x=TRUE)

summary(gSPTB1k) # best model
summary(gSPTB2k) # best model

EGk <- kequate("EG",0:16,0:16,gSPTB2k,gSPTB1k) 
getEq(EGk)

# SG design ------------------------------------
SPTB21k_sg   <- kefreq(T2_sptbS,0:16, T1_sptbS, 0:16) # SG (2->1)
#gSPTB21kt_sgN <- glm(frequency~I(X)+I(X^2)+I(X^3)+I(A)+I(A^2)+I(A^3)+I(A^4)+I(X):I(A), family="poisson",data=SPTB21k_sg,x=TRUE)
gSPTB21kt_sg <- glm(frequency~I(X)+I(X^2)+I(X^3)+I(A)+I(A^2)+I(A^3)+I(A^4)+I(X):I(A), family="poisson",data=SPTB21k_sg,x=TRUE)
summary(gSPTB21kt_sg) # best model

SGk <- kequate("SG",0:16,0:16,gSPTB21kt_sg) 
getEq(SGk)
#-----------------------------------------------
# NEC design:  # Age/Gender/Education effect
#-----------------------------------------------
# Use S1 and S3 at timepoint T1 and T2: Use data file: dfT1T2 
# Removed missing on Educ (20) and Age (5). 1000 (1) + 946 (S3)
data_merge <- as.data.frame(cbind(dfT1T2$T1_sptb,dfT1T2$T2_sptb))

data_merge$Age       <- dfT1T2$Ageatinclusiony 
data_merge$RAge      <- dfT1T2$Retired
data_merge$Gender    <- dfT1T2$Gender 
data_merge$Educ      <- dfT1T2$Educ   
data_merge$treatment <- c(rep(0,900),rep(1,877))  

# Estimate propensity score ------------------------------------------

# Nonsignificant variable: Gender - so skip all models with gender.

mod6 <- glm(treatment ~ Educ + Age, data =data_merge, family = binomial(logit)) 
summary(mod6) 
BIC(mod6)

mod6a <- glm(treatment ~ Educ + Age+ I(Age^2)+I(Age^3)+Educ:Age, data =data_merge, family = binomial(logit)) 
summary(mod6a) 
BIC(mod6a)
AIC(mod6a)

# Save propensity scores for best model 
propscore_mod6 <- mod6a$fitted.values # best models fitted values
data_merge$ps <- propscore_mod6 # best models fitted propensity score values to data frame

#-------------------------------------------------------------------------------
library(Hmisc)

# number of strata

cat.prop <- 10 # 10 
nstrat <- cat.prop

# Add categorized propensity scores
library(Hmisc)

data_merge$cat_ps <- as.numeric(cut2(data_merge$ps, g = nstrat)) # Add PS scores
data_merge$cat_w <- as.numeric(cut2(data_merge$w, g = nstrat)) # Add IPW scores

#---------------------------------------------------
data11_ps <- data_merge[1:900, ]    # Sample 1
data12_ps <- data_merge[901:1777, ] # Sample 3

#-----------------------------------------------
# Check Covariate balance with ASMD 
#-----------------------------------------------
p <- data11_ps
q <- data12_ps

p$Gender_num <- ifelse(p$Gender == "Male", 1, 0)
q$Gender_num <- ifelse(q$Gender == "Male", 1, 0)

asmd_Gender <- c()

cat.prop <- nstrat

for(i in 1:cat.prop){
  tr0 <- q$Gender_num[q$cat_ps == i]
  tr1 <- p$Gender_num[p$cat_ps == i] 
  asmd_Gender[i] <- abs((mean(tr1) - mean(tr0))/sqrt((var(tr1) + var(tr0))/2))
}
round(asmd_Gender,2)
abs((mean(p$Gender_num) - mean(q$Gender_num))/sqrt((var(p$Gender_num) + var(q$Gender_num))/2))

# Educ

asmd_Educ <- c()

for(i in 1:cat.prop){
  tr0e <- q$Educ[q$cat_ps == i]
  tr1e <- p$Educ[p$cat_ps == i] 
  asmd_Educ[i] <- abs((mean(tr1e) - mean(tr0e))/sqrt((var(tr1e) + var(tr0e))/2))
}
round(asmd_Educ,2)
abs((mean(p$Educ) - mean(q$Educ))/sqrt((var(p$Educ) + var(q$Educ))/2))

# Age

asmd_Age <- c()

for(i in 1:cat.prop){
  tr0e <- q$Age[q$cat_ps == i]
  tr1e <- p$Age[p$cat_ps == i] 
  asmd_Age[i] <- abs((mean(tr1e) - mean(tr0e))/sqrt((var(tr1e) + var(tr0e))/2))
}
round(asmd_Age,2)
abs((mean(p$Age) - mean(q$Age))/sqrt((var(p$Age) + var(q$Age))/2))

# RAge - Retired 35-60, 65-80

asmd_RAge <- c()

for(i in 1:cat.prop){
  tr0e <- q$RAge[q$cat_ps == i]
  tr1e <- p$RAge[p$cat_ps == i] 
  asmd_RAge[i] <- abs((mean(tr1e) - mean(tr0e))/sqrt((var(tr1e) + var(tr0e))/2))
}
round(asmd_RAge,2)
abs((mean(p$RAge) - mean(q$RAge))/sqrt((var(p$RAge) + var(q$RAge))/2))

#----------------------------------------------------------------------
# Check Covariate balance with ASMD 
#------------------------------------------------------------------

asmd_Agew <- c()

for(i in 1:cat.prop){
  tr0e <- q$Age[q$cat_w == i]
  tr1e <- p$Age[p$cat_w == i] 
  asmd_Agew[i] <- abs((mean(tr1e) - mean(tr0e))/sqrt((var(tr1e) + var(tr0e))/2))
}
round(asmd_Agew,2)
abs((mean(p$Age) - mean(q$Age))/sqrt((var(p$Age) + var(q$Age))/2))

#----------------------------------------------------------------------------------
# 3. NEC design 	
#----------------------------------------------------------------------------------
# 10 categories
#------------------------
data12P2     <- as.data.frame(cbind(data12_ps$V2, data12_ps$cat_ps)) # V2 = SPTB time 2 # mod6
data11Q2     <- as.data.frame(cbind(data11_ps$V1, data11_ps$cat_ps)) # V1 = SPTB time 1 # mod6 

freqXP12nec2 <- as.data.frame(table(factor(data12P2$V1, levels = 0:16, ordered = TRUE),
                                    factor(data12P2$V2, levels = 1:nstrat, ordered = TRUE), dnn=c("t12,ps12")))
freqYQ11nec2 <- as.data.frame(table(factor(data11Q2$V1, levels = 0:16, ordered = TRUE),
                                    factor(data11Q2$V2, levels = 1:nstrat, ordered = TRUE),dnn=c("t11,ps11")))

data12Pnec2 <- data.frame(frequency=freqXP12nec2$Freq, t12=rep(0:16,nstrat), ps12=rep(1:nstrat, each=17))
data11Qnec2 <- data.frame(frequency=freqYQ11nec2$Freq, t11=rep(0:16,nstrat), ps11=rep(1:nstrat, each=17))

#------------------------------------
# Fit glm models in the NEC design
#------------------------------------

# Test X -------------------------------------------------------------
glmx12b <- glm(formula = frequency ~ I(ps12) + I(ps12^2) + 
                 I(t12)+I(t12^2)+I(ps12):(t12), family = poisson(), data = data12Pnec2, x = TRUE)
# best model -------------------------------------------------------------
glmx12c <- glm(formula = frequency ~ I(ps12) + I(ps12^2) + I(ps12^3) + 
                +I(t12)+I(t12^2)+I(t12^3)+I(t12):(ps12), 
               family = poisson(), data = data12Pnec2, x = TRUE)
#-------------------------------------------------------------------------
summary(glmx12b)
summary(glmx12c)

#--------------------------------------------------------------
# Best models 40-80

glmx12new2 <- glm(formula = frequency ~ I(ps12) + 
                    +I(t12)+I(t12^2), 
                  family = poisson(), data = data12Pnec2, x = TRUE)
summary(glmx12new2)

glmy12new2 <- glm(formula = frequency ~ I(ps11)+  
                    +I(t11)+I(t11^2),
                  family = poisson(), data = data11Qnec2, x = TRUE)
summary(glmy12new2)

#----------------------------------------------------------------------------
# Best models when 35-85
glmy11c <- glm(formula = frequency ~ I(ps11) + I(ps11^2) + I(ps11^3) + 
                 +I(t11)+I(t11^2)+I(t11^3)+I(t11):(ps11), 
               family = poisson(), data = data11Qnec2, x = TRUE)
summary(glmy11c) # AIC = 880
glmx12c <- glm(formula = frequency ~ I(ps12) + I(ps12^2) + I(ps12^3) + 
                 +I(t12)+I(t12^2)+I(t12^3)+I(t12):(ps12), 
               family = poisson(), data = data12Pnec2, x = TRUE)
summary(glmx12c) # AIC = 835

#---------------------------------------------------------------
# best model -------------------------------------------------------------
glmy11c <- glm(formula = frequency ~ I(ps11) + I(ps11^2) + I(ps11^3) + 
                 +I(t11)+I(t11^2)+I(t11^3)+I(t11):(ps11), 
               family = poisson(), data = data11Qnec2, x = TRUE)
#-------------------------------------------------------------------------
summary(glmy11c)
BIC(glmy11c)

# Perform the equating ----------------------------------------------------

# equate PSE NEC PS:
nec.ps_mod6 <- kequate("NEC", 0:16, 0:16, glmy11c, glmx12c, kernel = "gaussian") 

# Using ages 40-80
nec.ps_mod6 <- kequate("NEC", 0:16, 0:16, glmy12new2, glmx12new2, kernel = "gaussian") 

plot(nec.ps_mod6@equating$eqYx)

plot(nec.ps_mod6@equating$SEEYx)
lines(nec.ps_mod6@equating$SEEYx)

getEquating(nec.ps_mod6)
getEquating(SGk)
getEquating(EGk)
round(cbind(SGk@equating$eqYx,nec.ps_mod6@equating$eqYx,EGk@equating$eqYx,SGk@equating$SEEYx,nec.ps_mod6@equating$SEEYx,EGk@equating$SEEYx),2)

round(cbind(nec.ps_mod6@equating$eqYx,nec.ps_mod6@equating$SEEYx),2)

#-------------------
# PLOTS
#-------------------

# library
library(ggplot2)
library(dplyr)

datatime12 <- as.numeric(data_merge$V1) 
datatime11 <- as.numeric(data_merge$V2)

data <- data.frame(values = c(datatime11,                  
                              datatime12),
                   group = c(rep("Y", 900), # 1000
                             rep("X", 891))) # 946

histplot <- ggplot(data, aes(x = values, fill = group)) +        
  geom_histogram(position = "identity", alpha = 0.4, bins = 17) +
  scale_fill_manual(values=c("grey20", "grey60")) + theme_bw() # remove this line for color version

histplot + theme_bw() + theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("Scores") +
  ylab("Frequency") + 
  labs(fill = "Test groups")

#-------------------------------------------------------------------

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#----------------------------------------------------------------

#-----------------------------#
# Plot the equated scores 
#-----------------------------#
# Plotting dimension 5.5 x 6
x      <- 0:16 
Scores <- 0:16

EqScores <- data.frame(EGk@equating$eqYx - x,
                       SGk@equating$eqYx- x, 
                       nec.ps_mod6@equating$eqYx - x, 
                       Scores)

Misspecifications <- c("EG" = "solid", "SG" = "dotdash", 
                       "NEC" = "dotted")

colors <- c("EG" = "black", "SG" = "orange", 
            "NEC" = "blue")

p1 <- ggplot(data = EqScores, aes(x=Scores)) +
  geom_line(aes(y = EGk@equating$eqYx - x, linetype = "EG")) + 
  geom_line(aes(y = SGk@equating$eqYx - x, linetype = "SG")) +  
  geom_line(aes(y = nec.ps_mod6@equating$eqYx - x, linetype = "NEC")) + 
  labs(x = "Scores",
       y = expression(paste(varphi - x)),
       color = "") + 
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank())  +
  scale_linetype_manual(values = Misspecifications) 

SEE_com <- data.frame(EGk@equating$SEEYx, 
                      SGk@equating$SEEYx, 
                      nec.ps_mod6@equating$SEEYx, 
                      Scores)

colors <- c("EG" = "black", "SG" = "orange", 
            "NEC" = "blue")

p2 <- ggplot(data = SEE_com, aes(x=Scores)) +
  geom_line(aes(y = EGk@equating$SEEYx, linetype = "EG")) + 
  geom_line(aes(y = SGk@equating$SEEYx, linetype = "SG")) +  
  geom_line(aes(y = nec.ps_mod6@equating$SEEYx, linetype = "NEC")) + 
  
  labs(x = "Scores",
       y = "SEE",
       color = "") + 
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank())  +
  scale_linetype_manual(values = Misspecifications) 
multiplot(p1, p2, cols = 1) 

# dim: 5 x 5.5
#------------------------------------------------------------------

# Plot densities of Age and Education
age <- c(23, 45, 34, 23, 45, 34, 29, 38, 27, 36, 47, 50, 33)

# Create the density object
age_density <- density(age)

# Plot the density
plot(age_density, main="Density Plot of Age", xlab="Age", ylab="Density")
#----------------------------------------------------------------------
dfT1m  <- as.data.frame(T1T2_S1S3_mrAE[1:1000,])
dfT2m <- as.data.frame(T1T2_S1S3_mrAE[1001:1946,])

# Age 
age_densityT1 <- density(dfT1m$Ageatinclusiony)
age_densityT2 <- density(dfT2m$Ageatinclusiony)

plot(age_densityT1, main="Density Plot", xlab="Years", ylab="Density")


plot(age_densityT2, main="Density Plot", xlab="Years", ylab="Density")

lines(age_densityT2,lty="2",col="blue")

legend("topright",lty=c(1,2),col="black","blue")


# Educ at T1
educ_densityT1 <- density(dfT1m$Educ)
educ_densityT2 <- density(dfT2m$Educ)

plot(educ_densityT1, main="Density Plot", xlab="Years", ylab="Density")

plot(educ_densityT2, main="Density Plot", xlab="Years", ylab="Density")


#------------------------------------------------
# Plot difference with EG
#------------------------------------------------
eqEG  <-  EGk@equating$eqYx  # EG equated values
eqSG  <-  SGk@equating$eqYx  # SG Equated values 
eqNEC <-  nec.ps_mod6@equating$eqYx# NEC Equated values 

biasSG  <- eqSG-eqEG
biasNEC <- eqNEC-eqEG
biasEG <- eqEG-eqEG

plot(biasSG,xlim=c(1,16),type="l",lty=3,ylim=c(-2,2),xlab="Scores",ylab="Difference with EG")
lines(biasNEC,lty=2)
lines(biasEG,lty=1)
legend("topright",lty=c(3,2),c("SG","NEC"))
#------------------------------------------------------