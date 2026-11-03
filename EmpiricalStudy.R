#-------------------------------------------------
# Empirical Study Aligning Test Scores
# 2026
# This is the code used. Please note that the data is 
# not possible to share. Data transfer is only possible
# through contacting Betula steering group.
#-------------------------------------------------

## Set the working directory to the folder containing the data files
# setwd("path/to/data")

# Read in the data (your own) for the NEC design
library(haven)
T1T2_S1S3_mrAE <- read_sav("T1T2_S1S3_NEC.sav")
dfT1T2         <- as.data.frame(T1T2_S1S3_mrAE)

# Read in the data (your own) for the SG design 
T1T2_missremoveSPTB12_241011 <- read_sav("T1T2_SG.sav") 

# Change scores
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
        fill = "grey",
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

# Read in the data (your own) for the EG design
T1T2_S1_EG <- read_sav("T1T2_S1.sav")
T1T2_S3_EG <- read_sav("T1T2_S3.sav")

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

# EG design ------------------------------------
SPTB1k <- kefreq(T1_sptb,0:16) 
SPTB2k <- kefreq(T2_sptb,0:16) 

# Only showing the best models used (tried several others)
gSPTB1k   <- glm(frequency~I(X)+I(X^2)+I(X^3)+I(X^4), family="poisson",data=SPTB1k,x=TRUE)
gSPTB2k   <- glm(frequency~I(X)+I(X^2)+I(X^3)+I(X^4), family="poisson",data=SPTB2k,x=TRUE)
summary(gSPTB1k) 
summary(gSPTB2k) 

EGk <- kequate("EG",0:16,0:16,gSPTB2k,gSPTB1k) 
getEq(EGk)

# SG design ------------------------------------
SPTB21k_sg   <- kefreq(T2_sptbS,0:16, T1_sptbS, 0:16) # SG (2->1)
gSPTB21kt_sg <- glm(frequency~I(X)+I(X^2)+I(X^3)+I(A)+I(A^2)+I(A^3)+I(A^4)+I(X):I(A), family="poisson",data=SPTB21k_sg,x=TRUE)
summary(gSPTB21kt_sg) # best model

SGk <- kequate("SG",0:16,0:16,gSPTB21kt_sg) 
getEq(SGk)
#-----------------------------------------------
# NEC design:  # Age/Gender/Education effect
#-----------------------------------------------
# Use S1 and S3 at timepoint T1 and T2
data_merge <- data.frame(V1 = dfT1T2$T1_sptb,V2 = dfT1T2$T2_sptb)
data_merge$Age       <- dfT1T2$Ageatinclusiony 
data_merge$RAge      <- dfT1T2$Retired
data_merge$Gender    <- dfT1T2$Gender 
data_merge$Educ      <- dfT1T2$Educ   
data_merge$treatment <- c(rep(0,900),rep(1,877))  

# Estimate propensity score ------------------------------------------

# Nonsignificant variable: Gender - so skip all models with gender.
# Only best model shown here

mod6a <- glm(treatment ~ Educ + Age+ Educ:Age, data =data_merge, family = binomial(link="logit")) 
summary(mod6a) 
BIC(mod6a)
AIC(mod6a)

propscore_mod6F <- mod6a$fitted.values # Fitted values for best model
data_merge$ps   <- propscore_mod6F       # Add fitted propensity score values to data frame

#-------------------------------------------------------------------------------
library(Hmisc)

# Add categorized propensity scores
nstrat   <- 10 # number of strata
cat.prop <- nstrat
data_merge$cat_ps <- as.numeric(cut2(data_merge$ps, g = nstrat)) # PS scores

#---------------------------------------------------
data11_ps <- data_merge[data_merge$treatment == 0, ] # Sample 1
data12_ps <- data_merge[data_merge$treatment == 1, ] # Sample 3

#-----------------------------------------------
# Check Covariate balance with ASMD 
#-----------------------------------------------
p <- data11_ps
q <- data12_ps

p$Gender_num <- ifelse(p$Gender == "Male", 1, 0)
q$Gender_num <- ifelse(q$Gender == "Male", 1, 0)

asmd_Gender <- c()

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

#----------------------------------------------------------------------------------
# 3. NEC design 	
#----------------------------------------------------------------------------------
data12P2     <- data.frame(V1 = data12_ps$V2,V2 = data12_ps$cat_ps) # V2 = SPTB time 2 # mod6
data11Q2     <- data.frame(V1 = data11_ps$V1,V2 = data11_ps$cat_ps) # V1 = SPTB time 1 # mod6 

freqXP12nec2 <- as.data.frame(table(factor(data12P2$V1, levels = 0:16, ordered = TRUE),
                                    factor(data12P2$V2, levels = 1:nstrat, ordered = TRUE), dnn=c("t12","ps12")))
freqYQ11nec2 <- as.data.frame(table(factor(data11Q2$V1, levels = 0:16, ordered = TRUE),
                                    factor(data11Q2$V2, levels = 1:nstrat, ordered = TRUE),dnn=c("t11","ps11")))

data12Pnec2 <- data.frame(frequency=freqXP12nec2$Freq, t12=rep(0:16,nstrat), ps12=rep(1:nstrat, each=17))
data11Qnec2 <- data.frame(frequency=freqYQ11nec2$Freq, t11=rep(0:16,nstrat), ps11=rep(1:nstrat, each=17))

#------------------------------------
# Best glm models for the NEC design
#------------------------------------
glmx12nec <- glm(formula = frequency ~ I(ps12) + I(t12)+I(t12^2), 
                  family = poisson(), data = data12Pnec2, x = TRUE)
glmy12nec <- glm(formula = frequency ~ I(ps11)+I(t11)+I(t11^2),
                  family = poisson(), data = data11Qnec2, x = TRUE)
summary(glmx12nec)
summary(glmy12nec)

# Perform the equating ----------------------------------------------------

nec.ps_mod6 <- kequate("NEC", 0:16, 0:16, glmy12nec, glmx12nec, kernel = "gaussian") 

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
multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL) {
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

x      <- 0:16 
Scores <- 0:16

EqScores <- data.frame(EGk@equating$eqYx - x,
                       SGk@equating$eqYx- x, 
                       nec.ps_mod6@equating$eqYx - x, 
                       Scores)

Mspec <- c("EG" = "solid", "SG" = "dotdash", 
                       "NEC" = "dotted")

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
  scale_linetype_manual(values = Mspec) 

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
  scale_linetype_manual(values = Mspec) 
multiplot(p1, p2, cols = 1) 

#----------------------------------------------------------------------
dfT1m  <- as.data.frame(T1T2_S1S3_mrAE[1:900,])
dfT2m  <- as.data.frame(T1T2_S1S3_mrAE[901:1777,])

# Educ
freq_T1 <- table(factor(dfT1m$Educ, levels = 0:30))
freq_T2 <- table(factor(dfT2m$Educ, levels = 0:30))

# T1
barplot(freq_T1,
        main = "",
        xlab = "Education in Years",
        ylab = "Frequency",
        ylim = c(0, 300),
        names.arg = 0:30,
        axisnames = FALSE)

axis(1, at = seq(1, 31, by = 5), labels = seq(0, 30, by = 5))
axis(2, at = c(0, 50, 100, 150, 200, 250,300))

# T2
barplot(freq_T2,
        main = "",
        xlab = "Education in Years",
        ylab = "Frequency",
        ylim = c(0, 300),
        names.arg = 0:30,
        axisnames = FALSE)

axis(1, at = seq(1, 31, by = 5), labels = seq(0, 30, by = 5))
axis(2, at = c(0, 50, 100, 150, 200, 250,300))

#------------------------------------------------
# Plot difference from EG
#------------------------------------------------
scores <-  0:16

eqEG   <-  EGk@equating$eqYx   # Population equated values
eqSG   <-  SGk@equating$eqYx   # SG Equated values 
eqNEC  <-  nec.ps_mod6@equating$eqYx# NEC Equated values 

biasSG <- eqSG-eqEG
biasNEC<- eqNEC-eqEG

plot(
  scores,
  biasSG,
  type = "l",
  lty = 3,
  xlim = c(0, 16),
  ylim = c(-2, 2),
  xlab = "Scores",
  ylab = "Difference from EG"
)

lines(scores, biasNEC, lty = 2)
abline(h = 0, lty = 1)

legend(
  "topright",
  lty = c(3, 2),
  legend = c("SG", "NEC")
)