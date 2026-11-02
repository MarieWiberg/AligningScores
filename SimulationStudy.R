#----------------------------------------------------------------------------
# Simulation Study Aligning Test Scores
# Compare EG and NEC design when we have nonequivalent and equivalent groups
#----------------------------------------------------------------------------

# Case 1: Equivalent groups
# Case 2: Nonequivalent groups

rm(list=ls())
graphics.off()

# Required R packages
l <- c("haven","ltm", "kequate", "ggplot2", "equateIRT", "Hmisc")
lapply(l, require, character.only = TRUE)

# Set working directory
setwd("C:/...")
path <- getwd()

library(kequate)
library(equate)

# Sample size
N <- 2000

# total number of items
tot_item <- 16
J <- tot_item #amax

# population size for P and Q, respectively
pop_size <- 100000

# number of PS strata
nstrat <- 8 

nrep <- 200 # 1000

# Matrices to store results in
equated_scores_pop_PSE <- matrix(0,nrow = nrep, ncol = (J+1))
equated_scores_pop_CE <- matrix(0,nrow = nrep, ncol = (J+1))
equated_scores_pop_EG <- matrix(0,nrow = nrep, ncol = (J+1))

equated_scores_PS_PSE <- matrix(0,nrow = nrep, ncol = (J+1))
equated_scores_PS_CE <- matrix(0,nrow = nrep, ncol = (J+1))

equated_scores_EG <- matrix(0,nrow = nrep, ncol = (J+1))

SEE_PS_PSE <- matrix(0,nrow = nrep, ncol = (J+1))
SEE_PS_CE <- matrix(0,nrow = nrep, ncol = (J+1))
SEE_EG <- matrix(0,nrow = nrep, ncol = (J+1))

# Set the means and standard deviations for the two ability distributions
mean_p <- 0
sd_p   <- 1
mean_q <- 0.5 #0 #0.5
sd_q   <- 1

# number of covariate categories (three categorical covariates)
cov1_lim <- 2 # Covariate 1 has 3 categories Gender
cov2_lim <- 3 # Covariate 2 has 4 categories Age
cov3_lim <- 5 # Covariate 3 has 6 categories Educ

set.seed(123)

# Generate the latent ability variable for each individual
# in population P and Q using normal distributions
ability_p <- rnorm(pop_size, mean = mean_p, sd = sd_p)
ability_q <- rnorm(pop_size, mean = mean_q, sd = sd_q)

#--------------------------
# Generate item parameters
#--------------------------

# Discrimination for test X
akX <- runif(J, 0.5, 2)

# Difficulty for test X
bkX <- rnorm(J) 

# Discrimination for test Y
akY <- runif(J, 0.5, 2) 

# Difficulty for test Y
bkY <- rnorm(J) 

# Guessing parameter for test X and Y
ckX <- runif(J, 0, 0) # set to 0 as we're considering the 2-PL
ckY <- runif(J, 0, 0) # set to 0 as we're considering the 2-PL

#------------------------------------
# Item parameters for the covariates
#------------------------------------

# Moderate correlation scenario
akC1 <- runif(1, 0.5, 1.5)
bkC1 <- rnorm(1)
ckC1 <- runif(1, 0, 0)

akC2 <- runif(1, 0.5, 1.5)
bkC2 <- rnorm(1)
ckC2 <- runif(1, 0, 0)

akC3 <- runif(1, 0.5, 1.5)
bkC3 <- rnorm(1)
ckC3 <- runif(1, 0, 0)

# High correlation scenario
#akC1 <- runif(1, 1.5, 2)
#bkC1 <- rnorm(1)
#ckC1 <- runif(1, 0, 0)

#akC2 <- runif(1, 1.5, 2)
#bkC2 <- rnorm(1)
#ckC2 <- runif(1, 0, 0)

#akC3 <- runif(1, 1.5, 2)
#bkC3 <- rnorm(1)
#ckC3 <- runif(1, 0, 0)

dataP <- matrix(0, nrow = pop_size, ncol = (tot_item + cov1_lim+cov2_lim+cov3_lim))
dataQ <- matrix(0, nrow = pop_size, ncol = (tot_item + cov1_lim+cov2_lim+cov3_lim))

# Generate data from a 2-PL model
# First J columns are item responses for the main test. Then the covariates

for(i in 1:nrow(dataP)){
  dataP[i, 1:J] <- ckX + ((1 - ckX) / (1 + exp(-akX * (ability_p[i] - bkX)))) > runif(J)
  dataP[i, (tot_item+1):(tot_item+cov1_lim)] <- ckC1 + ((1 - ckC1) / (1 + exp(-akC1 * (ability_p[i] - bkC1)))) > runif(cov1_lim)
  dataP[i, (tot_item+cov1_lim+1):(tot_item+cov1_lim+cov2_lim)] <- ckC2 + ((1 - ckC2) / (1 + exp(-akC2 * (ability_p[i] - bkC2)))) > runif(cov2_lim)
  dataP[i, (tot_item+cov1_lim+cov2_lim+1):(tot_item+cov1_lim+cov2_lim+cov3_lim)] <- ckC3 + ((1 - ckC3) / (1 + exp(-akC3 * (ability_p[i] - bkC3)))) > runif(cov3_lim)
}

for(i in 1:nrow(dataQ)){
  dataQ[i, 1:J] <- ckY + ((1 - ckY) / (1 + exp(-akY * (ability_q[i] - bkY)))) > runif(J)
  dataQ[i, (tot_item+1):(tot_item+cov1_lim)]  <- ckC1 + ((1 - ckC1) / (1 + exp(-akC1 * (ability_q[i] - bkC1)))) > runif(cov1_lim)
  dataQ[i, (tot_item+cov1_lim+1):(tot_item+cov1_lim+cov2_lim)] <- ckC2 + ((1 - ckC2) / (1 + exp(-akC2 * (ability_q[i] - bkC2)))) > runif(cov2_lim)
  dataQ[i, (tot_item+cov1_lim+cov2_lim+1):(tot_item+cov1_lim+cov2_lim+cov3_lim)] <- ckC3 + ((1 - ckC3) / (1 + exp(-akC3 * (ability_q[i] - bkC3)))) > runif(cov3_lim)
}

# Calculate the sum score in the populations
sum_score_P <- rowSums(dataP[,1:J])
sum_score_Q <- rowSums(dataQ[,1:J])

# Add sum scores from P and Q into one vector
sum_score <- c(sum_score_P, sum_score_Q)

# Extract the three covariates generated from the 2-PL model
covariate1_score_P <- rowSums(dataP[, (tot_item + 1):(tot_item + cov1_lim)])
covariate1_score_Q <-  rowSums(dataQ[, (tot_item + 1):(tot_item + cov1_lim)])
covariate1_score <- c(covariate1_score_P, covariate1_score_Q)

covariate2_score_P <- rowSums(dataP[, (tot_item + cov1_lim+1):(tot_item + cov1_lim+cov2_lim)])
covariate2_score_Q <- rowSums(dataQ[, (tot_item + cov1_lim+1):(tot_item + cov1_lim+cov2_lim)])
covariate2_score <- c(covariate2_score_P, covariate2_score_Q)

covariate3_score_P <- rowSums(dataP[, (tot_item + cov1_lim+cov2_lim+1):(tot_item + cov1_lim+cov2_lim+cov3_lim)])
covariate3_score_Q <- rowSums(dataQ[, (tot_item + cov1_lim+cov2_lim+1):(tot_item + cov1_lim+cov2_lim+cov3_lim)])
covariate3_score <- c(covariate3_score_P, covariate3_score_Q)

p <- 1 / (1 + exp( 0.5 * covariate1_score - 0.2 * covariate2_score - 0.3 * covariate3_score))
mean(p)

# Administer X/Y to test-takers
z <- rbinom(2*pop_size, 1, p)


df <- data.frame(sum_score = sum_score,
                 covariate1_score = covariate1_score,
                 covariate2_score = covariate2_score,
                 covariate3_score = covariate3_score,
                 treatment = z)
#  View(df)
nstrat <- 8 

# Add categorized propensity scores to the population-level data
df$cat_propscore_pop <- as.numeric(cut2(p, g = nstrat))

# Split the population-level data into two groups based on treatment
pop_P <- df[df$treatment == 0, ]
pop_Q <- df[df$treatment == 1, ]


# Calculate the frequency tables for the population-level data
Xfreq_PS_pop <- kefreq(pop_P$sum_score, 0:J, pop_P$cat_propscore_pop, 1:nstrat)
Yfreq_PS_pop <- kefreq(pop_Q$sum_score, 0:J, pop_Q$cat_propscore_pop, 1:nstrat)

names(Xfreq_PS_pop) <- c("X", "A", "frequency")
names(Yfreq_PS_pop) <- c("Y", "A", "frequency")

#------------------------------------------------------------------------------
# Function presmooth_select NEC design
#------------------------------------------------------------------------------
presmooth_select <- function(data, max_power = 5, max_interaction_power = 3, lrt_pvalue_threshold = 0.05, x_var = "X", a_var = "A") {
  models <- list()

  # Fit all possible models
  for (i in 1:max_power) {
    for (j in 1:max_power) {
      x_terms <- paste("I(", x_var, "^", 1:i, ")", collapse = " + ")
      a_terms <- paste("I(", a_var, "^", 1:j, ")", collapse = " + ")
      
      formula <- paste0("frequency ~ ", x_terms, " + ", a_terms)
      model <- glm(as.formula(formula),
                   family = poisson,
                   data = data,
                   x = TRUE)
      models[[paste0("Xglm", i, j)]] <- model
      
    }
  }

  # Extract AIC, and BIC
  model_info <- lapply(models, function(x) {
    c(AIC = AIC(x),
      BIC = BIC(x)
    )
  })
  
  # Convert to data frame
  model_info_df <- do.call(rbind, model_info)
  rownames(model_info_df) <- names(models)
  
  # Find best models
  best_aic_model <- rownames(model_info_df)[which.min(model_info_df[, "AIC"])]
  best_bic_model <- rownames(model_info_df)[which.min(model_info_df[, "BIC"])]
  
  # STEP 2: INTERACTION TERMS
  interaction_models <- list()
  
  # Create all lower order interactions
  create_lower_order_interactions <- function(i, j, x_var, a_var) {
    terms <- c()
    for (x_power in 1:i) {
      for (a_power in 1:j) {
        terms <- c(terms, paste0("I(", x_var, "^", x_power, " * ", a_var, "^", a_power, ")"))
      }
    }
    return(paste(terms, collapse = " + "))
  }
  
  for (i in 1:max_interaction_power) {
    for (j in 1:max_interaction_power) {
      
      # Generate all interaction terms up to power i for X and j for A
      interaction_terms <- create_lower_order_interactions(i, j, x_var, a_var)
      
      aic_formula_str <- paste("frequency ~", as.character(formula(models[[best_aic_model]]))[3], "+", interaction_terms)
      bic_formula_str <- paste("frequency ~", as.character(formula(models[[best_bic_model]]))[3], "+", interaction_terms)

      interaction_model_aic <- glm(as.formula(aic_formula_str), family = poisson, data = data, x = TRUE)
      interaction_model_bic <- glm(as.formula(bic_formula_str), family = poisson, data = data, x = TRUE)

      interaction_models[[paste0("XglmInteraction", i, j, "AIC")]] <- interaction_model_aic
      interaction_models[[paste0("XglmInteraction", i, j, "BIC")]] <- interaction_model_bic
    }
  }
  
  # Extract AIC, BIC, and logLik for interaction models
  interaction_model_info <- lapply(interaction_models, function(x) {
    c(AIC = AIC(x),
      BIC = BIC(x),
      LogLik = logLik(x))
  })
  
  # Convert to data frame
  interaction_model_info_df <- do.call(rbind, interaction_model_info)
  rownames(interaction_model_info_df) <- names(interaction_models)
  
  # Find best interaction models
  best_aic_model_interaction <- rownames(interaction_model_info_df)[which.min(interaction_model_info_df[, "AIC"])]
  best_bic_model_interaction <- rownames(interaction_model_info_df)[which.min(interaction_model_info_df[, "BIC"])]

  return(list(models = models, model_info = model_info_df, 
              aic_mod = best_aic_model, bic_mod = best_bic_model, 
              best_aic_glm = models[[best_aic_model]],
              best_bic_glm = models[[best_bic_model]],
              interaction_models = interaction_models, interaction_model_info = interaction_model_info_df, 
              aic_mod_interaction = best_aic_model_interaction, 
              bic_mod_interaction = best_bic_model_interaction, 
              best_aic_glm_interaction = interaction_models[[best_aic_model_interaction]],
              best_bic_glm_interaction = interaction_models[[best_bic_model_interaction]]))
}

#--------------------------------------------------------------------------------
presmoothEG_select <- function(data, max_power = 5, lrt_pvalue_threshold = 0.05, x_var = "X") {
  models <- list()
  
  # Fit all possible models
  for (i in 1:max_power) {
    x_terms <- paste("I(", x_var, "^", 1:i, ")", collapse = " + ")
    
    formula <- paste0("frequency ~ ", x_terms)
    model <- glm(as.formula(formula),
                 family = poisson,
                 data = data,
                 x = TRUE)
    models[[paste0("Xglm", i)]] <- model
  }

  # Extract AIC, BIC
  model_info <- lapply(models, function(x) {
    c(AIC = AIC(x),
      BIC = BIC(x)
    )
  })
  
  # Convert to data frame
  model_info_df <- do.call(rbind, model_info)
  rownames(model_info_df) <- names(models)
  
  # Find best models
  best_aic_model <- rownames(model_info_df)[which.min(model_info_df[, "AIC"])]
  best_bic_model <- rownames(model_info_df)[which.min(model_info_df[, "BIC"])]
  
  aic_formula_str <- paste("frequency ~", as.character(formula(models[[best_aic_model]]))[3])
  bic_formula_str <- paste("frequency ~", as.character(formula(models[[best_bic_model]]))[3])
  
  return(list(models = models, model_info = model_info_df, 
              aic_mod = best_aic_model, bic_mod = best_bic_model, 
              best_aic_glm = models[[best_aic_model]],
              best_bic_glm = models[[best_bic_model]]
  ))}
#---------------------------------------------------------------------



#------------------------------------------------------------------------------
# Fit the log-linear models for the population-level data - using covariates
model_select_X_PS_pop <- presmooth_select(data = Xfreq_PS_pop,
                                             max_power = 5,
                                             max_interaction_power = 2,
                                             lrt_pvalue_threshold = 0.05,
                                             x_var = "X")

model_select_Y_PS_pop <- presmooth_select(data = Yfreq_PS_pop,
                                             max_power = 5,
                                             max_interaction_power = 2,
                                             lrt_pvalue_threshold = 0.05,
                                             x_var = "Y") 
model_select_Y_PS_pop

#----------------------------------------------------
# Perform equating using the population-level models
#----------------------------------------------------
# PSE
equating_pop_PSE <- kequate("NEAT_PSE",
                            0:J,
                            0:J,
                            model_select_X_PS_pop$best_aic_glm_interaction,
                            model_select_Y_PS_pop$best_aic_glm_interaction,
                            kernel = "gaussian")

# The true equating function for PSE
equated_scores_pop_PSE <- t(matrix(rep(equating_pop_PSE@equating$eqYx, nrep), 
                                   nrow = J+1, 
                                   ncol = nrep))
# CE
equating_pop_CE <- kequate("NEAT_CE",
                           0:J,
                           0:J,
                           1:nstrat,
                           model_select_X_PS_pop$best_aic_glm_interaction,
                           model_select_Y_PS_pop$best_aic_glm_interaction,
                           kernel = "gaussian")

# The true equating function for CE
equated_scores_pop_CE <- t(matrix(rep(equating_pop_CE@equating$eqYx, nrep), 
                                  nrow = J+1, 
                                  ncol = nrep))

# The true equating function for EG
Xfreq_EG_pop  <- freqtab(x=pop_P$sum_score,scales=0:16) # EG
Yfreq_EG_pop  <- freqtab(x=pop_Q$sum_score,scales=0:16) # EG

equating_pop_EG <- equate(Xfreq_EG_pop, Yfreq_EG_pop,type = "equi",design="EG")

#equated_scores_pop_EG <- t(matrix(rep(equating_pop_EG@equating$eqYx, nrep), 
equated_scores_pop_EG <- t(matrix(rep(equating_pop_EG$concordance$yx, nrep), 
                                  nrow = J+1, 
                                  ncol = nrep))

for(k in 1:nrep){
  
  random_rows <- sample(1:nrow(df), 2*N)
  tot_samp <- df[random_rows, ]
  
  samp_P <- tot_samp[tot_samp$treatment == 0, ]
  samp_Q <- tot_samp[tot_samp$treatment == 1, ]
  
  tot_samp <- rbind(samp_P, samp_Q)
  
  ####################################################
  #                                                  #
  #   Equating with propensity score                 # 
  #   that includes covariates                       #
  #                                                  #
  ####################################################
  
  # Estimate propensity score
  mod1 <- glm(treatment ~ covariate1_score + covariate2_score + covariate3_score, 
              data = tot_samp, family = binomial(logit))
  
  propscore_mod1 <- as.vector(mod1$fitted.values)
  
  tot_samp$PropensityScore_mod1 <- propscore_mod1
  
  # Categorize the propensity scores into "nstrat" groups
  tot_samp$cat_propscore_mod1 <- as.numeric(cut2(propscore_mod1, g = nstrat))
  
  # Split into P and Q sample, respectively
  samp_P$cat_ps <- tot_samp$cat_propscore_mod1[tot_samp$treatment == 0]
  samp_Q$cat_ps <- tot_samp$cat_propscore_mod1[tot_samp$treatment == 1]
  
  # Calculate frequency tables for main test score and categorised propensity score
  XAfreq_PS <- kefreq(samp_P$sum_score, 0:J, samp_P$cat_ps, 1:nstrat)
  YAfreq_PS <- kefreq(samp_Q$sum_score, 0:J, samp_Q$cat_ps, 1:nstrat)
  
  names(XAfreq_PS) <- c("X","A","frequency")
  names(YAfreq_PS) <- c("Y","A","frequency")
  
  # Best fitting log-linear models by AIC, BIC and LRT for XA:
  model_select_XA_PS <- presmooth_select(data = XAfreq_PS, 
                                         max_power = 5,
                                         max_interaction_power = 2, 
                                         lrt_pvalue_threshold = 0.05, 
                                         x_var = "X")  
  
  # Best fitting log-linear models by AIC, BIC and LRT for YA:
  model_select_YA_PS <- presmooth_select(data = YAfreq_PS, 
                                         max_power = 5, 
                                         max_interaction_power = 2, 
                                         lrt_pvalue_threshold = 0.05, 
                                         x_var = "Y") 
                                         
  # Equate test forms using post-stratification equating
  equating_PS_PSE <- kequate("NEAT_PSE", 
                             0:J, 
                             0:J, 
                             model_select_XA_PS$best_aic_glm_interaction, 
                             model_select_YA_PS$best_aic_glm_interaction, 
                             kernel = "gaussian") 
  
  # Save the equated scores and the SEE
  equated_scores_PS_PSE[k,] <- equating_PS_PSE@equating$eqYx
  SEE_PS_PSE[k,] <- equating_PS_PSE@equating$SEEYx
  
  
  # Equate test forms using chained equating
  equating_PS_CE <- kequate("NEAT_CE", 
                            0:J, 
                            0:J, 
                            1:nstrat,
                            model_select_XA_PS$best_aic_glm_interaction, 
                            model_select_YA_PS$best_aic_glm_interaction, 
                            kernel = "gaussian") 
  
  # save the equated scores and the SEE
  equated_scores_PS_CE[k,] <- equating_PS_CE@equating$eqYx
  SEE_PS_CE[k,] <- equating_PS_CE@equating$SEEYx

  #################
  #               #
  # EG equating   #
  #               #
  #################
  
  Xfreq_eg <- kefreq(samp_P$sum_score, 0:J)
  Yfreq_eg <- kefreq(samp_Q$sum_score, 0:J)
  
  names(Xfreq_eg) <- c("X","frequency")
  names(Yfreq_eg) <- c("Y","frequency")
  
  # Best fitting log-linear models by AIC and BIC for X:
  model_select_X_eg <- presmoothEG_select(data=Xfreq_eg,max_power = 5,
                     lrt_pvalue_threshold = 0.05, x_var = "X")
    
  # Best fitting log-linear models by AIC, and BIC for Y:
  model_select_Y_eg <- presmoothEG_select(data=Yfreq_eg,max_power = 5,
                                          lrt_pvalue_threshold = 0.05, x_var = "Y")
# OR use equate  
  XXfreq_eg  <- freqtab(x=samp_P$sum_score,scales=0:16) # EG
  YYfreq_eg  <- freqtab(x=samp_Q$sum_score,scales=0:16) # EG
  
  # ------------------------------
  # Equate from X to Y
  #-------------------------------
  EG <- equate(XXfreq_eg, YYfreq_eg,type = "equi",design="EG") # Equivalent Groups

    equating_eg <- kequate("EG", 0:J, 0:J, 
                               model_select_X_eg$best_aic, 
                               model_select_Y_eg$best_aic, 
                               kernel = "gaussian") 
  
  equated_scores_EG[k,] <- equating_eg@equating$eqYx 
  SEE_EG[k,] <- equating_eg@equating$SEEYx
  #equated_scores_EG[k,] <-  EG$concordance$yx  
  #SEE_EG[k,] <- EG$concordance$se
  
  print(k)
}
  
#------------------------------------------------ 
# Save the results to CSV files
#write.csv(equated_scores_PS_PSE, "equated_scores_PS_PSE.csv", row.names = FALSE)
#write.csv(SEE_PS_PSE, "SEE_PS_PSE.csv", row.names = FALSE)

#write.csv(equated_scores_PS_CE, "equated_scores_PS_CE.csv", row.names = FALSE)
#write.csv(SEE_PS_CE, "SEE_PS_CE.csv", row.names = FALSE)

#write.csv(equated_scores_EG, "equated_scores_EG_pop.csv", row.names = FALSE)
#write.csv(SEE_EG, "SEE_EG.csv", row.names = FALSE)

#write.csv(equated_scores_pop_PSE, "equated_scores_pop_PSE.csv", row.names = FALSE)
#write.csv(equated_scores_pop_CE, "equated_scores_pop_CE.csv", row.names = FALSE)

# -------------------------------------------------------------------------
# RESULTS
#--------------------------------------------------------------------------

library(ggplot2)

#####################
#                   #
#   bias function   #
#                   #
#####################

Bias <- function(results, parameter){
  rel.bias <- matrix(0, nrow = nrow(results), ncol = ncol(results))
  for(i in 1:nrow(results)){
    rel.bias[i,] <- (results[i,]-parameter[i,])
  }
  res <- apply(rel.bias, 2, mean)
  return(res) 
}

######################
#                    #    
#   RMSE function    #     
#                    # 
######################

RMSE <- function(results, parameter){
  rel.mse <- matrix(0, nrow = nrow(results), ncol = ncol(results))
  for (i in 1:nrow(results)){
    rel.mse[i,] <- (results[i,] - parameter[i,])^2
  }
  res <- sqrt(apply(rel.mse, 2, mean))
  return(res) 
}

#####################
#                   #    
#   SE function     #     
#                   # 
#####################

SE <- function(results){
  rel.se <- matrix(0, nrow = nrow(results), ncol = ncol(results))
  for (i in 1:nrow(results)){
    rel.se[i,] <- results[i,]
  }
  res <- apply(rel.se, 2, sd)
  return(res) 
}

# Load data
#load("N1=5000_J40_A20.RData")

# Bias 
bias_PS_PSE <- Bias(equated_scores_PS_PSE, equated_scores_pop_PSE)  
bias_PS_CE  <- Bias(equated_scores_PS_CE, equated_scores_pop_CE)  
bias_EG     <- Bias(equated_scores_EG, equated_scores_pop_EG)


# SE
SE_PS_PSE <- SE(equated_scores_PS_PSE)
SE_PS_CE  <- SE(equated_scores_PS_CE)
SE_EG     <- SE(equated_scores_EG)

# RMSE
rmse_PS_PSE <- RMSE(equated_scores_PS_PSE, equated_scores_pop_PSE)
rmse_PS_CE  <- RMSE(equated_scores_PS_CE, equated_scores_pop_CE)
rmse_EG     <- RMSE(equated_scores_EG, equated_scores_pop_EG)

# SEE 
SEE_PS_PSEs <- colMeans(SEE_PS_PSE)
SEE_PS_CEs  <- colMeans(SEE_PS_CE)
SEE_EGs     <- colMeans(SEE_EG)

#############
#           #
# Plot bias #
#           #
#############
J      <- 16
Scores <- 0:J

bias_df_More <- data.frame(
  bias_PS_PSE, 
  bias_PS_CE, 
  bias_EG,
  Scores)

eq_labels <- c("PS_PSE" = "solid", 
  "PS_CE" = "dotted", 
  "EG" = "dashed") 
  
plot_bias_MoreM <- ggplot(data = bias_df_More, aes(x=Scores)) +
  geom_line(aes(y = bias_PS_PSE, linetype = "NEC")) + 
 # geom_line(aes(y = bias_PS_CE, linetype = "PS_CE")) +
  geom_line(aes(y = bias_EG, linetype = "EG")) + 
  geom_hline(yintercept = 0) +
  labs(x = "Scores",
       y = "Bias",
       color = "") + 
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank())  +
        coord_cartesian(ylim = c(-0.10, 0.10))
  
plot_bias_MoreM

###########
#         #
# Plot SE #
#         #
###########

Scores <- 0:J
SE_df_More <- data.frame(SE_PS_PSE, SE_PS_CE,SE_EG, Scores)

eq_labels <- c("PS_PSE" = "solid", 
  "PS_CE" = "dotted", 
  "EG" = "dashed")

plot_SE_MoreM <- ggplot(data = SE_df_More, aes(x=Scores)) +
  geom_line(aes(y = SE_PS_PSE, linetype = "PS_PSE")) + 
  geom_line(aes(y = SE_PS_CE, linetype = "PS_CE")) +
  geom_line(aes(y = SE_EG, linetype = "EG")) +
  labs(x = "Scores",
       y = "SE",
       color = "") + 
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank())  +
  #scale_linetype_manual(values = eq_labels) 
coord_cartesian(ylim = c(0.0, 0.25))
plot_SE_MoreM

#############
#           #
# Plot RMSE #
#           #
#############

Scores <- 0:J
rmse_df_More <- data.frame(rmse_PS_PSE, 
  rmse_PS_CE, 
  rmse_EG, 
  Scores)

eq_labels <- c("PS_PSE" = "solid", 
  "PS_CE" = "dotted", 
  "EG" = "dashed")

plot_rmse_More <- ggplot(data = rmse_df_More, aes(x=Scores)) +
  geom_line(aes(y = rmse_PS_PSE, linetype = "NEC")) + 
  #geom_line(aes(y = rmse_PS_CE, linetype = "PS_CE")) +
  geom_line(aes(y = rmse_EG, linetype = "EG")) + 
  labs(x = "Scores",
       y = "RMSE",
       color = "") + 
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank())  +
  #scale_linetype_manual(values = eq_labels) 
  coord_cartesian(ylim = c(0.0, 0.25))
plot_rmse_More


############
#          #
# Plot SEE # 
#          #
############

SEEpsp <- colMeans(SEE_PS_PSE)
SEEpsc <- colMeans(SEE_PS_CE)
SEEeg  <- colMeans(SEE_EG)

Scores <- 0:J
SEE_df_MoreM <- data.frame(SEEpsp, 
                          SEEpsc, 
                          SEEeg,
                          Scores)

eq_labels <- c("PS_PSE" = "solid", 
  "PS_CE" = "dotted", 
  "EG" = "dashed")


plot_SEE_MoreM <- ggplot(data = SEE_df_MoreM, aes(x=Scores)) +
  geom_line(aes(y = SEEpsp, linetype = "NEC")) + 
  #geom_line(aes(y = SEEpsc, linetype = "PS_CE")) +
  geom_line(aes(y = SEEeg, linetype = "EG")) +
  labs(x = "Scores",
       y = "SEE",
       color = "") + 
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank())  +
  #scale_linetype_manual(values = eq_labels) 
  coord_cartesian(ylim = c(0.0, 0.30))

plot_SEE_MoreM