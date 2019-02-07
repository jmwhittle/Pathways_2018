library(lme4)
library(BMA)
library(MCMCpack)
library(readxl)
library(tidyverse); theme_set(theme_minimal())

bus_data <- read.csv("IRR_768_bus_data_3.csv") %>% dplyr::select(-FIRST_NAME, -LAST_NAME, -BIRTH_DATE, -ETHNICITY, -GENDER)

# Four year BUS study

# removing programs not used for pathways
bus_data <- bus_data %>% filter(PROGRAM_CODE != "CHEF-AAS" & PROGRAM_CODE != "CSIS-AS" & PROGRAM_CODE != "CSIS-AAS")

# fixing known data warehouse issues
bus_data$TOTAL_CREDITS_EARNED[is.na(bus_data$TOTAL_CREDITS_EARNED) == T] <- 0
bus_data$TERM_CREDITS[is.na(bus_data$TERM_CREDITS) == T] <- 0
bus_data$male <- ifelse(bus_data$GENDER_CODE == "M", 1, 0)
bus_data$FIRST_GENERATION_IND <- ifelse(bus_data$FIRST_GENERATION_IND == "Y", 1, 0)
bus_data$FALL_SPRING_RETENTION <- ifelse(bus_data$FALL_SPRING_RETENTION == "Y", 1, 0)
bus_data$FALL_SPRING_RETENTION[is.na(bus_data$FALL_SPRING_RETENTION) == T] <- 0
bus_data$EVER_CONCURRENT_IND <- ifelse(bus_data$EVER_CONCURRENT_IND == "Y", 1, 0)

bus_data$HIGH_SCHOOL_GPA <- ifelse(bus_data$HIGH_SCHOOL_GPA > 3, "AB",
                                   ifelse(bus_data$HIGH_SCHOOL_GPA > 2, "BC", 
                                          ifelse(bus_data$HIGH_SCHOOL_GPA > 1, "CD", 
                                                 ifelse(bus_data$HIGH_SCHOOL_GPA > 0, "DF", 
                                                        ifelse(is.na(bus_data$HIGH_SCHOOL_GPA) == T, "NA", "F")))))

# creating high risk ethnicity group dummy (Pacific Islander and Native American are known to retain at significantly lower levels than other groups)
bus_data$hr_rem <- ifelse(bus_data$ETHNICITY_CODE == "P" | bus_data$ETHNICITY_CODE == "I", 1, 0)

# imputing and adjusting problem data
## placement scores (max score is 120 so all values above this are incorrect)
bus_data$CPT_ARITHMEIC[bus_data$CPT_ARITHMEIC > 120] <- mean(bus_data$CPT_ARITHMEIC)
bus_data$CPT_COLLEGE_MATH[bus_data$CPT_COLLEGE_MATH > 120] <- mean(bus_data$CPT_COLLEGE_MATH)
bus_data$CPT_ELEM_ALGEBRA[bus_data$CPT_ELEM_ALGEBRA > 120] <- mean(bus_data$CPT_ELEM_ALGEBRA)
bus_data$CPT_READING[bus_data$CPT_READING > 120] <- mean(bus_data$CPT_READING)

## CB data
bus_data$CB_MEDIAN_INCOME[bus_data$CB_MEDIAN_INCOME < 0] <- mean(bus_data$CB_MEDIAN_INCOME)
bus_data$CB_MEDIAN_INCOME[is.na(bus_data$CB_MEDIAN_INCOME) == T] <- mean(bus_data$CB_MEDIAN_INCOME, na.rm = T)
bus_data$CB_ABOVE_SOME_COLLEGE[is.na(bus_data$CB_ABOVE_SOME_COLLEGE) == T] <- mean(bus_data$CB_ABOVE_SOME_COLLEGE, na.rm = T)
bus_data$CB_POPULATION[is.na(bus_data$CB_POPULATION) == T] <- mean(bus_data$CB_POPULATION, na.rm = T)
bus_data$CB_RENTER[is.na(bus_data$CB_RENTER) == T] <- mean(bus_data$CB_RENTER, na.rm = T)
bus_data$CB_PCT_COL <- bus_data$CB_ABOVE_SOME_COLLEGE/bus_data$CB_POPULATION
bus_data$CB_PCT_COL[is.na(bus_data$CB_PCT_COL) == T] <- 0

# withdrew from all courses in a term
## will have to interact both of these terms
bus_data$wd <- ifelse(is.na(bus_data$TERM_GPA) == T, 1, 0)
bus_data$TERM_GPA[is.na(bus_data$TERM_GPA) == T] <- 0

# 201840 dummy for PSM
bus_data$treated <- ifelse(bus_data$TERM_CODE == 201840, 1, 0)

# summary(bus_data)
# names(bus_data)

bus_data$credits_mo <- ifelse(bus_data$TERM_CREDITS >= 15, 1, 0) # credit momentum
bus_data$passing <- ifelse(bus_data$TERM_GPA >=2, 1, 0)
bus_data$male_age <- bus_data$male*bus_data$AGE_ON_FIRST_DAY
bus_data$col_first_gen <- bus_data$CB_PCT_COL*bus_data$FIRST_GENERATION_IND

bus_data$BUS_CREDITS[is.na(bus_data$BUS_CREDITS) == T] <- 0


################################################################
################ BUS student models ############################
################################################################

# BUS only model
bus_mo_bma <- as.formula(credits_mo ~ 
                           scale(AGE_ON_FIRST_DAY) +
                           male + 
                           male_age +
                           hr_rem + 
                           COLLEGE_READY_ENGLISH +
                           COLLEGE_READY_MATH +   
                           FIRST_GENERATION_IND +
                           EVER_CONCURRENT_IND +
                           CB_PCT_COL + 
                           scale(CB_MEDIAN_INCOME) + 
                           col_first_gen +
                           treated)

# bus_data credit momentum

bus_mo_model <- MCMClogit(bus_mo_bma, data = bus_data, glm.family="binomial",mcmc=40000,thin=40,burnin=5000)
bus_mo_sum <- summary(bus_mo_model)
# plot(bus_mo_model[,13], density = TRUE, trace = FALSE, main = "Pathways Estimated Treatment Effect on Credit Momentum", xlab = "") # checked

df_mo <- as.data.frame(bus_mo_model[,13])

# bus_data term credits
bus_tc_bma <- as.formula(scale(TERM_CREDITS) ~ 
                           scale(AGE_ON_FIRST_DAY) +
                           male + 
                           male_age +
                           hr_rem + 
                           COLLEGE_READY_ENGLISH +
                           COLLEGE_READY_MATH +   
                           FIRST_GENERATION_IND +
                           EVER_CONCURRENT_IND +
                           CB_PCT_COL + 
                           scale(CB_MEDIAN_INCOME) + 
                           col_first_gen +
                           treated)

bus_tc_model_mcmc <- MCMCregress(bus_tc_bma, data = bus_data, mcmc=40000,thin=40,burnin=5000)
bus_tc_sum <- summary(bus_tc_model_mcmc)
# plot(bus_tc_model_mcmc[,13], density = TRUE, trace = FALSE, main = "Pathways Estimated Treatment Effect on Fall Term Credits", xlab = "") # checked

df_tc <- as.data.frame(bus_tc_model_mcmc[,13])

# bus_data passing
bus_passing <- as.formula(passing ~
                            scale(AGE_ON_FIRST_DAY) +
                            male + 
                            male_age +
                            hr_rem + 
                            COLLEGE_READY_ENGLISH +
                            COLLEGE_READY_MATH +  
                            FIRST_GENERATION_IND +
                            EVER_CONCURRENT_IND +
                            CB_PCT_COL + 
                            scale(CB_MEDIAN_INCOME) +
                            col_first_gen +
                            TERM_CREDITS + 
                            treated
                          
)

bus_pass_model <- MCMClogit(bus_passing, data = bus_data, glm.family="binomial",mcmc=40000,thin=40,burnin=5000)
bus_pass_sum <- summary(bus_pass_model)
# plot(bus_pass_model[,14], density = TRUE, trace = FALSE, main = "Pathways Estimated Treatment Effect on Passing GPA", xlab = "") # checked

df_pass <- as.data.frame(bus_pass_model[,14])

# bus_data F-S retention
bus_reten <- as.formula(FALL_SPRING_RETENTION ~
                          scale(AGE_ON_FIRST_DAY) +
                          male + 
                          male_age +
                          hr_rem + 
                          COLLEGE_READY_ENGLISH +
                          COLLEGE_READY_MATH +  
                          FIRST_GENERATION_IND +
                          EVER_CONCURRENT_IND +
                          CB_PCT_COL + 
                          scale(CB_MEDIAN_INCOME) +
                          col_first_gen +
                          TERM_CREDITS + 
                          treated 
                        
)

bus_ret_model <- MCMCregress(bus_reten, data = bus_data, glm.family="binomial",mcmc=40000,thin=40,burnin=5000)
bus_ret_sum <- summary(bus_ret_model)
# plot(bus_ret_model[,14], density = TRUE, trace = FALSE, main = "Pathways Estimated Treatment Effect on F-S Retention", xlab = "") # checked

df_ret <- as.data.frame(bus_ret_model[,14])

# gateway momentum 
# ENGL_1010_2010_ENROLLMENTS
bus_gate_mcmc_el <- as.formula(ENGL_1010_2010_ENROLLMENTS ~ 
                                 scale(AGE_ON_FIRST_DAY) +
                                 male + 
                                 male_age +
                                 hr_rem + 
                                 COLLEGE_READY_ENGLISH +
                                 COLLEGE_READY_MATH +   
                                 FIRST_GENERATION_IND +
                                 EVER_CONCURRENT_IND +
                                 CB_PCT_COL + 
                                 scale(CB_MEDIAN_INCOME) + 
                                 col_first_gen +
                                 treated)

bus_gate_model_el <- MCMClogit(bus_gate_mcmc_el, data = bus_data, glm.family="binomial",mcmc=40000,thin=40,burnin=5000)
bus_engl_model_sum <- summary(bus_gate_model_el)
# plot(bus_gate_model_el[,13], density = TRUE, trace = FALSE, main = "Pathways Estimated Treatment Effect on ENGL gateway Momentum", xlab = "") # checked

df_engl <- as.data.frame(bus_gate_model_el[,13])

# QL_ENROLLMENTS
bus_ql_mcmc <- as.formula(QL_ENROLLMENTS ~ 
                            scale(AGE_ON_FIRST_DAY) +
                            male + 
                            male_age +
                            hr_rem + 
                            COLLEGE_READY_ENGLISH +
                            COLLEGE_READY_MATH +   
                            FIRST_GENERATION_IND +
                            EVER_CONCURRENT_IND +
                            CB_PCT_COL + 
                            scale(CB_MEDIAN_INCOME) + 
                            col_first_gen +
                            treated)

bus_ql_model <- MCMClogit(bus_ql_mcmc, data = bus_data, glm.family="binomial",mcmc=40000,thin=40,burnin=5000)
bus_ql_model_sum <- summary(bus_ql_model)
# plot(bus_ql_model[,13], density = TRUE, trace = FALSE, main = "Pathways Estimated Treatment Effect on QL gateway Momentum", xlab = "") # checked (actual affect is probably from self-guided placement)

df_ql <- as.data.frame(bus_ql_model[,13])

# Program gateway
# BUS_CREDITS
bus_gate_mcmc <- as.formula(BUS_CREDITS ~ 
                              scale(AGE_ON_FIRST_DAY) +
                              male + 
                              male_age +
                              hr_rem + 
                              COLLEGE_READY_ENGLISH +
                              COLLEGE_READY_MATH +   
                              FIRST_GENERATION_IND +
                              EVER_CONCURRENT_IND +
                              CB_PCT_COL + 
                              scale(CB_MEDIAN_INCOME) + 
                              col_first_gen +
                              treated)

bus_gate_model <- MCMCregress(bus_gate_mcmc, data = bus_data, mcmc=40000,thin=40,burnin=5000)
bus_gate_model_sum <- summary(bus_gate_model)
# plot(bus_gate_model[,13], density = TRUE, trace = FALSE, main = "Pathways Estimated Treatment Effect on Program Momentum", xlab = "") # checked


df_pro_gate <- as.data.frame(bus_gate_model[,13]) # this is close to seeming like a negative impact. On edge of 95%
