# Caleb Stone, July 2025: this scripts reformats and combines some data files 
# for later analysis in jamovi. Run this script after run_wrangling, run_wmt, and run_survey

### load libraries
library(data.table)
library(tidyverse)
library(magrittr)
library(reshape2)

### exp-flex
exp <- "exp-flex"

# load data files
avg_flex <- fread("res/exp-flex_avg.csv")
avg_flex_ss <- fread("res/exp-flex_avg-ss.csv")
svy_flex <- fread("res/exp-flex_svy.csv")

# fill in missing data - if no setting errors, replace NaN values with 0 for setting sticks/slips?
# avg_flex[setting_errors_mean == 0, ':=' (setting_sticks_mean = 0,
#                                          setting_slips_mean = 0)]

# calculate linear integrated speed-accuracy score (LISAS) # Vandierendonck (2017), Behav Res, 49:653–673 
# avg_flex[, ':=' (setting_sticks_PE = setting_sticks_mean * setting_errors_mean, # calculate setting_sticks as a proportion of total errors
#                  setting_slips_PE = setting_slips_mean * setting_errors_mean) # calculate setting_slips as a proportion of total errors
# ][, ':=' (rt_first_correct_mean_SD = sd(rt_first_correct_mean),  # calculate standard deviation across switch condition by subject and session for variables of interest
#           rt_subs_correct_mean_SD = sd(rt_subs_correct_mean),
#           rt_post_error_mean_SD = sd(rt_post_error_mean),
#           setting_sticks_PE_SD = sd(setting_sticks_PE),
#           setting_slips_PE_SD = sd(setting_slips_PE)), 
#   by=.(sub, ses)
# ][, ':=' (LISAS_rt_fst_cor = rt_first_correct_mean + (setting_sticks_PE*(rt_first_correct_mean_SD/setting_sticks_PE_SD)), # compute LISAS variables
#           LISAS_rt_sub_cor = rt_subs_correct_mean + (setting_slips_PE*(rt_subs_correct_mean_SD/setting_slips_PE_SD)))
# ][is.nan(LISAS_rt_fst_cor), LISAS_rt_fst_cor := rt_first_correct_mean # fill in missing LISAS scores with original values
# ][is.nan(LISAS_rt_sub_cor), LISAS_rt_sub_cor := rt_subs_correct_mean]

# calculate linear integrated speed-accuracy score (LISAS) # Vandierendonck (2017), Behav Res, 49:653–673 
avg_flex[, ':=' (LISAS_rt_fst_cor = rt_first_correct_mean + (setting_sticks_pe_mean*(rt_first_correct_sd/setting_sticks_pe_sd)), # compute LISAS variables
                 LISAS_rt_sub_cor = rt_subs_correct_mean + (setting_slips_pe_mean*(rt_subs_correct_sd/setting_slips_pe_sd)))
][is.nan(LISAS_rt_fst_cor), LISAS_rt_fst_cor := rt_first_correct_mean # fill in missing LISAS scores with original values
][is.nan(LISAS_rt_sub_cor), LISAS_rt_sub_cor := rt_subs_correct_mean]

# save output file
fln <- file.path("res", paste(paste(exp, "avg", sep = "_"), ".csv", sep = ""))
write_csv(avg_flex, fln)

# rename factor levels
avg_flex[, switch := ifelse(switch==0, 'non-switch', 'switch')]
avg_flex[, train_type := ifelse(train_type==1, 'stable', 'variable')]
avg_flex[, ses := ifelse(ses==2, 'training', 'testing')]
avg_flex_ss[, subses := ifelse(subses==1, 'first', 'last')]
avg_flex_ss[, switch := ifelse(switch==0, 'non-switch', 'switch')]
avg_flex_ss[, train_type := ifelse(train_type==1, 'stable', 'variable')]
avg_flex_ss[, ses := ifelse(ses==2, 'training', 'testing')]

# reshape data frame
avg_flex_wide_train <- dcast.data.table(avg_flex_ss, 
                                        sub + ses + train_type ~ subses, 
                                        value.var = c('setting_errors_mean', 'general_errors_mean', 'rt_subs_correct_mean'), 
                                        subset = .(ses=='training' & switch=='non-switch'),
                                        fun.aggregate=mean) # long to wide for training session
avg_flex_wide_test <- dcast.data.table(avg_flex, 
                                       sub + ses + train_type ~ switch,
                                       value.var = c('LISAS_rt_fst_cor', 'LISAS_rt_sub_cor', 'general_errors_mean'), 
                                       subset = .(ses=='testing'),
                                       fun.aggregate=mean) # long to wide for test session

avg_flex_wide <- rbindlist(list(avg_flex_wide_train, avg_flex_wide_test), fill=T) # combine into 1 data frame

# add survey data
svy_flex_totals <- svy_flex[, .(sub, total_wout_observing)]
avg_flex_wide <- svy_flex_totals[avg_flex_wide, on='sub']
setnames(avg_flex_wide, "total_wout_observing", "FFMQ")
setnames(avg_flex_wide, "sub", "subID")

# select columns of interest for analysis
# cols <- c("subID|FFMQ|ses|train_type|mean|LISAS")
# avg_flex_wide <- avg_flex_wide[, .SD, .SDcols = patterns(cols)]

# save output file
fln <- file.path("res", paste(paste(exp, "avg-wide_v2", sep = "_"), ".csv", sep = ""))
write_csv(avg_flex_wide, fln)


### exp-multi
exp <- "exp-multi"

# load data files
avg_multi <- fread("res/exp-multi_avg.csv")
mts_multi <- fread('res/exp-multi_mts_avg.csv')

# rename factor levels
# avg_multi[, subses := ifelse(subses==1, 'first', 'last')]
avg_multi[, switch := ifelse(switch==0, 'non-switch', 'switch')]
avg_multi[, train_type := ifelse(train_type==1, 'stable', 'variable')]
avg_multi[, ses := ifelse(ses==2, 'training', 'testing')]

mts_multi <- mts_multi[unique(avg_multi[, .(sub, train_type)]), on='sub']
mts_multi[, cond := ifelse(cond=='nc', 'neither', 'other')]
mts_multi[, stage := ifelse(stage=='3', 'testing', 'initial')]
setnames(mts_multi, 'stage', 'ses') # rename stage to ses 

# fill in missing data - if no setting errors, replace NaN values with 0 for setting sticks/slips
# avg_multi[setting_errors_mean == 0, ':=' (setting_sticks_mean = 0,
#                                           setting_slips_mean = 0)]

# reshape search task data
avg_multi_train <- dcast.data.table(avg_multi, 
                                sub + train_type + ses ~ switch, 
                                value.var = c('accuracy_mean', 
                                              'setting_errors_mean', 
                                              'general_errors_mean',
                                              'rt_first_correct_mean',
                                              'rt_subs_correct_mean'), #names(avg_multi)[7:14], 
                                subset = .(ses=='training' & switch=='non-switch')) # long to wide for training session

## need to complete when variables are decided
avg_multi_test <- dcast.data.table(avg_multi,
                              sub + train_type + ses ~ multi_cond,
                              value.var = c('accuracy_mean', 
                                            'setting_errors_mean', 
                                            'general_errors_mean'),
                              subset = .(ses=='testing'),
                              fun.aggregate = mean) # long to wide for test session


avg_multi_test_rt <- dcast.data.table(avg_multi,
                               sub + train_type + ses ~ multi_cond + multi_trial,
                               value.var = c('rt_first_correct_mean',
                                             'rt_subs_correct_mean'),
                               subset = .(ses=='testing'),
                               fun.aggregate = mean) 

avg_multi_test <- avg_multi_test[avg_multi_test_rt, on=c('sub', 'train_type', 'ses')]

# setnames(avg_multi_test, 
#          old=c('neither', 'none', 'other'), 
#          new=c('multi_cond_neither', 'multi_cond_none', 'multi_cond_other'))

avg_multi_wide <- rbindlist(list(avg_multi_train, avg_multi_test), fill=T) # combine into 1 data frame

# save data file
fln <- file.path("res", paste(paste(exp, "avg-wide_v4", sep = "_"), ".csv", sep = ""))
write_csv(avg_multi_wide, fln)

# reshape working memory task data
mts_multi_wide <- dcast.data.table(mts_multi,
                                   sub + ses + train_type ~ cond,
                                   value.var = c('accuracy_mean', 'rt_mean')) # long to wide for working memory task data


# save wokring memory data file
fln <- file.path("res", paste(paste(exp, "mts_avg-wide", sep = "_"), ".csv", sep = ""))
write_csv(mts_multi_wide, fln)
