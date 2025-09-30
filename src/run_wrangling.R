# lydia barnes, march 2024 this script extracts, formats, and summarises data from the 'doors'
# project.
# amended by KG. 2025, for the 'doors' project in 2025

### sources
library(tidyverse)
library(zeallot) #unpack/destructure with %<-%
library(stringr)

source(file.path("src", "get_subs.R"))
source(file.path("src", "get_switch.R"))
source(file.path("src", "get_data.R"))
source(file.path("src","get_setting_stability.R"))
source(file.path("src","get_transition_probabilities.R"))
source(file.path("src","get_learned_doors.R"))
source(file.path("src", "get_rts.R"))
source(file.path("src", "join_multi_data.R"))

### settings

# !you will want to update these settings a lot during piloting, when the task code or the way you
# test changes, or when you test participants on different subsets of the task phases
version <- ''
exp <- 'exp-flex' #'exp-multi' #'exp-flex'
sess <- c("ses-learn","ses-learn2","ses-train","ses-test") # these sessions are common to 
# both experiments so are the only 2 that need to be listed here. the sessions from the 
# multitask experiment are dealt with in the code below

### paths

# !if you open the project thru doors.Rproj, your working directory will automatically be the
# project path
project_path <- getwd()
if (!dir.exists(file.path(project_path, "res"))) {
  # check that the results directory exists. if it doesn't, create it.
  dir.create(file.path(project_path, "res"))
}

# !you will need to change the data path to match the location of OneDrive on your personal
# computer
file_path <- "data"
exp_path <- str_glue("/{exp}/{version}")
data_path <- file.path(file_path + exp_path)

if (!dir.exists(data_path)) {
  stop(paste0(data_path, " does not exist"))
}

### load an up-to-date list of participants
files <- list.files(data_path, pattern = str_glue('.*(beh.tsv)'), recursive = T) 
subs <- unique(str_split_i(files, "/", 1))

### extract events from the raw data

# make an empty data frame with all the variables (columns) that we will want
grp_data <- data.frame(
  sub = integer(), ses = integer(), subses = integer(), t = integer(), context = integer(), door = integer(),
  door_cc = integer(), door_oc = integer(), on = numeric(), off = numeric(), start = numeric(),
  switch = integer(), train_type = integer())

# make an empty data frame for the multitasking conditions (note that this will not
# get used in the exp-flex exp, only the exp-multi one)
grp_multi <- data.frame(
  sub = integer(), ses = integer(), t = integer(), context = integer(),
  mem_tgt_trial = integer(), mem_probe_trial = integer(), mem_context = integer()
)
# create empty data frame for the response time data # KG. commented out as now deal with during get_data
# grp_ons <- data.frame(
#   sub = integer(), ses = integer(), t = integer(), context = integer(), on = integer()
# )

# for each subject and session, use the function 'get_data' to load their raw data and attach it to
# our 'grp_data' data frame with one measurement (row) per event (click or hover)
for (sub in subs) {
  print(sub)
  
  sid <- as.numeric(substring(sub,5,7))
  for (ses in sess) {
    
    if (sid == 73 & ses == 'ses-learn') 
      next # skipping ses-learn for sub73 because the file was missing data
    
    train_type <- NA
    context_one_doors <- NA
    train_doors <- NA
    
    if (ses == "ses-test") {
      train_type <- grp_data %>%
        filter(sub == sid, ses == 2) %>%
        select(train_type) %>% 
        unique() %>% 
        pull()
      train_doors <- grp_data %>% 
        filter(sub==sid,ses==ses,door_cc==1) %>% 
        select(door,context) %>% 
        unique()
    }
    
      data <- get_data(data_path, exp, sub, ses, train_type, train_doors) # load and format raw data
      grp_data <- rbind(grp_data, data$resps) # add to the 'grp_data' data frame so we end up with all subjects and sessions in one spreadsheet
      if (grepl('exp-multi', exp) & grepl('ses-test', ses)){
        grp_multi <- rbind(grp_multi, data$multi)
      }
    }
  }
grp_data <- get_setting_stability(grp_data) # KG: re-wrote to label for 2025
grp_data <- grp_data %>% mutate(door_nc = case_when(door_cc==1 ~ 0, door_oc == 1 ~ 0, .default=1), .after="door_oc")

# save the formatted data
fnl <- file.path(project_path, "res", paste(paste(exp, "evt", sep = "_"), ".csv", sep = ""))
write_csv(grp_data, fnl)

### extract accuracy and response time averages from event data

# by trial
res <- grp_data %>%
  group_by(sub, ses, subses, t, context, train_type) %>%
  summarise(
    switch = max(switch), n_clicks = n(), n_cc = sum(door_cc), n_oc = sum(door_oc), n_nc = sum(door_nc),
    sticks = sum(door_oc[set_err_idx == 'stick'], na.rm=T),
    slips = sum(door_oc[set_err_idx == 'slip'], na.rm=T),
    accuracy = n_cc / n_clicks,
    setting_errors = n_oc / n_clicks,
    general_errors = n_nc / n_clicks,
    setting_sticks = sticks / n_oc,
    setting_slips = slips / n_oc
) %>% ungroup() 


# get the different types of RTs, cleaning is exp dependent
rt_dat <- get_rts(grp_data, exp, grp_multi)
# add the RTs to the res data frame
res <- inner_join(res, rt_dat, by=c("sub", "ses", "subses", "switch", "t", "context", "train_type"))

fnl <- file.path(project_path, "res", paste(paste(exp, "trl", sep = "_"), ".csv", sep = ""))
write_csv(res, fnl)

# by subject - for the data that the honours analysis will be applied to
#   grouping by subsession

# res <- read_csv(fnl)

# calculcate setting sticks and setting slips as proportion of setting errors
res <- res %>% 
  mutate(setting_sticks = replace_na(setting_sticks, 0),
         setting_slips = replace_na(setting_slips, 0)) %>% 
  mutate(setting_sticks_pe = setting_sticks*setting_errors,
         setting_slips_pe = setting_slips*setting_errors)

if (exp == 'exp-flex'){
  
  res_ss <- res %>%
    ungroup() %>% 
    group_by(sub, ses, subses, switch, train_type) %>%
    summarise(
      across(
        .cols = where(is.numeric),
        .fns = list(mean = ~mean(.x, na.rm = TRUE),
                    sd = ~sd(.x, na.rm = TRUE),
                    se = ~sd(.x, na.rm = TRUE)/sqrt(n())),
        .names = "{.col}_{.fn}"
      )
    ) %>%
    select(-starts_with("t_"),
           -starts_with("context_"),
           -starts_with('n_'),
           -starts_with('sticks_'),
           -starts_with('slips_')) %>%
    ungroup() %>%
    filter(ses != 1) %>%
    filter(ses != 10)
  fnl <- file.path(project_path, "res", paste(paste(exp, "avg-ss", sep = "_"), ".csv", sep = ""))
  write_csv(res_ss, fnl)
  
  #   just grouping by session
  res_s <- res %>%
    ungroup() %>%
    select(!subses) %>%
    group_by(sub, ses, switch, train_type) %>%
    summarise(
      across(
        .cols = where(is.numeric),
        .fns = list(mean = ~mean(.x, na.rm = TRUE),
                    sd = ~sd(.x, na.rm = TRUE),
                    se = ~sd(.x, na.rm = TRUE)/sqrt(n())),
        .names = "{.col}_{.fn}"
      )
    ) %>%
    select(-starts_with("t_"),
           -starts_with("context_"),
           -starts_with('n_'),
           -starts_with('sticks_'),
           -starts_with('slips_')) %>%
    ungroup() %>%
    filter(ses != 1) %>%
    filter(ses != 10)

  fnl <- file.path(project_path, "res", paste(paste(exp, "avg", sep = "_"), ".csv", sep = ""))
  write_csv(res_s, fnl)
} else {
  
  res_ss <- res %>%
    ungroup() %>% 
    group_by(sub, ses, subses, switch, train_type, multi_trial, multi_cond) %>%
    summarise(
      across(
        .cols = where(is.numeric),
        .fns = list(mean = ~mean(.x, na.rm = TRUE)),
        .names = "{.col}_{.fn}"
      )
    ) %>%
    select(-starts_with("t_"),
           -starts_with("context_"),
           -starts_with('n_'),
           -starts_with('sticks_'),
           -starts_with('slips_')) %>%
    ungroup() %>%
    filter(!(ses == 3 & switch == 1)) %>%
    filter(ses != 1)  %>%
    filter(ses != 10)
    fnl <- file.path(project_path, "res", paste(paste(exp, "avg-ss", sep = "_"), ".csv", sep = ""))
    write_csv(res_ss, fnl)
  
  
    res_s <- res %>%
      ungroup() %>% 
      select(!subses) %>%
      group_by(sub, ses, switch, train_type, multi_trial, multi_cond) %>%
      summarise(
        across(
          .cols = where(is.numeric),
          .fns = list(mean = ~mean(.x, na.rm = TRUE)),
          .names = "{.col}_{.fn}"
        )
      ) %>%
      select(-starts_with("t_"),
             -starts_with("context_"),
             -starts_with('n_'),
             -starts_with('sticks_'),
             -starts_with('slips_')) %>%
      ungroup() %>%
      filter(!(ses == 3 & switch == 1)) %>%
      filter(ses != 1)  %>%
      filter(ses != 10)
    fnl <- file.path(project_path, "res", paste(paste(exp, "avg", sep = "_"), ".csv", sep = ""))
    write_csv(res_s, fnl)
}


