join_multi_data <- function(data, multi_data, doors){
  # K. Garner - 2025
  # this joins together the grp level search task data with the multitask conditions
  
  # what I ultimately want is a vector that says - 'first' response of 
  # multitasking block, or 'subsequent' response of multitasking block
  # and one that says - other, neither, or none to denote multitasking condition
  # these vectors should be taken from multidata, but joined to data, by sub, ses, and trial number
  multi_data <- multi_data %>% mutate(multi_trial = case_when(mem_tgt_trial > 0 ~ 'first', .default ='subsequent'),
                                      multi_cond = case_when(mem_context == 1 | mem_context == 2 ~ 'other',
                                                             mem_context == 3 | mem_context == 4 ~ 'neither',
                                                             .default='none')) %>% 
    rename(ses=sess) %>%
    select(sub, ses, t, context, mem_context, multi_trial, multi_cond)
  
  # multi_data <- multi_data %>% 
  #   mutate(mem_grp = case_when(
  #     mem_tgt_trial > 0 ~ 'start',
  #     (mem_tgt_trial == 0) & (mem_probe_trial == 0) ~ 'middle',
  #     mem_probe_trial > 0 ~ 'end'))
  
  # multi_data <- multi_data %>% 
  #   mutate(mem_grp = case_when(
  #     mem_tgt_trial > 0 ~ 'first',
  #     T  ~ 'subs'))
  
  ## CS edits: add grouping variable to indicate which trials belong to the same working memory targets
  grp <- 0
  sub <- 1
  mem_grper <- function(x, y) {
    if (y != sub) { # reset things when encountering new subject
      sub <<- y
      grp <<- 0}
    if (x == 'first') {grp <<- grp + 1} # add 1 to grp label every time we encounter 'first' label of multi_trial variable
    else {grp = grp}
    return (grp)
    }
  
  multi_data <- multi_data %>%
    rowwise() %>%
    mutate(mem_grp_idx=mem_grper(multi_trial, sub)) %>%
    ungroup()
  
  ## CS edits: add working memory target locations to data frame and then check 
  # whether the selected door is in those locations
  doors <- doors %>% rename(mem_context=context)
  multi_data <- left_join(multi_data, doors, by=c('sub', 'mem_context'))
  data <- left_join(data, multi_data, by=c('sub', 'ses', 't', 'context'))
  data$door_in_wmt_locs <- apply(data[, c('door', 'loca', 'locb', 'locc', 'locd')], 
                                 1, 
                                 function(row) any(row[2:5] == row[1]))
  # get summary output
  data_filt <- data %>% 
    ungroup() %>%
    filter(ses==3 & door_nc==1 & multi_cond=='neither')
  pcnt <- data_filt %>%
    summarise(pcnt=sum(door_in_wmt_locs)/n())
  mean_pcnt <- data_filt %>%
    group_by(sub) %>%
    summarise(pcnt=sum(door_in_wmt_locs)/n()) %>% 
    summarise(mean(pcnt))
  
  print(paste0('Number of trials where general errors on neither trials match WMT locations: ', nrow(data_filt)))
  print(paste0('Percent of trials where general errors on neither trials match WMT locations: ', pcnt))
  print(paste0('Mean pcnt of trials where general errors on neither trials match WMT locations: ', mean_pcnt))
  return(data)
}

