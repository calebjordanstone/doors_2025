## CS: function to retrieve the doors belonging to "neither" context (i.e., context "3" and "4")

get_all_doors <- function(data_path, sub, ses) {
  path <- file.path(data_path, sub, ses, 'beh', paste(sub, ses, "task-mts_trls.tsv", sep='_'))
  dat <- read_table(path) 
  # extract doors from all target locations
  doors <- dat %>% 
    group_by(context) %>% 
    filter(row_number()==1) %>% 
    select(sub, loca, locb, locc, locd)
  
  return(doors)
  }
  

