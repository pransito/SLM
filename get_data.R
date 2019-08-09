library(utils)


# get the SLM data
# need to get quest data before using import_data_pdt.R

# get the variables from data_quest, and ID variable
desired_vars = names(data_quest)[grep('^SLM',names(data_quest))]
slm_quest = data_quest[c('data_par.P104_01',desired_vars)]
names(slm_quest)[1] = 'VPPG'

# get the var names
slm_quest_names = questionnaires_vars_names[desired_vars]

# get the group information
slm_quest = merge(slm_quest,dat_match[c('VPPG','HCPG','PhysioVP.x','Cohort')],by = 'VPPG')

# get the info which SLM was the belief-SLM
# i.e. the SLM which allowed for illusion of control
slm_quest = merge(slm_quest,tnl[c('VPPG','SLM_Set','SLM_Bemerkung')],by = 'VPPG',all.x = T,all.y = F)

## get the information which condition came first

# function to get the information from logfile
controllable = function(cur_file) {
  
  con = file(cur_file, "r")
  lines_20 = readLines(con, n = 20)
  close(con)
  
  
  if (length(grep('beeinflussbar',lines_20[20])) != 0) {
    return(TRUE)
  } else if (length(grep('zufaellig',lines_20[20])) != 0) {
    return(FALSE)
  } else {
    return('DID NOT FIND PROPER INFORMATION')
  }
}

# for this we will go into the original logfiles of each subject
MRI_SLM_path = 'S:/AG/AG-Spielsucht2/Daten/VPPG_Daten/MRT'
postpilot_SLM_path = 'S:/AG/AG-Spielsucht2/Daten/VPPG_Daten/Adlershof/Daten/SLM/POSTPILOT'
pgpilot_SLM_path = 'S:/AG/AG-Spielsucht2/Daten/VPPG_Daten/Adlershof/Daten/SLM/PG'
all_subs = slm_quest$VPPG

# check that all subjects are unique
stopifnot(all(!duplicated(all_subs)))

# prep a results list
res_list = list()

for (ii in 1:length(all_subs)) {
  # clear variables
  cur_sub_pp = NULL
  cur_sub = NULL
  
  # get the data of this subject
  cur_sub = all_subs[ii]
  cur_dat = subset(slm_quest, VPPG == cur_sub)

  # get the folder where the original data of this subject might be
  if (cur_dat$Cohort == 'MRI') {
    Cohort = 'MRI'
    setwd(MRI_SLM_path)
    cur_sub_pp = '999999999'
  } else if (cur_dat$Cohort == 'PGPilot') {
    Cohort = 'PGPilot'
    setwd(pgpilot_SLM_path)
    cur_sub_pp = cur_dat$PhysioVP.x
  } else if (cur_dat$Cohort == 'POSTPILOT') {
    Cohort = 'POSTPILOT'
    setwd(postpilot_SLM_path)
    cur_sub_pp = cur_dat$PhysioVP.x
  } else {
    stop('Unexcepted value for "Cohort"')
  }
  
  # get the folders available
  folders_available = dir()
  
  if (Cohort == 'PGPilot') {
    folders_available = paste0('PhysioVP',folders_available)
  }
  
  # change into the needed folder
  if ((cur_sub %in% folders_available) | (cur_sub_pp %in% folders_available)) {
    if (Cohort == 'MRI') {
      setwd(cur_sub)
    } else {
      if (Cohort == 'POSTPILOT') {
        setwd(cur_sub_pp)
      } else if (Cohort == 'PGPilot') {
        setwd(gsub('PhysioVP','',cur_sub_pp))
      }
    }
    
  } else {
    message(paste('The subject ', cur_sub,'was not among the available folders.'))
    next
  }
  
  # go down the rabbit hole to find the logfiles
  if (Cohort == 'MRI') {
    setwd('Behav/SLM/')
  } else if (Cohort == 'POSTPILOT' | Cohort == 'PGPilot') {
    # do nothing
  } else {
    stop('Cohort has unknown value!')
  }
  
  # check if run folder/files there
  check_1 = !((length(grep(paste0('run_?',1),dir())) == 1) & (length(grep(paste0('run_?',2),dir())) == 1))
  check_2 = !((length(grep(paste0('_s',1),dir())) == 2) & (length(grep(paste0('_s',2),dir())) == 2))
  
  if (check_1 & check_2) {
    res = 'MISSING DATA'
    res_list[[ii]] = res
    if (!is.null(cur_sub_pp)) {
      message(cur_sub_pp)
    } else {
      message(cur_sub)
    }
    
    next
    # # unpack if necessary
    # if (length(grep('zip',dir())) == 1) {
    #   cur_zip_file = dir()[grep('zip',dir())]
    #   unzip(cur_zip_file)
    #   message('I had to unzip!')
    #   next
    # } else {
    #   next
    # }
  }

  # get run1 and run2
  runs = c(1,2)
  res = c()
  for (jj in runs) {
    if (Cohort == 'MRI' | Cohort == 'POSTPILOT') {
      # get and set the current run folder
      cur_run_folder = dir()[grep(paste0('run_?',jj),dir())]
    } else if (Cohort == 'PGPilot') {
      # get and set the current run folder
      cur_run_folder = dir()[grep(paste0('_s',jj),dir())]
    }
    
    if (Cohort == 'MRI' | Cohort == 'POSTPILOT') {
      setwd(cur_run_folder)
    }
    
    # get the current files and select the correct one
    if (Cohort == 'MRI' | Cohort == 'POSTPILOT') {
      current_files = dir()
    } else {
      current_files = cur_run_folder
    }
    needed_file = current_files[grep('txt',current_files)]
    
    # extract the information what condition this SLM was
    res[jj] = controllable(needed_file)
    
    if (Cohort == 'MRI' | Cohort == 'POSTPILOT') {
      # go back one up
      setwd('..')
    }
    
  } 
  
  # pack the results
  res_list[[ii]] = res
}


slm_quest$first_controllable = unlist(lapply(res_list,FUN = first))
slm_quest$first_controllable = as.factor(agk.recode(slm_quest$first_controllable,'MISSING DATA',NA))


# save the data

# save into a workspace
setwd('C:/Users/genaucka/GitHub/SLM')
save(file = 'slm_quest.RData',list = c('slm_quest','slm_quest_names'))

