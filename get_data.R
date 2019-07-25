# get the SLM data
# need to get quest data before using import_data_pdt.R

# get the variables from data_quest, and ID variable
desired_vars = names(data_quest)[grep('^SLM',names(data_quest))]
slm_quest = data_quest[c('data_par.P104_01',desired_vars)]
names(slm_quest)[1] = 'VPPG'

# get the var names
slm_quest_names = questionnaires_vars_names[desired_vars]

# get the group information
slm_quest = merge(slm_quest,dat_match[c('VPPG','HCPG')],by = 'VPPG')

# get the info which SLM was the belief-SLM
# i.e. the SLM which allowed for illusion of control
slm_quest = merge(slm_quest,tnl[c('VPPG','SLM_Set','SLM_Bemerkung')],by = 'VPPG',all.x = T,all.y = F)

# save into a workspace
setwd('C:/Users/genaucka/GitHub/SLM')
save(file = 'slm_quest.RData',list = c('slm_quest','slm_quest_names'))



