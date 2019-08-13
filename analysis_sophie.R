# SLM ratings analyses
# authors: Alexander Genauck, Sophie Kreicker
# date: 13.08.2019
# email: alexander.genauck@charite.de

agk.load.ifnot.install('ez')
agk.load.ifnot.install('ggplot2')
agk.load.ifnot.install('nlme')
agk.load.ifnot.install('pastecs')
agk.load.ifnot.install('reshape')
# agk.load.ifnot.install('WRS') ## doesn't work - R says there is no package named "WRS", but this package might be necessary when computing the main model

# There was SET A or Set B
# Set A: Lemon and Cherry THEN Strawberry and Blueberry; first controllable, then not controllable
# Set B: Lemon and Cherry THEN Strawberry and Blueberry; first not controllable, then controllable

## function to load or install packages
agk.load.ifnot.install <- function(package_name){
  if(require(package_name,character.only = T,quietly = T)){
    print(paste (package_name,"is loaded correctly"))
  } else {
    print(paste("trying to install", package_name))
    install.packages(pkgs = c(package_name))
    if(require(package_name,character.only = T)){
      print(paste(package_name,"installed and loaded"))
    } else {
      stop(paste("could not install",package_name))
    }
  }
}

## setting
# if FALSE then SLM1 is controllable and SLM2 is not
recode_according_to_first_controllable = TRUE

# test for which variables
#var_pair = c('SLM1_9','SLM2_9')
var_pair = c('SLM1_fun','SLM2_fun')

## get data
load('slm_quest.RData')

## research question I : Is there a difference in how firmly gamblers vs. controls believe in how much control they have over the slot machine?
## research question II: Is there a difference in how much fun gamblers vs. controls have during gambling on the slot machines?
## using a mixed anova to find out if there is an interaction effect

## making sure that the subject variable is treated as a factor rather than a continous variable
slm_quest$VPPG <- factor(slm_quest$VPPG)

## recoding item 7 as it is negatively coded [needed if SLM_4 to SLM_8 are used for building a "fun" scale]
## for hypothesis: "More fun at controllable than at uncontrollable questionnaire?"
slm_quest$SLM1_7 <- 100 - slm_quest$SLM1_7
slm_quest$SLM2_7 <- 100 - slm_quest$SLM2_7

# code the fun variable
slm_quest$SLM1_fun = apply(slm_quest[,grep('^SLM1_[4-8]',names(slm_quest))],FUN=mean,MARGIN=1)
slm_quest$SLM2_fun = apply(slm_quest[,grep('^SLM2_[4-8]',names(slm_quest))],FUN=mean,MARGIN=1)

## between groups effect: gamblers vs. controls
## within group effect: (supposedly) having control vs. not

## only first two items relevant for research question
## both items should be included in the analysis and be treated as single variables

## renaming data frame
slm_quest_relevant <- slm_quest[c('VPPG','HCPG','SLM1_1','SLM1_2','SLM2_1','SLM2_2',
                                  'SLM1_9','SLM2_9','first_controllable','SLM1_fun','SLM2_fun')]

# make one SLM_infl SLM_noninfl variable
# according to original data, in SLM1_1 and SLM2_1: '1' is no and '2' is yes
# BUT THE DATA TELLS OTHERWISE, as if the data had been recoded;
# perhaps they were wrongly programmed, so that VAS is missing in the second SLM
slm_quest_relevant$SLM1_2[is.na(slm_quest_relevant$SLM1_2) & slm_quest_relevant$SLM1_1 == 1] = 0 # have a feeling of control: NO; set controllability scoring to 0
slm_quest_relevant$SLM2_2[is.na(slm_quest_relevant$SLM2_2) & slm_quest_relevant$SLM2_1 == 1] = 0 # have a feeling of control: NO; set controllability scoring to 0
slm_quest_relevant$SLM1_1 = NULL
slm_quest_relevant$SLM2_1 = NULL

# check that no NA left
message('There is no NA left in SLM1_2 control-VAS variable:')
message(all(!is.na(slm_quest_relevant$SLM1_2)))
message(all(!is.na(slm_quest_relevant$SLM2_2)))


# let us turn to question SLM1_9 and SLM2_9, because SLM1_2 and SLM2_2 not useable
# "I had always the feeling I had control over the SLM"

# omit rows that have no info on which SLM was first
slm_quest_relevant = slm_quest_relevant[!is.na(slm_quest_relevant$first_controllable),]
slm_quest_relevant$VPPG = droplevels(slm_quest_relevant$VPPG)

# recode to NA
message('The variables SLM1_9 and SLM2_9 are both free of missing values:')
message(all(!is.na(slm_quest_relevant$SLM1_9)))
message(all(!is.na(slm_quest_relevant$SLM2_9)))

message('The variables SLM1_fun and SLM2_fun are both free of missing values:')
message(all(!is.na(slm_quest_relevant$SLM1_fun)))
message(all(!is.na(slm_quest_relevant$SLM2_fun)))


# RECODE according to first_controllable variable
slm_quest_relevant$Controllable = NA
slm_quest_relevant$NotControllable = NA
slm_quest_relevant$Controllable_asis = NA
slm_quest_relevant$NotControllable_asis = NA
for (ii in 1:length(slm_quest_relevant$VPPG)) {
  if (recode_according_to_first_controllable) {
    if (slm_quest_relevant$first_controllable[ii]) {
      slm_quest_relevant$Controllable[ii] = slm_quest_relevant[[var_pair[1]]][ii]
      slm_quest_relevant$NotControllable[ii] = slm_quest_relevant[[var_pair[2]]][ii]
    } else {
      slm_quest_relevant$Controllable[ii] = slm_quest_relevant[[var_pair[2]]][ii]
      slm_quest_relevant$NotControllable[ii] = slm_quest_relevant[[var_pair[1]]][ii]
    }
  } else {
    slm_quest_relevant$Controllable[ii] = slm_quest_relevant[[var_pair[1]]][ii]
    slm_quest_relevant$NotControllable[ii] = slm_quest_relevant[[var_pair[2]]][ii]
  }
}

slm_quest_relevant$SLM1_2 = NULL
slm_quest_relevant$SLM2_2 = NULL
slm_quest_relevant$SLM1_9 = NULL
slm_quest_relevant$SLM2_9 = NULL
slm_quest_relevant$SLM1_fun = NULL
slm_quest_relevant$SLM2_fun = NULL
slm_quest_relevant$first_controllable = NULL

# long format
slm_quest_long = reshape2::melt(slm_quest_relevant,measure.vars = c('Controllable','NotControllable'))
names(slm_quest_long)[grep('value',names(slm_quest_long))] = 'rating_controllability'
names(slm_quest_long)[grep('variable',names(slm_quest_long))] = 'instr_controllability'


# descriptives over all
plot(slm_quest_long$rating_controllability ~ slm_quest_long$instr_controllability)
print(describeBy(slm_quest_long$rating_controllability,slm_quest_long$instr_controllability))
title('Boxplots overall')

plot(slm_quest_long$rating_controllability[slm_quest_long$HCPG == 'PG'] ~ slm_quest_long$instr_controllability[slm_quest_long$HCPG == 'PG'])
print(describeBy(slm_quest_long$rating_controllability[slm_quest_long$HCPG == 'PG'],slm_quest_long$instr_controllability[slm_quest_long$HCPG == 'PG']))
title('Boxplots just GD subjects')

warning('Controllable and NotControllable are taken from original data; can be switched off and then data is used as is. But that is exaclty the same.')


## control conditions/variables: not controllable/HC
## setting contrasts not necessary
## run mixed ANOVA
SLM_Model<-ezANOVA(data = slm_quest_long, dv = .(rating_controllability), wid = .(VPPG), 
                   between = .(HCPG), within = .(instr_controllability), type = 2, detailed = TRUE)
print(SLM_Model)