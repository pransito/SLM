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

## load or install packages
agk.load.ifnot.install('ez')
agk.load.ifnot.install('ggplot2')
agk.load.ifnot.install('nlme')
agk.load.ifnot.install('pastecs')
agk.load.ifnot.install('reshape')
# agk.load.ifnot.install('WRS') ## doesn't work - R says there is no package named "WRS", but this package might be necessary when computing the main model

## get data
load('slm_quest.RData')

## research question: Is there a difference in how firmly gamblers vs. controls believe in how much control they have over the slot machine?
## using a mixed anova to find out if there is an interaction effect

## making sure that the subject variable is treated as a factor rather than a continous variable
slm_quest$VPPG <- factor(slm_quest$VPPG)

## recoding item 7 as it is negatively coded
slm_quest$SLM1_7 <- 100 - slm_quest$SLM1_7
slm_quest$SLM2_7 <- 100 - slm_quest$SLM2_7

## between groups effect: gamblers vs. controls
## within group effect: (supposedly) having control vs. not

## only first two items relevant for research question
## both items should be included in the analysis and be treated as single variables

## renaming data frame
slm_quest_relevant <- slm_quest[c('VPPG','HCPG','SLM1_1','SLM1_2','SLM2_1','SLM2_2','first_controllable')]

# make one SLM_infl SLM_noninfl variable
warning('We have to check the original data! The SLM question for second SLM seemes to be poled the other way around.')
slm_quest_relevant$SLM1_2[is.na(slm_quest_relevant$SLM1_2) & slm_quest_relevant$SLM1_1 == 1] = 0
slm_quest_relevant$SLM2_2[is.na(slm_quest_relevant$SLM2_2) & slm_quest_relevant$SLM2_1 == 2] = 0
slm_quest_relevant$SLM1_1 = NULL
slm_quest_relevant$SLM2_1 = NULL

# check that no NA left
message('There is no NA left in dependent variable:')
message(all(!is.na(slm_quest_relevant$SLM1_2)))
message(all(!is.na(slm_quest_relevant$SLM2_2)))

# omit NA
slm_quest_relevant = na.omit(slm_quest_relevant)
slm_quest_long$VPPG = droplevels(slm_quest_long$VPPG)

# RECODE
slm_quest_relevant$Controllable = NA
slm_quest_relevant$NotControllable = NA



for (ii in 1:length(slm_quest_relevant$VPPG)) {
  if (slm_quest_relevant$first_controllable[ii]) {
    slm_quest_relevant$Controllable[ii] = slm_quest_relevant$SLM1_2[ii]
    slm_quest_relevant$NotControllable[ii] = slm_quest_relevant$SLM2_2[ii]
  } else {
    slm_quest_relevant$Controllable[ii] = slm_quest_relevant$SLM2_2[ii]
    slm_quest_relevant$NotControllable[ii] = slm_quest_relevant$SLM1_2[ii]
  }
}

slm_quest_relevant$SLM1_2 = NULL
slm_quest_relevant$SLM2_2 = NULL
slm_quest_relevant$first_controllable = NULL

# long format
slm_quest_long = reshape2::melt(slm_quest_relevant,measure.vars = c('Controllable','NotControllable'))
names(slm_quest_long)[grep('value',names(slm_quest_long))] = 'rating_controllability'
names(slm_quest_long)[grep('variable',names(slm_quest_long))] = 'instr_controllability'


# descriptives
plot(slm_quest_long$rating_controllability ~ slm_quest_long$instr_controllability)
describeBy(slm_quest_long$rating_controllability,slm_quest_long$instr_controllability)

warning('Controllable and NotControllable seemed to be switched around or still completely random; need to check original data.')


## control conditions/variables: not controllable/HC
## setting contrasts not necessary
## run mixed ANOVA
SLM_Model<-ezANOVA(data = slm_quest_long, dv = .(rating_controllability), wid = .(VPPG), 
                   between = .(HCPG), within = .(instr_controllability), type = 2, detailed = TRUE)
SLM_Model

