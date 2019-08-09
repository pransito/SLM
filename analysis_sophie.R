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

## remove irrelevant columns
COPY$SLM1_3a <- NULL
COPY$SLM1_3b <- NULL
COPY$SLM1_3c <- NULL
COPY$SLM1_3d <- NULL
COPY$SLM1_3e <- NULL
COPY$SLM1_3f <- NULL
COPY$SLM1_3g <- NULL
COPY$SLM1_4 <- NULL
COPY$SLM1_5 <- NULL
COPY$SLM1_6 <- NULL
COPY$SLM1_7 <- NULL
COPY$SLM1_8 <- NULL
COPY$SLM1_9 <- NULL

COPY$SLM2_3a <- NULL
COPY$SLM2_3b <- NULL
COPY$SLM2_3c <- NULL
COPY$SLM2_3d <- NULL
COPY$SLM2_3e <- NULL
COPY$SLM2_3f <- NULL
COPY$SLM2_3g <- NULL
COPY$SLM2_4 <- NULL
COPY$SLM2_5 <- NULL
COPY$SLM2_6 <- NULL
COPY$SLM2_7 <- NULL
COPY$SLM2_8 <- NULL
COPY$SLM2_9 <- NULL

## renaming data frame
slm_quest_relevant <- COPY

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

# RECODE
warning('Perhaps the recoding has already been done!!!')
slm_quest_relevant$SLM_infl = NA
slm_quest_relevant$SLM_ninfl = NA

for (ii in 1:length(slm_quest_relevant$SLM_infl)) {
  if(is.na(slm_quest_relevant$SLM_Set[ii])) {
    slm_quest_relevant$SLM_infl[ii] = NA
    slm_quest_relevant$SLM_ninfl[ii] = NA
    next
  }
  if(slm_quest_relevant$SLM_Set[ii] == 'A') {
    slm_quest_relevant$SLM_infl[ii] = slm_quest_relevant$SLM1_2[ii]
    slm_quest_relevant$SLM_ninfl[ii] = slm_quest_relevant$SLM2_2[ii]
    next
  }
  if(slm_quest_relevant$SLM_Set[ii] == 'B') {
    slm_quest_relevant$SLM_infl[ii] = slm_quest_relevant$SLM2_2[ii]
    slm_quest_relevant$SLM_ninfl[ii] = slm_quest_relevant$SLM1_2[ii]
    next
  }
}

warning('RECODING DOES NOT SEEM TO WORK!')

## within variables: SLM_infl vs. SLM_noninfl
## between variable: HC vs. PG

## descriptives
by(slm_quest_relevant$SLM_infl, list(slm_quest_relevant$HCPG), stat.desc, basic = FALSE)
by(slm_quest_relevant$SLM_ninfl, list(slm_quest_relevant$HCPG), stat.desc, basic = FALSE) ## what does "basic = FALSE" mean?

## control conditions/variables: not controllable/HC
## setting contrasts not necessary
## run mixed ANOVA
SLM_Model<-ezANOVA(data = slm_quest_relevant, dv = .('SLM_infl', 'SLM_ninfl'), wid = .(VPPG), between = .(HCPG), within = .(SLM_Set), type = 1, detailed = TRUE)
SLM_Model
class(slm_quest_relevant$SLM_ninfl)
class(slm_quest_relevant$SLM_infl)