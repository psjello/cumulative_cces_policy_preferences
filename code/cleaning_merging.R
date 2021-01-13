# CCES cumulative preferences 
# Cleaning, merging individual surveys


rm(list = ls())
wd <- "C:/Dagonel/projects/surveys/cces/"
setwd(wd)

library(readr)
library(haven)
library(sjlabelled)

library(tidyr)
library(dplyr)
library(stringr)










####################################################################################################


# Objectives
# - Load individual CCES surveys, selecting and renaming (commonalize) variables
# - Re-factor variables
#
#



# Preference item crosswalk (wide)
wide <- 
  read.csv(
    "cces_cumulative_preferences/output/preferences-crosswalk_wide-withwording.csv",
    stringsAsFactors=F)




# Individual CCES survey loops
cces <- NULL
years <- str_pad(seq(6,19,1),2,pad="0")

## Loop
for(y in years){

  #2006
  if(y=="06"){
    cat("\n\nLoad CCES",y,"\n")
    ccesy <-
      read.table(
        paste0(wd,"input/20",y,"/cces",y,".tab"),
        header=T,quote="",sep="\t",fill=TRUE)
    # names(ccesy) <- tolower(names(ccesy))
    
  #2007
  }else if(y=="07"){
    cat("\n\nLoad CCES",y,"\n")
    ccesy <- 
      read_sav(
        paste0(wd,"input/20",y,"/cces",y,".sav"))
    # names(ccesy) <- tolower(names(ccesy))
    
  #2008-2019
  }else if(!(y %in% c("06","07"))){
    cat("\n\nLoad CCES",y,"\n")
    ccesy <- 
      read_dta(
        paste0(wd,"input/20",y,"/cces",y,".dta"))
    # names(ccesy) <- tolower(names(ccesy))
  }
  
  
  #Common and unique (year-specific) variable names
  cat("...Common Year-specific variables names\n")
  commony <- wide[,c('q_code_common',paste0('y20',y))]
  names(commony)[2] <- "var"
  
  #Non-matching variables
  cat("...Non-matching variable names\n")
  nonmatchvars <- commony %>% filter(q_code_common!=var)
  
  #Missing variables (create NA columns)
  cat("...NA vectors for missing variables\n")
  missingvars <- commony[is.na(commony$var),'q_code_common']
  for(v in missingvars){
    cat("... ... CREATE NA column for missing variable",v,"\n")
    ccesy[[v]] <- NA
  }
  
  #Rename non-matching to common variables
  cat("...Rename non-missing to common (i.e. create new var)\n")
  for(v in nonmatchvars$var){
    cat("... ... RENAME non-matching variable",v,"\n")
    replacewith <- nonmatchvars[nonmatchvars$var==v,'q_code_common']
    ccesy[[replacewith]] <- ccesy[[v]]
  }
  
  
  ## Keep selected variables and add year variable
  cat("... keep selected variables\n")
  ccesy <- ccesy %>% select(commony$q_code_common)
  ccesy <- 
    data.frame(
      year=as.numeric(paste0("20",y)),
      ccesy,stringsAsFactors=F) %>%
    mutate(case_id=as.character(case_id))
  
  #Issue items as numeric double check
  cat("... as.numeric for issues\n")
  for(v in commony$q_code_common[-1]){
    ccesy[[v]] <- as.numeric(ccesy[[v]])
  }
  
  
  #2006-2011 issue item fix (gaymarriage)
  #see notes
  if(y %in% c("06","07","08","09","10","11")){
    ccesy <- ccesy %>% 
      mutate(
        gaymarriage=ifelse(gaymarriage==2,1,2))
    cat("... gaymarriage fix for year 2006-2011\n")
  }else{
    cat("... no need for gaymarriage fix\n")
  }
  
  
  #2014 issue item fix (repealaca)
  #CC14_324 wording says VOTE FOR ACA;
  #other years say REPEAL ACA
  if(y=="14"){
    ccesy <- ccesy %>% 
      mutate(
        repealaca=ifelse(repealaca==2,1,2))
    cat("... repealaca fix for year 2014\n")
  }else{
    cat("... no need for repealaca fix for year 2014\n")
  }
  
  #2018 issue item fix (enviro_35mpg)
  #CC18_415b wording says LOWER FROM 35 TO 25;
  #other years say RAISE FROM 25 TO 35
  if(y=="18"){
    ccesy <- ccesy %>% 
      mutate(
        enviro_35mpg=ifelse(enviro_35mpg==2,1,2))
    cat("... enviro_35mpg fix for year 2018\n")
  }else{
    cat("... no need for enviro_35mpg fix for year 2018\n")
  }
  
  
  #Append
  cat("... append to master data frame\n")
  cces <- rbind(cces,ccesy)
}
cces$case_id <- as.character(cces$case_id)










####################################################################################################


# Factoring variables

#Support/Oppose factoring
vars_supopp <- 
  grep("^(abortion|enviro|guns|immig|military)_|^(repealaca|gaymarriage)$",
       names(cces),value=T)
vars_supopp <- setdiff(vars_supopp,grep("_scale$|_vs_jobs$",names(cces),value=T))
for(v in vars_supopp){
  cat("\n\nVariable",v)
  print(table(cces[[v]],cces$year))
}
for(v in vars_supopp){
  cat("\nVariable",v)
  cces[[v]] <- ifelse(cces[[v]]>=3,NA,cces[[v]])
  cces[[v]] <- factor(
    cces[[v]],
    levels=c(1,2,NA),
    labels=c("Support","Oppose"))
  print(table(cces[[v]],cces$year))
}

#Spending
vars_spend <- grep("^spending_",names(cces),value=T)
vars_spend <- setdiff(vars_spend,c("spending_vs_tax","spending_cuts_most","spending_cuts_least"))
for(v in vars_spend){
  cat("\n\n\nVariable",v)
  print(table(cces[[v]],cces$year))
}
for(v in vars_spend){
  cat("\nVariable",v)
  cces[[v]] <- factor(
    cces[[v]],
    levels=c(seq(1,5,1),NA),
    labels=c(
      "Greatly increase","Slightly increase",
      "Maintain","Slightly decrease","Greatly decrease"))
  print(table(cces[[v]],cces$year))
}

#Other variables
## `abortion_scale`
cces$abortion_scale <- ifelse(cces$abortion_scale>=5,NA,cces$abortion_scale)
cces$abortion_scale <- factor(
  cces$abortion_scale,
  levels=c(seq(1,4,1),NA),
  labels=c(
    "Never permit",
    "Permit in rape, incest cases",
    "Permit in other cases",
    "Always allow"))
## `affirmativeaction`
cces$affirmativeaction <- ifelse(cces$affirmativeaction>=5,NA,cces$affirmativeaction)
cces$affirmativeaction <- factor(
  cces$affirmativeaction,
  levels=c(seq(1,4,1),NA),
  labels=c(
    "Strongly support",
    "Somewhat support",
    "Somewhat oppose",
    "Strongly oppose"))
## `affirmativeaction_scale`
cces$affirmativeaction_scale <- factor(
  cces$affirmativeaction_scale,
  levels=c(seq(1,7,1),NA),
  labels=c(
    "Strongly support",
    "Support",
    "Somewhat support",
    "Neither support nor oppose",
    "Somewhat oppose",
    "Oppose",
    "Strongly oppose"))
## `enviro_scale` (see notes)
cces$enviro_scale <- factor(
  cces$enviro_scale,
  levels=seq(1,5,1),
  labels=c(
    "Immediate action",
    "Some action",
    "More research needed",
    "No action necessary",
    "Climate change not occurring"))
## `gaymarriage_scale`
cces$gaymarriage_scale <- factor(
  cces$gaymarriage_scale,
  levels=seq(1,5,1),
  labels=c(
    "Strongly support",
    "Somewhat support",
    "Somewhat oppose",
    "Stongly oppose",
    "Don't know"))
## `guns_scale` 
cces$guns_scale <- factor(
  cces$guns_scale,
  levels=seq(1,3,1),
  labels=c(
    "More strict",
    "Less strict",
    "Kept as they are"))
## `enviro_vs_jobs`
cces$enviro_vs_jobs <- factor(
  cces$enviro_vs_jobs,
  levels=seq(1,6,1),
  labels=c(
    "Environment much more important",
    "Environment somewhat more important",
    "Environment and jobs of same importance",
    "Jobs somewhat more important",
    "Jobs much more important",
    "Haven't thought much about this"))
## `spending_cuts_...` (see notes)
cces$spending_cuts_least <- factor(
  cces$spending_cuts_least,
  levels=seq(1,4,1),
  labels=c(
    "Cut defense spending",
    "Cut domestic spending",
    "Raise taxes",
    "Borrow"))
cces$spending_cuts_most <- factor(
  cces$spending_cuts_most,
  levels=seq(1,4,1),
  labels=c(
    "Cut defense spending",
    "Cut domestic spending",
    "Raise taxes",
    "Borrow"))










####################################################################################################



# Final aesthetic tweaks

#Reordering
cces <- cces %>% select(year,case_id,names(cces)[2:(ncol(cces)-1)])

#Variable labels
cces <- set_label(cces,c("Survey year","Case ID",wide$q_label[1:43]))


# Export
dat <- sjlabelled::as_label(cces) 
write_dta(
  dat,
  paste0(wd,"cces_cumulative_preferences/output/cces_cumulative_preferences.dta"),
  version=14)






# Variable-specific NOTES
#
# <=2007: (affirmativeaction_scale) includes response option "not sure"
# >=2010: (enviro_scale) includes response option, value 5, "global warming doesn't exist"
#   2018: CC18_415b (enviro_35mpg) wording says LOWER FROM 35 TO 25; other years say RAISE FROM 25 TO 35
#   2008: (gaymarriage) includes response option "not sure"
# >=2012: (gaymarriage) wording switches from (Y/N) amendment to ban, to (sup/opp) gay marriage;
#           values <2012 are switched accordingly to match >=2012 direction
#   2018: CC18322a (immig_border) wording includes BORDER WALL; other years do not mention a wall
# <=2007: (rank_budget_...) includes response option, value 4, "Borrow"
#   2014: CC14_324 (repealaca) asks about VOTING FOR ACA; other years ask about REPEALING ACA


