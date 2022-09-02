# CES cumulative preferences 
# Cleaning, merging, appending common contents
#
#
# Objectives
# - Load individual CES surveys, selecting and renaming (commonalizing) variables
# - Re-factor variables

rm(list = ls())


wd <- "D:/Dropbox/CCES_SDA/"
setwd(wd)

library(tidyverse)
library(haven)
library(sjlabelled)


# Preference item crosswalk (wide)

wide <- 
  read.csv(
    "Cumulative Policy/output/preferences-crosswalk_wide-withwording.csv",
    stringsAsFactors=F)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Commonalizing ----
# 
# Looping over yearly CES common content surveys
# to clean and commonalize policy preference items


# ... Loop preparation

#File paths
paths <- c(
  "2006/Data/COMMON/DATASET/cces06_common.dta",
  "2007/Data/Commmon/CCES0043_OUTPUT.sav",
  "2008/Data/cces08_common_output.dta",
  "2009/Data/COMMON/cces09_cmn_output.dta",
  "2010/Data/CCES10_Common_OUTPUT.dta",
  "2011/Common/CCES11_Common_OUTPUT.dta",
  "2012/Data/Common_Content/CCES12_Common_OUTPUT_vv_20130930.dta",
  "2013/Data/CCES13_Common_OUTPUT.dta",
  "2014/Data/Common/CCES14_Common_OUTPUT_Aug2015_vm_timings.dta",
  "2015/Data/CCES15_Common_OUTPUT_timings.dta",
  "2016/Data/Common/CCES16_Common_OUTPUT_Jul2017_VV.dta",
  "2017/Data/CCES17_COMMON/CCES17_COMMON_OUTPUT_Jan2018.dta",
  "2018/Data/Common/CCES18_Common_OUTPUT_vv_topost.dta",
  "2019/Data/Common/CCES19_Common_OUTPUT.dta",
  "2020/Data/Common content/CCES20_Common_OUTPUT_vv.dta",
  "2021/Data/CCES21_Common_OUTPUT.dta")

#Empty dataframe
ces <- NULL


# ... Loop

for(path in paths){
  
  #Survey year
  y = as.numeric(substr(path, 1, 4))
  
  #2007 condition
  if(y == 2007){
    cat("\n\nLoad CCES", y, "\n")
    cesy <- read_sav(path)
    
    #else condition
  }else if(y != 2007){
    cat("\n\nLoad CCES", y, "\n")
    cesy <- read_dta(path)
  }
  
  
  #Common and unique (year-specific) variable names
  cat("...Common Year-specific variables names\n")
  commony <- wide[,c('q_code_common', paste0('y', y))]
  names(commony)[2] <- "var"
  
  #Non-matching variables
  cat("...Non-matching variable names\n")
  nonmatchvars <- commony %>% filter(q_code_common != var)
  
  #Missing variables (create NA columns)
  cat("...NA vectors for missing variables\n")
  missingvars <- commony[is.na(commony$var), 'q_code_common']
  for(v in missingvars){
    cat("... ... CREATE NA column for missing variable", v, "\n")
    cesy[[v]] <- NA
  }
  
  #Rename non-matching to common variables
  cat("...Rename non-missing to common (i.e. create new var)\n")
  for(v in nonmatchvars$var){
    cat("... ... RENAME non-matching variable", v, "\n")
    replacewith <- nonmatchvars[nonmatchvars$var == v, 'q_code_common']
    cesy[[replacewith]] <- cesy[[v]]
  }
  
  
  ## Keep selected variables and add year variable
  cat("... keep selected variables\n")
  cesy <- cesy %>% select(commony$q_code_common)
  cesy <- 
    data.frame(
      year = y,
      cesy, stringsAsFactors = F) %>%
    mutate(case_id=as.character(case_id))
  
  #Issue items as numeric double check
  cat("... as.numeric for issues\n")
  for(v in commony$q_code_common[-1]){
    cesy[[v]] <- as.numeric(cesy[[v]])
  }
  
  
  #Append
  cat("... append to master data frame\n")
  ces <- rbind(ces, cesy)
}

#Character class case_id
ces$case_id <- as.character(ces$case_id)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Re-factoring ----


# ... Support/Oppose ####

vars_supopp <- 
  grep("^(abortion|enviro|guns|healthcare|immig|military|trade)_|gaymarriage_(ban|legalize)",
       names(ces),
       value = T)

vars_supopp <- 
  setdiff(
    vars_supopp,
    grep(
      "_scale$|_vs_jobs$",
      names(ces),
      value = T))

# for(v in vars_supopp){
#   cat("\n\nVariable",v)
#   print(table(ces[[v]], ces$year))
# }

#Refactoring
for(v in vars_supopp){
  # cat("\nVariable",v)
  ces[[v]] <- ifelse(ces[[v]] >= 3, NA, ces[[v]])
  ces[[v]] <- factor(
    ces[[v]],
    levels=c(1,2,NA),
    labels=c("Support","Oppose"))
  # print(table(ces[[v]], ces$year))
}



# ... Spending

vars_spend <- grep(
  "^spending_",
  names(ces),
  value=T)

vars_spend <- setdiff(
  vars_spend,
  c("spending_vs_tax", "spending_cuts_most", "spending_cuts_least"))

# for(v in vars_spend){
#   cat("\n\n\nVariable",v)
#   print(table(ces[[v]],ces$year))
# }

#Refactoring
for(v in vars_spend){
  # cat("\nVariable",v)
  ces[[v]] <- factor(
    ces[[v]],
    levels=c(seq(1,5,1),NA),
    labels=c(
      "Greatly increase","Slightly increase",
      "Maintain","Slightly decrease","Greatly decrease"))
  # print(table(ces[[v]], ces$year))
}



# ... Other variables ####

#abortion_scale
ces$abortion_scale <- ifelse(
  ces$abortion_scale >= 5, 
  NA, 
  ces$abortion_scale)
ces$abortion_scale <- factor(
  ces$abortion_scale,
  levels = c(seq(1,4,1),NA),
  labels = c(
    "Never permit",
    "Permit in rape, incest cases",
    "Permit in other cases",
    "Always allow"))

#affirmativeaction
ces$affirmativeaction <- ifelse(
  ces$affirmativeaction >= 5,
  NA,
  ces$affirmativeaction)
ces$affirmativeaction <- factor(
  ces$affirmativeaction,
  levels = c(seq(1,4,1),NA),
  labels = c(
    "Strongly support",
    "Somewhat support",
    "Somewhat oppose",
    "Strongly oppose"))

#affirmativeaction_scale
ces$affirmativeaction_scale <- factor(
  ces$affirmativeaction_scale,
  levels = c(seq(1,7,1),NA),
  labels = c(
    "Strongly support",
    "Support",
    "Somewhat support",
    "Neither support nor oppose",
    "Somewhat oppose",
    "Oppose",
    "Strongly oppose"))

#enviro_scale (see notes)
ces$enviro_scale <- factor(
  ces$enviro_scale,
  levels=seq(1,5,1),
  labels=c(
    "Immediate action",
    "Some action",
    "More research needed",
    "No action necessary",
    "Climate change not occurring"))

#gaymarriage_scale
ces$gaymarriage_scale <- factor(
  ces$gaymarriage_scale,
  levels=seq(1,5,1),
  labels=c(
    "Strongly support",
    "Somewhat support",
    "Somewhat oppose",
    "Stongly oppose",
    "Don't know"))

#guns_scale
ces$guns_scale <- factor(
  ces$guns_scale,
  levels=seq(1,3,1),
  labels=c(
    "More strict",
    "Less strict",
    "Kept as they are"))

#enviro_vs_jobs
ces$enviro_vs_jobs <- factor(
  ces$enviro_vs_jobs,
  levels=seq(1,6,1),
  labels=c(
    "Environment much more important",
    "Environment somewhat more important",
    "Environment and jobs of same importance",
    "Jobs somewhat more important",
    "Jobs much more important",
    "Haven't thought much about this"))

#spending_cuts_... (see notes)
ces$spending_cuts_least <- factor(
  ces$spending_cuts_least,
  levels=seq(1,4,1),
  labels=c(
    "Cut defense spending",
    "Cut domestic spending",
    "Raise taxes",
    "Borrow"))
ces$spending_cuts_most <- factor(
  ces$spending_cuts_most,
  levels=seq(1,4,1),
  labels=c(
    "Cut defense spending",
    "Cut domestic spending",
    "Raise taxes",
    "Borrow"))





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Variable reordering ----


# Reordering

ces <- ces %>% select(year, case_id, names(ces)[2:(ncol(ces)-1)])


#Variable labels
ces <- set_label(
  ces,
  c("Survey year","Case ID",wide$q_label[1:55]))


#Export
dat <- sjlabelled::as_label(ces) 
write_dta(
  dat,
  "Cumulative Policy/cumulative_ces_policy_preferences.dta",
  version=14)




