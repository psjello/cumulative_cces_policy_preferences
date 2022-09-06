# CES cumulative preferences 
# Crosswalk (wide, long) making

library(tidyverse)
setwd("D:/Dropbox/CCES_SDA/Cumulative Policy/cumulative_cces_policy_preferences/")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# INput folder preparing  ----


# (1) Load wide-version of year-question crosswalk
# (2) Reshape wide-version to long (rows as year/unique question ID dyads)
# (3) Export wide-version to .csv and add unique question wording


# Wide version -- no wording
wide_sanswording <- 
  read.csv(
    "input/crosswalk_original-common_wide-sanswording.csv",
    fileEncoding="UTF-8-BOM",stringsAsFactors=F)

# Common <-> unique variable name crosswalk
commoncross <-
  wide_sanswording %>% 
  gather(cces_year,q_code,-c(q_code_common,q_label,topic)) %>% 
  mutate(cces_year=as.numeric(gsub("y","",cces_year))) %>%
  arrange(cces_year,q_code) %>% select(cces_year,topic,q_code_common,q_code,q_label)
write.csv(
  commoncross,
  "input/crosswalk-merged_original-common.csv",
  row.names=F)

# Wording crosswalk (made in Excel from `commoncross`)
# !!! DO NOT OVERWRITE !!!
wordingcross <- 
  read.csv(
    "input/crosswalk_original_wording.csv",
    stringsAsFactors=F)


# Long version -- no wording
long_sanswording <-
  wide_sanswording %>% 
  gather(cces_year,q_code,-c(q_code_common,q_label,topic)) %>% 
  mutate(wording=NA,cces_year=as.numeric(gsub("y","",cces_year))) %>%
  arrange(cces_year,q_code) %>% select(cces_year,topic,q_code_common,q_code,wording)
write.csv(
  long_sanswording,
  "input/crosswalk_original-common_long-sanswording.csv",
  row.names=F)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exported and guide-ready CSVs  ----


# Long version -- WITH wording (via wording crosswalk)
long <- left_join(commoncross,wordingcross %>% filter(!is.na(wording)))
write.csv(
  long %>% filter(!is.na(q_code)),
  "output/preferences-crosswalk_long-withwording.csv",
  row.names=F)


# Wide version -- WITH wording (via wording crosswalk)
temp <- long %>% 
  mutate(cces_year=paste0("y",cces_year,"_wording")) %>% 
  select(cces_year,q_code_common,q_label,topic,wording) %>% 
  pivot_wider(names_from="cces_year", values_from="wording") #%>% arrange(topic,q_code_common)
wide <- left_join(wide_sanswording,temp) %>% arrange(topic,q_code_common)
wide <- wide %>% select(topic,q_code_common,q_label,sort(grep("y20",names(wide),value=T)))
write.csv(
  apply(wide,2,as.character),
  "output/preferences-crosswalk_wide-withwording.csv",
  row.names=F)

#Latex ready version of Wide -- sans wording

write_csv(
  as.data.frame(
    lapply(
      wide_sanswording, 
      function(y) as.character(gsub("_", "\\_", y, fixed = TRUE)) )),
  "output/latex-guide_preferences-crosswalk_wide-sanswording.csv")








