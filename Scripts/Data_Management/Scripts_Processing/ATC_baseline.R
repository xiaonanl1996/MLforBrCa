###################################
# This script is for mapping UKB self-report meds to ATC category
# Ref: Wu2019 and https://www.who.int/tools/atc-ddd-toolkit/atc-classification
# Author: Xiaonan Liu
# Date: 17Aug2021
###################################


library(yaml)
library(stringr)


config = yaml.load_file("./config.yml")
source(file.path(config$scripts$cleaning, "dataset_generator.R"))


# 1. Read in ATC mapping from Wu2019 

ATC_mapping<-read.csv(file.path(config$cleaning$mapping,'Wu2019_Sup1.csv'))[,1:4]%>%na.omit

# Create a df that contains level 4 ATC codes and the corresponding UKB meds code

max_column=max(str_count(ATC_mapping$Medication.ATC.code,"\\|"))+1 # Number of | decides number of extra ATC codes columns
# Separate multiple ATC codes into separate columns
ATC_level4 <- separate(data=ATC_mapping,col = Medication.ATC.code,into = paste0("ATC_Code",1:max_column),sep="\\|")%>%
  # Label the ATC code from Level 5 to Level 4
  # https://www.who.int/tools/atc-ddd-toolkit/atc-classification
  mutate_at(vars(starts_with("ATC_Code")),~ifelse(nchar(substring(.,1,5))==5,substring(.,1,5),NA))

# Warning is expected, it basically informs us there were NA created because for some UKB med code, there's only one ATC code associated


# 2. Extract and Transfer UKB med data from wide to long 

# Extract the raw UKB medication data (FID 20003 at baseline)
data <- DBfunc$DB_extract(extract_cols = c(
  "ID", paste0("VeI_MedCode.0.", seq(0, 47, by=1))
))

colname <- "VeI_MedCode.0."

long <- data %>% pivot_longer(
  cols = starts_with(colname),
  names_to = c(".value","Array"),
  names_pattern = "(.*).0.(.*)"
) %>%
  filter(!is.na(VeI_MedCode))%>%
  mutate(VeI_MedCode=as.numeric(VeI_MedCode))%>%
  select(-Array)


# 3. Merge data with the ATC mapping 
ATC_data<-left_join(long,ATC_level4,by=c("VeI_MedCode"="Coding"))%>%
  pivot_longer(
    cols = starts_with("ATC_Code"),
    names_to = c(".value","Array"),
    names_sep = "_"
  )%>%
  filter(!is.na(ATC))%>%
  select(-Array)

# Frequency of ATC columns
#freq<-table(ATC_data$ATC)


#cols<-names(freq)

#for (i in cols) {
# ATC_data[,i]=as.numeric(str_detect(ATC_data$ATC,i))
#}


# Remove duplicates: Within each ID, if he/she took at least one med that belongs to particular ATC group, assign 1 to that ATC group indicator
# Take too long
#ATC_indc<-ATC_data%>%
 # select(-c("VeI_MedCode","Category","Drug.name","ATC")) %>%
#  group_by(ID)%>%
 # mutate_at(all_of(cols),~ifelse(any(.==1),1,0))%>%
  #distinct(ID, .keep_all= TRUE)
  
## Another approach
#ATC_indc<-ATC_data %>% 
 # select(-c("VeI_MedCode","Category","Drug.name","ATC")) %>%
  #group_by(ID)%>%
  #summarise(across(everything(), max))

# Save the output
saveRDS(ATC_data,file = file.path(config$data$derived,paste0('ATC_baseline_',format(Sys.time(), "%Y%m%d"),'.rds')))








