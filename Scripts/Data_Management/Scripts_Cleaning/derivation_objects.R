# Jennifer Collister
# 22/09/20

library(glue)
library(lubridate)
library(readxl)

# Load the project config file for filepaths etc
if (!exists("config")) {
  library(yaml)
  config = yaml.load_file("config.yml")
}

# Source the function definitions
# XL: Remove 'Reorganise' in the file path
source(file.path(config$scripts$cleaning, "basic_functions.R"), local = TRUE)

# makeEnum <- function(inputList) {
#   # Borrowed from https://stackoverflow.com/a/41509345
#   myEnum <- as.list(inputList)
#   enumNames <- names(myEnum)
#   if (is.null(enumNames)) {
#     names(myEnum) <- myEnum
#   } else if ("" %in% enumNames) {
#     stop("The inputList has some but not all names assigned. They must be all assigned or none assigned")
#   }
#   return(myEnum)
# }
# visits <- makeEnum(list(baseline = c("0", "baseline assessment"), 
#                     repeat_visit = c("1", "repeat visit"), 
#                     imaging = c("2", "imaging visit"), 
#                     repeat_imaging = c("3","repeat imaging visit")))

# Formatting of existing UKB variables

ID <- function() {
  list(
    name = "ID",
    source = "ID",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "ID",
    description = "The unique participant identifier"
  )
}

BaC_Sex <- function() {
  list(
    name = "BaC_Sex",
    source = "BaC_Sex.0.0",
    mapper = FN_unorder,
    post_exclusion = FALSE,
    display_name = "Gender",
    description = "Participant's self-reported gender"
  )
}

Rec_DateAssess <- function() {
  list(
    name = "Rec_DateAssess",
    source = c("Rec_DateAssess.0.0"),
    mapper = FN_toDate,
    post_exclusion = FALSE,
    display_name = "Date of baseline assessment",
    description = "Date of baseline assessment"
  )
}

Eth_Ethnicity <- function() {
  list(
    name = "Eth_Ethnicity",
    source = "Eth_Ethnicity.0.0",
    mapper = FN_factor(
      levelorder = c(
        "White",
        "British",
        "Irish",
        "Any other white background",
        "Mixed",
        "White and Black Caribbean",
        "White and Black African",
        "White and Asian",
        "Any other mixed background",
        "Asian or Asian British",
        "Indian",
        "Pakistani",
        "Bangladeshi",
        "Any other Asian background",
        "Black or Black British",
        "Caribbean",
        "African",
        "Any other Black background",
        "Chinese",
        "Other ethnic group",
        "Do not know",
        "Prefer not to answer"
      )
    ),
    post_exclusion = FALSE,
    display_name = "Ethnic group",
    description = "The participant's self-reported ethnicity (raw UKB categories)"
  )
}


TEU_BaC_DateOfBirth <- function() {
  list(
    name = "TEU_BaC_DateOfBirth",
    source = c("BaC_BirthMonth.0.0", "BaC_BirthYear.0.0"),
    mapper = FN_MYtoDate(
      day = 15,
      monthField = "BaC_BirthMonth.0.0",
      yearField = "BaC_BirthYear.0.0"
    ),
    post_exclusion = FALSE,
    display_name = "Date of Birth",
    description = "The participant's approximate date of birth, derived from self-reported month and year with date estimated as 15th"
  )
}

TEU_BaC_AgeAtRec <- function() {
  list(
    name = "TEU_BaC_AgeAtRec",
    source = c("TEU_BaC_DateOfBirth", "Rec_DateAssess"),
    mapper = function(data) {
      as.numeric(round(difftime(data[["Rec_DateAssess"]], data[["TEU_BaC_DateOfBirth"]], unit =
                                  "days") / 365.25,
                       digits = 2))
    },
    post_exclusion = FALSE,
    display_name = "Age at recruitment, years",
    description = "The participant's approximate age at recruitment, derived from date of assessment centre visit and self-reported month and year of birth (date of birth estimated as 15th of the month)"
  )
}

TEU_BaC_AgeCat <- function() {
  list(
    name = "TEU_BaC_AgeCat",
    source = "TEU_BaC_AgeAtRec",
    mapper = FN_buckets(
      breaks = c(40, 50, 60, 70),
      labels = c("40-49", "50-59", "60-69"),
      right = FALSE
    ),
    post_exclusion = FALSE,
    display_name = "Age at recruitment, years",
    description = "Categorised age in years"
  )
}


TEU_BlP_SBP.0.0 <- function() {
  list(
    name = "TEU_BlP_SBP.0.0",
    source = c("BlP_SBPAuto.0.0", "BlP_SBPMan.0.0"),
    mapper = function(data) {
      coalesce(data[["BlP_SBPAuto.0.0"]], data[["BlP_SBPMan.0.0"]])
    },
    post_exclusion = FALSE,
    display_name = "First SBP at baseline",
    description = "First SBP measurement at baseline, either automated or manual"
  )
}

TEU_BlP_SBP.0.1 <- function() {
  list(
    name = "TEU_BlP_SBP.0.1",
    source = c("BlP_SBPAuto.0.1", "BlP_SBPMan.0.1"),
    mapper = function(data) {
      coalesce(data[["BlP_SBPAuto.0.1"]], data[["BlP_SBPMan.0.1"]])
    },
    post_exclusion = FALSE,
    display_name = "Second SBP at baseline",
    description = "Second SBP measurement at baseline, either automated or manual"
  )
}

TEU_BlP_DBP.0.0 <- function() {
  list(
    name = "TEU_BlP_DBP.0.0",
    source = c("BlP_DBPAuto.0.0", "BlP_DBPMan.0.0"),
    mapper = function(data) {
      coalesce(data[["BlP_DBPAuto.0.0"]], data[["BlP_DBPMan.0.0"]])
    },
    post_exclusion = FALSE,
    display_name = "First DBP at baseline",
    description = "First DBP measurement at baseline, either automated or manual"
  )
}

TEU_BlP_DBP.0.1 <- function() {
  list(
    name = "TEU_BlP_DBP.0.1",
    source = c("BlP_DBPAuto.0.1", "BlP_DBPMan.0.1"),
    mapper = function(data) {
      coalesce(data[["BlP_DBPAuto.0.1"]], data[["BlP_DBPMan.0.1"]])
    },
    post_exclusion = FALSE,
    display_name = "Second DBP at baseline",
    description = "Second DBP measurement at baseline, either automated or manual"
  )
}

TEU_BlP_Pulse.0.0 <- function() {
  list(
    name = "TEU_BlP_Pulse.0.0",
    source = c("BlP_PulseRateAuto.0.0", "BlP_PulseRate.0.0"),
    mapper = function(data) {
      coalesce(data[["BlP_PulseRateAuto.0.0"]], data[["BlP_PulseRate.0.0"]])
    },
    post_exclusion = FALSE,
    display_name = "First Pulse rate at baseline",
    description = "First pulse measurement at baseline, from FID 95 and FID 102"
  )
}

TEU_BlP_Pulse.0.1 <- function() {
  list(
    name = "TEU_BlP_Pulse.0.1",
    source = c("BlP_PulseRateAuto.0.1", "BlP_PulseRate.0.1"),
    mapper = function(data) {
      coalesce(data[["BlP_PulseRateAuto.0.1"]], data[["BlP_PulseRate.0.1"]])
    },
    post_exclusion = FALSE,
    display_name = "Second Pulse rate at baseline",
    description = "Second pulse rate measurement at baseline, from FID 95 and FID 102"
  )
}

TEU_BlP_Pulse.avg <- function() {
  list(
    name = "TEU_BlP_Pulse.avg",
    source = c("TEU_BlP_Pulse.0.0",
               "TEU_BlP_Pulse.0.1"),
    mapper = FN_average(colnames = c("TEU_BlP_Pulse.0.0",
                                     "TEU_BlP_Pulse.0.1")),
    post_exclusion = FALSE,
    display_name = "Baseline pulse rate",
    description = "The average pulse rate during BP measurement at baseline"
  )
}


TEU_BlP_nSBP <- function() {
  list(
    name = "TEU_BlP_nSBP",
    source = c("TEU_BlP_SBP.0.0", "TEU_BlP_SBP.0.1"),
    mapper = function(data) {
      rowSums(!is.na(data[, c("TEU_BlP_SBP.0.0", "TEU_BlP_SBP.0.1")]))
    },
    post_exclusion = FALSE,
    display_name = "No. SBP",
    description = "Number of SBP measurements taken at baseline"
  )
}

TEU_BlP_nDBP <- function() {
  list(
    name = "TEU_BlP_nDBP",
    source = c("TEU_BlP_DBP.0.0", "TEU_BlP_DBP.0.1"),
    mapper = function(data) {
      rowSums(!is.na(data[, c("TEU_BlP_DBP.0.0", "TEU_BlP_DBP.0.1")]))
    },
    post_exclusion = FALSE,
    display_name = "No. DBP",
    description = "Number of DBP measurements taken at baseline"
  )
}

TEU_BlP_SBP.avg <- function() {
  list(
    name = "TEU_BlP_SBP.avg",
    source = c("TEU_BlP_SBP.0.0",
               "TEU_BlP_SBP.0.1"),
    mapper = FN_average(colnames = c("TEU_BlP_SBP.0.0",
                                     "TEU_BlP_SBP.0.1")),
    post_exclusion = FALSE,
    display_name = "Baseline SBP",
    description = "The average systolic blood pressure at baseline"
  )
}


TEU_BlP_DBP.avg <- function() {
  list(
    name = "TEU_BlP_DBP.avg",
    source = c("TEU_BlP_DBP.0.0",
               "TEU_BlP_DBP.0.1"),
    mapper = FN_average(colnames = c("TEU_BlP_DBP.0.0",
                                     "TEU_BlP_DBP.0.1")),
    post_exclusion = FALSE,
    display_name = "Baseline DBP",
    description = "The average diastolic blood pressure at baseline"
  )
}


TEU_HoH_PreTaxInc <- function() {
  list(
    name = "TEU_HoH_PreTaxInc",
    source = c("HoH_PreTaxInc.0.0", "HoH_PreTaxInc_P.0.0"),
    mapper = function(data) {
      y <- ifelse(is.na(data[["HoH_PreTaxInc.0.0"]]),
                  as.character(data[["HoH_PreTaxInc_P.0.0"]]),
                  as.character(data[["HoH_PreTaxInc.0.0"]]))
      y <- fct_collapse(
        y,
        "Less than 18,000" = "Less than 18,000",
        "18,000 to 30,999" = c("18,000 to 30,999", "18,000 to 31,000"),
        "31,000 to 51,999" = c("31,000 to 51,999", "31,000 to 52,000"),
        "52,000 to 100,000" = "52,000 to 100,000",
        "Greater than 100,000" = "Greater than 100,000",
        "Do not know" = "Do not know",
        "Prefer not to answer" = "Prefer not to answer"
      )
      y <-
        factor(
          y,
          levels = c(
            "Less than 18,000",
            "18,000 to 30,999",
            "31,000 to 51,999",
            "52,000 to 100,000",
            "Greater than 100,000",
            "Do not know",
            "Prefer not to answer"
          )
        )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Household Income, GBP",
    description = "Participant's pre-tax household income"
  )
}

TEU_HouseholdIncome <- function() {
  list(
    name = "TEU_HouseholdIncome", 
    source = c("TEU_HoH_PreTaxInc"), 
    mapper = FN_labelfactor(),
    post_exclusion = FALSE,
    display_name = "Household Income, GBP",
    description = "Participant's pre-tax houshold income, self-reported. The 'do not know' and 'prefer not to answer' categories have been combined with NAs into 'unanswered'"
  )
}

ElD_MobPhUsage <- function(){
  list(
    name = "ElD_MobPhUsage",
    source = "ElD_MobPhUsage.0.0",
    mapper = FN_labelfactor(levels=c("Less than 5mins","5-29 mins","30-59 mins","1-3 hours",
                                     "4-6 hours","More than 6 hours"),
                            labels=c("Less than 5mins","5-29 mins","30-59 mins","1-3 hours",
                                     "4-6 hours","More than 6 hours")),
    post_exclusion = FALSE,
    display_name = "Weekly usage of mobile phone in last 3 months",
    description = "Self-reported weekly usage of mobile phone in last 3 months"
  )
}

Sle_Duration <- function() {
  list(
    name = "Sle_Duration",
    source = "Sle_Duration.0.0",
    mapper = FN_toNA(),
    post_exclusion = FALSE,
    display_name = "Sleep Duration, hours",
    description = "Participant's self-reported average sleep duration in hours"
  )
}

Sle_RisingAM <- function() {
  list(
    name = "Sle_RisingAM",
    source = "Sle_RisingAM.0.0",
    mapper = FN_labelfactor(levels=c("Not at all easy","Not very easy","Fairly easy","Very easy"),
                            labels=c("Not at all easy","Not very easy","Fairly easy","Very easy")),
    post_exclusion = FALSE,
    display_name = "Getting up in morning",
    description = "On an average day, how easy the participant finds getting up in the morning."
  )
}

Sle_Chronotype <- function() {
  list(
    name = "Sle_Chronotype",
    source = "Sle_Chronotype.0.0",
    mapper = FN_labelfactor(),
    post_exclusion = FALSE,
    display_name = "Morning/evening person (Chronotype)",
    description = "Self-reported chronotype of participant"
  )
}

Sle_DaytimeNap <- function() {
  list(
    name = "Sle_DaytimeNap",
    source = "Sle_DaytimeNap.0.0",
    mapper = FN_labelfactor(),
    post_exclusion = FALSE,
    display_name = "Nap during day",
    description = "Self-reported nap frequency during day"
  )
}

Sle_Insomnia <- function() {
  list(
    name = "Sle_Insomnia",
    source = "Sle_Insomnia.0.0",
    mapper = FN_labelfactor(),
    post_exclusion = FALSE,
    display_name = "Insomnia",
    description = "Self-reported insomnia frequency"
  )
}

Sle_Snoring <- function() {
  list(
    name = "Sle_Snoring",
    source = "Sle_Snoring.0.0",
    mapper = FN_labelfactor(),
    post_exclusion = FALSE,
    display_name = "Snoring",
    description = "Self-reported snoring status"
  )
}

Sle_DaytimeSleeping<- function() {
  list(
    name = "Sle_DaytimeSleeping",
    source = "Sle_DaytimeSleeping.0.0",
    mapper = FN_labelfactor(levels=c("Never/rarely","Often","Sometimes","All of the time"),
                            labels=c("Never/rarely","Often","Sometimes","All of the time")),
    post_exclusion = FALSE,
    display_name = "Daytime dozing/sleeping",
    description = "Self-reported daytime dozing/sleeping frequency"
  )
}

BSM_BMI<-function(){
  list(
    name = 'BSM_BMI',
    source = 'BSM_BMI.0.0',
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = 'BMI',
    description = 'Body mass index (BMI) Kg/m2'
  )
}


PhA_METsWkAllAct <- function() {
  list(
    name = "PhA_METsWkAllAct",
    source = "PhA_METsWkAllAct.0.0",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Weekly METs",
    description = paste0("Summed MET minutes per week for all activity, derived from participant self-reported weekly exercise. This variable was generated as part of ",
                         text_spec("UKB application 12184", link = "http://bmjopen.bmj.com/content/6/3/e010038"), 
                         " and made available on the Data Showcase.")
  )
}

PhA_WalkDays <- function() {
  list(
    name = "PhA_WalkDays",
    source = "PhA_WalkDays.0.0",
    mapper = FN_toNA(c(-1,-2,-3)),
    post_exclusion = FALSE,
    display_name = "Number of days walked 10+min per week",
    description = "In a typical week, how many days did you walk for at least 10 min at a time. Unable to walk was 
    categorised as NA"
  )
}

PhA_DrivingTime <- function() {
  list(
    name = "PhA_DrivingTime",
    source = "PhA_DrivingTime.0.0",
    mapper = function(x){
      # -10 means less than an hour a day
      x[x==-10]=0 
      FN_toNA()(x)},
    post_exclusion = FALSE,
    display_name = "Time spent driving",
    description = "In a typical day, how many hours do you spend driving. Less than an hour was 
    coded as 0"
  )
}

PhA_ComputTime <- function() {
  list(
    name = "PhA_ComputTime",
    source = "PhA_ComputTime.0.0",
    mapper = function(x){
      # -10 means less than an hour a day
      x[x==-10]=0 
      FN_toNA()(x)},
    post_exclusion = FALSE,
    display_name = "Time spent using computer",
    description = "In a typical day, how many hours do you spend using computer. Less than an hour was 
    coded as 0"
  )
}

PhA_TVTime <- function() {
  list(
    name = "PhA_TVTime",
    source = "PhA_TVTime.0.0",
    mapper = function(x){
      # -10 means less than an hour a day
      x[x==-10]=0 
      FN_toNA()(x)},
    post_exclusion = FALSE,
    display_name = "Time spent watching TV",
    description = "In a typical day, how many hours do you spend watching TV. Less than an hour was 
    coded as 0"
  )
}


# General function for extracting Physical activity/Physical measure vars
PhA <- function() {
  renaming<-read.csv(file.path(config$cleaning$renaming))
  ## Physical activity
  PhA_vars<-renaming%>%
    filter(grepl("^PhA_",NewVarName))%>%
    # remove pilot variables
    filter(!grepl("(pilot)",Field_Description))%>%
    # remove fields with special categories 
    filter(!NewVarName%in%c("PhA_WalkDays","PhA_DrivingTime","PhA_ComputTime","PhA_TVTime"))
  
  PhA_list<-as.list(PhA_vars$NewVarName)
  names(PhA_list)<-PhA_vars$Field_Description
  
  ## Carotid ultrasound
  #CaU_list<-vars_to_list(renaming = renaming,name_pattern = "^CaU_",extra_vars = c("CaU_Method" , "CaU_Completed" , "CaU_ReasonNotAttempt", 
                                                                                   #"CaU_ReasonNotComplete", "CaU_ImageL" , "CaU_ImageR","CaU_Report"))
  
  ## Arterial stiffness 
  AS_list<-vars_to_list(renaming = renaming,name_pattern = "^ArS_",extra_vars = c("ArS_Skip","ArS_AbsNotchPulseW","ArS_PulseWVMaual"))
  
  ## Body composition by impedance
  Imp_vars<-renaming%>%
    filter(grepl("^Imp_",NewVarName))%>%
    # remove manual entry variables (only 31 ppl for each variable)
    filter(!grepl("manual entry",Field_Description))%>%
    # remove fields with special categories 
    filter(!NewVarName%in%c("Imp_Weight","Imp_BMI"))
  
  Imp_list<-as.list(Imp_vars$NewVarName)
  names(Imp_list)<-Imp_vars$Field_Description
  
  ## Eye meaures
  ESC_list<-vars_to_list(renaming = renaming,name_pattern = "^ESC_",extra_vars = NULL)
  
  ## Bone
  BDH_list<-vars_to_list(renaming = renaming,name_pattern = "^BDH_",extra_vars = "BDH_DeviceID")
  
  
  ## total
  Ph_list<-c(PhA_list,#CaU_list,
             AS_list,Imp_list,ESC_list,BDH_list)
  
  
  lapply(1:length(Ph_list), function(i)
    
  list(
    name = Ph_list[[i]], 
    source = c(paste0(Ph_list[[i]],".0.0")), 
    mapper = function(x){
      if(is.numeric(x)){
        FN_toNA()(x)
      }else{FN_labelfactor()(x)}
    },
    post_exclusion = FALSE,
    display_name = names(Ph_list[i]),
    description = names(Ph_list[i])
  )
  )
}

Sun_Exp <- function(){
  renaming<-read.csv(file.path(config$cleaning$renaming))
  Sun_list<-vars_to_list(renaming = renaming,name_pattern = "^Sun_",extra_vars = NULL)
  
  lapply(1:length(Sun_list), function(i)
    
    list(
      name = Sun_list[[i]], 
      source = c(paste0(Sun_list[[i]],".0.0")), 
      mapper = function(x){
        if(is.numeric(x)){
          x[x==-10]=0
          FN_toNA()(x)
        }else{FN_labelfactor()(x)}
      },
      post_exclusion = FALSE,
      display_name = names(Sun_list[i]),
      description = paste0(names(Sun_list[i]),"Note: -10 was assigned as 0")
    )
  )
  
}

TEU_Spi_FVC.avg <- function() {
  list(
    name = "TEU_Spi_FVC.avg",
    source = paste0("Spi_FVC.0.",0:2),
    mapper = FN_average(colnames = paste0("Spi_FVC.0.",0:2)),
    post_exclusion = FALSE,
    display_name = "Baseline FVC",
    description = "The average FVC at baseline"
  )
}

TEU_Spi_FEV1.avg <- function() {
  list(
    name = "TEU_Spi_FEV1.avg",
    source = paste0("Spi_FEV1.0.",0:2),
    mapper = FN_average(colnames = paste0("Spi_FEV1.0.",0:2)),
    post_exclusion = FALSE,
    display_name = "Baseline FEV1",
    description = "The average FEV1 at baseline"
  )
}

TEU_Spi_PEF.avg <- function() {
  list(
    name = "TEU_Spi_PEF.avg",
    source = paste0("Spi_PEF.0.",0:2),
    mapper = FN_average(colnames = paste0("Spi_PEF.0.",0:2)),
    post_exclusion = FALSE,
    display_name = "Baseline PEF",
    description = "The average PEF at baseline"
  )
}

CoF_RTTTimeID <- function(){
  list(
    name = "CoF_RTTTimeID",
    source = "CoF_RTTTimeID.0.0",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Reaction time",
    description = "Mean time to correctly identify matches"
  )
}

CoF_NM <- function(){
  list(
    name = "CoF_NM",
    source = "CoF_NM.0.0",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Numeric memory",
    description = "Maximum digits remembered correctly (-1 represents Abandoned)"
  )
}

CoF_FlScore <- function(){
  list(
    name = "CoF_FlScore",
    source = "CoF_FlScore.0.0",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Fluid Intelligence score",
    description = "Fluid Intelligence score"
  )
}

CoF_PrMemResult <- function(){
  list(
    name = "CoF_PrMemResult",
    source = "CoF_PrMemResult.0.0",
    mapper = FN_labelfactor(),
    post_exclusion = FALSE,
    display_name = "Prospective memory result",
    description = "Prospective memory result"
  )
}


HeT_HearingTestL <- function(){
  list(
    name = "HeT_HearingTestL",
    source = "HeT_HearingTestL.0.0",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Hearing test for left ear",
    description = "Speech-reception-threshold (SRT) estimate (left)"
  )
}

HeT_HearingTestR <- function(){
  list(
    name = "HeT_HearingTestR",
    source = "HeT_HearingTestR.0.0",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Hearing test for right ear",
    description = "Speech-reception-threshold (SRT) estimate (right)"
  )
}

HGS_L <- function(){
  list(
    name = "HGS_L",
    source = "HGS_L.0.0",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Hand grip strength (left)",
    description = "Hand grip strength (left)"
  )
}

HGS_R <- function(){
  list(
    name = "HGS_R",
    source = "HGS_R.0.0",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Hand grip strength (right)",
    description = "Hand grip strength (right)"
  )
}

CoF_RTTTimeID <- function() {
  list(
    name = "CoF_RTTTimeID",
    source = "CoF_RTTTimeID.0.0",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Reaction Time, s",
    description = "Reaction time in a game of snap, in seconds"
  )
}


TEU_Edu_HighestQual <- function() {
  list(
    name = "TEU_Edu_HighestQual",
    source = c(paste0("Edu_Qualif.0.", seq(0, 5, by=1)),
               paste0("Edu_Qualif_p.0.", seq(0, 4, by=1))),
    mapper = function(data) {
      qual_list <- c(
        "None of the above",
        "CSEs or equivalent",
        "O levels/GCSEs or equivalent",
        "A levels/AS levels or equivalent",
        "Other professional qualifications eg: nursing, teaching",
        "NVQ or HND or HNC or equivalent",
        "College or University degree"
      )
      for(i in seq(1, length(qual_list), by=1)) {
        data[data == qual_list[i]] <- as.character(i)
      }
      y <- do.call(pmax, c(data, list(na.rm=TRUE)))
      #y[is.na(y)] <- 1
      y[y=="Prefer not to answer"]=NA
      y <- factor(y,
                  levels = seq(1, length(qual_list), by=1),
                  labels = qual_list
      )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Highest Qualification",
    description = "Highest of a participant's self-reported educational qualifications"
  )
}


TEU_Rec_AssessCentre <- function() {
  list(
    name = "TEU_Rec_AssessCentre",
    source = "Rec_AssessCentre.0.0",
    mapper = function(x) {
      map <- read.csv_kdrive(file.path(config$cleaning$coding,"coding10_AssessmentCentre.csv"))
      # https://biobank.ndph.ox.ac.uk/showcase/coding.cgi?id=10
      y <- merge(x,
                 map,
                 by.x = "x",
                 by.y = "Code",
                 all.x = TRUE,
                 sort = FALSE)
      y <- y[["meaning"]]
    },
    post_exclusion = FALSE,
    display_name = "AssessCentre",
    description = "Which assessment centre did the participant attend"
  )
}

TEU_Rec_Country <- function() {
  list(
    name = "TEU_Rec_Country",
    source = "Rec_AssessCentre.0.0",
    mapper = function(x) {
      y <- dplyr::case_when(
        x %in% c(
          10003,
          11001,
          11002,
          11006,
          11007,
          11008,
          11009,
          11010,
          11011,
          11012,
          11013,
          11014,
          11016,
          11017,
          11018,
          11020,
          11021,
          11024,
          11025,
          11026,
          11027,
          11028
        ) ~ "England",
        x %in% c(11004, 11005) ~ "Scotland",
        x %in% c(11003, 11022, 11023) ~ "Wales",
        TRUE ~ x
      )
      if (!all(y %in% c("England", "Scotland", "Wales"))) {
        warning(paste0("Unrecognised centre code: ", y[!y %in% c("England", "Scotland", "Wales")]))
      }
      y <- factor(y, levels=c("England", "Scotland", "Wales"))
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "UK country of residence",
    description = "Which country does the participant live in"
  )
}

TownsendDepInd <- function(){
  list(
    name = 'TownsendDepInd',
    source = c("BaC_DeprivInd.0.0"),
    mapper = FN_id,
    post_exclusion =TRUE,
    display_name = 'Townsend Deprivation Index',
    description = 'Townsend Deprivation Index'
  )
}


TEU_ethnicgrp <- function() {
  list(
    name = "TEU_ethnicgrp",
    source = "Eth_Ethnicity.0.0",
    mapper = function(x) {
      y <- dplyr::case_when(
        x %in% c("White", "British", "Irish", "Any other white background") ~ "White",
        x %in% c(
          "Mixed",
          "White and Black Caribbean",
          "White and Black African",
          "White and Asian",
          "Any other mixed background"
        ) ~ "Mixed",
        x %in% c("Indian", "Pakistani", "Bangladeshi") ~ "S. Asian",
        x %in% c(
          "Black or Black British",
          "Caribbean",
          "African",
          "Any other Black background"
        ) ~ "Black",
        x %in% c(
          "Other ethnic group",
          "Asian or Asian British",
          "Any other Asian background",
          "Chinese"
        ) ~ "Other",
        x %in% c("Do not know", "Prefer not to answer") ~ "Unanswered",
        is.na(x) ~ "Unanswered",
        TRUE ~ "Error"
      )
      y <-
        factor(
          y,
          ordered = FALSE,
          levels = c("White", "Black", "S. Asian", "Mixed",
                     "Other", "Unanswered")
        )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Ethnic group",
    description = "The participant's self-reported ethnicity, condensed into categories.\n'White', 'British', 'Irish' and 'Any other white background' were coded as 'White'.\n'Indian', 'Pakinstani' and 'Bangladeshi' were coded as 'S. Asian'.\n'Black or Black British', 'Carribean', 'African' and 'Any other Black background' were coded as 'Black'.\n'Mixed', 'White and Black Caribbean', 'White and Black African', 'White and Asian' and 'Any other mixed background' were coded as 'Mixed'.\n'Other ethnic group', 'Asian or Asian British', 'Any other Asian background' and 'Chinese' were coded as 'Other'"
  )
}


TEU_Alc_Status <- function() {
  list(
    name = "TEU_Alc_Status",
    source = "Alc_Status.0.0",
    mapper = FN_labelfactor(levels=c("Never","Previous","Current"),
                            labels=c("Never","Previous","Current")),
    post_exclusion = FALSE,
    display_name = "Alcohol status",
    description = "Self-reported alcohol drinking status"
  )
}

TEU_Smo_Status <- function() {
  list(
    name = "TEU_Smo_Status",
    source = "Smo_Status.0.0",
    mapper = FN_labelfactor(levels=c("Never","Previous","Current"),
                            labels=c("Never","Previous","Current")),
    post_exclusion = FALSE,
    display_name = "Smoking status",
    description = "Self-reported smoking status"
  )
}

TEU_Smo_NumCig <-function(){
  list(
    name = "TEU_Smo_NumCig",
    source = c("Smo_CurNCig.0.0","TEU_Smo_Status"),
    mapper = function(data){
      data=data%>%
        mutate(y=ifelse(TEU_Smo_Status%in%c("Never","Previous"),0,Smo_CurNCig.0.0))
      return(data$y)
    },
    post_exclusion = FALSE,
    display_name = "Number of cigarettes currently smoked daily",
    description = "Self-reported number of cigarettes currently smoked daily (previous or none smokers were assigned as 0 for this variable)"
  )
}

PsF_LeisureAct <- function() {
  list(
    name = "PsF_LeisureAct",
    source = "PsF_LeisureAct.0.0",
    mapper = FN_labelfactor(),
    post_exclusion = FALSE,
    display_name = "Leisure/social activities",
    description = "Self-reported leisure/social activities"
  )
}

PsF_VisitFreq <- function(){
  list(
    name = "PsF_VisitFreq",
    source = "PsF_VisitFreq.0.0",
    mapper = FN_labelfactor(),
    post_exclusion = FALSE,
    display_name = "Frequency of friend/family visits",
    description = "Frequency of friend/family visits"
  )
}

PsF_Confide<- function(){
  list(
    name = "PsF_Confide",
    source = "PsF_Confide.0.0",
    mapper = FN_labelfactor(),
    post_exclusion = FALSE,
    display_name = "Able to confide",
    description = "Able to confide"
  )
}


TEU_Alc_WeeklyAlcUnits <- function() {
  list(
    name = "TEU_Alc_WeeklyAlcUnits",
    source = c(
      "Alc_RedWineWk.0.0",
      "Alc_WhiteWineWk.0.0",
      "Alc_BeerCiderWk.0.0",
      "Alc_SpiritsWk.0.0",
      "Alc_FortWineWk.0.0",
      "Alc_OtherAlcWk.0.0"
    ),
    mapper = function(data) {
      alcservings <- data
      for (alc in c(
        "Alc_RedWineWk.0.0",
        "Alc_WhiteWineWk.0.0",
        "Alc_BeerCiderWk.0.0",
        "Alc_SpiritsWk.0.0",
        "Alc_FortWineWk.0.0",
        "Alc_OtherAlcWk.0.0"
      )) {
        alcservings[[alc]][alcservings[[alc]] < 0 | is.na(alcservings[[alc]])] <-  0
      }
      
      weekly_alcunits <-
        #	Red wine (1 glass, 125ml, ABV 12% = 1.5 units)
        (1.5 * alcservings$Alc_RedWineWk.0.0) +
        # White wine, champagne (1 glass, 125ml, ABV 12% = 1.5 units)
        (1.5 * alcservings$Alc_WhiteWineWk.0.0) +
        #	Fortified wines: e.g. sherry, port (1 measure, 50ml, ABV 20% = 1 unit)
        (1.0 * alcservings$Alc_FortWineWk.0.0) +
        #	Beer, cider including bitter, lager, stout, ale, Guinness (1 pint, 568ml, ABV 3.6% = 2 units)
        (2.0 * alcservings$Alc_BeerCiderWk.0.0) +
        #	Spirits, liquors (1 measure or shot, 25ml, ABV 40% = 1 unit)
        (1.0 * alcservings$Alc_SpiritsWk.0.0) +
        #	For "other" types of alcohol, will use alcopops as proxy ( 1 drink, 275ml, ABV 5.5% = 1.5 units)
        (1.5 * alcservings$Alc_OtherAlcWk.0.0)
      
      # Truncate alcohol consumption at upper 95th percentile
      upper95 <- quantile(weekly_alcunits, 0.95, na.rm = TRUE)
      weekly_alcunits[weekly_alcunits > upper95 & !is.na(weekly_alcunits)] <- upper95
      weekly_alcunits[is.na(weekly_alcunits)] <- 0
      
      return(weekly_alcunits)
    },
    post_exclusion = FALSE,
    display_name = "Alcohol units per week",
    description = "Total weekly units of alcohol, derived from self-reported average weekly consumption of each different type alcohol and truncated at the upper 95th percentile. This data was available for participants who said they drank alcohol more than once or twice a week."
  )
}


TEU_Alc_Binge <- function() {
  list(
    name = "TEU_Alc_Binge",
    source = c("TEU_Alc_WeeklyAlcUnits", "BaC_Sex"),
    mapper = function(data) {
      y <- dplyr::case_when(data[["BaC_Sex"]] == "Female" &
                              data[["TEU_Alc_WeeklyAlcUnits"]] > 7 ~ TRUE,
                            data[["BaC_Sex"]] == "Male" &
                              data[["TEU_Alc_WeeklyAlcUnits"]] > 14 ~ TRUE,
                            TRUE ~ FALSE)
    },
    post_exclusion = FALSE,
    display_name = "Harmful alcohol consumption",
    description = "Does the patient's self-reported weekly alcohol consumption exceed the threshold for binge drinking. Data on weekly alcohol consumption was available for participants who said they drank alcohol more than once or twice a week, those who drank less frequently were not considered to have harmful alcohol consumption."
  )
}

# XL add: 19/11/2020
PhA_METsWkAllAct <- function(){
  list(
    name = 'PhA_METsWkAllAct',
    source = "PhA_METsWkAllAct.0.0",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = 'Summed MET',
    description = 'Summed MET minutes per week for all activity'
  )
}

TEU_Edu_ISCED <- function() {
  list(
    name = "TEU_Edu_ISCED",
    source = "TEU_Edu_HighestQual",
    mapper = function(x) {
      # Convert UKB qualification categories into ISCED education categories
      y <- dplyr::case_when(
        x == "College or University degree" ~ "5: Tertiary",
        x == "NVQ or HND or HNC or equivalent" ~ "5: Tertiary",
        x == "Other professional qualifications eg: nursing, teaching" ~ "4: Post-secondary non-tertiary",
        x == "A levels/AS levels or equivalent" ~ "2-3: Secondary",
        x == "O levels/GCSEs or equivalent" ~ "2-3: Secondary",
        x == "CSEs or equivalent" ~ "2-3: Secondary",
        x == "None of the above" ~ "1: Primary",
        #x == "Prefer not to answer" ~ NA_character_,
        is.na(x) ~ NA_character_
      )
      y <- factor(
        y,
        levels = c(
          "5: Tertiary",
          "4: Post-secondary non-tertiary",
          "2-3: Secondary" ,
          "1: Primary"
        )
      )
      
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Highest level of education (ISCED)",
    description = "ISCED category of participant's highest attained qualification"
  )
}



TEU_FaH_CVD <- function() {
  list(
    name = "TEU_FaH_CVD",
    source = c(
      paste0("FaH_FatherIll.0.", c(0:9)),
      paste0("FaH_MotherIll.0.", c(0:10)),
      paste0("FaH_SibIll.0.", c(0:11))
    ),
    mapper = FN_FamHist(
      conditions = c("Heart disease", "High blood pressure", "Stroke"),
      label = "CVD"
    ),
    post_exclusion = FALSE,
    display_name = "Family history of CVD",
    description = "Family history of CVD (Heart disease, high blood pressure, stroke), derived by combining reported medical history of father, mother and siblings (adopted relatives were not included)"
  )
}

TEU_FaH<- function(label,conditions) {
  list(
    name = paste0("TEU_FaH_",label),
    source = c(
      paste0("FaH_FatherIll.0.", c(0:9)),
      paste0("FaH_MotherIll.0.", c(0:10)),
      paste0("FaH_SibIll.0.", c(0:11))
    ),
    mapper = FN_FamHist(
      conditions = conditions,
      label = label
    ),
    post_exclusion = FALSE,
    display_name = paste0("Family history of ",label),
    description = paste0("Family history of ",label, "(",paste(conditions,collapse = " "),") derived by combining reported medical history of father, mother and siblings (adopted relatives were not included)")
  )
}

# Rest of the family history fields (except adopted)
TEU_FaH_Info<-function(){
  
  renaming<-read.csv(file.path(config$cleaning$renaming))
  ## Family history
  FaH_vars<-renaming%>%
    filter(grepl("^FaH_",NewVarName))%>%
    # remove adopted related fields
    filter(!grepl("adopted|Adopted",Field_Description))%>%
    # remove fields with special categories 
    filter(!NewVarName%in%c("FaH_FatherIll","FaH_MotherIll","FaH_SibIll"))
  
  FaH_list<-as.list(FaH_vars$NewVarName)
  names(FaH_list)<-FaH_vars$Field_Description
  
  
  lapply(1:length(FaH_list), function(i)
    
    list(
      name = FaH_list[[i]], 
      source = c(paste0(FaH_list[[i]],".0.0")), 
      mapper = function(x){
        if(is.numeric(x)){
          FN_toNA()(x)
        }else{FN_labelfactor()(x)}
      },
      post_exclusion = FALSE,
      display_name = names(FaH_list[i]),
      description = names(FaH_list[i])
    )
  )
  
  
}

VeI_PregnantNow <- function() {
  list(
    name = "VeI_PregnantNow", 
    source = c("VeI_PregnantNow.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Pregnant",
    description = "Was the participant pregnant at baseline"
  )
}


TEU_BrCa_313_PRS <- function() {
  list(
    name = "TEU_BrCa_313_PRS", 
    source = c("ID"), 
    mapper = FN_JoinPRS(filepath="K:/TEU/UKB_Genetic_Data/PRS_Pipeline/prs/projects/brca313-mavaddat2018/outputs/prs_noCallRateHWE_20210811.sscore",
                        colname="SCORE1_AVG"),
    post_exclusion = FALSE,
    display_name = "BrCa 313 PRS",
    description = "Breast cancer polygenic risk score, 313 SNPs from Mavaddat 2018 paper"
  )
}

TEU_BrCa_313_PRS_quintiles <- function() {
  list(
    name = "TEU_BrCa_313_PRS_quintiles", 
    source = c("TEU_BrCa_313_PRS"), 
    mapper = FN_quantiles(quant=5),
    post_exclusion = TRUE,
    display_name = "BrCa 313 PRS quintiles",
    description = "Quintiles of the 313 SNP breast cancer PRS score"
  )
}

TEU_BrCa_313_PRS_percent <-function(){
  list(
    name = "TEU_BrCa_313_PRS_percent", 
    source = c("TEU_BrCa_313_PRS"), 
    mapper = function(x){
      cut(x,breaks=c(quantile(x,probs=c(0,0.01,0.05,0.1,0.2,0.4,0.6,0.8,0.9,0.95,0.99,1),na.rm=TRUE)),
          labels=c("<1%","1-5%","5-10%","10-20%","20-40%","40-60%","60-80%","80-90%","90-95%","95-99%",">99%"))
    },
    post_exclusion = TRUE,
    display_name = "BrCa 313 PRS percentiles",
    description = "Percentiles of the 313 SNP breast cancer PRS score according to Mavaddat2015"
  )
}


TEU_BrCa_100k_PRS <- function() {
  list(
    name = "TEU_BrCa_100k_PRS", 
    source = c("ID"), 
    mapper = FN_JoinPRS(filepath="K:/TEU/UKB_Genetic_Data/PRS_Pipeline/prs/projects/brca100k-fritsche2020/outputs/prs_noCallRateHWE_20210811.sscore",
                        colname="SCORE1_AVG"),
    post_exclusion = FALSE,
    display_name = "BrCa 100k PRS",
    description = "Breast cancer polygenic risk score, 100k SNPs from Fritsche 2020 paper"
  )
}

TEU_BrCa_100k_PRS_quintiles <- function() {
  list(
    name = "TEU_BrCa_100k_PRS_quintiles", 
    source = c("TEU_BrCa_100k_PRS"), 
    mapper = FN_quantiles(quant=5),
    post_exclusion = TRUE,
    display_name = "BrCa 100k PRS quintiles",
    description = "Quintiles of the 100k SNP breast cancer PRS score"
  )
}

TEU_BrCa_100k_PRS_percent <- function() {
  list(
    name = "TEU_BrCa_100k_PRS_percent", 
    source = c("TEU_BrCa_100k_PRS"), 
    mapper = function(x){
      cut(x,breaks=c(quantile(x,probs=c(0,0.01,0.05,0.1,0.2,0.4,0.6,0.8,0.9,0.95,0.99,1),na.rm=TRUE)),
          labels=c("<1%","1-5%","5-10%","10-20%","20-40%","40-60%","60-80%","80-90%","90-95%","95-99%",">99%"))
    },
    post_exclusion = TRUE,
    display_name = "BrCa 100k PRS percentiles",
    description = "Percentiles of the 100k SNP breast cancer PRS score according to Mavaddat2015"
  )
}


# General function for extracting blood biochemistry vars
BBC_Result <- function() {
  
  # List all variables from BBC category
  renaming<-read.csv(file.path(config$cleaning$renaming))
  
  BBC_list<-vars_to_list(renaming=renaming,name_pattern = "^BBC_.*_Result$",extra_vars = NULL)
  
  lapply(1:length(BBC_list), function(i) 
    
    list(
      name = BBC_list[[i]], 
      source = c(paste0(BBC_list[[i]],".0.0")), 
      mapper = FN_id,
      post_exclusion = FALSE,
      display_name = names(BBC_list[i]),
      description = paste0(names(BBC_list[i])," assay from baseline blood serum")
    )
    
    )
  
}

Blood_count <- function(){
  
  renaming<-read.csv(file.path(config$cleaning$renaming))
  
  BlA_vars<-renaming%>%filter(grepl("^BlA_",NewVarName))%>%
    filter(grepl("Count$|percentage$|concentration$|fraction$|haemoglobin$|volume$|crit$|width$|wavelength$",
                                                                        Field_Description,ignore.case = TRUE))
  
  BlA_list<-as.list(BlA_vars$NewVarName)
  names(BlA_list)<-BlA_vars$Field_Description
  
  lapply(1:length(BlA_list), function(i) 
    
    list(
      name = BlA_list[[i]], 
      source = c(paste0(BlA_list[[i]],".0.0")), 
      mapper = FN_id,
      post_exclusion = FALSE,
      display_name = names(BlA_list[i]),
      description = paste0(names(BlA_list[i])," from baseline blood serum")
    )
    
  )
}

Urine_Assays<-function(){
  
  renaming<-read.csv(file.path(config$cleaning$renaming))
  
  Uri_vars<-renaming%>%filter(grepl("^Uri_",NewVarName))%>%
    filter(!grepl("DeviceID$|Time$",NewVarName))
  
  Uri_list<-as.list(Uri_vars$NewVarName)
  names(Uri_list)<-Uri_vars$Field_Description
  
  lapply(1:length(Uri_list), function(i) 
    
    list(
      name = Uri_list[[i]], 
      source = c(paste0(Uri_list[[i]],".0.0")), 
      mapper = function(x){
        if(is.numeric(x)){
          FN_toNA()(x)
        }else{FN_labelfactor()(x)}
      },
      post_exclusion = FALSE,
      display_name = names(Uri_list[i]),
      description = paste0(names(Uri_list[i])," from baseline urine samples")
    )
    
  )
}

Diet <- function(){
  renaming<-read.csv(file.path(config$cleaning$renaming))
  
  die_vars<-renaming%>%
    filter(grepl("^Die_",NewVarName))%>%
    # remove pilot variables
    filter(!grepl("(pilot)",Field_Description))
    
  die_list<-as.list(die_vars$NewVarName)
  names(die_list)<-die_vars$Field_Description
  
  lapply(1:length(die_list), function(i) 
    
    list(
      name = die_list[[i]], 
      source = c(paste0(die_list[[i]],".0.0")), 
      mapper = function(x){
        if(is.numeric(x)){
          x[x==-10]=0
          FN_toNA()(x)
        }else{FN_labelfactor()(x)}
      },
      post_exclusion = FALSE,
      display_name = names(die_list[i]),
      description = paste0(names(die_list[i])," from baseline urine samples")
    )
    
  )
  
  
}


GeP_PC <- function(pc=1) {
  list(
    name = paste0("GeP_PC_", pc), 
    source = glue("GeP_PC.0.{pc}"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = glue("Principal component {pc}"),
    description = glue("Genetic principal component {pc}, from Bycroft")
  )
}

GeP_Batch <- function() {
  list(
    name = "GeP_Batch", 
    source = "GeP_Batch.0.0", 
    mapper = function(x) {
      coding <- read.csv_kdrive(file.path(config$cleaning$coding, "coding22000_flat_GenotypingArray.csv"))
      y <- factor(coding$L1[match(x, coding$Code)])
    },
    post_exclusion = FALSE,
    display_name = "Genotype measurement batch",
    description = "Genotype measurement batch"
  )
}

GeP_Array <- function() {
  list(
    name = "GeP_Array", 
    source = "GeP_Batch.0.0", 
    mapper = function(x){
      coding <- read.csv_kdrive(file.path(config$cleaning$coding, "coding22000_flat_GenotypingArray.csv"))
      y <- factor(coding$L0[match(x, coding$Code)], levels=c("Axiom", "BiLEVE"))
    },
    post_exclusion = FALSE,
    display_name = "Genotype array",
    description = "Genotype array - UK BiLEVE or Biobank Axiom Array"
  )
}

GeP_ethnic <- function() {
  list(
    name = "GeP_ethnic", 
    source = "GeP_ethnic.0.0", 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Genotype ethnic grouping",
    description = "Genotype ethnic grouping"
  )
}


GeP_UsedInPCA <- function() {
  list(
    name = "GeP_UsedInPCA", 
    source = c("GeP_UsedInPCA.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "string",
    description = "text"
  )
}

GeP_Outliers <- function() {
  list(
    name = "GeP_Outliers", 
    source = c("GeP_Outliers.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "string",
    description = "text"
  )
}

GeP_Plate <- function() {
  list(
    name = "GeP_Plate", 
    source = c("GeP_Plate.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "string",
    description = "text"
  )
}

GeP_Sex <- function() {
  list(
    name = "GeP_Sex", 
    source = c("GeP_Sex.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "string",
    description = "text"
  )
}

VeI_BirthWt <- function() {
  list(
    name = "VeI_BirthWt", 
    source = c("VeI_BirthWt.0.0"), 
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = "Birth weight",
    description = "Self-reported birth weight (in Kg)"
  )
}



TEU_ATC <- function(length="Full") {
  
  ATC <- readRDS("K:\\TEU\\MLforBrCa\\Git_Repo\\Data\\Derived\\ATC_baseline_20211108.rds")
  
  codes <- unique(ATC$ATC)
  
  if(length=="Full"){
    length=length(codes)
  }
  
  lapply(1:length,function(i)
  
  list(
    name = codes[i],
    source = "ID",
    mapper = function(x) {
      
      subset<-ATC%>%
        # User of each ATC group
        filter(ATC==codes[i])%>%
        distinct(ID)%>%
        mutate(ind=1)
      
      y <- subset[["ind"]][match(x, subset$ID)]
      # Fill NA with 0
      y[is.na(y)]<-0
      return(y)
    },
    post_exclusion = FALSE,
    display_name = paste0(codes[i],"user at baseline"),
    description = "Mapped UKB medication group (FID 20003) to Level 4 Anatomical Therapeutic Chemical (ATC) group"
  )
  )
}



# From JC: Job variable 
TEU_Emp_CurrStat <- function() {
  list(
    name = "TEU_Emp_CurrStat",
    source = c("Emp_CurrStatUnc.0.0", "Emp_CurrStat.0.0"),
    mapper = function(data) {
      # XL change: Changed the order of uncorrected and corrected
      y <- factor(FN_toNA(values = c("Prefer not to answer"))(coalesce(as.character(data[["Emp_CurrStat.0.0"]]), 
                           as.character(data[["Emp_CurrStatUnc.0.0"]]))),
                  ordered=FALSE)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Employment status",
    description = "Employment status at baseline, derived by taking the employment status from TQ and applying corrections made by UKB in light of participant jobs self-reported in VI"
  )
}

TEU_Emp_JobCode_v2 <- function() {
  list(
    name = "Emp_JobCode.0.0",
    source = "ID",
    mapper = function(x) {
      v2_emp <- DBfunc$DB_extract(extract_cols = c("ID", "Emp_JobCode.0.0"),
                                  db = "K:/TEU/UKB33952_Data/Data_Downloads/V2_database_duckdb0.2.1/ukb_v2.db",
                                  name_map = "K:/TEU/UKB33952_Data/Data_Dictionary/Renaming_List_UPDATE_Nov2019_TEU.csv")
      y <- v2_emp[["Emp_JobCode.0.0"]][match(x, v2_emp$ID)]
      y <- as.numeric(y)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = "Job code",
    description = "Job code of participant at baseline self-reported in verbal interview"
  )
}

TEU_HTN_Emp_category <- function() {
  list(
    name = "TEU_Emp_category",
    source = c("Emp_JobCode_visit.0.0", "TEU_Emp_CurrStat"),
    mapper = function(data) {
      map <- read.csv_kdrive(file.path(config$cleaning$coding,"coding2_flat_Employment.csv"))%>%
        # Code stored in Emp_JobCode_visit (FID 20277) have been truncated to the first 4 digits
        mutate(Code=substr(as.character(Code),1,4))%>%
        # make Code column unique (o.w. joining issue!)
        group_by(Code) %>%
        filter(row_number()==1)
      
      data <- left_join(data, map[,c("Code", "L0")], by=c("Emp_JobCode_visit.0.0" = "Code"))
      data$TEU_EmpCat <- coalesce(as.character(data[["L0"]]), 
                                  as.character(data[["TEU_Emp_CurrStat"]]))
      
      y <- dplyr::case_when(
        is.na(data$TEU_EmpCat) ~ "Unemployed/unanswered",
        data$TEU_EmpCat %in% c("Managers and Senior Officials", "Professional Occupations",
                               "Associate Professional and Technical Occupations",
                               "Administrative and Secretarial Occupations") ~ "White collar",
        data$TEU_EmpCat == "Skilled Trades Occupations" ~ "Skilled trades",
        data$TEU_EmpCat %in% c("Personal Service Occupations",
                               "Sales and Customer Service Occupations") ~ "Services",
        data$TEU_EmpCat %in% c("Process, Plant and Machine Operatives",
                               "Elementary Occupations") ~ "Blue collar",
        data$TEU_EmpCat %in% c("Other job (free text entry)",
                               "In paid employment or self-employed") ~ "Other employment",
        data$TEU_EmpCat == "Retired" ~ "Retired",
        data$TEU_EmpCat == "Unable to work because of sickness or disability" ~ "Disability",
        data$TEU_EmpCat %in% c("Looking after home and/or family",
                               "Unemployed", "Full or part-time student",
                               "Doing unpaid or voluntary work",
                               "None of the above", "Prefer not to answer") ~ "Unemployed/unanswered",
        TRUE ~ "Error?"
      )
      y <- factor(y, 
                  levels=c("White collar", "Skilled trades", "Services", 
                           "Blue collar", "Other employment", "Retired", 
                           "Disability", "Unemployed/unanswered"), 
                  labels=c("Professional and Administrative", "Skilled trades", "Services", 
                           "Manual and Industrial", "Other employment", "Retired", 
                           "Unable to work because of sickness or disability", 
                           "Unemployed/unanswered"),
                  ordered=FALSE)
      
    },
    post_exclusion = FALSE,
    display_name = "Employment category",
    description = "Employment category at baseline, collapsed from job codes self-reported in verbal interview"
  )
}

# Admin censoring date (hospital inpatient)
#https://biobank.ndph.ox.ac.uk/showcase/exinfo.cgi?src=Data_providers_and_dates#:~:text=Censoring%20dates,that%20provider%20is%20mostly%20complete.
# England 31/03/2017, Scotland 31/10/2016, Wales 29/02/2016
Admin_HES_CensorDate<-function(record_level=FALSE){
  list(
    name = 'Admin_HES_CensorDate',
    source = 'TEU_Rec_Country',
    mapper = function(x){
      if(record_level){
        HES <- read_yaml(file.path(config$data$portal$HES, "censoring.yml")) %>%
          lapply(., FUN=FN_toDate)
        # deaths <- read_yaml(file.path(config$data$portal$deaths, "censoring.yml"))
        # datelist <- lapply(x, FUN = function(z) {min(FN_toDate(HES[[z]]), FN_toDate(deaths[[z]]))})
        # y <- do.call(c, datelist)
      }
      else {
        HES <- read_yaml(file.path(dirname(config$data$database), "censoring.yml"))$HES %>%
          lapply(., FUN=FN_toDate)
      } 
      y <- dplyr::case_when(
        x=='England' ~ HES$England,
        x=='Scotland' ~ HES$Scotland,
        x=='Wales' ~ HES$Wales
      )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Administrative censoring date from UKB HES',
    description = 'Censoring date according to origin of hospital data'
  )
}

# Admin censoring date (Cancer outcome)
# https://biobank.ndph.ox.ac.uk/showcase/exinfo.cgi?src=Data_providers_and_dates
# Note there is no record-level option for cancer registry data
Admin_CaR_CensorDate<-function(){
  list(
    name = 'Admin_CaR_CensorDate',
    source = 'TEU_Rec_Country',
    mapper = function(x){

      CaR <- read_yaml(file.path(dirname(config$data$database), "censoring.yml"))$Cancer %>%
        lapply(., FUN=FN_toDate)
      
      y <- dplyr::case_when(
        x=='England' ~ CaR$England,
        x=='Scotland' ~ CaR$Scotland,
        x=='Wales' ~ CaR$Wales
      )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Administrative censoring date for cancer registries data',
    description = 'Administrative censoring date for cancer registries data by country, using assessment centre as a proxy for country'
  )
}

# Admin censoring date (Death registry)
# https://biobank.ndph.ox.ac.uk/showcase/exinfo.cgi?src=Data_providers_and_dates

Admin_Dth_CensorDate<-function(record_level=FALSE){
  list(
    name = 'Admin_Dth_CensorDate',
    source = 'TEU_Rec_Country',
    mapper = function(x){
      if(record_level){
        deaths <- read_yaml(file.path(config$data$portal$deaths, "censoring.yml")) %>%
          lapply(., FUN=FN_toDate)
      } else {
        deaths <- read_yaml(file.path(dirname(config$data$database), "censoring.yml"))$Death %>%
          lapply(., FUN=FN_toDate)
      }
      
      y <- dplyr::case_when(
        x=='England' ~ deaths$England,
        x=='Scotland' ~ deaths$Scotland,
        x=='Wales' ~ deaths$Wales
      )
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Administrative censoring date for death registry data',
    description = 'Administrative censoring date for death registries data by country, using assessment centre as a proxy for country'
  )
}

Admin_CensorDate <- function(sources=c("HES", "Dth", "CaR")){
  list(
    name = 'Admin_CensorDate',
    source = glue("Admin_{sources}_CensorDate"),
    mapper = function(data){
      y <- do.call(pmin, data[,glue("Admin_{sources}_CensorDate")])
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Administrative censoring date',
    description = 'Administrative censoring date, taken as the minimum of (death censoring and the maximum of (cancer registry and HES censoring))'
  )
}


# Lost to follow-up
BaC_LostFUDate<-function(){
  list(
    name = 'BaC_LostFUDate',
    source = 'BaC_DateLostFU.0.0',
    mapper = FN_toDate,
    post_exclusion = FALSE,
    display_name = 'Date lost to follow-up',
    description = 'Date lost to follow-up'
  )
}


TEU_BreastCancer <- function() {
  code_list <- list(ICD10="C50", ICD9="174")
  type <- "prevalent"
  
  list(
    list(
      name = "BrCaDate_Prevalent",
      source = c("ID", "Rec_DateAssess",
                 paste0("CaR_DiagDate.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD10.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD9.", seq(0, 14, by=1), ".0")),
      mapper = FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                         code_list=code_list, type="prevalent", keepcol="DiagDate"),
      post_exclusion = FALSE,
      display_name = "Prevalent BrCa dx date",
      description = "Date of first breast cancer diagnosis before baseline assessment, from cancer registries"
    ),
    list(
      name = "BrCaDx_Prevalent",
      source = c("ID", "Rec_DateAssess",
                 paste0("CaR_DiagDate.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD10.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD9.", seq(0, 14, by=1), ".0")),
      mapper = function(data) {
        codingICD10 <- read.csv(file.path(config$cleaning$coding, "coding19_flat_Icd10.csv"))
        ICD10 <- FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                           code_list=code_list, type="prevalent", keepcol="DiagICD10")(data)
        meaningICD10 <- codingICD10$meaning[match(ICD10, codingICD10$Code)]
        
        codingICD9 <- read.csv(file.path(config$cleaning$coding, "coding87_flat_Icd9.csv"))
        ICD9 <- FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                          code_list=code_list, type="prevalent", keepcol="DiagICD9")(data)
        meaningICD9 <- codingICD9$meaning[match(ICD9, codingICD9$Code)]
        
        y <- coalesce(meaningICD10, meaningICD9)
        return(y)
      },
      post_exclusion = FALSE,
      display_name = "Prevalent BrCa dx",
      description = "Type of first breast cancer diagnosis before baseline assessment, from cancer registries"
    ),
    list(
      name = "BrCaDate_Incident",
      source = c("ID", "Rec_DateAssess", "TEU_HES_BrCa_incdate",
                 paste0("CaR_DiagDate.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD10.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD9.", seq(0, 14, by=1), ".0")),
      mapper = function(data){
        CaR <- FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                         code_list=code_list, type="incident", keepcol="DiagDate")(data)
        y <- coalesce(CaR, data$TEU_HES_BrCa_incdate)
        },
      post_exclusion = FALSE,
      display_name = "Incident BrCa dx date",
      description = "Date of first breast cancer diagnosis after baseline assessment, from cancer registries"
    ),
    list(
      name = "BrCaDx_Incident",
      source = c("ID", "Rec_DateAssess", "TEU_HES_BrCa_inc", "TEU_HES_BrCa_incdate", 
                 paste0("CaR_DiagDate.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD10.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD9.", seq(0, 14, by=1), ".0")),
      mapper = function(data) {
        codingICD10 <- read.csv(file.path(config$cleaning$coding, "coding19_flat_Icd10.csv"))
        ICD10 <- FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                           code_list=code_list, type="incident", keepcol="DiagICD10")(data)
        meaningICD10 <- codingICD10$meaning[match(ICD10, codingICD10$Code)]
        
        codingICD9 <- read.csv(file.path(config$cleaning$coding, "coding87_flat_Icd9.csv"))
        ICD9 <- FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                          code_list=code_list, type="incident", keepcol="DiagICD9")(data)
        meaningICD9 <- codingICD9$meaning[match(ICD9, codingICD9$Code)]
        
        CaR <- coalesce(meaningICD10, meaningICD9)
        y <- coalesce(CaR, data$TEU_HES_BrCa_inc)
        return(y)
      },
      post_exclusion = FALSE,
      display_name = "Incident BrCa dx",
      description = "Type of first breast cancer diagnosis after baseline assessment, from cancer registries"
    ),
    list(
      name = "BrCaInSitu_Prevalent",
      source = c("ID", "Rec_DateAssess",
                 paste0("CaR_DiagDate.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD10.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD9.", seq(0, 14, by=1), ".0")),
      mapper = FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                         code_list=list(ICD10="D05", ICD9="2330"), type="prevalent", keepcol="DiagDate"),
      post_exclusion = FALSE,
      display_name = "Prevalent breast carcinoma in situ dx date",
      description = "Date of first breast carcinoma in situ diagnosis before baseline assessment, from cancer registries"
    ),
    list(
      name = "BrCaInSitu_Incident",
      source = c("ID", "Rec_DateAssess",
                 paste0("CaR_DiagDate.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD10.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD9.", seq(0, 14, by=1), ".0")),
      mapper = FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                         code_list=list(ICD10="D05", ICD9="2330"), type="incident", keepcol="DiagDate"),
      post_exclusion = FALSE,
      display_name = "Incident breast carcinoma in situ dx date",
      description = "Date of first breast carcinoma in situ diagnosis after baseline assessment, from cancer registries"
    ),
    list(
      name = "OtherCancerDx_Date",
      source =c("ID", "Rec_DateAssess",
                paste0("CaR_DiagDate.", seq(0, 16, by=1), ".0"),
                paste0("CaR_DiagICD10.", seq(0, 16, by=1), ".0"),
                paste0("CaR_DiagICD9.", seq(0, 14, by=1), ".0")),
      mapper = FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                         code_list=list(ICD10=c("C50", "C44"), ICD9=c("174", "173")), 
                         exclude=TRUE, type="all", keepcol="DiagDate"),
      post_exclusion = FALSE,
      display_name = "Diagnosis date of other cancer",
      description = "Date of first diagnosis of any cancer except breast cancer or non-melanoma skin cancer"
    ),
    list(
      name = "OtherCancerDx_Prevalent",
      source =c("ID", "Rec_DateAssess",
                paste0("CaR_DiagDate.", seq(0, 16, by=1), ".0"),
                paste0("CaR_DiagICD10.", seq(0, 16, by=1), ".0"),
                paste0("CaR_DiagICD9.", seq(0, 14, by=1), ".0")),
      mapper = function(data) {
        codingICD10 <- read.csv(file.path(config$cleaning$coding, "coding19_flat_Icd10.csv"))
        ICD10 <- FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                           code_list=list(ICD10=c("C50", "C44"), ICD9=c("174", "173")), exclude=TRUE,type="prevalent", keepcol="DiagICD10")(data)
        meaningICD10 <- codingICD10$meaning[match(ICD10, codingICD10$Code)]
        
        codingICD9 <- read.csv(file.path(config$cleaning$coding, "coding87_flat_Icd9.csv"))
        ICD9 <- FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                          code_list=list(ICD10=c("C50", "C44"), ICD9=c("174", "173")), exclude=TRUE,type="prevalent", keepcol="DiagICD9")(data)
        meaningICD9 <- codingICD9$meaning[match(ICD9, codingICD9$Code)]
        
        meaning <- coalesce(meaningICD10, meaningICD9)
        
        # Create indicator
        y <- ifelse(is.na(meaning),0,1)
          
        return(y)
      },
      post_exclusion = FALSE,
      display_name = "Prevalent other cancer dx",
      description = "Indicator of any prevalent cancer except breast cancer or non-melanoma skin cancer (0=no;1=yes)"
    )
  )
}

# Other cause dth date
TEU_Dth_NotBrCa_dthdate <-function(record_level=FALSE, ICD10_codes, exclude=FALSE){
  list(
    name = 'TEU_Dth_NotBrCa_dthdate',
    source = if(record_level){c("ID")} else {c('ID',"Dth_ICD10Underlying.0.0", "Dth_ICD10Underlying.1.0","Dth_Date.0.0", "Dth_Date.1.0")},
    mapper = function(data){
      
      y<-FN_Dth_filtercodes(ICD10_codes = ICD10_codes,return_label = 'dth_date', record_level=record_level, exclude=exclude)(data)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Non BrCa death date',
    description = 'Death date caused by non BrCa from Death Registry data'
  )
}

# Other cause dth code
TEU_Dth_NotBrCa_dthcause <-function(record_level=FALSE, ICD10_codes, exclude=FALSE){
  list(
    name = 'TEU_Dth_NotBrCa_dthcause',
    source = if(record_level){c("ID")} else {c('ID',"Dth_ICD10Underlying.0.0", "Dth_ICD10Underlying.1.0","Dth_Date.0.0", "Dth_Date.1.0")},
    mapper = function(data){
      
      y<-FN_Dth_filtercodes(ICD10_codes = ICD10_codes,return_label = 'dth_code', record_level=record_level, exclude=exclude)(data)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Non BrCa cause of death',
    description = 'Death caused by non BrCa from Death Registry data'
  )
}

# BrCa censoring date (Based on Death date by non Breast Cancer + Admin censoring date + lost to follow-up + Incident Mastectomy)
BrCa_censordate<-function(){
  list(
    name = 'BrCa_censordate',
    source = c('TEU_Dth_NotBrCa_dthdate','Admin_CensorDate','BaC_LostFUDate','TEU_HES_Mast_incdate'),
    mapper = function(data){
      y<-pmin(data$TEU_Dth_NotBrCa_dthdate,data$Admin_CensorDate,data$BaC_LostFUDate,data$TEU_HES_Mast_incdate,
              na.rm = TRUE)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'BrCa censoring date',
    description = 'Censoring date for BrCa outcome'
  )
}

# BrCa event status (0=censored 1=BrCa event)
TEU_BrCa_status<-function(){
  list(
    name = 'TEU_BrCa_status',
    source = c('BrCa_censordate','BrCaDate_Incident'),
    mapper = function(data){
      # Check if censoring date has NA
      if (anyNA(data$BrCa_censordate)==TRUE){
        warning('Missing Censoring Date: Need to double check!')
      }
      data<-data%>%
        mutate(status=case_when(
          !is.na(BrCaDate_Incident) & BrCaDate_Incident<=BrCa_censordate ~ 1,
          is.na(BrCaDate_Incident) |(!is.na(BrCaDate_Incident)&BrCaDate_Incident>BrCa_censordate) ~ 0))
      
      return(data$status)
      
    },
    post_exclusion = FALSE,
    display_name = 'BrCa event status',
    description = 'Event status of BrCa (0=censored, 1=BrCa event)'
    
  )
}

# BrCa follow-up time
TEU_BrCa_time<-function(){
  list(
    name = 'TEU_BrCa_time',
    source = c('TEU_BrCa_status','BrCa_censordate','BrCaDate_Incident','Rec_DateAssess'),
    mapper = function(data){
      
      data=data %>%
        mutate(
          time=case_when(
            TEU_BrCa_status==0 ~ as.numeric(difftime(BrCa_censordate, Rec_DateAssess, unit='days'))/365.25,
            TEU_BrCa_status==1 ~ as.numeric(difftime(BrCaDate_Incident, Rec_DateAssess, unit='days'))/365.25
            )
          ) %>%
        mutate(
          time=ifelse(time < 0, 0, time)
        )
      
      return(data$time)
      
    },
    post_exclusion = FALSE,
    display_name = 'BrCa follow up time',
    description = 'If event status=0, this fields returns time difference in days between censoring date and baseline date.
    If event status=1, this fields returns time to BrCa event.'
  )
}

# BrCa follow-up age
TEU_BrCa_age<-function(){
  list(
    name = 'TEU_BrCa_age',
    source = c('TEU_BaC_AgeAtRec','TEU_BrCa_time'),
    mapper = function(data){
      data$TEU_BaC_AgeAtRec+data$TEU_BrCa_time
    },
    post_exclusion = FALSE,
    display_name = 'BrCa follow up age',
    description = 'If event status=0, this fields returns age in years between censoring date and baseline date.
    If event status=1, this fields returns age at BrCa event.'
  )
}

# 1. HES source 
TEU_HES_BrCa_inc<-function(record_level=FALSE){
  list(
    name='TEU_HES_BrCa_inc',
    source=if(record_level){
      c("ID","Rec_DateAssess", "Admin_CaR_CensorDate")
    } else {
      c("ID", "Rec_DateAssess", "Admin_CaR_CensorDate", 
        paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
        paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
        paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
        paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1))#,
        #paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
        #paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1))
        )
    },
    mapper=function(data){
      # Only interested in HES dx after cancer registry censoring
      data <- data %>% mutate(Rec_DateAssess = Admin_CaR_CensorDate)
      y <- FN_HES_First(
        ICD9_xlsx = file.path(config$cleaning$mapping,'HES_ICD9_Mapping_20210707.xlsx'),
        ICD10_xlsx = file.path(config$cleaning$mapping,'HES_ICD10_Mapping_20210707.xlsx'),
        #OPCS4_xlsx = file.path(config$cleaning$mapping,'HES_OPCS4_Mapping_20210707.xlsx'),
        condition = 'BrCa',
        return_label = 'followup',
        record_level = record_level)(data)
      return(y)
      },
    post_exclusion=FALSE,
    display_name='Breast cancer in HES',
    description='Breast cancer status identified from HES (ICD-9, ICD-10) data after baseline'
  )
}

TEU_HES_BrCa_incdate<-function(record_level=FALSE){
  list(
    name='TEU_HES_BrCa_incdate',
    source=if(record_level){
      c("ID","Rec_DateAssess", "Admin_CaR_CensorDate")
    } else {
      c("ID", "Rec_DateAssess", "Admin_CaR_CensorDate",
        paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
        paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
        paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
        paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1))#,
        #paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
        #paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1))
        )
    },
    mapper=function(data){
      # Only interested in HES dx after cancer registry censoring
      data <- data %>% mutate(Rec_DateAssess = Admin_CaR_CensorDate)
      y <- FN_HES_First(
        ICD9_xlsx = file.path(config$cleaning$mapping,'HES_ICD9_Mapping_20210707.xlsx'),
        ICD10_xlsx = file.path(config$cleaning$mapping,'HES_ICD10_Mapping_20210707.xlsx'),
        #OPCS4_xlsx = file.path(config$cleaning$mapping,'HES_OPCS4_Mapping_20210707.xlsx'),
        condition = 'BrCa',
        return_label = 'followup_date',
        record_level = record_level)(data)
      return(y)
      },
    post_exclusion=FALSE,
    display_name='Date of breast cancer in HES',
    description='Date of breast cancer identified from HES (ICD-9, ICD-10) data after baseline'
  )
}


# Mastectomy

TEU_Mastectomy<-function(record_level=FALSE){
  list(
    list(
      name='TEU_HES_Mast_prev',
      source=if(record_level){
        c("ID","Rec_DateAssess")
      } else {
        c("ID", "Rec_DateAssess",
          #paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
          #paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
          #paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
          #paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1)),
          paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
          paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1)))
      },
      mapper=FN_HES_First(#ICD9_xlsx = file.path(config$cleaning$mapping,'HES_ICD9_Mapping_20210128.xlsx'),
        #ICD10_xlsx = file.path(config$cleaning$mapping,'HES_ICD10_Mapping_20210707.xlsx'),
        OPCS4_xlsx = file.path(config$cleaning$mapping,'HES_OPCS4_Mapping_20210707.xlsx'),
        condition = 'BrCa',
        return_label = 'baseline',
        record_level = record_level),
      post_exclusion=FALSE,
      display_name='Mastectomy status identified from HES data prior to or at baseline',
      description='Mastectomy status identified from HES (OPCS-4) data prior to or at baseline'
    ),
    list(
      name='TEU_HES_Mast_prevtype',
      source=if(record_level){
        c("ID","Rec_DateAssess")
      } else {
        c("ID", "Rec_DateAssess",
          #paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
          #paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
          #paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
          #paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1)),
          paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
          paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1)))
      },
      mapper=FN_HES_First(#ICD9_xlsx = file.path(config$cleaning$mapping,'HES_ICD9_Mapping_20210128.xlsx'),
        #ICD10_xlsx = file.path(config$cleaning$mapping,'HES_ICD10_Mapping_20210707.xlsx'),
        OPCS4_xlsx = file.path(config$cleaning$mapping,'HES_OPCS4_Mapping_20210707.xlsx'),
        condition = 'BrCa',
        return_label = 'baseline_comp',
        record_level = record_level),
      post_exclusion=FALSE,
      display_name='Mastectomy status identified from HES data prior to or at baseline',
      description='Mastectomy status identified from HES (OPCS-4) data prior to or at baseline'
    ),
    list(
      name='TEU_HES_Mast_prevdate',
      source=if(record_level){
        c("ID","Rec_DateAssess")
      } else {
        c("ID", "Rec_DateAssess",
          #paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
          #paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
          #paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
          #paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1)),
          paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
          paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1)))
      },
      mapper=FN_HES_First(#ICD9_xlsx = file.path(config$cleaning$mapping,'HES_ICD9_Mapping_20210128.xlsx'),
        #ICD10_xlsx = file.path(config$cleaning$mapping,'HES_ICD10_Mapping_20210707.xlsx'),
        OPCS4_xlsx = file.path(config$cleaning$mapping,'HES_OPCS4_Mapping_20210707.xlsx'),
        condition = 'BrCa',
        return_label = 'baseline_date',
        record_level = record_level),
      post_exclusion=FALSE,
      display_name='Mastectomy status identified from HES data prior to or at baseline',
      description='Mastectomy status identified from HES (OPCS-4) data prior to or at baseline'
    ),
    list(
      name='TEU_HES_Mast_inc',
      source=if(record_level){
        c("ID","Rec_DateAssess")
      } else {
        c("ID", "Rec_DateAssess",
          #paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
          #paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
          #paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
          #paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1)),
          paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
          paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1)))
      },
      mapper=FN_HES_First(#ICD9_xlsx = file.path(config$cleaning$mapping,'HES_ICD9_Mapping_20210128.xlsx'),
        #ICD10_xlsx = file.path(config$cleaning$mapping,'HES_ICD10_Mapping_20210707.xlsx'),
        OPCS4_xlsx = file.path(config$cleaning$mapping,'HES_OPCS4_Mapping_20210707.xlsx'),
        condition = 'BrCa',
        return_label = 'followup',
        record_level = record_level),
      post_exclusion=FALSE,
      display_name='Mastectomy status identified from HES data after baseline',
      description='Mastectomy status identified from HES (OPCS-4) data after baseline'
    ),
    list(
      name='TEU_HES_Mast_incdate',
      source=if(record_level){
        c("ID","Rec_DateAssess")
      } else {
        c("ID", "Rec_DateAssess",
          #paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
          #paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
          #paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
          #paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1)),
          paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
          paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1)))
      },
      mapper=FN_HES_First(#ICD9_xlsx = file.path(config$cleaning$mapping,'HES_ICD9_Mapping_20210128.xlsx'),
        #ICD10_xlsx = file.path(config$cleaning$mapping,'HES_ICD10_Mapping_20210707.xlsx'),
        OPCS4_xlsx = file.path(config$cleaning$mapping,'HES_OPCS4_Mapping_20210707.xlsx'),
        condition = 'BrCa',
        return_label = 'followup_date',
        record_level = record_level),
      post_exclusion=FALSE,
      display_name='Mastectomy date from HES data after baseline',
      description='Mastectomy date from HES (OPCS-4) data after baseline'
    ),
    list(
      name='TEU_HES_Mast_inctype',
      source=if(record_level){
        c("ID","Rec_DateAssess")
      } else {
        c("ID", "Rec_DateAssess",
          #paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
          #paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
          #paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
          #paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1)),
          paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
          paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1)))
      },
      mapper=FN_HES_First(#ICD9_xlsx = file.path(config$cleaning$mapping,'HES_ICD9_Mapping_20210128.xlsx'),
        #ICD10_xlsx = file.path(config$cleaning$mapping,'HES_ICD10_Mapping_20210707.xlsx'),
        OPCS4_xlsx = file.path(config$cleaning$mapping,'HES_OPCS4_Mapping_20210707.xlsx'),
        condition = 'BrCa',
        return_label = 'followup_comp',
        record_level = record_level),
      post_exclusion=FALSE,
      display_name='Mastectomy type from HES data after baseline',
      description='Mastectomy type from HES (OPCS-4) data after baseline'
    )
  )
}


# XL: Added Admin_HES_CensorDate at the back because we are using operation data from HES.
Admin_CensorDate_BrCaHES <- function(){
  list(
    name = 'Admin_CensorDate',
    source = c("Admin_HES_CensorDate", "Admin_Dth_CensorDate", "Admin_CaR_CensorDate"),
    mapper = function(data){
      y <- pmin(data$Admin_Dth_CensorDate, pmax(data$Admin_CaR_CensorDate, data$Admin_HES_CensorDate),data$Admin_HES_CensorDate)
      return(y)
    },
    post_exclusion = FALSE,
    display_name = 'Administrative censoring date',
    description = 'Administrative censoring date, taken as the minimum of (death censoring and the maximum of (cancer registry and HES censoring))'
  )
}


TEU_HES_prev<-function(record_level=TRUE,length="Full"){
  # Set length as an argument so that one don't have to run the whole list for testing! 
  # E.g. one can specify length=2 so that it will return the indicator of first 2 CaR code separately.
  ICD10<-read_excel(file.path(config$cleaning$mapping,"L2_ICD10_Mapping_20211013.xlsx"), col_types=c('text'))
  
  list<-unique(ICD10$Conditions)
  
  #Remove special categories 
  remove<-grepl(paste(paste0("^",c("U","V","W","X","Y","Z")),collapse = "|"),list)
  
  list<-as.list(list[!remove]) 
  
  # Make labels for each category
  names(list)<-sapply(1:length(list),function(i) substr(list[[i]],1,3))
  
  # Obtain level 2 cancer ICD10 codes 
  CaR_codes<-ICD10%>%
    filter(L0 %in%c("Chapter II Neoplasms","Chapter XV Pregnancy, childbirth and the puerperium"))%>%
    pull(Code)%>%
    substr(.,1,3)%>%
    unique
  
  ### For HES dx
  HES_list<-list;HES_list[which(names(HES_list)%in%CaR_codes)]<-NULL
  
  if(length=="Full"){
    length=length(HES_list)
  }
  
  lapply(1:length, function(i)
    
  list(
    name=paste0('TEU_HES_',names(HES_list[i]),'_prev'),
    source=if(record_level){
      c("ID","Rec_DateAssess")
    } else {
      c("ID", "Rec_DateAssess",
        paste0("HES_ICD9Diag.0.", seq(0, 46, by=1)),
        paste0("HES_ICD9DateFirst.0.",seq(0,46,by=1)),
        paste0("HES_ICD10Diag.0.", seq(0, 212, by=1)),
        paste0("HES_ICD10DateFirst.0.",seq(0,212,by=1)),
        paste0("HES_OPCS4Code.0.",seq(0,116,by=1)),
        paste0("HES_OPCS4DateFirst.0.",seq(0,116,by=1)))
    },
    mapper=FN_HES_First(#ICD9_xlsx = file.path(config$cleaning$mapping,'MACE/HES_ICD9_Mapping_20210128.xlsx'),
                        ICD10_xlsx = file.path(config$cleaning$mapping,'L2_ICD10_Mapping_20211013.xlsx'),
                        #OPCS4_xlsx = file.path(config$cleaning$mapping,'MACE/HES_OPCS4_Mapping_20210128.xlsx'),
                        condition = HES_list[[i]],
                        return_label = 'baseline',
                        record_level = record_level),
    post_exclusion=FALSE,
    display_name=paste0('Prevalent ',HES_list[[i]],' status at baseline'),
    description=paste0(HES_list[[i]],' status identified from HES (ICD-10) data prior to or at baseline')
  )
  )
}

TEU_CaR_prev <- function(length="Full") {
  # Set length as an argument so that one don't have to run the whole list for testing! 
  # E.g. one can specify length=2 so that it will return the indicator of first 2 CaR code separately.
  ICD10<-read_excel(file.path(config$cleaning$mapping,"L2_ICD10_Mapping_20211013.xlsx"), col_types=c('text'))
  
  list<-unique(ICD10$Conditions)
  
  #Remove special categories 
  remove<-grepl(paste(paste0("^",c("U","V","W","X","Y","Z")),collapse = "|"),list)
  
  list<-as.list(list[!remove]) 
  
  # Make labels for each category
  names(list)<-sapply(1:length(list),function(i) substr(list[[i]],1,3))
  
  ### For CaR dx
  
  # Obtain level 2 cancer ICD10 codes 
  CaR_codes<-ICD10%>%
    filter(L0 %in%c("Chapter II Neoplasms","Chapter XV Pregnancy, childbirth and the puerperium"))%>%
    pull(Code)%>%
    substr(.,1,3)%>%
    unique
  
  CaR_list<-list[CaR_codes]
  ##
  if(length=="Full"){
    length=length(CaR_list)
  }
  
  lapply(1:length, function(i)
    
    list(
      name = paste0("TEU_CaR_",names(CaR_list[i]),"_prev"),
      source = c("ID", "Rec_DateAssess",
                 paste0("CaR_DiagDate.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD10.", seq(0, 16, by=1), ".0"),
                 paste0("CaR_DiagICD9.", seq(0, 14, by=1), ".0")),
      mapper = function(data){
        
        dx<-FN_Cancer(colgroup="CaR_", not_na="DiagICD", 
                      code_list=list(ICD10=names(CaR_list[i]), ICD9=""), type="prevalent", keepcol="DiagICD10")(data)
        
        # Create indicator
        y <- ifelse(is.na(dx),0,1)
        
        return(y)
      },
      post_exclusion = FALSE,
      display_name = paste0("Prevalent ",CaR_list[[i]]," status at baseline"),
      description = paste0(CaR_list[[i]],' status identified from Cancer Registry (ICD-10) data prior to or at baseline')
    )
    
    )
  
}

TEU_OC<-function(){
  list(
    list(
      name = 'TEU_OC_Use',
      source = c("FSF_OCPillEver.0.0"),
      mapper = FN_labelfactor(),
      post_exclusion = FALSE,
      display_name = 'Oral contraceptive use',
      description = 'Self-reported oral contraceptive use at baseline'
    ),
    list(
      name = 'TEU_OC_Years',
      source = c("FSF_OCPillEver.0.0","FSF_OCPillAgeStart.0.0","FSF_OCPillAgeLast.0.0","TEU_BaC_AgeAtRec"),
      mapper = OC_HRT(return_label="Years"),
      post_exclusion = FALSE,
      display_name = 'Years of using Oral contraceptive',
      description = 'Self-reported years of using oral contraceptive till baseline'
    )
    
  )
}


TEU_HRT<-function(){
  list(
    list(
      name = 'TEU_HRT_Use',
      source = c("FSF_HRTEver.0.0"),
      mapper = FN_labelfactor(),
      post_exclusion = FALSE,
      display_name = 'HRT use',
      description = 'Self-reported hormone-replacement therapy use'
    ),
    list(
      name = 'TEU_HRT_Years',
      source = c("FSF_HRTEver.0.0","FSF_HRTAgeStart.0.0","FSF_HRTAgeLast.0.0","TEU_BaC_AgeAtRec"),
      mapper = OC_HRT(return_label="Years"),
      post_exclusion = FALSE,
      display_name = 'Years of using HRT',
      description = 'Self-reported years of using hormone-replacement therapy'
    )
    
  )
}

# Age at birth

TEU_BirthAge<-function(){
  list(
    list(
      name = 'TEU_FirstBirthAge',
      source = c("FSF_FirstLiveBirthAge.0.0","FSF_FirstBirthAge.0.0"),
      mapper = function(data) {
        y<-coalesce(data[["FSF_FirstLiveBirthAge.0.0"]], data[["FSF_FirstBirthAge.0.0"]])
        y[y%in%c(-3,-4)]=NA
        return(y)
      },
      post_exclusion = FALSE,
      display_name = 'Age at first birth',
      description = 'Self-reported first birth age'
    ),
    list(
      name = 'TEU_LastBirthAge',
      source = c("FSF_LastLiveBirthAge.0.0","FSF_FirstBirthAge.0.0"),
      mapper = function(data) {
        y<-coalesce(data[["FSF_LastLiveBirthAge.0.0"]], data[["FSF_FirstBirthAge.0.0"]])
        y[y%in%c(-3,-4)]=NA
        return(y)
      },
      post_exclusion = FALSE,
      display_name = 'Age at last birth',
      description = 'Self-reported last birth age'
    )
    
  )
}


# General function for extracting Early life reproductive vars
ELR <- function() {
  renaming<-read.csv(file.path(config$cleaning$renaming))
  ## Female specific
  FSF_list<-vars_to_list(renaming=renaming,name_pattern="^FSF",extra_vars=c(
    "FSF_ProgOnlyOCPillType_p",
    "FSF_OCPillEver","FSF_OCPillAgeStart","FSF_OCPillAgeLast",
    "FSF_HRTEver","FSF_HRTAgeStart","FSF_HRTAgeLast",
    "FSF_FirstLiveBirthAge","FSF_LastLiveBirthAge","FSF_FirstBirthAge","FSF_Menopause"
  ))
  
  ## Early life factors
  ELF_list<-vars_to_list(renaming=renaming,name_pattern="^ELF",extra_vars=NULL)
  
  ## Total
  ELR_list<-c(FSF_list,ELF_list)
  
  lapply(1:length(ELR_list), function(i)
    
    list(
      name = ELR_list[[i]], 
      source = c(paste0(ELR_list[[i]],".0.0")), 
      mapper = function(x){
        if(is.numeric(x)){
          x[x==-10]=0
          FN_toNA(values=c(-6,-1,-3,-2,-4))(x)
        }else{FN_labelfactor()(x)}
      },
      post_exclusion = FALSE,
      display_name = names(ELR_list[i]),
      description = names(ELR_list[i])
    )
  )
}

FSF_Menopause <- function(){
  list(
    name = 'FSF_Menopause',
    source = "FSF_Menopause.0.0",
    mapper = FN_id,
    post_exclusion = FALSE,
    display_name = 'Had menopause',
    description = 'Had menopause'
  )
}



