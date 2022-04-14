# Jennifer Collister
# 30/03/2020
# Create functions 
library(data.table)
library(tools)
library(readxl)

# Load the project config file for filepaths etc
if (!exists("config")) {
  library(yaml)
  config = yaml.load_file("config.yml")
}

read.csv_kdrive <- function(filename, maxErrors=2, sleep=20) {
  attempts <- 0
  retval <- NULL
  
  while(is.null(retval) && attempts < maxErrors) {
    if(attempts > 0) {Sys.sleep(sleep)}
    attempts <- attempts + 1
    retval <- tryCatch(suppressWarnings(
      read.csv(filename)),
      error = function(cond) {
        message(paste0("Error message: ", cond))
        if(attempts < maxErrors) {message(paste0("Trying again in ", sleep, " seconds..."))}
        return(NULL)
      }
    )
  }
  if(is.null(retval)){
    message("Failed to read file. Either it does not exist (typo?) or you are currently disconnected from the network drives")
  } else if (attempts>1) {
    message("Succeeded!")
  }
  
  return(retval)
}

read.xlsx_kdrive <- function(filename,col_types=NULL, sheet=NULL, maxErrors=2, sleep=20) {
  attempts <- 0
  retval <- NULL
  
  while(is.null(retval) && attempts < maxErrors) {
    if(attempts > 0) {Sys.sleep(sleep)}
    attempts <- attempts + 1
    retval <- tryCatch(suppressWarnings(
      read_excel(filename,col_types = col_types, sheet = sheet)),
      error = function(cond) {
        message(paste0("Error message: ", cond))
        if(attempts < maxErrors) {message(paste0("Trying again in ", sleep, " seconds..."))}
        return(NULL)
      }
    )
  }
  if(is.null(retval)){
    message("Failed to read file. Either it does not exist (typo?) or you are currently disconnected from the network drives")
  } else if (attempts>1) {
    message("Succeeded!")
  }
  
  return(retval)
}

cache.file <- function(filename, FUN, ...) {
  file <- evalWithMemoization(
    FUN(filename, ...),
    key = c(filename, file.info(filename)$mtime)
  )
}
# Formatting of existing UKB variables

FN_id <- function(x){x}

FN_unorder <- function(x){factor(x, ordered=FALSE)}

FN_factor <- function(levelorder, ordered=FALSE){
  function(x){
    factor(x, levels=levelorder, ordered=ordered)
  }
}

FN_toNumeric <- function(x) {
  as.numeric(x)
}

FN_toDate <- function(x){
  as.Date(x, origin=as.Date("1970-01-01"))
}

# Make certain values (e.g. -1,-3) as NA 
FN_toNA <- function(values=c(-1,-3)){
  function(x){
    x[x%in%values]=NA
    x
  }
}


# Derived variables

FN_labelfactor <- function(levels=NULL, labels=NULL, recodeNA=c("Prefer not to answer","Do not know")){
  function(x){
    
    if(!is.null(recodeNA)){
      
      x[x%in%recodeNA]=NA
      
    }
    
    if(is.null(levels)&is.null(labels)){
      # If both null, set to default, which is the same setting as factor()
      levels<-sort(unique(x))
      labels<-levels
    }
    
    y <- factor(x, levels=levels, labels=labels)
    
    return(y)
  }
}

FN_average <- function(colnames, na.rm=TRUE){
  function(data){
    rowMeans(data[,colnames], na.rm)
  }
}

FN_MYtoDate <- function(day, monthField, yearField, format="%d%B%Y"){
  function(data){
    as.Date(paste0(as.character(day), as.character(data[[monthField]]), as.character(data[[yearField]])), format)
  }
}


FN_buckets <- function(breaks, labels=NULL, right=TRUE){
  function(x){
    cut(x, breaks=breaks, labels=labels, right=right)
  }
}

FN_quantiles <- function(quant=4, labels=NULL, na.rm=TRUE){
  function(x){
    # if(anyNA(x)){warning("This vector contains NA values")}
    quantiles <- quantile(x, probs=seq(0, 1, 1/quant), na.rm=na.rm)
    if(is.null(labels)){
      labels <- c("Q1: lowest", paste0("Q", seq(2, quant-1)), paste0("Q", quant, ": highest"))
      }
    test <- cut(x, breaks=quantiles, labels=labels, right=TRUE, include.lowest=TRUE)
  }
}

FN_FamHist <- function(conditions, label){
  function(data){
    y <- apply(data[,c(grep("FaH_FatherIll.0.", colnames(data), fixed=TRUE),
                       grep("FaH_MotherIll.0.", colnames(data), fixed=TRUE),
                       grep("FaH_SibIll.0.", colnames(data), fixed=TRUE)
    )
    ], 1, function(x) any(x %in% conditions)
    )
    y <- factor(as.numeric(y), levels=c(0,1), 
                labels=c(paste0("No family history of ", label), paste0("Family history of ", label)))
    return(y)
  }
}

FN_HMHmeds_any <- function(data){
  # Combine the first medication field across males and females
  medcombine <- coalesce(data[["HMH_MedCholBPDiabHorm.0.0"]], data[["HMH_MedCholBPDiab.0.0"]])
  # Create a new medication variable: yes/no/do not know/prefer not to answer/NA
  medlist <- c("Cholesterol lowering medication",
               "Blood pressure medication",
               "Oral contraceptive pill or minipill",
               "Hormone replacement therapy",
               "Insulin"
               )
  y <- dplyr::case_when(
    is.na(medcombine) ~ "Unanswered",
    medcombine == "None of the above" ~ "No",
    medcombine == "Do not know" ~ "Do not know",
    medcombine == "Prefer not to answer" ~ "Prefer not to answer",
    medcombine %in% medlist ~ "Yes",
    TRUE ~ "Unexpected answer"
  )
  y <- factor(y, levels = c("Yes", "No", "Do not know", "Prefer not to answer", "Unanswered"))
  return(y)
}

# XL add: 17/11/2020
FN_HMHmeds_any_raw <- function(data){
  # Combine the first medication field across males and females
  medcombine <- coalesce(data[["HMH_MedCholBPDiabHorm.0.0"]], data[["HMH_MedCholBPDiab.0.0"]])
  # Create a new medication variable: yes/no/do not know/prefer not to answer/NA
  medlist <- c("Cholesterol lowering medication",
               "Blood pressure medication",
               "Oral contraceptive pill or minipill",
               "Hormone replacement therapy",
               "Insulin"
  )
  y <- dplyr::case_when(
    is.na(medcombine) ~ NA_character_,
    medcombine == "None of the above" ~ "No",
    medcombine == "Do not know" ~ "Do not know",
    medcombine == "Prefer not to answer" ~ "Prefer not to answer",
    medcombine %in% medlist ~ "Yes",
    TRUE ~ "Unexpected answer"
  )
  y <- factor(y, levels = c("Yes", "No", "Do not know", "Prefer not to answer"))
  return(y)
}


FN_HMHmeds_type <- function(medtype, string){
  function(data){
    x <- FN_HMHmeds_any(data)
    if(anyNA(x)){
      stop("Unexpected value in source data for self-reported medication.")
    }
    
    # Now check for the requested medication across the columns
    y <- apply(data[,c(grep("HMH_MedCholBPDiab.0.", colnames(data), fixed=TRUE),
                       grep("HMH_MedCholBPDiabHorm.0.", colnames(data), fixed=TRUE))], 1, function(x) any(x==medtype))
    # And incorporate the info on whether this participant is taking any other medication
    y[x %in% c("Prefer not to answer", "Do not know", "Unanswered")] <- "Unanswered"
    y[is.na(y)] <- "FALSE"
    y <- factor(y, levels=c("TRUE", "FALSE", "Unanswered"), 
                labels=c(paste0("Self-reported ", string), paste0("Did not report ", string), "Unanswered"))
    return(y)
  }
}

FN_Vascular_any <- function(data) {
  # Combine the vascular condition columns
  vcon <- coalesce(data[["HMH_HeartProbs.0.0"]], data[["HMH_HeartProbs.0.1"]], 
                   data[["HMH_HeartProbs.0.2"]], data[["HMH_HeartProbs.0.3"]])
  # Create a new medication variable: yes/no/do not know/prefer not to answer/NA
  condlist <- c("High blood pressure", "Stroke", "Angina", "Heart attack")
  y <- dplyr::case_when(
    is.na(vcon) ~ "Unanswered",
    vcon == "None of the above" ~ "No",
    vcon == "Do not know" ~ "Do not know",
    vcon == "Prefer not to answer" ~ "Prefer not to answer",
    vcon %in% condlist ~ "Yes",
    TRUE ~ "Unexpected answer"
  )
  y <- factor(y, levels = c("Yes", "No", "Do not know", "Prefer not to answer", "Unanswered"))
  return(y)
}

# XL add: Same as above except not assigning 'Unanswered' to NA
FN_Vascular_any_raw <- function(data) {
  # Combine the vascular condition columns
  vcon <- coalesce(data[["HMH_HeartProbs.0.0"]], data[["HMH_HeartProbs.0.1"]], 
                   data[["HMH_HeartProbs.0.2"]], data[["HMH_HeartProbs.0.3"]])
  # Create a new medication variable: yes/no/do not know/prefer not to answer/NA
  condlist <- c("High blood pressure", "Stroke", "Angina", "Heart attack")
  y <- dplyr::case_when(
    is.na(vcon) ~ NA_character_,
    vcon == "None of the above" ~ "No",
    vcon == "Do not know" ~ "Do not know",
    vcon == "Prefer not to answer" ~ "Prefer not to answer",
    vcon %in% condlist ~ "Yes",
    TRUE ~ "Unexpected answer"
  )
  y <- factor(y, levels = c("Yes", "No", "Do not know", "Prefer not to answer"))
  return(y)
}


FN_Vascular_condition <- function(conditions, string) {
  function(data){
    x <- FN_Vascular_any(data)
    if(anyNA(x)){
      stop("Unexpected value in source data for self-reported medication.")
    }
    
    # Now check for the requested condition across the columns
    y <- apply(data[,c("HMH_HeartProbs.0.0", "HMH_HeartProbs.0.1", "HMH_HeartProbs.0.2", "HMH_HeartProbs.0.3")],
               1, function(x) any(x %in% conditions))
    # And incorporate the info on whether this participant reported any condition
    y[x %in% c("Prefer not to answer", "Do not know", "Unanswered")] <- "Unanswered"
    y[is.na(y)] <- "FALSE"
    y <- factor(y, levels=c("TRUE", "FALSE", "Unanswered"), 
                labels=c(paste0("Self-reported ", string), paste0("Did not report ", string), "Unanswered"))
    return(y)
  }
}

FN_MissingCategory <- function(missingvals, categ_name){
  function(x){
    # XL add: Need to assign variable as factor format first 
    x<-factor(x)
    # Categorise missing data - change levels so specified levels and NA are both "Unanswered"
    labels <- c(levels(x)[-which(levels(x) %in% missingvals)], categ_name)
    y <- as.character(x)
    y[y %in% missingvals] <- categ_name
    y[is.na(y)] <- categ_name
    y <- factor(y, levels=labels, ordered=FALSE)
    return(y)
  }
}

FN_JoinPRS <- function(filepath, colname) {
  function(x) {
    if(file_ext(filepath)=="rds"){
      prs <- readRDS(filepath)
    } else if(file_ext(filepath)=="sscore"){
      prs <- read.delim(filepath, header=TRUE) %>%
        rename(ID = IID)
    } else {
      warning("Unidentified file type for PRS")
    }
    y <- prs[[colname]][match(x, prs$ID)]
    return(y)
  }
}

FN_Cancer <- function(colgroup="CaR_", not_na="DiagICD", code_list, exclude=FALSE, type="all", keepcol) {
  function(data) {
    long <- data %>% pivot_longer(
      cols = starts_with(colgroup),
      names_to = c(".value", "instance"),
      names_pattern = paste0(colgroup, "(.*)\\.(.*).0")
    ) %>%
      filter_at(vars(starts_with(not_na)),any_vars(!is.na(.)))
    
    filtered <- long %>%
      # Keep only cancer of interest
      {
        if(exclude==FALSE){
          filter(., substr(DiagICD10, 1, nchar(code_list$ICD10[1])) %in% code_list$ICD10 | 
                   substr(DiagICD9, 1, nchar(code_list$ICD9[1])) %in% code_list$ICD9)
        } else {
          filter(., !substr(DiagICD10, 1, nchar(code_list$ICD10[1])) %in% code_list$ICD10 & 
                   !substr(DiagICD9, 1, nchar(code_list$ICD9[1])) %in% code_list$ICD9)
        }
      } %>%
      # Convert date field to date type
      mutate(DiagDate = FN_toDate(DiagDate)) %>%
      # Keep first diagnosis per individual, prefer ICD10 diagnoses over ICD9
      group_by(ID) %>%
      arrange(DiagDate, DiagICD10, .by_group = TRUE) %>%
      filter(row_number()==1) %>%
      # Restrict to prevalent/incident cases
      {
        if(type=="all"){
          .
        } else if (type=="prevalent"){
          filter(., DiagDate <= Rec_DateAssess)
        } else if (type=="incident"){
          filter(., DiagDate > Rec_DateAssess)
        }
      } 
    
    y <- filtered[[keepcol]][match(data$ID, filtered$ID)]
    
    return(y)
  }
}

FN_VItoLong <- function(data, colname, instance, mapper) {
  
  if(strsplit(mapper, "\\.")[[1]][2]=="csv"){
    mapper_file <- read.csv_kdrive(mapper)
  } else if(strsplit(mapper, "\\.")[[1]][2]=="xlsx") {
    mapper_file <- read.xlsx_kdrive(mapper)
  }
  
  if (is.null(instance)) {
    pattern <- paste0(colname, "(.*)\\.(.*)")
  } else {
    pattern <- paste0(colname, "(.*)\\.", instance, "\\.(.*)")
  }

  long <- evalWithMemoization(
    data %>% pivot_longer(
      cols = starts_with(colname),
      names_to = c(".value", "measure"),
      names_pattern = pattern
    ) %>%
      drop_na(starts_with(colname)) %>%
      mutate(Code = as.numeric(Code)) %>%
      left_join(x = ., y = mapper_file, by= "Code"),
    key = c(data, colname, instance, mapper_file)
  )
}

# XL: Below is for filtering VI diagnoses codes
FN_VI_filtercodes <- function(dx_codes, colname, instance = 0, return_label = "dx", mapper) {
  function(data) {
    
    if(strsplit(mapper, "\\.")[[1]][2]=="csv"){
      mapper_file <- read.csv_kdrive(mapper)
    } else if(strsplit(mapper, "\\.")[[1]][2]=="xlsx") {
      mapper_file <- read.xlsx_kdrive(mapper)
    }
    
    
    long_dx <- evalWithMemoization(
      FN_VItoLong(
        data,
        colname = colname,
        instance = instance,
        mapper = mapper
      ) %>%
        filter(Code %in% dx_codes) %>%
        group_by(ID) %>%
        arrange(Year) %>%
        slice_head %>%
        mutate(dx = factor(meaning),
               Year = ifelse(Year %in% c(-1, -3), NA, Year),
               Year_date = date_decimal(Year),
               duration = as.numeric(round(
                 difftime(Rec_DateAssess, Year_date, unit = "days") / 365.25,
                 digits = 2
               ))),
      key = c(data, dx_codes, instance, mapper_file)
    )
    
    y <- long_dx[[return_label]][match(data$ID, long_dx$ID)]
    return(y)
  }
}

#XL add: 19/11/2020 Below is for filtering VI medication codes
# Can produce meds taken status and number of meds taken in that category
FN_VImed_filtercodes <- function(med_codes, med_name= 'statin', colname, instance = 0, return_label, mapper) {
  function(data) {
    
    mapper_file <- read.csv_kdrive(mapper)

    long_med <- evalWithMemoization(
      FN_VItoLong(
        data,
        colname = colname,
        instance = instance,
        mapper = mapper
      ) %>%
        filter(Code %in% med_codes)%>%
        # Meds taken status
        mutate(!!med_name:=1)%>%
        # Number of meds taken in that category
        add_count(ID)%>%
        rename(!!paste0(med_name,'_num'):=n)%>%
        #remove duplicate ID
        distinct(ID,.keep_all = TRUE)
      ,
      key = c(data, med_codes, instance, mapper_file)
    )
    
    y <- long_med[[return_label]][match(data$ID, long_med$ID)]
    y[is.na(y)] <- 0
    
    # If want to return med status, transfer to factor with level No/Yes; If want to return number of meds, transfer to factor format
    if(return_label==med_name){
      y<-factor(y,levels = c(0,1),labels = c('No','Yes'))
    } else if(return_label==paste0(med_name,'_num')){
      y<-factor(y)
    }
    
    return(y)
  }
}

# XL add: based on returned mapping list xlsx filter VI code
FN_VI_comorb<-function(condition,returned_mapping){
  function(data){
    
    # Coding list of interest
    dx_codes<-returned_mapping[which(returned_mapping$Conditions==condition),]$coding
    y<- FN_VI_filtercodes(dx_codes = dx_codes,
                          colname = "VeI_NonCancer",
                          instance = 0,
                          return_label = "dx",
                          mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv"))(data)
    # If not blank, assign yes
    y<- factor(ifelse(is.na(y), 0, 1), levels = c(0,1), labels = c('No','Yes'))
    return(y)
  }
  
}


FN_HES_episodes <- function(episodePath=file.path(config$data$portal$HES, "hesin.txt")) {
                        
  episodes <- evalWithMemoization(
    fread(episodePath,na.strings = "") %>%
      select(eid, ins_index, dsource, epistart, admidate) %>%
      rename(ID = eid) %>%
      mutate(DateFirst = as.Date(coalesce(epistart, admidate), format="%d/%m/%Y")) %>%
      select(-epistart, -admidate),
    key = episodePath
  )
  
  return(episodes)
}

FN_HES_diagnoses <- function(icd=10, HESPath=config$data$portal$HES) {
  
  episodePath=file.path(HESPath, "hesin.txt")
  diagnosisPath=file.path(HESPath, "hesin_diag.txt")
  
  # Not going to spend ages building in loads of tolerance but here's a simple check!
  if(is.character(icd)) {
    icd <- as.numeric(str_remove(tolower(icd), "icd"))
  }
  stopifnot(icd %in% c(9, 10))
  
  codecol <- glue("diag_icd{icd}")
  
  diagnoses <- evalWithMemoization(
    fread(diagnosisPath,na.strings = "") %>%
      rename(ID = eid,
             Code = !!codecol) %>%
      select(ID, ins_index, arr_index, level, Code) %>%
      filter(!is.na(Code)) %>%
      inner_join(FN_HES_episodes(episodePath), by=c("ID", "ins_index")),
    key = c(episodePath, diagnosisPath, icd)
  )
  return(diagnoses)
}

FN_HES_operations <- function(opcs=4, HESPath=config$data$portal$HES) {
  
  episodePath=file.path(HESPath, "hesin.txt")
  operationPath=file.path(HESPath, "hesin_oper.txt")
  
  # Not going to spend ages building in loads of tolerance but here's a simple check!
  if(is.character(opcs)) {
    opcs <- as.numeric(str_remove(tolower(opcs), "opcs"))
  }
  stopifnot(opcs %in% c(3, 4))
  
  codecol <- glue("oper{opcs}")
  
  operations <- evalWithMemoization(
    fread(operationPath,na.strings = "") %>%
      rename(ID = eid,
             Code = !!codecol) %>%
      select(ID, ins_index, arr_index, level, Code) %>%
      filter(!is.na(Code)) %>%
      inner_join(FN_HES_episodes(episodePath), by=c("ID", "ins_index")),
    key = c(episodePath, operationPath, opcs)
  )
  return(operations)
}

FN_Death_registry <- function(deathPath=config$data$portal$deaths) {
  
  recordPath <- file.path(deathPath, "death.txt")
  causePath <- file.path(deathPath, "death_cause.txt")
  
  # A few individuals have multiple differing date records, when 
  # a second death certificate was issued after a post mortem
  # In these cases, we want to take the updated cause of death
  # So we are interested in the greatest instance index per individual
  # See K:/TEU/UKB33952_Data/Data_Portal/Death_Registry/DeathLinkage.pdf
  # Section 2. Data Cleaning
  deaths <- evalWithMemoization(
    fread(recordPath,na.strings = "") %>%
      select(eid, ins_index, dsource, date_of_death) %>%
      rename(ID = eid) %>%
      group_by(ID) %>% 
      top_n(1, ins_index) %>%
      mutate(Dth_Date = as.Date(date_of_death, format="%d/%m/%Y")) %>%
      select(-date_of_death),
    key = recordPath
  )
  
  causes <- evalWithMemoization(
    fread(causePath,na.strings = "") %>% 
      rename(ID = eid,
            Dth_ICD10All = cause_icd10) %>%
      inner_join(deaths, by=c("ID", "ins_index")),
    key = c(causePath, deaths)
  )
  
  return(causes)
}

FN_Death_registry_primary <- function(deathPath=config$data$portal$deaths) {
  
  primary <- FN_Death_registry(deathPath=deathPath) %>%
    filter(arr_index==0) %>%
    rename(Dth_ICD10Underlying = Dth_ICD10All)
  
  return(primary)
}
FN_Death_registry_secondary <- function(deathPath=config$data$portal$deaths) {
  
  secondary <- FN_Death_registry(deathPath=deathPath) %>%
    filter(arr_index>0) %>%
    rename(Dth_ICD10Secondary = Dth_ICD10All)

  return(secondary)
}

FN_HEStoLong <- function(data, colname, removeNAfrom) {
  
  pattern <- paste0(colname, "(.*)\\.0\\.(.*)")
  
  long <- evalWithMemoization({
    data <- data %>% pivot_longer(
      cols = starts_with(colname),
      names_to = c(".value", "measure"),
      names_pattern = pattern
    ) 
    data %>%
      drop_na(!!removeNAfrom) %>%
      select(-measure)
  },
  key = c(data, colname, removeNAfrom))
}


# XL add: 10/12/2020

# First occurrence dataset within each HES source (ICD9 or ICD10 or OPCS4)
# data should contain ID, RecDate and corresponding HES dx and date fields
# Return dataset with ID, RecDate, Dx, DateFirst for first occurrence of condition from each HES source
FN_eachHES_First<-function(data,HES_xlsx,condition='MACE',colname,removeNAfrom, record_level=FALSE){
  
  # Produce code of interest from analysis codings xlsx
  #HES_codes=HES_xlsx[which(HES_xlsx$Conditions==condition),]$Code
  HES_file <- read.xlsx_kdrive(HES_xlsx, col_types=c('text'))
  
  HESPath=config$data$portal$HES
  
  # First occurrence of condition
  sub_long <- evalWithMemoization({
    if(record_level==FALSE){
      HESlong <- FN_HEStoLong(data = data,
                              colname = colname,
                              removeNAfrom = removeNAfrom)
    } else {
      
      HESlong <- switch(colname, 
                        "HES_ICD9" = FN_HES_diagnoses(icd=9, HESPath),
                        "HES_ICD10" = FN_HES_diagnoses(icd=10, HESPath),
                        "HES_OPCS4" = FN_HES_operations(opcs=4, HESPath)) %>%
        left_join(., data, by="ID")
    }
    HESlong %>%
      rename_at(vars(starts_with('Diag')), ~ paste0('Code')) %>%
      # Left join to get 'Conditions' and 'ConditionsType' column
      left_join(x = ., y = HES_file, by = "Code")%>%
      # Filter based on condition of interest
      filter(Conditions==condition)%>%
      group_by(ID) %>%
      # Select the first occurrence
      dplyr::slice(which.min(DateFirst)) %>%
      # Keep essential columns only
      select(any_of(c("ID", "Rec_DateAssess", "ConditionsType_TL", "ConditionsType", "DateFirst")))
  },
  key = c(data, colname, removeNAfrom, HES_file, condition, record_level,HESPath))
}


# First occurrence dataset among all HES source (ICD9 + ICD10 + OPCS4)
# Note: One can specify which source they do not wish to use as NULL (e.g. ICD9_xlsx=NULL)
# * return_label='baseline': condition status at baseline
# * return_label='baseline_comp': condition subtype at baseline
# * return_label='followup': condition status at followup 
# * return_label='followup_date': Date of condition at follow up (used for deriving outcome)
# * return_label='followup_comp': condition subtype at follow up
FN_HES_First<-function(ICD9_xlsx=NULL,ICD10_xlsx=NULL,OPCS4_xlsx=NULL,condition='MACE',return_label='baseline', record_level=FALSE){
  function(data){
    
    HES_total<-NULL
    
    # Get first occurrence dataset from each HES source and rbind together
    if (!is.null(ICD9_xlsx)){
      HES_total<-FN_eachHES_First(data%>%select(ID,'Rec_DateAssess', 
                                                contains("HES_ICD9Diag.0."), 
                                                contains("HES_ICD9DateFirst.0.")),
                                  HES_xlsx = ICD9_xlsx,
                                  condition = condition,
                                  colname = 'HES_ICD9',
                                  removeNAfrom = c('Diag','DateFirst'),
                                  record_level = record_level)
    }
    
    if (!is.null(ICD10_xlsx)){
      HES_total<-HES_total%>%bind_rows(
        FN_eachHES_First(data%>%select(ID,'Rec_DateAssess',
                                       contains("HES_ICD10Diag.0."),
                                       contains("HES_ICD10DateFirst.0.")),
                         HES_xlsx = ICD10_xlsx,
                         condition = condition,
                         colname = 'HES_ICD10',
                         removeNAfrom = c('Diag','DateFirst'),
                         record_level = record_level))
    }
    
    if (!is.null(OPCS4_xlsx)){
      HES_total<-HES_total%>%bind_rows(
        FN_eachHES_First(data%>%select(ID,'Rec_DateAssess',
                                       contains("HES_OPCS4Code.0."),
                                       contains("HES_OPCS4DateFirst.0.")),
                         HES_xlsx = OPCS4_xlsx,
                         condition = condition,
                         colname = 'HES_OPCS4',
                         removeNAfrom = c('Code','DateFirst'),
                         record_level = record_level))
    }
    
    # Select first occurrence among all HES source
    long_dx=HES_total%>%
      group_by(ID)%>%
      # Select the first occurrence
      dplyr::slice(which.min(DateFirst))%>%
      
      mutate(
        # Indicator column for previous dx (prior to or at baseline)
        baseline=ifelse(DateFirst<=Rec_DateAssess,1,0),
        # Date of condition (prior to or at baseline)
        baseline_date=if_else(DateFirst<=Rec_DateAssess,DateFirst,as.Date(NA)),
        # Subtype column (prior to or at baseline)
        baseline_comp=ifelse(DateFirst<=Rec_DateAssess,ConditionsType,NA),
        # Indicator column for dx (Follow-Up)
        followup=ifelse(DateFirst>Rec_DateAssess,1,0),
        # Date of condition (Follow-up)
        followup_date=if_else(DateFirst>Rec_DateAssess,DateFirst,as.Date(NA)),
        # Subtype column (Follow-up)
        followup_comp=ifelse(DateFirst>Rec_DateAssess,ConditionsType,NA)
      )
    
    y <- long_dx[[return_label]][match(data$ID, long_dx$ID)]
    
    if(return_label%in%c('baseline','followup')){
      y[is.na(y)]=0
      y<-factor(y,levels = c(0,1),labels = c('No','Yes'))
    }
    return(y)
    
  }
}

# Filter death registry data based on ICD10 codes
# * return_label='dth': return 'yes' for ones with primary cause of death by specified ICD10 codes  
# * return_label='dth_date': return death date by specified ICD10 codes 
# * return_label='dth_code': return ICD 10 code of cause of death
FN_Dth_filtercodes <- function(ICD10_codes, return_label = "dth", exclude=FALSE, record_level=FALSE) {
  function(data) {
    
    if(record_level){
      ID <- data
      deaths <- FN_Death_registry_primary()
    } else {
      ID <- data$ID
      deaths <- data %>%
        mutate(Dth_ICD10Underlying = coalesce(Dth_ICD10Underlying.1.0, Dth_ICD10Underlying.0.0),
              Dth_Date = coalesce(Dth_Date.1.0, Dth_Date.0.0))
    }

    subdata <- deaths %>% 
      {
        if(exclude==FALSE) {
          filter(., Dth_ICD10Underlying %in% ICD10_codes)
        } else {
          filter(., !Dth_ICD10Underlying %in% ICD10_codes)
        }
      } %>%
      mutate(dth = 'Yes',
             dth_date = Dth_Date,
             dth_code = Dth_ICD10Underlying)

    y <- subdata[[return_label]][match(ID, subdata$ID)]
    return(y)
  }
}

# For preparing list of variables from one category
vars_to_list<-function(renaming=renaming,name_pattern,extra_vars=NULL){
  # renaming: where to specify the renaming csv 
  # name_pattern: The pattern of variable names you are looking for
  # extra_vars: any extra variables you wish to exclude? If so, then specify.
  
  vars<-renaming%>%
    filter(grepl(name_pattern,NewVarName))%>%
    {
      if(!is.null(extra_vars)){
        filter(.,!NewVarName %in% extra_vars)
      }else{.}
    }
  
  # Create list
  list<-as.list(vars$NewVarName)
  names(list)<-vars$Field_Description
  
  return(list)
  
}



OC_HRT<-function(return_label){
  # return_label="Ind" returns the use indicator of OC or HRT (depending on input data you specify)
  # return_label="Years" returns the years of using OC or HRT (depending on input data you specify)
  function(data){
    
    data<-data%>%
      mutate_all(FN_toNA())%>%
      rename_at(vars(ends_with("Ever.0.0")),list(~paste0("Ever")))%>%
      rename_at(vars(ends_with("Start.0.0")),list(~paste0("Start")))%>%
      rename_at(vars(ends_with("Last.0.0")),list(~paste0("Last")))
    
    OC_HRT<-data%>%
      # Status of OC/HRT use
      mutate(Ind=as.factor(case_when(
        Ever=="No" ~ 'Never',
        Ever=='Yes' & !is.na(Last) & Last!=-11 ~ 'F',
        Ever=='Yes' & Last==-11 ~ 'C',
        TRUE ~ NA_character_
      )))%>%
      # Years of OC/HRT use
      mutate(Years=case_when(
        Ind=="F" ~ as.integer(round(Last-Start,digits = 0)),
        Ind=="C" ~ as.integer(round(TEU_BaC_AgeAtRec-Start,digits = 0)),
        TRUE ~ NA_integer_
      ))#%>%
    # Paste Ind and Years to get same categories as BOADICEA
    #unite(.,col="OC_use",Ind,Years,na.rm = TRUE,sep=':')
    
    return(OC_HRT[[return_label]])
  }

}





