# Jennifer Collister
# 30/09/20

# Load the project config file for filepaths etc
if (!exists("config")) {
  library(yaml)
  config = yaml.load_file("config.yml")
}



specs <- function() {
  
  # If you're here to write a new spec, you can run this source line interactively
  # to load all the variable derivation objects into your working environment
  # so you get autocomplete when typing them!
  source(file.path(config$scripts$cleaning, "derivation_objects.R"),
         local = if (sys.nframe() == 0L) {
           FALSE
           } else {
             TEUmaps <- new.env()
             }
         )
  if (exists("TEUmaps")) {
    attach(TEUmaps)
    on.exit(detach(TEUmaps))
  }
  
  # Dataset specifications
  
  TEUvars_common <- list(
    ID,
    BaC_Sex,
    TEU_BaC_DateOfBirth,
    Rec_DateAssess,
    TEU_BaC_AgeAtRec,
    TEU_ethnicgrp,
    TEU_Rec_AssessCentre,
    TEU_Rec_Country
  )
  
  TEUvars_BP <- list(
    TEU_BlP_SBP.0.0,
    TEU_BlP_SBP.0.1,
    TEU_BlP_DBP.0.0,
    TEU_BlP_DBP.0.1,
    TEU_BlP_nSBP,
    TEU_BlP_nDBP,
    TEU_BlP_SBP.avg,
    TEU_BlP_DBP.avg,
    TEU_BlP_Pulse.0.0,
    TEU_BlP_Pulse.0.1,
    TEU_BlP_Pulse.avg
  )
  
  
  UKB_genetic <- list(
    ID,
    GeP_UsedInPCA, # Identifies participants which met UKB QC for inclusion in PCA
    GeP_Outliers, # Identifies participants who are outliers for missingness and heterozygosity
    GeP_ethnic, # Identifies participants with genetic White British ancestry
    GeP_Array, # We should adjust our PRS analyses by array
    GeP_Batch, # We may wish to adjust for batch effect
    # GeP_Plate, # We may wish to adjust for plate effect
    GeP_PC(pc=1),
    GeP_PC(pc=2),
    GeP_PC(pc=3),
    GeP_PC(pc=4),
    GeP_PC(pc=5),
    GeP_PC(pc=6),
    GeP_PC(pc=7),
    GeP_PC(pc=8),
    GeP_PC(pc=9),
    GeP_PC(pc=10), # Genetic Principal Components of ancestry
    GeP_Sex, # Used to check for sex discordance
    BaC_Sex # Used to check for sex discordance
  )

  
  # variables required for exclusion criteria in MLforBrCa
  BrCa_excl <- c(
    TEUvars_common,
    UKB_genetic,
    TEU_BreastCancer(),
    TEU_Mastectomy(record_level=TRUE),
    list(
      Admin_HES_CensorDate(record_level=TRUE),
      Admin_CaR_CensorDate,
      Admin_Dth_CensorDate(record_level=TRUE),
      Admin_CensorDate_BrCaHES(),
      BaC_LostFUDate,
      TEU_Dth_NotBrCa_dthdate(record_level=TRUE, ICD10_codes=paste0("C50", seq(1, 9, by=1)), exclude=TRUE),
      TEU_Dth_NotBrCa_dthcause(record_level=TRUE, ICD10_codes=paste0("C50", seq(1, 9, by=1)), exclude=TRUE),
      TEU_HES_BrCa_inc(record_level=TRUE),
      TEU_HES_BrCa_incdate(record_level=TRUE),
      BrCa_censordate,
      TEU_BrCa_status,
      TEU_BrCa_time,
      TEU_BrCa_313_PRS,
      TEU_BrCa_313_PRS_quintiles,
      TEU_BrCa_100k_PRS,
      TEU_BrCa_100k_PRS_quintiles,
      TEU_BrCa_100k_PRS_percent,
      FSF_Menopause
    )
  )
  
    
  BrCa_PRS <- c(
    BrCa_excl,
    TEUvars_BP,
    # Physical activity/Physical measures
    PhA(),
    TEU_OC(),TEU_HRT(),
    ELR(),
    TEU_BirthAge(),
    TEU_FaH_Info(),
    Blood_count(),
    Urine_Assays(),
    Sun_Exp(),
    Diet(),
    # Blood Biochemistry
    BBC_Result(),
    TEU_ATC(length="Full"),
    list(
      TEU_BaC_AgeCat,
      # Socio-demographics
      TownsendDepInd,
      TEU_Edu_HighestQual,
      TEU_Edu_ISCED,
      TEU_HoH_PreTaxInc,
      TEU_HouseholdIncome,
      TEU_Emp_CurrStat,
      TEU_HTN_Emp_category,
      BSM_BMI,
      # Lifestyle
      TEU_Smo_Status,
      TEU_Alc_Status,
      TEU_Alc_WeeklyAlcUnits,
      ElD_MobPhUsage,
      Sle_Duration,
      Sle_RisingAM,
      Sle_Chronotype,
      Sle_DaytimeNap,
      Sle_Insomnia,
      Sle_Snoring,
      Sle_DaytimeSleeping,
      PsF_LeisureAct,
      PhA_WalkDays,
      PhA_DrivingTime,
      PhA_ComputTime,
      PhA_TVTime,
      # Family History
      TEU_FaH_CVD,
      TEU_FaH(label="BrCa",conditions=c("Breast cancer")),
      TEU_FaH(label="sevdep",conditions=c("Severe depression")),
      TEU_FaH(label="diab",conditions=c("Diabetes")),
      TEU_FaH(label="OthCa",conditions=c("Bowel cancer","Prostate cancer","Lung cancer")),
      TEU_FaH(label="dem",conditions=c("Alzheimer's disease/dementia")),
      TEU_FaH(label="park",conditions=c("Parkinson's disease")),
      TEU_FaH(label="bron",conditions=c("Chronic bronchitis/emphysema")),
      # Early Life factors
      VeI_BirthWt,
      # Sparse physical measures
      HeT_HearingTestL,
      HeT_HearingTestR,
      HGS_L,
      HGS_R,
      TEU_Spi_FVC.avg,
      TEU_Spi_FEV1.avg,
      TEU_Spi_PEF.avg,
      CoF_NM, 
      CoF_FlScore,
      CoF_PrMemResult,
      CoF_RTTTimeID,
      PsF_VisitFreq,
      PsF_Confide
      )
  )  
  
  # Note on below: Technically the chunk below should be embedded with BrCa_PRS spec
  # However running HES_CaR would take 2 days... so instead, I separated them out below
  # Need to merge data below to BrCa_PRS later on.
  HES_CaR <- c(
    BrCa_excl,
    # Health conditions
    TEU_HES_prev(record_level=TRUE,length="Full"),
    # CaR 
    TEU_CaR_prev(length="Full")
  )
 
  return(environment())
}

TEU_SPECS <- specs()
