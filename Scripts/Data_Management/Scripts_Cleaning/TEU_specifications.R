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
  
  HTN_control_comorb <- list(
    TEU_VeI_CVD,
    TEU_VeI_diab,
    TEU_VeI_arrhy,
    TEU_VeI_osteo,
    TEU_VeI_joint,
    TEU_VeI_epil,
    TEU_VeI_mig,
    TEU_VeI_anx,
    TEU_VeI_dep,
    TEU_VeI_asthCOPD
    
    
  )
 
  # TEUvars_raw added by XL 
  # This block of variables are for exploring how we handle categories such as 'Prefer not to answer' and 'Do not know'
  TEUvars_raw <-list(
    ID,
    Eth_Ethnicity,
    #Edu_HighestQual,
    Alc_Status,
    Smo_Status,
    TEU_HoH_PreTaxInc,
    HMH_BowelSc,
    HMH_Diabetes,
    HMH_IllDisab,
    HMH_VascCond,
    HMH_Meds_any,
    HMH_HTNAge
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

  MACE_summary <- c(
    TEUvars_common,
    list(
      # MACE at baseline
      TEU_HES_MACE_prev(record_level=FALSE),
      TEU_VeI_MACE_nonc,
      TEU_VeI_MACE_op,
      TEU_HMH_MACE_prev,
      TEU_MACE_prev,
      # MACE outcome
      TEU_HES_MACE_fudate(record_level=FALSE),
      TEU_Dth_MACE_dthdate(record_level=FALSE),
      TEU_MACE_eventdate,
      TEU_Dth_NotMACE_dthdate(record_level=FALSE),
      Admin_HES_CensorDate(record_level=FALSE),
      BaC_LostFUDate,
      TEU_MACE_censordate,
      TEU_MACE_status,
      TEU_MACE_time,
      TEU_MACE_time_yrs,
      # MACE subtypes 
      TEU_HES_MACE_fucomp(record_level=FALSE),
      TEU_MACE_fucomp,
      TEU_MACE_MI,
      TEU_MACE_Stroke
    )
    
  )
  
  MACE_recordlevel <- c(
    TEUvars_common,
    list(
      # MACE at baseline
      TEU_HES_MACE_prev(record_level=TRUE),
      TEU_VeI_MACE_nonc,
      TEU_VeI_MACE_op,
      TEU_HMH_MACE_prev,
      TEU_MACE_prev,
      # MACE outcome
      TEU_HES_MACE_fudate(record_level=TRUE),
      TEU_Dth_MACE_dthdate(record_level=TRUE),
      TEU_MACE_eventdate,
      TEU_Dth_NotMACE_dthdate(record_level=TRUE),
      Admin_HES_CensorDate(record_level=TRUE),
      BaC_LostFUDate,
      TEU_MACE_censordate,
      TEU_MACE_status,
      TEU_MACE_time,
      TEU_MACE_time_yrs,
      # MACE subtypes 
      TEU_HES_MACE_fucomp(record_level=TRUE),
      TEU_MACE_fucomp,
      TEU_MACE_MI,
      TEU_MACE_Stroke,
      TEU_Dth_MACE_dthtype(record_level=TRUE)
    )
    
  )
  
  HTN_control <- c(
    TEUvars_common,
    TEUvars_BP,
    TEU_VeI_HTN_prevalent(),
    list(
      TEU_selfrepHTN_dx,
      TEU_VeI_HTNmeds_rubric,
      TEU_selfrepHTN_meds,
      VeI_PregnantNow,
      TEU_BaC_AgeCat,
      TEU_BlP_measuredHTN,
      TEU_evidenceHTN,
      TEU_awareHTN,
      TEU_treatedHTN,
      TEU_controlledHTN,
      TEU_HMH_BowelCancerScreen,
      TEU_Edu_HighestQual,
      TEU_Edu_ISCED,
      TEU_Emp_CurrStat,
      TEU_Emp_JobCode_v2,
      TEU_HTN_Emp_category,
      TEU_HoH_PreTaxInc,
      TEU_HouseholdIncome,
      TEU_Emp_CurrStat,
      TEU_Emp_JobCode_v2,
      TEU_HTN_Emp_category,
      TEU_TownsendDepInd_Quint,
      TEU_CountryIncome,
      TEU_HMH_Meds_BP,
      TEU_Smo_Status,
      TEU_Alc_Status,
      TEU_Alc_WeeklyAlcUnits,
      TEU_Alc_WeeklyCat,
      TEU_Alc_Binge,
      TEU_Pha_METsover1200,
      TEU_FaH_CVD,
      TEU_BSM_BMIcat,
      TEU_BSM_WaistCircCat,
      TEU_SBP_PRS,
      TEU_DBP_PRS,
      TEU_BP_PRS,
      TEU_BP_PRS_quintiles,
      TEU_HMH_VascCond,
      TEU_HMH_prevHTN,
      TEU_HMH_prevstroke,
      TEU_HMH_prevCVD,
      HMH_IllDisab,
      HMH_Diabetes,
      HMH_HTNAge,
      TEU_BlP_HTNseverity,
      TEU_VeI_seriouscomb,
      TEU_VeI_cancer,
      HTN_comorb_num,
      TEU_VeI_numHTNmeds,
      TEU_VeI_numHTNmedscat
      
    )
  )
  
  
  
  HTN_control_PRS <- c(
    UKB_genetic,
    HTN_control
  )

  HTN_control_MACE <- c(
    HTN_control,
    HTN_control_comorb,
    Prosp_comorb_num,
    Prosp_comorb_numcat,
    UKB_genetic,
    MACE_recordlevel,
    BBC_LDL_Result,
    TEU_LDL_Quintiles,
    TEU_LDLctrl_v1,
    TEU_Emp_JobCode_v2, # When using v3 data until we grab emp_jobcode.0.0
    TEU_MACE_HaemStroke,
    TEU_BlP_SBP_quintiles,
    TEU_SBP_PRS_quintiles,
    GeP_Array
  )
  
  Cholstrl_control<-c(
    TEU_VeI_statin(),
    HTN_control_comorb,
    UKB_genetic,
    list(
      TEU_VeI_seriouscomb,
      TEU_VeI_cancer,
      VeI_PregnantNow,
      TEU_BaC_AgeCat,
      BSM_BMI,
      TEU_BSM_BMIcat,
      TEU_Smo_Status,
      TEU_Alc_Status,
      TEU_Alc_WeeklyAlcUnits,
      TEU_Alc_WeeklyCat,
      PhA_METsWkAllAct,
      TEU_Pha_METsover1200,
      TEU_FaH_CVD,
      TEU_HMH_BowelCancerScreen,
      HTN_comorb_num,
      HTN_comorb_numcat,
      TownsendDepInd,
      TEU_TownsendDepInd_Quint,
      TEU_HoH_PreTaxInc,
      TEU_HouseholdIncome,
      TEU_Emp_CurrStat,
      TEU_Emp_JobCode_v2,
      TEU_HTN_Emp_category,
      TEU_Edu_HighestQual,
      TEU_Edu_ISCED,
      TEU_CountryIncome,
      TEU_LDL_C_PRS,
      BBC_CHOL_Result,
      BBC_HDL_Result,
      BBC_LDL_Result,
      TEU_LDLctrl_v1
      
    )
  )
 
  Cholstrl_prosp <- c(
    MACE_recordlevel,
    Cholstrl_control,
    TEUvars_BP,
    list(
      Prosp_comorb_num,
      Prosp_comorb_numcat
    )
  )

                   
  Cholesterol_PRS <- c(
    TEUvars_common,
    TEU_VeI_CVD_operation(dx_codes = c(1069, 1070, 1095, 1105)),
    list(
      TEU_BaC_AgeCat,
      TEU_HMH_BowelCancerScreen,
      TEU_Edu_HighestQual,
      TEU_Edu_ISCED,
      TEU_HoH_PreTaxInc,
      TEU_TownsendDepInd_Quint,
      TEU_HMH_Meds_Chol,
      TEU_Smo_Status,
      TEU_Alc_Status,
      TEU_Alc_WeeklyAlcUnits,
      TEU_Alc_Binge,
      TEU_Pha_METsover1200,
      TEU_FaH_CVD,
      TEU_BSM_BMIcat,
      TEU_BSM_WaistCircCat,
      TEU_LDL_C_PRS,
      TEU_LDL_C_PRS_deciles,
      GeP_Batch,
      TEU_HMH_VascCond,
      TEU_HMH_prevHTN,
      TEU_HMH_prevstroke,
      TEU_HMH_prevCVD,
      HMH_IllDisab,
      HMH_Diabetes,
      BBC_CHOL_Result,
      BBC_HDL_Result,
      BBC_LDL_Result,
      GeP_PC(pc=1),
      GeP_PC(pc=2),
      GeP_PC(pc=3),
      GeP_PC(pc=4),
      ADO_DateFirstMI,
      ADO_DateFirstIStroke
    )
  )
  

  T2DM_PRS <- c(
    TEUvars_common,
    TEU_VeI_Diabetes_meds(),
    TEU_VeI_T2DM_prevalent(),
    list(
      
      # Self-report
      TEU_HMH_Meds_Diab,
      TEU_HMH_gest_diabetes,
      HMH_Diabetes,
      HMH_DiabetesAge,
      TEU_VeI_T1D,
      TEU_VeI_Diab_other,
      
      
      # HES
      TEU_HES_T2DM_base(record_level=FALSE),
      TEU_HES_T2DM_excl(record_level=FALSE),
      
      # PRS
      TEU_T2DM_PRS,
      TEU_T2DM_PRS_quintiles
    )
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
  
  test2<-c(
    TEUvars_common,
    TEU_ATC(length=2)
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
