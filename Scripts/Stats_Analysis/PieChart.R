
library(ggplot2)
library(yaml)
library(lessR)
library(scales)

config = yaml.load_file(here::here("./config.yml"))
source(here::here(config$functions$general))

# Read in tot_train to get the names of features
tot_train<-readRDS(file.path(config$data$derived,"tot_train.rds"))

input_features<-tot_train%>%select(!c("TEU_BrCa_time","TEU_BrCa_status","ID",
                                      "OtherCancerDx_Prevalent","GeP_Array",paste0("GeP_PC_",1:10))
                                   )

# Check the length
dim(input_features)

#############################

# Health conditions
HES_CaR=input_features%>%select(starts_with(c("TEU_HES_","TEU_CaR_")))

ncol(HES_CaR)

# Lifestyle
life=input_features%>%select(TEU_Smo_Status,
                             TEU_Alc_Status,
                             TEU_Alc_WeeklyAlcUnits,
                             ElD_MobPhUsage,
                             PsF_LeisureAct,
                             starts_with(c("Die_","PhA_","Sun_","Sle_")))

# Family History
FaH=input_features%>%select(starts_with(c("TEU_FaH","FaH")))

# Early Life and reproductive
ELF=input_features%>%select(starts_with(c("TEU_OC_","TEU_HRT_","ELF_","FSF_","TEU_FirstBirth","TEU_LastBirth")))

# Blood and urine assays
blood_urine=input_features%>%select(starts_with(c("BBC_","BlA_","Uri_")))

# Physical measures
phy_measures=input_features%>%select(
  #HeT_HearingTestL,
  #HeT_HearingTestR,
  HGS_L,
  HGS_R,
  TEU_Spi_FVC.avg,
  TEU_Spi_FEV1.avg,
  TEU_Spi_PEF.avg,
  #CoF_NM, 
  #CoF_FlScore,
  #CoF_PrMemResult,
  CoF_RTTTimeID,
  PsF_VisitFreq,
  PsF_Confide,
  starts_with(c("TEU_BlP_","ArS_","Imp_","ESC_","BDH_"))
)

# Socio-demo + PRS (because those 2 have really small counts)
socio<-input_features%>%select(
  TownsendDepInd,
  TEU_Edu_HighestQual,
  #TEU_Edu_ISCED,
  #TEU_HoH_PreTaxInc,
  TEU_HouseholdIncome,
  TEU_Emp_CurrStat,
  TEU_Emp_category,
  BSM_BMI,
  TEU_BaC_AgeAtRec,
  TEU_Rec_Country,
  ends_with("PRS")
)

# Medications
meds=input_features%>%select(!c(names(HES_CaR),names(life),names(FaH),names(ELF),names(blood_urine),
                           names(phy_measures),names(socio),"TEU_BrCa_313_PRS","TEU_BrCa_100k_PRS"))



#################################
# Create df for pie chart

list<-list(HES_CaR,life,FaH,ELF,blood_urine,phy_measures,socio,meds)


#values=sapply(1:length(list),function(i) (ncol(list[[i]])/ncol(input_features))*100)


#df<-data.frame(
#  features=c("Health conditions (including cancer)","Lifestyle factors","Family History","Early life and reproductive factors",
#             "Blood and Urine assays","Physical measures","Socio-demographics and PRS","Medication use"),
#  value=c(values)
  
#)

#blank_theme <- theme_minimal()+
#  theme(
#    axis.title.x = element_blank(),
#    axis.title.y = element_blank(),
#    panel.border = element_blank(),
#    panel.grid=element_blank(),
#    axis.ticks = element_blank(),
#    plot.title=element_text(size=14, face="bold")
#  )

#pie<-ggplot(df,aes(x="",y=value,fill=features))+geom_bar(width=1,stat = "identity")+
#  coord_polar("y",start=0)+  blank_theme +
#  theme(axis.text.x=element_blank()) +
#  labs(fill="Categories of 1813 input features")

#pie(df$value , labels = df$features)

#################################
# Create df for donut chart using lessR
counts=lapply(1:length(list),function(i) ncol(list[[i]]))


# Append names for counts
perc=sapply(1:length(list),function(i) paste0("n=",ncol(list[[i]])," (",round((ncol(list[[i]])/ncol(input_features))*100),"%)"))

names=c("Health conditions (including cancer) ","Lifestyle factors ","Family History ","Early life and reproductive factors ",
                "Blood and Urine assays ","Physical measures ","Socio-demographics and PRS ","Medication use ")


names(counts)=paste0(names,perc)

counts_col=factor(c(unlist(sapply(1:length(counts),function(i) rep(names(counts)[i],counts[[i]])))))


#PieChart(counts_col, data = df,
 #        main = NULL)

cat<-data.frame(counts_col)

# Two types of colour style: One is blue the other is gray
cols <-  hcl.colors(length(levels(counts_col)), "Oslo")

cols <-  hcl.colors(length(levels(counts_col)), "Grays")

PieChart(counts_col, data = cat, hole = 0,
         values_position = "",
         #values_size = 0.5,
         values_color = "black",
         fill = cols,labels_cex = 0.7,
         main="",
         height = 2)



