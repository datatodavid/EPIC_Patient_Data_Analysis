library(tidyverse)
# library(dplyr)
# library(ggplot2)
# library(lubridate)
library(magrittr)


EPIC = read.csv("EPIC 15 MONTH FROM 5.18.20 WITH PT ID WITH RD DE IDENTIFIED FOR COEPS.csv", 
               stringsAsFactors = F, na.strings = c("NA", " ", ""))

#head(EPIC)
names(EPIC) = make.names(names(EPIC), unique=T)
names(EPIC) = gsub("Plan..Insurance.", "Plan.Insurance", names(EPIC))
names(EPIC) = gsub("X2nd.to.last.A1C", "Second.to.last.A1C", names(EPIC))
names(EPIC) = gsub("Pt..Portal.Status", "Pt.Portal.Status", names(EPIC))
names(EPIC) = gsub("Last.EPIC..Visit", "Last.EPIC.Visit", names(EPIC))
names(EPIC) = gsub('ï..Age', "Age", names(EPIC))
colnames(EPIC)

# EPIC_RN = EPIC %>% 
#   filter(., !is.na(RD.Int) & is.na(RD.Visit.Date))
# write.csv(EPIC_RN, "EPIC_RN.csv", row.names = F)
# EPIC_RN2 = EPIC %>%
#   filter(., is.na(RD.Int) & !is.na(RD.Visit.Date))
# write.csv(EPIC_RN2, "EPIC_RN2.csv", row.names = F)

#lapply(EPIC, class)
EPIC$Date.Last.24.hour.Microalbumin = 
  as.Date(EPIC$Date.Last.24.hour.Microalbumin, format="%m/%d/%Y")
EPIC$Date.Last.LDL = as.Date(EPIC$Date.Last.LDL, format="%m/%d/%Y")
EPIC$Date.Last.A1C = as.Date(EPIC$Date.Last.A1C, format="%m/%d/%Y")
EPIC$Date.Last.HDL = as.Date(EPIC$Date.Last.HDL, format="%m/%d/%Y")
EPIC$Date.Last.Random.Microalbumin = 
  as.Date(EPIC$Date.Last.Random.Microalbumin, format="%m/%d/%Y")
EPIC$Date.Last.Timed.Microalbumin = 
  as.Date(EPIC$Date.Last.Timed.Microalbumin, format="%m/%d/%Y")
EPIC$Date.Last.Cholesterol = as.Date(EPIC$Date.Last.Cholesterol, format="%m/%d/%Y")
EPIC$Date.Last.Triglycerides = as.Date(EPIC$Date.Last.Triglycerides, format="%m/%d/%Y")
EPIC$Cur.Statin.Rx.Dt = as.Date(EPIC$Cur.Statin.Rx.Dt, format="%m/%d/%Y")
EPIC$RD.Visit.Date = as.Date(EPIC$RD.Visit.Date, format="%m/%d/%Y")
EPIC$Last.EPIC.peds.visit = as.Date(EPIC$Last.EPIC.peds.visit, format="%m/%d/%Y")
EPIC$Last.EPIC.adult.visit = as.Date(EPIC$Last.EPIC.adult.visit, format="%m/%d/%Y")
EPIC$Last.EPIC.Visit = as.Date(EPIC$Last.EPIC.Visit, format="%m/%d/%Y")

EPIC$Last.A1C = gsub("%", "", EPIC$Last.A1C, fixed=T)
EPIC$Last.A1C = gsub(">14.0", "14.1", EPIC$Last.A1C, fixed=T)
EPIC$Last.A1C = gsub("1>14.0", "14.1", EPIC$Last.A1C, fixed=T)
EPIC$Last.A1C = gsub(">14", "14.1", EPIC$Last.A1C, fixed=T)
EPIC$Last.A1C = gsub("GREATER THAN 14.0", "14.1", EPIC$Last.A1C, fixed=T)
EPIC$Last.A1C = gsub("`", "", EPIC$Last.A1C, fixed=T)
EPIC$Last.A1C = gsub("929", "9.1", EPIC$Last.A1C)
EPIC$Last.A1C = gsub(",.*", "", EPIC$Last.A1C)
EPIC$Last.A1C = gsub("IN-PROCESS", NA, EPIC$Last.A1C)
EPIC$Last.A1C = as.numeric(EPIC$Last.A1C)

EPIC$Second.to.last.A1C = gsub("0", "15", EPIC$Second.to.last.A1C)
EPIC$Second.to.last.A1C = gsub("202", "8", EPIC$Second.to.last.A1C)
EPIC$Second.to.last.A1C = as.numeric(EPIC$Second.to.last.A1C)

EPIC$Diab.Duration = gsub(" years", "", EPIC$Diab.Duration)
EPIC$Diab.Duration = gsub("None", NA, EPIC$Diab.Duration)
EPIC$Diab.Duration = as.numeric(EPIC$Diab.Duration)

EPIC$Age = ifelse(grepl("month", EPIC$Age, fixed=T), 
                 as.numeric(gsub(" m.*", "", EPIC$Age))/12,
                 gsub(" y.*", "", EPIC$Age))
EPIC$Age = as.numeric(EPIC$Age)  



#unique(EPIC$Race)                 
EPIC = EPIC %>% 
  mutate(., Race.Simplified = 
           ifelse(Race == "White", "White",
           ifelse(Race == "White\nUnknown/Not Reported", "White",
           ifelse(Race == "Unknown/Not Reported/White", "White",
           ifelse(Race == "Not Reported\nWhite", "White",
           ifelse(Race == "White\nNot Reported", "White",
           ifelse(Race == "White\nWhite", "White",
           ifelse(Race == "White\nWhite\nUnknown/Not Reported", "White",
           ifelse(Race == "White\nUnknown/Not Reported\nNot Reported", "White",
           ifelse(Race == "Other", "Other",
           ifelse(Race == "Other\nUnknown/Not Reported", "Other",
           ifelse(Race == "Other\nOther", "Other",
           ifelse(Race == "Native Hawaiian/Other Pacific Islander",
                  "Native Hawaiian/Other Pacific Islander",
           ifelse(Race == "Unknown/Not Reported\nNot Reported", NA,
           ifelse(Race == "Unknown/Not Reported\nUnknown/Not Reported", NA,
           ifelse(Race == "Unknown/Not Reported", NA, 
           ifelse(Race == "Not Reported", NA, 
           ifelse(Race == "Black/African American\nUnknown/Not Reported", 
                  "Black/African American", 
           ifelse(Race == "Unknown/Not Reported\nBlack/African American", 
                  "Black/African American", 
           ifelse(Race == "Black/African American\nBlack/African American", 
                  "Black/African American", 
           ifelse(Race == "Black/African American\nNot Reported", 
                  "Black/African American", 
           ifelse(Race == "Not Reported\nBlack/African American", 
                  "Black/African American", 
           ifelse(Race == "Black/African American", "Black/African American",                   
           ifelse(Race == "Not Reported\nAsian", "Asian", 
           ifelse(Race == "Asian", "Asian",                  
           ifelse(Race == "American Indian/Alaska Native", 
                  "American Indian/Alaska Native",
                  "Multiracial"))))))))))))))))))))))))))
#unique(EPIC$Race.Simplified)
#EPIC %>% group_by(., Race.Super.Simplified) %>% summarise(n())

EPIC = EPIC %>% 
  mutate(., Race.Super.Simplified = 
           ifelse(Race.Simplified == "White", "White",
           ifelse(Race.Simplified == "Black/African American", 
                  "Black/African American", 
           ifelse(is.na(Race.Simplified), NA, "Other"))))



EPIC$Ethnicity = gsub(" \\[.*", "", EPIC$Ethnicity)
EPIC$Ethnicity = gsub("Not Reported", NA, EPIC$Ethnicity)
EPIC$Ethnicity = gsub("Unknown", NA, EPIC$Ethnicity)
#unique(EPIC$Ethnicity)

#### LANG TESTS ####
# EPIClang1 = EPIC %>% select(., Patient_ID, Pref.Language,
#                    , PATIENT.LANGUAGE, Language.1, Language.2,
#                   Spoken.Language, Written.Language) %>% 
#                   filter(., (Pref.Language == "English" | Pref.Language ==  "Spanish") &
#                   (PATIENT.LANGUAGE != Language.1 | 
#                            Pref.Language != PATIENT.LANGUAGE |
#                            Pref.Language != Language.1))
# EPIClang1
# write.csv(EPIClang1, "EPIClang1.csv", row.names = F)
# 
# EPIClang2 = EPIC %>%  select(., Patient_ID, Pref.Language,
#                 , PATIENT.LANGUAGE, Language.1, Language.2,
#                 Spoken.Language, Written.Language) %>% 
#   filter(., is.na(Patient_ID) | is.na(Pref.Language) |
#            is.na(Language.1) | is.na(Language.2) |
#           is.na(Written.Language)) 
# EPIClang2
##### 
EPIC = EPIC %>% 
  mutate(., Lang.Simplified = 
           ifelse(Pref.Language == "English", "English",
                  ifelse(Pref.Language == "Spanish", "Spanish", 
                         ifelse(Pref.Language == "No", NA,
                         ifelse(is.na(Pref.Language), NA, 
                                "Other")))))
#unique(EPIC$Lang.Simplified)
# 
EPIC$Pref.Language = gsub("No", NA, EPIC$Pref.Language)
EPIC$Pref.Language = gsub("Other - please contact MI Department at x79800", 
                          "Other", EPIC$Pref.Language)
# #unique(EPIC$Pref.Language)


EPIC$CGM.Type = gsub("Other - see comments", "Other", EPIC$CGM.Type)
#unique(EPIC$CGM.Type)


# unique(EPIC$Pump.Brand)
# 
# EPIC %>% filter(., Pump.Brand == "") %>% 
#   summarise(n())

#unique(EPIC$Primary.Cvg.1) 
#Just need Medicare vs all AND medicare + medicaid vs all
EPIC = EPIC %>% 
  mutate(., Medicaid = 
           ifelse(grepl("Medicaid", Primary.Cvg.1, ignore.case=T), 
           "Medicaid", 
           ifelse(is.na(Primary.Cvg.1), NA,
                  "Other")))


# lapply(EPIC, class)
# unique(EPIC$Last.LDL)
EPIC$Last.LDL = gsub("TNP", NA, EPIC$Last.LDL, ignore.case = T)
EPIC$Last.LDL = gsub("Test not performed", NA, EPIC$Last.LDL, ignore.case = T)
EPIC$Last.LDL = gsub(">400", "401", EPIC$Last.LDL, fixed=T)
EPIC$Last.LDL = gsub("SEE COMMENT", NA, EPIC$Last.LDL, fixed=T)
EPIC$Last.LDL = gsub("Comment", NA, EPIC$Last.LDL, fixed=T)
EPIC$Last.LDL = gsub(" mg/dL", "", EPIC$Last.LDL, fixed=T, ignore.case = T)
EPIC$Last.LDL = gsub("N/A", NA, EPIC$Last.LDL, fixed=T, ignore.case = T)
EPIC$Last.LDL = gsub("76, 85", "80", EPIC$Last.LDL, fixed=T)
EPIC$Last.LDL = gsub("169, 148", "159", EPIC$Last.LDL, fixed=T)
EPIC$Last.LDL = gsub("109, 126", "118", EPIC$Last.LDL, fixed=T)
#EPIC$Last.LDL = gsub("1.67", NA, EPIC$Last.LDL)
EPIC$Last.LDL = gsub(",.*", "", EPIC$Last.LDL)
EPIC$Last.LDL = as.numeric(EPIC$Last.LDL)
#EPIC %>% filter(., Last.LDL<30) %>% group_by(., Last.LDL) %>%  summarise(n())
#unique(EPIC$On.Statins)


EPIC$Last.Cholesterol = gsub("TNP", NA, EPIC$Last.Cholesterol, ignore.case = T)
EPIC$Last.Cholesterol = gsub("ND", NA, EPIC$Last.Cholesterol, ignore.case = T)
EPIC$Last.Cholesterol = gsub(" mg/dL", "", EPIC$Last.Cholesterol, fixed=T)
EPIC$Last.Cholesterol = gsub("<100", "99", EPIC$Last.Cholesterol, fixed=T)
#EPIC$Last.Cholesterol = gsub("3", NA, EPIC$Last.Cholesterol)
EPIC$Last.Cholesterol = gsub(",.*", "", EPIC$Last.Cholesterol)
EPIC$Last.Cholesterol = as.numeric(EPIC$Last.Cholesterol)

               
EPIC$Last.HDL = gsub("TNP", NA, EPIC$Last.HDL, ignore.case = T)
EPIC$Last.HDL = gsub("ND", NA, EPIC$Last.HDL, ignore.case = T)
EPIC$Last.HDL = gsub(" mg/dL", "", EPIC$Last.HDL, fixed=T)
EPIC$Last.HDL = gsub(",.*", "", EPIC$Last.HDL)
EPIC$Last.HDL = as.numeric(EPIC$Last.HDL)

EPIC$Last.Triglycerides = gsub("TNP", NA, EPIC$Last.Triglycerides, ignore.case = T)
EPIC$Last.Triglycerides = gsub("ND", NA, EPIC$Last.Triglycerides, ignore.case = T)
EPIC$Last.Triglycerides = gsub("<45", "44", EPIC$Last.Triglycerides, fixed=T)
EPIC$Last.Triglycerides = gsub("GREATER THAN 525", NA, EPIC$Last.Triglycerides, fixed=T)
EPIC$Last.Triglycerides = gsub(" mg/dL", "", EPIC$Last.Triglycerides, fixed=T, ignore.case = T)
EPIC$Last.Triglycerides = gsub("N/A", NA, EPIC$Last.Triglycerides, fixed=T, ignore.case = T)
EPIC$Last.Triglycerides = gsub(",.*", "", EPIC$Last.Triglycerides)
EPIC$Last.Triglycerides = as.numeric(EPIC$Last.Triglycerides)
#class(EPIC$Last.LDL)
EPIC = EPIC %>% 
  mutate(., Age.Group = 
               ifelse(Age<10, "<10",
               ifelse((Age >=10 & Age <22), "10-21",
               ifelse((Age >=22 & Age <40), "22-39",
               ifelse((Age >=40 & Age <=75), "40-75",
               ifelse(Age>75, "76+", NA))))),
            LDL.Cat = 
               ifelse(Last.LDL<100, "<100",
               ifelse((Last.LDL >=100 & Last.LDL <130), "100-129",
               ifelse((Last.LDL >=130 & Last.LDL <159), "130-159",
               ifelse(Last.LDL>=160, ">=160", NA)))),
            LDL.100.Split = 
               ifelse(Last.LDL<100, "<100",
               ifelse(Last.LDL>=100, ">=100", NA)),
            LDL.130.Split = 
               ifelse(Last.LDL<130, "<130",
               ifelse(Last.LDL>=130, ">=130", NA)),            
            A1C.Group = 
               ifelse(Last.A1C<7, "A1C<7",
               ifelse((Last.A1C >=7 & Last.A1C <=9), "A1C 7-9",
               ifelse(Last.A1C>9, "A1C>9", NA))))

#unique(EPIC$Pump.Brand.Name)
EPIC$Pump.Brand = gsub("T-Slim", "t:slim", EPIC$Pump.Brand)
EPIC$Pump.Brand = gsub("T:Slim", "t:slim", EPIC$Pump.Brand)
EPIC$Pump.Brand = gsub("T Slim", "t:slim", EPIC$Pump.Brand)
EPIC$Pump.Brand = gsub("Tandem", "t:slim", EPIC$Pump.Brand)
EPIC$Pump.Brand = gsub("T-Flex", "t:flex", EPIC$Pump.Brand)
EPIC$Pump.Brand = gsub(",", "", EPIC$Pump.Brand)
EPIC$Pump.Brand = gsub("None", NA, EPIC$Pump.Brand)
EPIC$Pump.Brand = gsub("Control IQ", "Control-IQ", EPIC$Pump.Brand)
EPIC$Pump.Brand = gsub("Basal IQ", "Basal-IQ", EPIC$Pump.Brand)
EPIC$Pump.Brand = gsub("Other (Comment)", "Other", EPIC$Pump.Brand, fixed = T)
EPIC$Pump.Brand = gsub("Other(Comment)", "Other", EPIC$Pump.Brand, fixed = T)
EPIC = separate(EPIC, Pump.Brand, c('Pump.Brand.Name', 'Pump.Model', 'X2.Software'), sep=' ')
#colnames(EPIC)
#EPIC %>% select(., Pump.Brand.Name, Pump.Model, X2.Software)



EPICselect = EPIC %>% 
  select(., Patient_ID, Age, Age.Group, Diab.Duration, Race,  
         Race.Super.Simplified, Ethnicity, Sex, Pref.Language, Lang.Simplified,
         CGM.Type, Pump.Brand.Name, Pump.Model, X2.Software, 
         Primary.Cvg.1, Medicaid, Pt.Portal.Status, On.Statins, 
         Cur.Statin.Rx.Dt, Last.LDL, Date.Last.LDL, LDL.Cat, LDL.100.Split, LDL.130.Split,
         Last.A1C, Date.Last.A1C, Second.to.last.A1C, A1C.Group,
         Last.Cholesterol, Date.Last.Cholesterol,
         Last.HDL, Date.Last.HDL, Last.Triglycerides, Date.Last.Triglycerides, 
         Last.EPIC.Visit, RD.Int, RD.Visit.Date)

#EPIC %>% group_by(LDL.100.Split) %>%  summarise(., min(Last.EPIC.Visit), max(Last.EPIC.Visit))

EPIC1075 = EPICselect %>% 
  filter(., Age.Group != "<10" & Age.Group != "76+" )
#write.csv(EPIC1075, "EPIC1075ALLclean.csv", row.names = F) 

#write.csv(EPICselect, "EPICSelectALLclean.csv", row.names = F)
#head(EPICselect, 20)
#lapply(EPICselect, class)
# EPIC %>% filter(., is.na(Plan.Insurance)) %>% tally()
# EPIC %>% filter(., is.na(Primary.Cvg.1)) %>% tally()
# EPIC %>% filter(., is.na(Primary.Cvg.2)) %>% tally()
# EPIC %>% filter(., Primary.Cvg.1 != Primary.Cvg.2)
# EPIC %>% select(., Plan.Insurance, Primary.Cvg.1) %>% 
#                  filter(., !is.na(Plan.Insurance) & Plan.Insurance != Primary.Cvg.1)
# EPIC %>% filter(., Primary.Cvg.1 != Plan.Insurance)
# EPIC %>% filter(., grepl("Medicaid", Primary.Cvg.1, ignore.case=T)) %>%
#   filter(., Age.Group == "10-21") %>% tally()

# EPIC_INSURANCE = sort(unique(EPIC$Primary.Cvg.1))
#write.csv(EPIC_INSURANCE, "EPIC_INSURANCE.csv", row.names = F)
?describe
#EPIC_INSURANCE 

unique(EPICselect$LDL.Cat)

#write.csv(EPICselect,"EPICselecttest.csv", row.names = FALSE)

#install.packages("xlsx")
#library('xlsx')
unique(colnames(EPIC))
##### DIETICIAN CHECK 10-18 #####
EPIC1018RD = EPICselect %>% 
  filter(., Age >=10 & Age <= 18 & Last.EPIC.Visit >= 2019-05-19)
#write.csv(EPIC1018RD, "EPIC1018RDclean.csv", row.names = F)

EPIC1018RDSeen = EPIC1018RD %>% 
  summarise(., Total.Pop.10.18 = n(),
  Pop.RD.visit.10.18 = 
      sum(!is.na(RD.Int) | !is.na(RD.Visit.Date), na.rm=T),
  Perc.RD.visit.10.18 = 
      100*sum((!is.na(RD.Int) | !is.na(RD.Visit.Date)), na.rm=T)/n(),    
  Pop.RD.visit.w.LDL.Drawn = 
      sum((!is.na(RD.Int) | !is.na(RD.Visit.Date)) & !is.na(Last.LDL), na.rm=T),
  Perc.RD.visit.w.LDL.Drawn = 
      100*sum((!is.na(RD.Int) | !is.na(RD.Visit.Date)) & !is.na(Last.LDL), na.rm=T)
         /sum(!is.na(Last.LDL), na.rm=T),    
  Pop.LDL.above.100.RD.visit = 
      sum(LDL.100.Split == ">=100" & 
            (!is.na(RD.Int) | !is.na(RD.Visit.Date)), na.rm=T),
  Perc.LDL.above.100.RD.visit = 
      100*sum(LDL.100.Split == ">=100" & 
                (!is.na(RD.Int) | !is.na(RD.Visit.Date)), na.rm = T)
         /sum(LDL.100.Split == ">=100", na.rm=T),
  Pop.LDL.above.130.RD.visit = 
      sum(LDL.130.Split == ">=130" & 
                (!is.na(RD.Int) | !is.na(RD.Visit.Date)), na.rm=T),
  Perc.LDL.above.130.RD.visit = 
    100*sum(LDL.130.Split == ">=130" & 
              (!is.na(RD.Int) | !is.na(RD.Visit.Date)), na.rm=T)
        /sum(LDL.130.Split == ">=130", na.rm=T),
  Pop.LDL.above.160.RD.visit = 
      sum((!is.na(RD.Int) | !is.na(RD.Visit.Date)) & LDL.Cat == ">=160", na.rm=T),
  Perc.LDL.above.160.RD.visit = 
      100*sum(LDL.Cat == ">=160" & 
            (!is.na(RD.Int) | !is.na(RD.Visit.Date)), na.rm = T)
    /sum(LDL.Cat == ">=160", na.rm=T))
#unique(EPIC1018RD$LDL.130.Split)

EPIC1018RDVisitDateONLY = EPIC1018RD %>% 
  summarise(., Total.Pop.10.18 = n(),
            Pop.RD.visit.10.18 = 
              sum(!is.na(RD.Visit.Date), na.rm=T),
            Perc.RD.visit.10.18 = 
              100*sum((!is.na(RD.Visit.Date)), na.rm=T)/n(),    
            Pop.RD.visit.w.LDL.Drawn = 
              sum((!is.na(RD.Visit.Date)) & !is.na(Last.LDL), na.rm=T),
            Perc.RD.visit.w.LDL.Drawn = 
              100*sum((!is.na(RD.Visit.Date)) & !is.na(Last.LDL), na.rm=T)
            /sum(!is.na(Last.LDL), na.rm=T),    
            Pop.LDL.above.100.RD.visit = 
              sum(LDL.100.Split == ">=100" & 
                    (!is.na(RD.Visit.Date)), na.rm=T),
            Perc.LDL.above.100.RD.visit = 
              100*sum(LDL.100.Split == ">=100" & 
                        (!is.na(RD.Visit.Date)), na.rm = T)
            /sum(LDL.100.Split == ">=100", na.rm=T),
            Pop.LDL.above.130.RD.visit = 
              sum(LDL.130.Split == ">=130" & 
                    (!is.na(RD.Visit.Date)), na.rm=T),
            Perc.LDL.above.130.RD.visit = 
              100*sum(LDL.130.Split == ">=130" & 
                        (!is.na(RD.Visit.Date)), na.rm=T)
            /sum(LDL.130.Split == ">=130", na.rm=T),
            Pop.LDL.above.160.RD.visit = 
              sum((!is.na(RD.Visit.Date)) & LDL.Cat == ">=160", na.rm=T),
            Perc.LDL.above.160.RD.visit = 
              100*sum(LDL.Cat == ">=160" & 
                        (!is.na(RD.Visit.Date)), na.rm = T)
            /sum(LDL.Cat == ">=160", na.rm=T))


write.csv(EPIC1018RDVisitDateONLY, "EPIC1018RDVisitDateONLY.csv", row.names = F)

#####################################################################
######################### DEEP DIVE 10-21 ###########################
EPIC1021 = EPICselect %>% 
  filter(., Age.Group == "10-21")

#write.csv(EPIC1021, "EPIC1021ALLclean.csv", row.names = F)
unique(colnames(EPIC1021))
#library(tidyverse)


## SEX ##
EPIC1021Sex = EPIC1021 %>% 
  group_by(., Sex) %>% 
  summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
            On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
            Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
            Group.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
            LDL.Cat.Pop = 
              sum(!is.na(Date.Last.LDL)), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
            LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
  mutate(., LDL.Cat = "All", Last.LDL.Drawn = "Ever") %>% 
  union_all(
    EPIC1021 %>% 
      group_by(., Sex) %>% 
      summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                Group.Pop.On.Statins.w.LDL.Drawn = 
                  sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                LDL.Cat.Pop = 
                  sum(LDL.Cat == "<100", na.rm=T), 
                LDL.Cat.Pop.On.Statins = 
                  sum(On.Statins == "Yes" & LDL.Cat == "<100", na.rm=T),
                LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
      mutate(., LDL.Cat = "<100", Last.LDL.Drawn = "Ever") %>% 
      union_all(
        EPIC1021 %>% 
          group_by(., Sex) %>% 
          summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                    On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                    Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                    Group.Pop.On.Statins.w.LDL.Drawn = 
                      sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                    LDL.Cat.Pop = 
                      sum(LDL.Cat == "100-129", na.rm=T), 
                    LDL.Cat.Pop.On.Statins = 
                      sum(On.Statins == "Yes" & LDL.Cat == "100-129", na.rm=T),
                    LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                    LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                    On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>% 
          mutate(., LDL.Cat = "100-129", Last.LDL.Drawn = "Ever") %>% 
          union_all(  
            EPIC1021 %>% 
              group_by(., Sex) %>% 
              summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                        On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                        Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                        Group.Pop.On.Statins.w.LDL.Drawn = 
                          sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                        LDL.Cat.Pop = 
                          sum(LDL.Cat == "130-159", na.rm=T), 
                        LDL.Cat.Pop.On.Statins = 
                          sum(On.Statins == "Yes" & LDL.Cat == "130-159", na.rm=T),
                        LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                        LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                        On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>% 
              mutate(., LDL.Cat = "130-159", Last.LDL.Drawn = "Ever") %>% 
              union_all(
                EPIC1021 %>% 
                  group_by(., Sex) %>% 
                  summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                            On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                            Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                            Group.Pop.On.Statins.w.LDL.Drawn = 
                              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                            LDL.Cat.Pop = 
                              sum(LDL.Cat == ">=160", na.rm=T), 
                            LDL.Cat.Pop.On.Statins = 
                              sum(On.Statins == "Yes" & LDL.Cat == ">=160", na.rm=T),
                            LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
                  mutate(., LDL.Cat = ">=160", Last.LDL.Drawn = "Ever") %>% 
                  union_all(
                    EPIC1021 %>% 
                      group_by(., Sex) %>% 
                      summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                                On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                                Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                                Group.Pop.On.Statins.w.LDL.Drawn = 
                                  sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                                LDL.Cat.Pop = 
                                  sum(LDL.100.Split == ">=100", na.rm=T), 
                                LDL.Cat.Pop.On.Statins = 
                                  sum(On.Statins == "Yes" & LDL.100.Split == ">=100", na.rm=T),
                                LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                                LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                                On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
                      mutate(., LDL.Cat = ">=100", Last.LDL.Drawn = "Ever") %>% 
                      union_all(
                        EPIC1021 %>% 
                          group_by(., Sex) %>% 
                          summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                                    On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                                    Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                                    Group.Pop.On.Statins.w.LDL.Drawn = 
                                      sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                                    LDL.Cat.Pop = 
                                      sum(LDL.130.Split == ">=130", na.rm=T), 
                                    LDL.Cat.Pop.On.Statins = 
                                      sum(On.Statins == "Yes" & LDL.130.Split == ">=130", na.rm=T),
                                    LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                                    LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                                    On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
                          mutate(., LDL.Cat = ">=130", Last.LDL.Drawn = "Ever") 
                      ))))))    

EPIC1021Sex = EPIC1021Sex %>% mutate(., Group = "Sex")
EPIC1021Sex = EPIC1021Sex %>% 
  select(., Total.Pop, Group, Sex, Group.Pop, On.Statin.Perc.by.Group.Pop, Last.LDL.Drawn,
         Group.Pop.w.LDL.Drawn, Group.Pop.On.Statins.w.LDL.Drawn, LDL.Cat,  everything())

write.csv(EPIC1021Sex, "EPIC1021Sex.csv", row.names = F)

## RACE ##
EPIC1021Race = EPIC1021 %>% 
  group_by(., Race.Super.Simplified) %>% 
  summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
            On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
            Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
            Group.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
            LDL.Cat.Pop = 
              sum(!is.na(Date.Last.LDL)), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
            LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
  mutate(., LDL.Cat = "All", Last.LDL.Drawn = "Ever") %>% 
  union_all(
    EPIC1021 %>% 
      group_by(., Race.Super.Simplified) %>% 
      summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                Group.Pop.On.Statins.w.LDL.Drawn = 
                  sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                LDL.Cat.Pop = 
                  sum(LDL.Cat == "<100", na.rm=T), 
                LDL.Cat.Pop.On.Statins = 
                  sum(On.Statins == "Yes" & LDL.Cat == "<100", na.rm=T),
                LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
      mutate(., LDL.Cat = "<100", Last.LDL.Drawn = "Ever") %>% 
      union_all(
        EPIC1021 %>% 
          group_by(., Race.Super.Simplified) %>% 
          summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                    On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                    Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                    Group.Pop.On.Statins.w.LDL.Drawn = 
                      sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                    LDL.Cat.Pop = 
                      sum(LDL.Cat == "100-129", na.rm=T), 
                    LDL.Cat.Pop.On.Statins = 
                      sum(On.Statins == "Yes" & LDL.Cat == "100-129", na.rm=T),
                    LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                    LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                    On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>% 
          mutate(., LDL.Cat = "100-129", Last.LDL.Drawn = "Ever") %>% 
          union_all(  
            EPIC1021 %>% 
              group_by(., Race.Super.Simplified) %>% 
              summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                        On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                        Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                        Group.Pop.On.Statins.w.LDL.Drawn = 
                          sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                        LDL.Cat.Pop = 
                          sum(LDL.Cat == "130-159", na.rm=T), 
                        LDL.Cat.Pop.On.Statins = 
                          sum(On.Statins == "Yes" & LDL.Cat == "130-159", na.rm=T),
                        LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                        LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                        On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>% 
              mutate(., LDL.Cat = "130-159", Last.LDL.Drawn = "Ever") %>% 
              union_all(
                EPIC1021 %>% 
                  group_by(., Race.Super.Simplified) %>% 
                  summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                            On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                            Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                            Group.Pop.On.Statins.w.LDL.Drawn = 
                              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                            LDL.Cat.Pop = 
                              sum(LDL.Cat == ">=160", na.rm=T), 
                            LDL.Cat.Pop.On.Statins = 
                              sum(On.Statins == "Yes" & LDL.Cat == ">=160", na.rm=T),
                            LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
                  mutate(., LDL.Cat = ">=160", Last.LDL.Drawn = "Ever") %>% 
                  union_all(
                    EPIC1021 %>% 
                      group_by(., Race.Super.Simplified) %>% 
                      summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                                On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                                Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                                Group.Pop.On.Statins.w.LDL.Drawn = 
                                  sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                                LDL.Cat.Pop = 
                                  sum(LDL.100.Split == ">=100", na.rm=T), 
                                LDL.Cat.Pop.On.Statins = 
                                  sum(On.Statins == "Yes" & LDL.100.Split == ">=100", na.rm=T),
                                LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                                LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                                On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
                      mutate(., LDL.Cat = ">=100", Last.LDL.Drawn = "Ever") %>% 
                      union_all(
                        EPIC1021 %>% 
                          group_by(., Race.Super.Simplified) %>% 
                          summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                                    On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                                    Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                                    Group.Pop.On.Statins.w.LDL.Drawn = 
                                      sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                                    LDL.Cat.Pop = 
                                      sum(LDL.130.Split == ">=130", na.rm=T), 
                                    LDL.Cat.Pop.On.Statins = 
                                      sum(On.Statins == "Yes" & LDL.130.Split == ">=130", na.rm=T),
                                    LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                                    LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                                    On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
                          mutate(., LDL.Cat = ">=130", Last.LDL.Drawn = "Ever") 
                      ))))))    

EPIC1021Race = EPIC1021Race %>% mutate(., Group = "Race")
EPIC1021Race = EPIC1021Race %>% 
  select(., Total.Pop, Group, Race.Super.Simplified, Group.Pop, On.Statin.Perc.by.Group.Pop, Last.LDL.Drawn,
         Group.Pop.w.LDL.Drawn, Group.Pop.On.Statins.w.LDL.Drawn, LDL.Cat,  everything())

write.csv(EPIC1021Race, "EPIC1021Race.csv", row.names = F)

## ETHNICITY ##
EPIC1021Ethnicity = EPIC1021 %>% 
  group_by(., Ethnicity) %>% 
  summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
            On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
            Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
            Group.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
            LDL.Cat.Pop = 
              sum(!is.na(Date.Last.LDL)), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
            LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
  mutate(., LDL.Cat = "All", Last.LDL.Drawn = "Ever") %>% 
  union_all(
    EPIC1021 %>% 
      group_by(., Ethnicity) %>% 
      summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                Group.Pop.On.Statins.w.LDL.Drawn = 
                  sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                LDL.Cat.Pop = 
                  sum(LDL.Cat == "<100", na.rm=T), 
                LDL.Cat.Pop.On.Statins = 
                  sum(On.Statins == "Yes" & LDL.Cat == "<100", na.rm=T),
                LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
      mutate(., LDL.Cat = "<100", Last.LDL.Drawn = "Ever") %>% 
      union_all(
        EPIC1021 %>% 
          group_by(., Ethnicity) %>% 
          summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                    On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                    Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                    Group.Pop.On.Statins.w.LDL.Drawn = 
                      sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                    LDL.Cat.Pop = 
                      sum(LDL.Cat == "100-129", na.rm=T), 
                    LDL.Cat.Pop.On.Statins = 
                      sum(On.Statins == "Yes" & LDL.Cat == "100-129", na.rm=T),
                    LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                    LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                    On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>% 
          mutate(., LDL.Cat = "100-129", Last.LDL.Drawn = "Ever") %>% 
          union_all(  
            EPIC1021 %>% 
              group_by(., Ethnicity) %>% 
              summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                        On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                        Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                        Group.Pop.On.Statins.w.LDL.Drawn = 
                          sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                        LDL.Cat.Pop = 
                          sum(LDL.Cat == "130-159", na.rm=T), 
                        LDL.Cat.Pop.On.Statins = 
                          sum(On.Statins == "Yes" & LDL.Cat == "130-159", na.rm=T),
                        LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                        LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                        On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>% 
              mutate(., LDL.Cat = "130-159", Last.LDL.Drawn = "Ever") %>% 
              union_all(
                EPIC1021 %>% 
                  group_by(., Ethnicity) %>% 
                  summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                            On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                            Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                            Group.Pop.On.Statins.w.LDL.Drawn = 
                              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                            LDL.Cat.Pop = 
                              sum(LDL.Cat == ">=160", na.rm=T), 
                            LDL.Cat.Pop.On.Statins = 
                              sum(On.Statins == "Yes" & LDL.Cat == ">=160", na.rm=T),
                            LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
                  mutate(., LDL.Cat = ">=160", Last.LDL.Drawn = "Ever") %>% 
                  union_all(
                    EPIC1021 %>% 
                      group_by(., Ethnicity) %>% 
                      summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                                On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                                Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                                Group.Pop.On.Statins.w.LDL.Drawn = 
                                  sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                                LDL.Cat.Pop = 
                                  sum(LDL.100.Split == ">=100", na.rm=T), 
                                LDL.Cat.Pop.On.Statins = 
                                  sum(On.Statins == "Yes" & LDL.100.Split == ">=100", na.rm=T),
                                LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                                LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                                On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
                      mutate(., LDL.Cat = ">=100", Last.LDL.Drawn = "Ever") %>% 
                      union_all(
                        EPIC1021 %>% 
                          group_by(., Ethnicity) %>% 
                          summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                                    On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                                    Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                                    Group.Pop.On.Statins.w.LDL.Drawn = 
                                      sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                                    LDL.Cat.Pop = 
                                      sum(LDL.130.Split == ">=130", na.rm=T), 
                                    LDL.Cat.Pop.On.Statins = 
                                      sum(On.Statins == "Yes" & LDL.130.Split == ">=130", na.rm=T),
                                    LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                                    LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                                    On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
                          mutate(., LDL.Cat = ">=130", Last.LDL.Drawn = "Ever") 
                      ))))))    

EPIC1021Ethnicity = EPIC1021Ethnicity %>% mutate(., Group = "Ethnicity")
EPIC1021Ethnicity = EPIC1021Ethnicity %>% 
  select(., Total.Pop, Group, Ethnicity, Group.Pop, On.Statin.Perc.by.Group.Pop, Last.LDL.Drawn,
         Group.Pop.w.LDL.Drawn, Group.Pop.On.Statins.w.LDL.Drawn, LDL.Cat,  everything())

write.csv(EPIC1021Ethnicity, "EPIC1021Ethnicity.csv", row.names = F)

## LANGUAGE ##
EPIC1021Lang = EPIC1021 %>% 
  group_by(., Lang.Simplified) %>% 
  summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
            On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
            Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
            Group.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
            LDL.Cat.Pop = 
              sum(!is.na(Date.Last.LDL)), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
            LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
  mutate(., LDL.Cat = "All", Last.LDL.Drawn = "Ever") %>% 
  union_all(
    EPIC1021 %>% 
      group_by(., Lang.Simplified) %>% 
      summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                Group.Pop.On.Statins.w.LDL.Drawn = 
                  sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                LDL.Cat.Pop = 
                  sum(LDL.Cat == "<100", na.rm=T), 
                LDL.Cat.Pop.On.Statins = 
                  sum(On.Statins == "Yes" & LDL.Cat == "<100", na.rm=T),
                LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
      mutate(., LDL.Cat = "<100", Last.LDL.Drawn = "Ever") %>% 
      union_all(
        EPIC1021 %>% 
          group_by(., Lang.Simplified) %>% 
          summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                    On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                    Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                    Group.Pop.On.Statins.w.LDL.Drawn = 
                      sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                    LDL.Cat.Pop = 
                      sum(LDL.Cat == "100-129", na.rm=T), 
                    LDL.Cat.Pop.On.Statins = 
                      sum(On.Statins == "Yes" & LDL.Cat == "100-129", na.rm=T),
                    LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                    LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                    On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>% 
          mutate(., LDL.Cat = "100-129", Last.LDL.Drawn = "Ever") %>% 
          union_all(  
            EPIC1021 %>% 
              group_by(., Lang.Simplified) %>% 
              summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                        On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                        Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                        Group.Pop.On.Statins.w.LDL.Drawn = 
                          sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                        LDL.Cat.Pop = 
                          sum(LDL.Cat == "130-159", na.rm=T), 
                        LDL.Cat.Pop.On.Statins = 
                          sum(On.Statins == "Yes" & LDL.Cat == "130-159", na.rm=T),
                        LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                        LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                        On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>% 
              mutate(., LDL.Cat = "130-159", Last.LDL.Drawn = "Ever") %>% 
              union_all(
                EPIC1021 %>% 
                  group_by(., Lang.Simplified) %>% 
                  summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                            On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                            Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                            Group.Pop.On.Statins.w.LDL.Drawn = 
                              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                            LDL.Cat.Pop = 
                              sum(LDL.Cat == ">=160", na.rm=T), 
                            LDL.Cat.Pop.On.Statins = 
                              sum(On.Statins == "Yes" & LDL.Cat == ">=160", na.rm=T),
                            LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
                  mutate(., LDL.Cat = ">=160", Last.LDL.Drawn = "Ever") %>% 
                  union_all(
                    EPIC1021 %>% 
                      group_by(., Lang.Simplified) %>% 
                      summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                                On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                                Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                                Group.Pop.On.Statins.w.LDL.Drawn = 
                                  sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                                LDL.Cat.Pop = 
                                  sum(LDL.100.Split == ">=100", na.rm=T), 
                                LDL.Cat.Pop.On.Statins = 
                                  sum(On.Statins == "Yes" & LDL.100.Split == ">=100", na.rm=T),
                                LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                                LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                                On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
                      mutate(., LDL.Cat = ">=100", Last.LDL.Drawn = "Ever") %>% 
                      union_all(
                        EPIC1021 %>% 
                          group_by(., Lang.Simplified) %>% 
                          summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                                    On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                                    Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                                    Group.Pop.On.Statins.w.LDL.Drawn = 
                                      sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                                    LDL.Cat.Pop = 
                                      sum(LDL.130.Split == ">=130", na.rm=T), 
                                    LDL.Cat.Pop.On.Statins = 
                                      sum(On.Statins == "Yes" & LDL.130.Split == ">=130", na.rm=T),
                                    LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                                    LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                                    On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
                          mutate(., LDL.Cat = ">=130", Last.LDL.Drawn = "Ever") 
                      ))))))    

EPIC1021Lang = EPIC1021Lang %>% mutate(., Group = "Language")
EPIC1021Lang = EPIC1021Lang %>% 
  select(., Total.Pop, Group, Lang.Simplified, Group.Pop, On.Statin.Perc.by.Group.Pop, Last.LDL.Drawn,
         Group.Pop.w.LDL.Drawn, Group.Pop.On.Statins.w.LDL.Drawn, LDL.Cat,  everything())

write.csv(EPIC1021Lang, "EPIC1021Lang.csv", row.names = F)

## Medicaid ##
EPIC1021Medicaid = EPIC1021 %>% 
  group_by(., Medicaid) %>% 
  summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
            On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
            Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
            Group.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
            LDL.Cat.Pop = 
              sum(!is.na(Date.Last.LDL)), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
            LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
  mutate(., LDL.Cat = "All", Last.LDL.Drawn = "Ever") %>% 
  union_all(
    EPIC1021 %>% 
      group_by(., Medicaid) %>% 
      summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                Group.Pop.On.Statins.w.LDL.Drawn = 
                  sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                LDL.Cat.Pop = 
                  sum(LDL.Cat == "<100", na.rm=T), 
                LDL.Cat.Pop.On.Statins = 
                  sum(On.Statins == "Yes" & LDL.Cat == "<100", na.rm=T),
                LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
      mutate(., LDL.Cat = "<100", Last.LDL.Drawn = "Ever") %>% 
      union_all(
        EPIC1021 %>% 
          group_by(., Medicaid) %>% 
          summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                    On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                    Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                    Group.Pop.On.Statins.w.LDL.Drawn = 
                      sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                    LDL.Cat.Pop = 
                      sum(LDL.Cat == "100-129", na.rm=T), 
                    LDL.Cat.Pop.On.Statins = 
                      sum(On.Statins == "Yes" & LDL.Cat == "100-129", na.rm=T),
                    LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                    LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                    On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>% 
          mutate(., LDL.Cat = "100-129", Last.LDL.Drawn = "Ever") %>% 
          union_all(  
            EPIC1021 %>% 
              group_by(., Medicaid) %>% 
              summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                        On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                        Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                        Group.Pop.On.Statins.w.LDL.Drawn = 
                          sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                        LDL.Cat.Pop = 
                          sum(LDL.Cat == "130-159", na.rm=T), 
                        LDL.Cat.Pop.On.Statins = 
                          sum(On.Statins == "Yes" & LDL.Cat == "130-159", na.rm=T),
                        LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                        LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                        On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>% 
              mutate(., LDL.Cat = "130-159", Last.LDL.Drawn = "Ever") %>% 
              union_all(
                EPIC1021 %>% 
                  group_by(., Medicaid) %>% 
                  summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                            On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                            Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                            Group.Pop.On.Statins.w.LDL.Drawn = 
                              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                            LDL.Cat.Pop = 
                              sum(LDL.Cat == ">=160", na.rm=T), 
                            LDL.Cat.Pop.On.Statins = 
                              sum(On.Statins == "Yes" & LDL.Cat == ">=160", na.rm=T),
                            LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
                  mutate(., LDL.Cat = ">=160", Last.LDL.Drawn = "Ever") %>% 
                  union_all(
                    EPIC1021 %>% 
                      group_by(., Medicaid) %>% 
                      summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                                On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                                Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                                Group.Pop.On.Statins.w.LDL.Drawn = 
                                  sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                                LDL.Cat.Pop = 
                                  sum(LDL.100.Split == ">=100", na.rm=T), 
                                LDL.Cat.Pop.On.Statins = 
                                  sum(On.Statins == "Yes" & LDL.100.Split == ">=100", na.rm=T),
                                LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                                LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                                On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
                      mutate(., LDL.Cat = ">=100", Last.LDL.Drawn = "Ever") %>% 
                      union_all(
                        EPIC1021 %>% 
                          group_by(., Medicaid) %>% 
                          summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                                    On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                                    Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                                    Group.Pop.On.Statins.w.LDL.Drawn = 
                                      sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                                    LDL.Cat.Pop = 
                                      sum(LDL.130.Split == ">=130", na.rm=T), 
                                    LDL.Cat.Pop.On.Statins = 
                                      sum(On.Statins == "Yes" & LDL.130.Split == ">=130", na.rm=T),
                                    LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                                    LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                                    On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
                          mutate(., LDL.Cat = ">=130", Last.LDL.Drawn = "Ever") 
                      ))))))    

EPIC1021Medicaid = EPIC1021Medicaid %>% mutate(., Group = "Medicaid")
EPIC1021Medicaid = EPIC1021Medicaid %>% 
  select(., Total.Pop, Group, Medicaid, Group.Pop, On.Statin.Perc.by.Group.Pop, Last.LDL.Drawn,
         Group.Pop.w.LDL.Drawn, Group.Pop.On.Statins.w.LDL.Drawn, LDL.Cat,  everything())

write.csv(EPIC1021Medicaid, "EPIC1021Medicaid.csv", row.names = F)

## A1C Groups ##
EPIC1021A1C = EPIC1021 %>% 
  group_by(., A1C.Group) %>% 
  summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
            On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
            Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
            Group.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
            LDL.Cat.Pop = 
              sum(!is.na(Date.Last.LDL)), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
            LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
  mutate(., LDL.Cat = "All", Last.LDL.Drawn = "Ever") %>% 
  union_all(
    EPIC1021 %>% 
      group_by(., A1C.Group) %>% 
      summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                Group.Pop.On.Statins.w.LDL.Drawn = 
                  sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                LDL.Cat.Pop = 
                  sum(LDL.Cat == "<100", na.rm=T), 
                LDL.Cat.Pop.On.Statins = 
                  sum(On.Statins == "Yes" & LDL.Cat == "<100", na.rm=T),
                LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
      mutate(., LDL.Cat = "<100", Last.LDL.Drawn = "Ever") %>% 
      union_all(
        EPIC1021 %>% 
          group_by(., A1C.Group) %>% 
          summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                    On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                    Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                    Group.Pop.On.Statins.w.LDL.Drawn = 
                      sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                    LDL.Cat.Pop = 
                      sum(LDL.Cat == "100-129", na.rm=T), 
                    LDL.Cat.Pop.On.Statins = 
                      sum(On.Statins == "Yes" & LDL.Cat == "100-129", na.rm=T),
                    LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                    LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                    On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>% 
          mutate(., LDL.Cat = "100-129", Last.LDL.Drawn = "Ever") %>% 
          union_all(  
            EPIC1021 %>% 
              group_by(., A1C.Group) %>% 
              summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                        On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                        Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                        Group.Pop.On.Statins.w.LDL.Drawn = 
                          sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                        LDL.Cat.Pop = 
                          sum(LDL.Cat == "130-159", na.rm=T), 
                        LDL.Cat.Pop.On.Statins = 
                          sum(On.Statins == "Yes" & LDL.Cat == "130-159", na.rm=T),
                        LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                        LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                        On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>% 
              mutate(., LDL.Cat = "130-159", Last.LDL.Drawn = "Ever") %>% 
              union_all(
                EPIC1021 %>% 
                  group_by(., A1C.Group) %>% 
                  summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                            On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                            Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                            Group.Pop.On.Statins.w.LDL.Drawn = 
                              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                            LDL.Cat.Pop = 
                              sum(LDL.Cat == ">=160", na.rm=T), 
                            LDL.Cat.Pop.On.Statins = 
                              sum(On.Statins == "Yes" & LDL.Cat == ">=160", na.rm=T),
                            LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
                  mutate(., LDL.Cat = ">=160", Last.LDL.Drawn = "Ever") %>% 
                  union_all(
                    EPIC1021 %>% 
                      group_by(., A1C.Group) %>% 
                      summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                                On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                                Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                                Group.Pop.On.Statins.w.LDL.Drawn = 
                                  sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                                LDL.Cat.Pop = 
                                  sum(LDL.100.Split == ">=100", na.rm=T), 
                                LDL.Cat.Pop.On.Statins = 
                                  sum(On.Statins == "Yes" & LDL.100.Split == ">=100", na.rm=T),
                                LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                                LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                                On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
                      mutate(., LDL.Cat = ">=100", Last.LDL.Drawn = "Ever") %>% 
                      union_all(
                        EPIC1021 %>% 
                          group_by(., A1C.Group) %>% 
                          summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                                    On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                                    Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                                    Group.Pop.On.Statins.w.LDL.Drawn = 
                                      sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                                    LDL.Cat.Pop = 
                                      sum(LDL.130.Split == ">=130", na.rm=T), 
                                    LDL.Cat.Pop.On.Statins = 
                                      sum(On.Statins == "Yes" & LDL.130.Split == ">=130", na.rm=T),
                                    LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                                    LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                                    On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
                          mutate(., LDL.Cat = ">=130", Last.LDL.Drawn = "Ever") 
                      ))))))    

EPIC1021A1C = EPIC1021A1C %>% mutate(., Group = "A1C")
EPIC1021A1C = EPIC1021A1C %>% 
  select(., Total.Pop, Group, A1C.Group, Group.Pop, On.Statin.Perc.by.Group.Pop, Last.LDL.Drawn,
         Group.Pop.w.LDL.Drawn, Group.Pop.On.Statins.w.LDL.Drawn, LDL.Cat,  everything())

write.csv(EPIC1021A1C, "EPIC1021A1C.csv", row.names = F)

unique(colnames(EPIC1021))

EPIC1021MASTER = EPIC1021Sex %>% union_all(
  EPIC1021Race %>% union_all(
    EPIC1021Ethnicity %>%  union_all(
      EPIC1021Lang %>% union_all(
        EPIC1021Medicaid %>% union_all(
          EPIC1021A1C
        )))))
EPIC1021MASTER = EPIC1021MASTER %>% unite("Group.Desc", Sex,Race.Super.Simplified,Ethnicity,Lang.Simplified,Medicaid,A1C.Group,
                        sep = '', na.rm=T, remove=T)


#### 10-21 SUBGROUP SUMMARY STATS ####
EPICdata1021LDLSummary = c("Last.LDL", summary(EPIC1021$Last.LDL), describe(EPIC1021$Last.LDL))
#unique(EPICselect$Age.Group)
EPIC1021Female = EPIC1021 %>% filter(Sex=="Female")
EPIC1021Male = EPIC1021 %>% filter(Sex=="Male")
EPIC1021BlackAfrAm = EPIC1021 %>% filter(Race.Super.Simplified=="Black/African American")
EPIC1021OtherRace = EPIC1021 %>% filter(Race.Super.Simplified=="Other")
EPIC1021White = EPIC1021 %>% filter(Race.Super.Simplified=="White")
EPIC1021Hispanic = EPIC1021 %>% filter(Ethnicity=="Hispanic or Latino")
EPIC1021NotHispanic = EPIC1021 %>% filter(Ethnicity=="Not Hispanic or Latino")
EPIC1021English = EPIC1021 %>% filter(Lang.Simplified=="English")
EPIC1021OtherLang = EPIC1021 %>% filter(Lang.Simplified=="Other")
EPIC1021Spanish = EPIC1021 %>% filter(Lang.Simplified=="Spanish")
EPIC1021Medicaid = EPIC1021 %>% filter(Medicaid=="Medicaid")
EPIC1021OtherIns = EPIC1021 %>% filter(Medicaid=="Other")
EPIC1021A1C7to9 = EPIC1021 %>% filter(A1C.Group=="A1C 7-9")
EPIC1021A1Cbelow7 = EPIC1021 %>% filter(A1C.Group=="A1C<7")
EPIC1021A1Cabove9 = EPIC1021 %>% filter(A1C.Group=="A1C>9")
EPIC1021Statin = EPIC1021 %>% filter(On.Statins=="Yes")
EPIC1021NOStatin = EPIC1021 %>% filter(On.Statins=="No" | is.na(On.Statins))

EPIC1021FemaleLDLSummary = c("Last.LDL", summary(EPIC1021Female$Last.LDL), describe(EPIC1021Female$Last.LDL))
EPIC1021MaleLDLSummary = c("Last.LDL", summary(EPIC1021Male$Last.LDL), describe(EPIC1021Male$Last.LDL))
EPIC1021BlackAfrAmLDLSummary = c("Last.LDL", summary(EPIC1021BlackAfrAm$Last.LDL), describe(EPIC1021BlackAfrAm$Last.LDL))
EPIC1021OtherRaceLDLSummary = c("Last.LDL", summary(EPIC1021OtherRace$Last.LDL), describe(EPIC1021OtherRace$Last.LDL))
EPIC1021WhiteLDLSummary = c("Last.LDL", summary(EPIC1021White$Last.LDL), describe(EPIC1021White$Last.LDL))
EPIC1021HispanicLDLSummary = c("Last.LDL", summary(EPIC1021Hispanic$Last.LDL), describe(EPIC1021Hispanic$Last.LDL))
EPIC1021NotHispanicLDLSummary = c("Last.LDL", summary(EPIC1021NotHispanic$Last.LDL), describe(EPIC1021NotHispanic$Last.LDL))
EPIC1021EnglishLDLSummary = c("Last.LDL", summary(EPIC1021English$Last.LDL), describe(EPIC1021English$Last.LDL))
EPIC1021OtherLangLDLSummary = c("Last.LDL", summary(EPIC1021OtherLang$Last.LDL), describe(EPIC1021OtherLang$Last.LDL))
EPIC1021SpanishLDLSummary = c("Last.LDL", summary(EPIC1021Spanish$Last.LDL), describe(EPIC1021Spanish$Last.LDL))
EPIC1021MedicaidLDLSummary = c("Last.LDL", summary(EPIC1021Medicaid$Last.LDL), describe(EPIC1021Medicaid$Last.LDL))
EPIC1021OtherInsLDLSummary = c("Last.LDL", summary(EPIC1021OtherIns$Last.LDL), describe(EPIC1021OtherIns$Last.LDL))
EPIC1021A1C7to9LDLSummary = c("Last.LDL", summary(EPIC1021A1C7to9$Last.LDL), describe(EPIC1021A1C7to9$Last.LDL))
EPIC1021A1Cbelow7LDLSummary = c("Last.LDL", summary(EPIC1021A1Cbelow7$Last.LDL), describe(EPIC1021A1Cbelow7$Last.LDL))
EPIC1021A1Cabove9LDLSummary = c("Last.LDL", summary(EPIC1021A1Cabove9$Last.LDL), describe(EPIC1021A1Cabove9$Last.LDL))
EPIC1021StatinLDLSummary = c("Last.LDL", summary(EPIC1021Statin$Last.LDL), describe(EPIC1021Statin$Last.LDL))
EPIC1021NOStatinLDLSummary = c("Last.LDL", summary(EPIC1021NOStatin$Last.LDL), describe(EPIC1021NOStatin$Last.LDL))
LDLSummary = c("Last.LDL", summary(EPIC1021Male$Last.LDL), describe(EPIC1021Male$Last.LDL))
LDLSummary = c("Last.LDL", summary(EPIC1021Male$Last.LDL), describe(EPIC1021Male$Last.LDL))

EPICdata1021LDLSummaryALL = list( EPICdata1021LDLSummary,
  EPIC1021MaleLDLSummary, EPIC1021FemaleLDLSummary, EPIC1021BlackAfrAmLDLSummary,
  EPIC1021OtherRaceLDLSummary, EPIC1021WhiteLDLSummary, EPIC1021HispanicLDLSummary,
  EPIC1021NotHispanicLDLSummary, EPIC1021EnglishLDLSummary, EPIC1021OtherLangLDLSummary,
  EPIC1021SpanishLDLSummary, EPIC1021MedicaidLDLSummary, EPIC1021OtherInsLDLSummary,
  EPIC1021A1C7to9LDLSummary, EPIC1021A1Cbelow7LDLSummary, EPIC1021A1Cabove9LDLSummary,
  EPIC1021StatinLDLSummary, EPIC1021NOStatinLDLSummary)

EPIC1021SummarySTATS = plyr::ldply(EPICdata1021LDLSummaryALL, data.frame)
EPIC1021SummarySTATS = EPIC1021SummarySTATS %>% mutate(.,
  Group = c("10-21", rep("Sex", 2), rep("Race", 3), rep("Ethnicity", 2), rep("Pref.Language", 3),
            rep("Medicaid", 2), rep("A1C", 3), rep("Statin", 2)),
  SubGroup = 
   c("ALL", "Female", "Male", "Black/African American", "Other Race", "White", "Hispanic", "Not Hispanic", 
     "English", "Other Lang", "Spanish", "Medicaid", "Other Ins", "7-9", "<7", ">9", "On Statins",
     "Not on Statins"))
EPIC1021SummarySTATS = EPIC1021SummarySTATS %>% select(., Group, SubGroup, everything())
write.csv(EPIC1021SummarySTATS, "EPIC1021SummarySTATS_LastLDL.csv", row.names = F)

#write.csv(EPIC1021MASTER, "EPIC1021MASTER.csv", row.names = F)


##################### MY EXPERIMENTS #######################################

## Pump ##
EPIC1021Sex = EPIC1021 %>% 
  group_by(., Sex) %>% 
  summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
            On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
            Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
            Group.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
            LDL.Cat.Pop = 
              sum(!is.na(Date.Last.LDL)), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
            LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
  mutate(., LDL.Cat = "All", Last.LDL.Drawn = "Ever") %>% 
  union_all(
    EPIC1021 %>% 
      group_by(., Sex) %>% 
      summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                Group.Pop.On.Statins.w.LDL.Drawn = 
                  sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                LDL.Cat.Pop = 
                  sum(LDL.Cat == "<100", na.rm=T), 
                LDL.Cat.Pop.On.Statins = 
                  sum(On.Statins == "Yes" & LDL.Cat == "<100", na.rm=T),
                LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
      mutate(., LDL.Cat = "<100", Last.LDL.Drawn = "Ever") %>% 
      union_all(
        EPIC1021 %>% 
          group_by(., Sex) %>% 
          summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                    On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                    Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                    Group.Pop.On.Statins.w.LDL.Drawn = 
                      sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                    LDL.Cat.Pop = 
                      sum(LDL.Cat == "100-129", na.rm=T), 
                    LDL.Cat.Pop.On.Statins = 
                      sum(On.Statins == "Yes" & LDL.Cat == "100-129", na.rm=T),
                    LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                    LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                    On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>% 
          mutate(., LDL.Cat = "100-129", Last.LDL.Drawn = "Ever") %>% 
          union_all(  
            EPIC1021 %>% 
              group_by(., Sex) %>% 
              summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                        On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                        Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                        Group.Pop.On.Statins.w.LDL.Drawn = 
                          sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                        LDL.Cat.Pop = 
                          sum(LDL.Cat == "130-159", na.rm=T), 
                        LDL.Cat.Pop.On.Statins = 
                          sum(On.Statins == "Yes" & LDL.Cat == "130-159", na.rm=T),
                        LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                        LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                        On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>% 
              mutate(., LDL.Cat = "130-159", Last.LDL.Drawn = "Ever") %>% 
              union_all(
                EPIC1021 %>% 
                  group_by(., Sex) %>% 
                  summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                            On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                            Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                            Group.Pop.On.Statins.w.LDL.Drawn = 
                              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                            LDL.Cat.Pop = 
                              sum(LDL.Cat == ">=160", na.rm=T), 
                            LDL.Cat.Pop.On.Statins = 
                              sum(On.Statins == "Yes" & LDL.Cat == ">=160", na.rm=T),
                            LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
                  mutate(., LDL.Cat = ">=160", Last.LDL.Drawn = "Ever") %>% 
                  union_all(
                    EPIC1021 %>% 
                      group_by(., Sex) %>% 
                      summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                                On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                                Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                                Group.Pop.On.Statins.w.LDL.Drawn = 
                                  sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                                LDL.Cat.Pop = 
                                  sum(LDL.100.Split == ">=100", na.rm=T), 
                                LDL.Cat.Pop.On.Statins = 
                                  sum(On.Statins == "Yes" & LDL.100.Split == ">=100", na.rm=T),
                                LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                                LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                                On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
                      mutate(., LDL.Cat = ">=100", Last.LDL.Drawn = "Ever") %>% 
                      union_all(
                        EPIC1021 %>% 
                          group_by(., Sex) %>% 
                          summarise(., Total.Pop = nrow(.), Group.Pop = n(), 
                                    On.Statin.Perc.by.Group.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Group.Pop,
                                    Group.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
                                    Group.Pop.On.Statins.w.LDL.Drawn = 
                                      sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
                                    LDL.Cat.Pop = 
                                      sum(LDL.130.Split == ">=130", na.rm=T), 
                                    LDL.Cat.Pop.On.Statins = 
                                      sum(On.Statins == "Yes" & LDL.130.Split == ">=130", na.rm=T),
                                    LDL.Cat.Perc.by.Group.Pop = 100*LDL.Cat.Pop/Group.Pop, 
                                    LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Group.Pop.w.LDL.Drawn, 
                                    On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
                          mutate(., LDL.Cat = ">=130", Last.LDL.Drawn = "Ever") 
                      ))))))    

EPIC1021Sex = EPIC1021Sex %>% mutate(., Group = "Sex")
EPIC1021Sex = EPIC1021Sex %>% 
  select(., Total.Pop, Group, Sex, Group.Pop, On.Statin.Perc.by.Group.Pop, Last.LDL.Drawn,
         Group.Pop.w.LDL.Drawn, Group.Pop.On.Statins.w.LDL.Drawn, LDL.Cat,  everything())

write.csv(EPIC1021Sex, "EPIC1021Sex.csv", row.names = F)











write.csv(EPIC1021Sex, "EPIC1021Sex.csv", row.names = F)



  #### OLD VERSION 10-21 DEEP DIVE ####
# % OF PATIENTS WITH LDL IN SELECT RANGE OVER ALL LDLS IN FILE
# GROUPED BY SEX
EPIC1021Sex = 
EPIC1021 %>% 
  filter(!is.na(LDL.Group)) %>% 
  group_by(., LDL.Group, Sex) %>% 
  summarise(., Count = n(), LDL.Perc.Sex = 100*sum(!is.na(LDL.Group))/nrow(.), 
    On.Statin.Perc.Sex = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group))) %>% 
union_all(
EPIC1021 %>% 
  filter(!is.na(LDL.100.Split)) %>% 
  group_by(., Sex) %>% 
  summarise(., Count = n(), LDL.Perc.Sex = 100*sum(LDL.100.Split == ">=100")/nrow(.), 
    On.Statin.Perc.Sex = 100*sum(On.Statins == "Yes" 
              & LDL.100.Split == ">=100")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = ">=100") %>% 
union_all(
EPIC1021 %>% 
  filter(!is.na(LDL.130.Split)) %>% 
  group_by(., Sex) %>% 
  summarise(., Count = n(), LDL.Perc.Sex = 100*sum(LDL.130.Split == ">=130")/nrow(.), 
    On.Statin.Perc.Sex = 100*sum(On.Statins == "Yes" 
              & LDL.130.Split == ">=130")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = ">=130")) %>% 
union_all(
EPIC1021 %>% 
  group_by(., Sex) %>% 
  summarise(., Count = n(), LDL.Perc.Sex = 100*sum(!is.na(LDL.Group))/nrow(.), 
    On.Statin.Perc.Sex = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = "All")))
EPIC1021Sex
#write.csv(EPIC1021Sex,"EPIC1021Sex.csv", row.names = FALSE)

# % OF PATIENTS WITH LDL IN SELECT RANGE OVER ALL LDLS IN FILE
# GROUPED BY A1C.GROUP (<7, 7-9, >9)
EPIC1021A1C = 
EPIC1021 %>% 
  filter(!is.na(LDL.Group) & !is.na(A1C.Group)) %>% 
  group_by(., LDL.Group, A1C.Group) %>% 
  summarise(., Count = n(),  LDL.Perc.A1C = 100*sum(!is.na(LDL.Group))/nrow(.), 
    On.Statin.Perc.A1C = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group)))  %>% 
  arrange(., LDL.Group, match(A1C.Group, c("A1C<7", "A1C 7-9", "A1C>9"))) %>% 
union_all(
EPIC1021 %>% 
  filter(!is.na(LDL.100.Split) & !is.na(A1C.Group)) %>% 
  group_by(., A1C.Group) %>% 
  summarise(., Count = n(), LDL.Perc.A1C = 100*sum(LDL.100.Split == ">=100")/nrow(.), 
    On.Statin.Perc.A1C = 100*sum(On.Statins == "Yes" 
              & LDL.100.Split == ">=100")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = ">=100")  %>% 
  arrange(., LDL.Group, match(A1C.Group, c("A1C<7", "A1C 7-9", "A1C>9"))) %>% 
union_all(
EPIC1021 %>% 
  filter(!is.na(LDL.130.Split) & !is.na(A1C.Group)) %>% 
  group_by(., A1C.Group) %>% 
  summarise(., Count = n(), LDL.Perc.A1C = 100*sum(LDL.130.Split == ">=130")/nrow(.), 
    On.Statin.Perc.A1C = 100*sum(On.Statins == "Yes" 
              & LDL.130.Split == ">=130")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = ">=130")) %>% 
  arrange(., LDL.Group, match(A1C.Group, c("A1C<7", "A1C 7-9", "A1C>9"))) %>% 
union_all(
EPIC1021 %>% 
  filter(., !is.na(A1C.Group)) %>% 
  group_by(., A1C.Group) %>% 
  summarise(., Count = n(), LDL.Perc.A1C = 100*sum(!is.na(LDL.Group))/nrow(.), 
    On.Statin.Perc.A1C = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = "All")) %>% 
  arrange(., LDL.Group, match(A1C.Group, c("A1C<7", "A1C 7-9", "A1C>9"))))
EPIC1021A1C
#write.csv(EPIC1021A1C,"EPIC1021A1C.csv", row.names = FALSE)

# % OF PATIENTS WITH LDL IN SELECT RANGE OVER ALL LDLS IN FILE
# GROUPED BY RACE.SUPER.SIMPLIFIED
EPIC1021Race = 
EPIC1021 %>% 
  filter(!is.na(LDL.Group) & !is.na(Race.Super.Simplified)) %>% 
  group_by(., LDL.Group, Race.Super.Simplified) %>% 
  summarise(., Count = n(), LDL.Perc.Race = 100*sum(!is.na(LDL.Group))/nrow(.), 
    On.Statin.Perc.Race = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group))) %>% 
  arrange(., LDL.Group, desc(Race.Super.Simplified)) %>% 
union_all(
EPIC1021 %>% 
  filter(!is.na(LDL.100.Split) & !is.na(Race.Super.Simplified)) %>% 
  group_by(., Race.Super.Simplified) %>% 
  summarise(., Count = n(), LDL.Perc.Race = 100*sum(LDL.100.Split == ">=100")/nrow(.), 
    On.Statin.Perc.Race = 100*sum(On.Statins == "Yes" 
              & LDL.100.Split == ">=100")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = ">=100")  %>% 
  arrange(., LDL.Group, desc(Race.Super.Simplified)) %>% 
union_all(
EPIC1021 %>% 
  filter(!is.na(LDL.130.Split) & !is.na(Race.Super.Simplified)) %>% 
  group_by(., Race.Super.Simplified) %>% 
  summarise(., Count = n(), LDL.Perc.Race = 100*sum(LDL.130.Split == ">=130")/nrow(.), 
    On.Statin.Perc.Race = 100*sum(On.Statins == "Yes" 
              & LDL.130.Split == ">=130")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = ">=130")) %>% 
  arrange(., LDL.Group, desc(Race.Super.Simplified)) %>% 
union_all(
EPIC1021 %>% 
  group_by(., Race.Super.Simplified) %>% 
  summarise(., Count = n(), LDL.Perc.Race = 100*sum(!is.na(LDL.Group))/nrow(.), 
    On.Statin.Perc.Race = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = "All")))
EPIC1021Race
#write.csv(EPIC1021Race,"EPIC1021Race.csv", row.names = FALSE)

# % OF PATIENTS WITH LDL IN SELECT RANGE OVER ALL LDLS IN FILE
# GROUPED BY ETHNICITY
EPIC1021Ethnicity = 
EPIC1021 %>% 
  filter(!is.na(LDL.Group) & !is.na(Ethnicity)) %>% 
  group_by(., LDL.Group, Ethnicity) %>% 
  summarise(., Count = n(), LDL.Perc.Ethnicity = 100*sum(!is.na(LDL.Group))/nrow(.), 
    On.Statin.Perc.Ethnicity = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group))) %>% 
union_all(
EPIC1021 %>% 
  filter(!is.na(LDL.100.Split) & !is.na(Ethnicity)) %>% 
  group_by(., Ethnicity) %>% 
  summarise(., Count = n(), LDL.Perc.Ethnicity = 100*sum(LDL.100.Split == ">=100")/nrow(.), 
    On.Statin.Perc.Ethnicity = 100*sum(On.Statins == "Yes" 
              & LDL.100.Split == ">=100")/sum(!is.na(LDL.Group))) %>%  
  mutate(., LDL.Group = ">=100") %>% 
union_all(
EPIC1021 %>% 
  filter(!is.na(LDL.130.Split) & !is.na(Ethnicity)) %>% 
  group_by(., Ethnicity) %>% 
  summarise(., Count = n(), LDL.Perc.Ethnicity = 100*sum(LDL.130.Split == ">=130")/nrow(.), 
    On.Statin.Perc.Ethnicity = 100*sum(On.Statins == "Yes" 
              & LDL.130.Split == ">=130")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = ">=130")) %>% 
union_all(
EPIC1021 %>% 
  filter(!is.na(Ethnicity)) %>% 
  group_by(., Ethnicity) %>% 
  summarise(., Count = n(), LDL.Perc.Ethnicity = 100*sum(!is.na(LDL.Group))/nrow(.), 
    On.Statin.Perc.Ethnicity = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = "All")))
EPIC1021Ethnicity
#write.csv(EPIC1021Ethnicity,"EPIC1021Ethnicity.csv", row.names = FALSE)

# % OF PATIENTS WITH LDL IN SELECT RANGE OVER ALL LDLS IN FILE
# GROUPED BY PREFERRED LANGUAGE
EPIC1021Lang = 
EPIC1021 %>% 
  filter(!is.na(LDL.Group) & !is.na(Pref.Language)) %>% 
  group_by(., LDL.Group, Pref.Language) %>% 
  summarise(., Count = n(), LDL.Perc.Lang = 100*sum(!is.na(LDL.Group))/nrow(.), 
    On.Statin.Perc.Lang = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group))) %>% 
union_all(
EPIC1021 %>% 
  filter(!is.na(LDL.100.Split) & !is.na(Pref.Language)) %>% 
  group_by(., Pref.Language) %>% 
  summarise(., Count = n(), LDL.Perc.Lang = 100*sum(LDL.100.Split == ">=100")/nrow(.), 
    On.Statin.Perc.Lang = 100*sum(On.Statins == "Yes" 
              & LDL.100.Split == ">=100")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = ">=100") %>% 
union_all(
EPIC1021 %>% 
  filter(!is.na(LDL.130.Split) & !is.na(Pref.Language)) %>% 
  group_by(., Pref.Language) %>% 
  summarise(., Count = n(), LDL.Perc.Lang = 100*sum(LDL.130.Split == ">=130")/nrow(.), 
    On.Statin.Perc.Lang = 100*sum(On.Statins == "Yes" 
              & LDL.130.Split == ">=130")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = ">=130")) %>% 
union_all(
EPIC1021 %>% 
  group_by(., Pref.Language) %>% 
  summarise(., Count = n(), LDL.Perc.Lang = 100*sum(!is.na(LDL.Group))/nrow(.), 
    On.Statin.Perc.Lang = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = "All"))) 
EPIC1021Lang = EPIC1021Lang %>% filter(!is.nan(On.Statin.Perc.Lang)) %>% 
  arrange(LDL.Group, desc(LDL.Perc.Lang))
tail(EPIC1021Lang)
#write.csv(EPIC1021Lang,"EPIC1021Lang.csv", row.names = FALSE)

# % OF PATIENTS WITH LDL IN SELECT RANGE OVER ALL LDLS IN FILE
# GROUPED BY MEDICAID
EPIC1021Medicaid = 
EPIC1021 %>% 
  filter(!is.na(LDL.Group)) %>% 
  group_by(., LDL.Group, Medicaid) %>% 
  summarise(., Count = n(),  LDL.Perc.Medicaid = 100*sum(!is.na(LDL.Group))/nrow(.), 
    On.Statin.Perc.Medicaid = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group))) %>% 
union_all(
EPIC1021 %>% 
  filter(!is.na(LDL.100.Split)) %>% 
  group_by(., Medicaid) %>% 
  summarise(., Count = n(), LDL.Perc.Medicaid = 100*sum(LDL.100.Split == ">=100")/nrow(.), 
    On.Statin.Perc.Medicaid = 100*sum(On.Statins == "Yes" 
              & LDL.100.Split == ">=100")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = ">=100") %>% 
union_all(
EPIC1021 %>% 
  filter(!is.na(LDL.130.Split)) %>% 
  group_by(., Medicaid) %>% 
  summarise(., Count = n(), LDL.Perc.Medicaid = 100*sum(LDL.130.Split == ">=130")/nrow(.), 
    On.Statin.Perc.Medicaid = 100*sum(On.Statins == "Yes" 
              & LDL.130.Split == ">=130")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = ">=130")) %>% 
union_all(
EPIC1021 %>% 
  group_by(., Medicaid) %>% 
  summarise(., Count = n(), LDL.Perc.Medicaid = 100*sum(!is.na(LDL.Group))/nrow(.), 
    On.Statin.Perc.Medicaid = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = "All")))
EPIC1021Medicaid
#write.csv(EPIC1021Medicaid,"EPIC1021Medicaid.csv", row.names = FALSE)



##### XLSX ATTEMPT - NOT QUITE WORKING####
#options(java.parameters = "-Xmx6000m")
library(xlsx)
#write.xlsx(EPIC1021, file="EPIC1021.xlsx", sheetName="Master", row.names=FALSE)
#write.xlsx(EPIC1021A1C, file="EPIC1021.xlsx", sheetName="A1C", 
                append=TRUE, row.names=FALSE)
#write.xlsx(EPIC1021Ethnicity, file="EPIC1021.xlsx", sheetName="Ethnicity", 
                append=TRUE, row.names=FALSE)
#write.xlsx(EPIC1021Lang, file="EPIC1021.xlsx", sheetName="Pref_Lang", 
                append=TRUE, row.names=FALSE)
#write.xlsx(EPIC1021Medicaid, file="EPIC1021.xlsx", sheetName="Medicaid", 
                append=TRUE, row.names=FALSE)
#write.xlsx(EPIC1021Race, file="EPIC1021.xlsx", sheetName="Race", 
                append=TRUE, row.names=FALSE)
#write.xlsx(EPIC1021Sex, file="EPIC1021.xlsx", sheetName="Sex", 
                append=TRUE, row.names=FALSE)

#######################################################################
########################## OTHER CALCS ################################
#######################################################################

# EPICselect %>%
#   group_by(Age.Group, .drop=FALSE) %>%
#   group_by_drop_default()
# EPICselect %>%
#   group_by(LDL.Group, .drop=FALSE) %>%
#   group_by_drop_default()

##### First Attempt ####
EPICdata = 
  EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Patients = n(), Total.Patients.Perc = 100*n()/nrow(.),
            LDL.on.file.Group.Perc = sum(!is.na(LDL.Group))/n(), 
            On.Statin.LDL.on.file.Perc = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group)),
            On.Statin.Group.Perc = 100*sum(On.Statins == "Yes")/n()) %>% 
  mutate(., LDL.Group = "All", Last.LDL.Drawn = "Ever")

                    # LDL Ever Taken #
EPICdata = 
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total_Patients = n(), LDL.Perc = 100*n()/nrow(.),
    On.Statin.Perc = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = "All", Last.LDL.Drawn = "Ever") %>% 
union_all(
  EPICselect %>% 
  group_by(., LDL.Group, Age.Group) %>% 
  summarise(., Total_Patients = n(), LDL.Perc = 100*n()/nrow(.),
    On.Statin.Perc = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group))) %>% 
  mutate(., Last.LDL.Drawn = "Ever") %>% 
union_all(
EPICselect %>% 
  filter(!is.na(LDL.100.Split)) %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total_Patients = sum(LDL.100.Split == ">=100"), 
            LDL.Perc = 100*sum(LDL.100.Split == ">=100")/nrow(.), 
    On.Statin.Perc = 100*sum(On.Statins == "Yes" 
              & LDL.100.Split == ">=100")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = ">=100", Last.LDL.Drawn = "Ever") %>% 
union_all(
EPICselect %>% 
  filter(!is.na(LDL.130.Split)) %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total_Patients = sum(LDL.130.Split == ">=130"), 
            LDL.Perc = 100*sum(LDL.130.Split == ">=130")/nrow(.), 
    On.Statin.Perc = 100*sum(On.Statins == "Yes" 
              & LDL.130.Split == ">=130")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = ">=130", Last.LDL.Drawn = "Ever") %>% 
  

union_all(
                    #LDL Within 1 Year #
EPICselect %>% 
  filter(Date.Last.LDL > "2019-02-12") %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total_Patients = n(), LDL.Perc = 100*n()/nrow(.),
    On.Statin.Perc = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = "All", Last.LDL.Drawn = "1 year") %>% 
union_all(
  EPICselect %>% 
  filter(Date.Last.LDL > "2019-02-12") %>% 
  group_by(., LDL.Group, Age.Group) %>% 
  summarise(., Total_Patients = n(), LDL.Perc = 100*n()/nrow(.),
    On.Statin.Perc = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group))) %>% 
  mutate(., Last.LDL.Drawn = "1 year") %>% 
union_all(
EPICselect %>% 
  filter(!is.na(LDL.100.Split) & Date.Last.LDL > "2019-02-12") %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total_Patients = sum(LDL.100.Split == ">=100"), 
            LDL.Perc = 100*sum(LDL.100.Split == ">=100")/nrow(.), 
    On.Statin.Perc = 100*sum(On.Statins == "Yes" 
              & LDL.100.Split == ">=100")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = ">=100", Last.LDL.Drawn = "1 year") %>% 
union_all(
EPICselect %>% 
  filter(!is.na(LDL.130.Split) & Date.Last.LDL > "2019-02-12") %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total_Patients = sum(LDL.130.Split == ">=130"), 
            LDL.Perc = 100*sum(LDL.130.Split == ">=130")/nrow(.), 
    On.Statin.Perc = 100*sum(On.Statins == "Yes" 
              & LDL.130.Split == ">=130")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = ">=130", Last.LDL.Drawn = "1 year") %>% 
  
union_all(
                    #LDL Within 3 Years #
EPICselect %>% 
  filter(Date.Last.LDL > "2017-02-12") %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total_Patients = n(), LDL.Perc = 100*n()/nrow(.),
    On.Statin.Perc = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = "All", Last.LDL.Drawn = "3 years") %>% 
union_all(
  EPICselect %>% 
  filter(Date.Last.LDL > "2017-02-12") %>% 
  group_by(., LDL.Group, Age.Group) %>% 
  summarise(., Total_Patients = n(), LDL.Perc = 100*n()/nrow(.),
    On.Statin.Perc = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group))) %>% 
  mutate(., Last.LDL.Drawn = "3 years") %>% 
union_all(
EPICselect %>% 
  filter(!is.na(LDL.100.Split) & Date.Last.LDL > "2017-02-12") %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total_Patients = sum(LDL.100.Split == ">=100"), 
            LDL.Perc = 100*sum(LDL.100.Split == ">=100")/n(), 
            On.Statin.Perc = 100*sum(On.Statins == "Yes" 
              & LDL.100.Split == ">=100")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = ">=100", Last.LDL.Drawn = "3 years") %>% 
union_all(
#### loooooookieeeee herrreeeeeeee
  EPICselect %>% 
  filter(!is.na(LDL.130.Split) & Date.Last.LDL > "2017-02-12") %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total_Patients = sum(LDL.130.Split == ">=130"), 
            LDL.Perc.by.Group = 100*sum(LDL.130.Split == ">=130")/nrow(.), 
            LDL.Perc.by.LDL.O.F = 100*sum(LDL.130.Split == ">=130")/sum(!is.na(LDL.Group)), 
    On.Statin.Perc.by.Group = 100*sum(On.Statins == "Yes" 
              & LDL.130.Split == ">=130")/nrow(.),
    On.Statin.Perc.by.LDL.O.F = 100*sum(On.Statins == "Yes" 
              & LDL.130.Split == ">=130")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = ">=130", Last.LDL.Drawn = "3 years") %>%   
  
  
union_all(
                    #LDL Within 5 Years #
EPICselect %>% 
  filter(Date.Last.LDL > "2015-02-12") %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total_Patients = n(), LDL.Perc = 100*n()/nrow(.),
    On.Statin.Perc = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = "All", Last.LDL.Drawn = "5 years") %>% 
union_all(
  EPICselect %>% 
  filter(Date.Last.LDL > "2015-02-12") %>% 
  group_by(., LDL.Group, Age.Group) %>% 
  summarise(., Total_Patients = n(), LDL.Perc = 100*n()/nrow(.),
    On.Statin.Perc = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group))) %>% 
  mutate(., Last.LDL.Drawn = "5 years") %>% 
union_all(
  EPICselect %>% 
  filter(!is.na(LDL.100.Split) & Date.Last.LDL > "2015-02-12") %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total_Patients = sum(LDL.100.Split == ">=100"), 
            LDL.Perc = 100*n()/nrow(.),
    On.Statin.Perc = 100*sum(On.Statins == "Yes" 
              & LDL.100.Split == ">=100")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = ">=100", Last.LDL.Drawn = "5 years") %>% 
union_all(
EPICselect %>% 
  filter(!is.na(LDL.130.Split) & Date.Last.LDL > "2015-02-12") %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total_Patients = sum(LDL.130.Split == ">=130"), 
            LDL.Perc = 100*sum(LDL.130.Split == ">=130")/nrow(.), 
    On.Statin.Perc = 100*sum(On.Statins == "Yes" 
              & LDL.130.Split == ">=130")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = ">=130", Last.LDL.Drawn = "5 years")
)))
)))
)))
)))
)))
# EPICselect %>% filter(!is.na(LDL.130.Split)) %>% tally()
EPICselect %>% filter(!is.na(LDL.130.Split)
                     & Date.Last.LDL > "2017-02-12") %>% tally()


EPICselect %>% 
  filter(Date.Last.LDL > "2015-02-12") %>% 
  group_by(., LDL.Group, Age.Group) %>% 
  summarise(., Total_Patients = n(), Age_Group_Perc = 100*n()/nrow(.),
            LDL.Perc = 100*sum(!is.na(Last.LDL))/n(),
            On.Statin.Perc = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group))) %>% 
  mutate(., Last.LDL.Drawn = "5 years") 


EPICdata = EPICdata %>% filter(!is.na(LDL.Group)) %>% 
  mutate(., Population.Perc = 100*Total_Patients/7021.0)
tail(EPICdata)
write.csv(EPICdata,"EPICDataALLAnalysis.csv", row.names = FALSE)


##### FULL ANALYSIS v2.0 #####

EPIC_ANALYSIS = 
  # LDL.Last.Drawn = EVER #
EPICselect %>% 
group_by(., Age.Group) %>% 
summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
          On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
          Age.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
          Age.Pop.On.Statins.w.LDL.Drawn = 
            sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
          LDL.Cat.Pop = 
            sum(!is.na(Date.Last.LDL)), 
          LDL.Cat.Pop.On.Statins = 
            sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
          LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
          LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
          On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
mutate(., LDL.Cat = "All", Last.LDL.Drawn = "Ever") %>% 
union_all(
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
            On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
            Age.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
            Age.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
            LDL.Cat.Pop = 
              sum(LDL.Cat == "<100", na.rm=T), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & LDL.Cat == "<100", na.rm=T),
            LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
  mutate(., LDL.Cat = "<100", Last.LDL.Drawn = "Ever") %>% 
  union_all(
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
            On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
            Age.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
            Age.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
            LDL.Cat.Pop = 
              sum(LDL.Cat == "100-129", na.rm=T), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & LDL.Cat == "100-129", na.rm=T),
            LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
  mutate(., LDL.Cat = "100-129", Last.LDL.Drawn = "Ever") %>% 
  union_all(  
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
            On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
            Age.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL), na.rm=T),
            Age.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
            LDL.Cat.Pop = 
              sum(LDL.Cat == "130-159", na.rm=T), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & LDL.Cat == "130-159", na.rm=T),
            LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
  mutate(., LDL.Cat = "130-159", Last.LDL.Drawn = "Ever") %>% 
  union_all(
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
            On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
            Age.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
            Age.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
            LDL.Cat.Pop = 
              sum(LDL.Cat == ">=160", na.rm=T), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & LDL.Cat == ">=160", na.rm=T),
            LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
  mutate(., LDL.Cat = ">=160", Last.LDL.Drawn = "Ever") %>% 
  union_all(
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
            On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
            Age.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
            Age.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
            LDL.Cat.Pop = 
              sum(LDL.100.Split == ">=100", na.rm=T), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & LDL.100.Split == ">=100", na.rm=T),
            LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
  mutate(., LDL.Cat = ">=100", Last.LDL.Drawn = "Ever") %>% 
  union_all(
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
            On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
            Age.Pop.w.LDL.Drawn = sum(!is.na(Date.Last.LDL)),
            Age.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & !is.na(Date.Last.LDL), na.rm=T),
            LDL.Cat.Pop = 
              sum(LDL.130.Split == ">=130", na.rm=T), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & LDL.130.Split == ">=130", na.rm=T),
            LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>% 
  mutate(., LDL.Cat = ">=130", Last.LDL.Drawn = "Ever") %>% 
union_all(
# LDL.Last.Drawn = <1 year #
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
            On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
            Age.Pop.w.LDL.Drawn = sum(Date.Last.LDL > "2019-02-12", na.rm=T),
            Age.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & Date.Last.LDL > "2019-02-12", na.rm=T),
            LDL.Cat.Pop = 
              sum(Date.Last.LDL > "2019-02-12", na.rm=T), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & Date.Last.LDL > "2019-02-12", na.rm=T),
            LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
  mutate(., LDL.Cat = "All", Last.LDL.Drawn = "<1 year") %>% 
  union_all(
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
            On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
            Age.Pop.w.LDL.Drawn = sum(Date.Last.LDL > "2019-02-12", na.rm=T),
            Age.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & Date.Last.LDL > "2019-02-12", na.rm=T),
            LDL.Cat.Pop = 
              sum(LDL.Cat == "<100" & Date.Last.LDL > "2019-02-12", na.rm=T), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & LDL.Cat == "<100" & 
                    Date.Last.LDL > "2019-02-12", na.rm=T),
            LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
  mutate(., LDL.Cat = "<100", Last.LDL.Drawn = "<1 year") %>% 
  union_all(
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
            On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
            Age.Pop.w.LDL.Drawn = sum(Date.Last.LDL > "2019-02-12", na.rm=T),
            Age.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & Date.Last.LDL > "2019-02-12", na.rm=T),
            LDL.Cat.Pop = 
              sum(LDL.Cat == "100-129" & Date.Last.LDL > "2019-02-12", na.rm=T), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & LDL.Cat == "100-129" & 
                    Date.Last.LDL > "2019-02-12", na.rm=T),
            LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
  mutate(., LDL.Cat = "100-129", Last.LDL.Drawn = "<1 year") %>% 
  union_all(  
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
            On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
            Age.Pop.w.LDL.Drawn = sum(Date.Last.LDL > "2019-02-12", na.rm=T),
            Age.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & Date.Last.LDL > "2019-02-12", na.rm=T),
            LDL.Cat.Pop = 
              sum(LDL.Cat == "130-159" & Date.Last.LDL > "2019-02-12", na.rm=T), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & LDL.Cat == "130-159" & 
                    Date.Last.LDL > "2019-02-12", na.rm=T),
            LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
  mutate(., LDL.Cat = "130-159", Last.LDL.Drawn = "<1 year") %>% 
  union_all(
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
            On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
            Age.Pop.w.LDL.Drawn = sum(Date.Last.LDL > "2019-02-12", na.rm=T),
            Age.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & Date.Last.LDL > "2019-02-12", na.rm=T),
            LDL.Cat.Pop = 
              sum(LDL.Cat == ">=160" & Date.Last.LDL > "2019-02-12", na.rm=T), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & LDL.Cat == ">=160" & 
                    Date.Last.LDL > "2019-02-12", na.rm=T),
            LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
  mutate(., LDL.Cat = ">=160", Last.LDL.Drawn = "<1 year") %>% 
  union_all(
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
            On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
            Age.Pop.w.LDL.Drawn = sum(Date.Last.LDL > "2019-02-12", na.rm=T),
            Age.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & Date.Last.LDL > "2019-02-12", na.rm=T),
            LDL.Cat.Pop = 
              sum(LDL.100.Split == ">=100" & Date.Last.LDL > "2019-02-12", na.rm=T), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & LDL.100.Split == ">=100" & 
                    Date.Last.LDL > "2019-02-12", na.rm=T),
            LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
  mutate(., LDL.Cat = ">=100", Last.LDL.Drawn = "<1 year") %>% 
union_all(
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
            On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
            Age.Pop.w.LDL.Drawn = sum(Date.Last.LDL > "2019-02-12", na.rm=T),
            Age.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & Date.Last.LDL > "2019-02-12", na.rm=T),
            LDL.Cat.Pop = 
              sum(LDL.130.Split == ">=130" & Date.Last.LDL > "2019-02-12", na.rm=T), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & LDL.130.Split == ">=130" & 
                    Date.Last.LDL > "2019-02-12", na.rm=T),
            LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>% 
  mutate(., LDL.Cat = ">=130", Last.LDL.Drawn = "<1 year") %>% 
union_all(
    # LDL.Last.Drawn = <3 years #
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
            On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
            Age.Pop.w.LDL.Drawn = sum(Date.Last.LDL > "2017-02-12", na.rm=T),
            Age.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & Date.Last.LDL > "2017-02-12", na.rm=T),
            LDL.Cat.Pop = 
              sum(Date.Last.LDL > "2017-02-12", na.rm=T), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & Date.Last.LDL > "2017-02-12", na.rm=T),
            LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
  mutate(., LDL.Cat = "All", Last.LDL.Drawn = "<3 years") %>% 
union_all(
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
          On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
          Age.Pop.w.LDL.Drawn = sum(Date.Last.LDL > "2017-02-12", na.rm=T),
          Age.Pop.On.Statins.w.LDL.Drawn = 
            sum(On.Statins == "Yes" & Date.Last.LDL > "2017-02-12", na.rm=T),
          LDL.Cat.Pop = 
            sum(LDL.Cat == "<100" & Date.Last.LDL > "2017-02-12", na.rm=T), 
          LDL.Cat.Pop.On.Statins = 
            sum(On.Statins == "Yes" & LDL.Cat == "<100" & 
                  Date.Last.LDL > "2017-02-12", na.rm=T),
          LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
          LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
          On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
   mutate(., LDL.Cat = "<100", Last.LDL.Drawn = "<3 years") %>% 
union_all(
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
            On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
            Age.Pop.w.LDL.Drawn = sum(Date.Last.LDL > "2017-02-12", na.rm=T),
            Age.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & Date.Last.LDL > "2017-02-12", na.rm=T),
            LDL.Cat.Pop = 
              sum(LDL.Cat == "100-129" & Date.Last.LDL > "2017-02-12", na.rm=T), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & LDL.Cat == "100-129" & 
                    Date.Last.LDL > "2017-02-12", na.rm=T),
            LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
    mutate(., LDL.Cat = "100-129", Last.LDL.Drawn = "<3 years") %>% 
union_all(  
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
          On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
          Age.Pop.w.LDL.Drawn = sum(Date.Last.LDL > "2017-02-12", na.rm=T),
          Age.Pop.On.Statins.w.LDL.Drawn = 
            sum(On.Statins == "Yes" & Date.Last.LDL > "2017-02-12", na.rm=T),
          LDL.Cat.Pop = 
            sum(LDL.Cat == "130-159" & Date.Last.LDL > "2017-02-12", na.rm=T), 
          LDL.Cat.Pop.On.Statins = 
            sum(On.Statins == "Yes" & LDL.Cat == "130-159" & 
                  Date.Last.LDL > "2017-02-12", na.rm=T),
          LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
          LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
          On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
mutate(., LDL.Cat = "130-159", Last.LDL.Drawn = "<3 years") %>% 
union_all(
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
            On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
            Age.Pop.w.LDL.Drawn = sum(Date.Last.LDL > "2017-02-12", na.rm=T),
            Age.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & Date.Last.LDL > "2017-02-12", na.rm=T),
            LDL.Cat.Pop = 
              sum(LDL.Cat == ">=160" & Date.Last.LDL > "2017-02-12", na.rm=T), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & LDL.Cat == ">=160" & 
                    Date.Last.LDL > "2017-02-12", na.rm=T),
            LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
  mutate(., LDL.Cat = ">=160", Last.LDL.Drawn = "<3 years") %>% 
  union_all(
EPICselect %>% 
    group_by(., Age.Group) %>% 
    summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
              On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
              Age.Pop.w.LDL.Drawn = sum(Date.Last.LDL > "2017-02-12", na.rm=T),
              Age.Pop.On.Statins.w.LDL.Drawn = 
                sum(On.Statins == "Yes" & Date.Last.LDL > "2017-02-12", na.rm=T),
              LDL.Cat.Pop = 
                sum(LDL.100.Split == ">=100" & Date.Last.LDL > "2017-02-12", na.rm=T), 
              LDL.Cat.Pop.On.Statins = 
                sum(On.Statins == "Yes" & LDL.100.Split == ">=100" & 
                      Date.Last.LDL > "2017-02-12", na.rm=T),
              LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
              LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
              On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
  mutate(., LDL.Cat = ">=100", Last.LDL.Drawn = "<3 years") %>% 
union_all(
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
      On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
      Age.Pop.w.LDL.Drawn = sum(Date.Last.LDL > "2017-02-12", na.rm=T),
      Age.Pop.On.Statins.w.LDL.Drawn = 
        sum(On.Statins == "Yes" & Date.Last.LDL > "2017-02-12", na.rm=T),
      LDL.Cat.Pop = 
        sum(LDL.130.Split == ">=130" & Date.Last.LDL > "2017-02-12", na.rm=T), 
      LDL.Cat.Pop.On.Statins = 
        sum(On.Statins == "Yes" & LDL.130.Split == ">=130" & 
              Date.Last.LDL > "2017-02-12", na.rm=T),
      LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
      LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
      On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>% 
  mutate(., LDL.Cat = ">=130", Last.LDL.Drawn = "<3 years") %>% 
union_all(
# LDL.Last.Drawn = <5 years #
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
            On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
            Age.Pop.w.LDL.Drawn = sum(Date.Last.LDL > "2015-02-12", na.rm=T),
            Age.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & Date.Last.LDL > "2015-02-12", na.rm=T),
            LDL.Cat.Pop = 
              sum(Date.Last.LDL > "2015-02-12", na.rm=T), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & Date.Last.LDL > "2015-02-12", na.rm=T),
            LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%  
  mutate(., LDL.Cat = "All", Last.LDL.Drawn = "<5 years") %>% 
  union_all(
EPICselect %>% 
group_by(., Age.Group) %>% 
summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
          On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
          Age.Pop.w.LDL.Drawn = sum(Date.Last.LDL > "2015-02-12", na.rm=T),
          Age.Pop.On.Statins.w.LDL.Drawn = 
            sum(On.Statins == "Yes" & Date.Last.LDL > "2015-02-12", na.rm=T),
          LDL.Cat.Pop = 
            sum(LDL.Cat == "<100" & Date.Last.LDL > "2015-02-12", na.rm=T), 
          LDL.Cat.Pop.On.Statins = 
            sum(On.Statins == "Yes" & LDL.Cat == "<100" & 
                  Date.Last.LDL > "2015-02-12", na.rm=T),
          LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
          LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
          On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
mutate(., LDL.Cat = "<100", Last.LDL.Drawn = "<5 years") %>% 
  union_all(
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
            On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
            Age.Pop.w.LDL.Drawn = sum(Date.Last.LDL > "2015-02-12", na.rm=T),
            Age.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & Date.Last.LDL > "2015-02-12", na.rm=T),
            LDL.Cat.Pop = 
              sum(LDL.Cat == "100-129" & Date.Last.LDL > "2015-02-12", na.rm=T), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & LDL.Cat == "100-129" & 
                    Date.Last.LDL > "2015-02-12", na.rm=T),
            LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
  mutate(., LDL.Cat = "100-129", Last.LDL.Drawn = "<5 years") %>% 
      union_all(  
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
            On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
            Age.Pop.w.LDL.Drawn = sum(Date.Last.LDL > "2015-02-12", na.rm=T),
            Age.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & Date.Last.LDL > "2015-02-12", na.rm=T),
            LDL.Cat.Pop = 
              sum(LDL.Cat == "130-159" & Date.Last.LDL > "2015-02-12", na.rm=T), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & LDL.Cat == "130-159" & 
                    Date.Last.LDL > "2015-02-12", na.rm=T),
            LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
  mutate(., LDL.Cat = "130-159", Last.LDL.Drawn = "<5 years") %>% 
          union_all(
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
            On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
            Age.Pop.w.LDL.Drawn = sum(Date.Last.LDL > "2015-02-12", na.rm=T),
            Age.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & Date.Last.LDL > "2015-02-12", na.rm=T),
            LDL.Cat.Pop = 
              sum(LDL.Cat == ">=160" & Date.Last.LDL > "2015-02-12", na.rm=T), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & LDL.Cat == ">=160" & 
                    Date.Last.LDL > "2015-02-12", na.rm=T),
            LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
  mutate(., LDL.Cat = ">=160", Last.LDL.Drawn = "<5 years") %>% 
  union_all(
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
            On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
            Age.Pop.w.LDL.Drawn = sum(Date.Last.LDL > "2015-02-12", na.rm=T),
            Age.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & Date.Last.LDL > "2015-02-12", na.rm=T),
            LDL.Cat.Pop = 
              sum(LDL.100.Split == ">=100" & Date.Last.LDL > "2015-02-12", na.rm=T), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & LDL.100.Split == ">=100" & 
                    Date.Last.LDL > "2015-02-12", na.rm=T),
            LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>%   
  mutate(., LDL.Cat = ">=100", Last.LDL.Drawn = "<5 years") %>% 
                  union_all(
EPICselect %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total.Pop = nrow(.), Age.Pop = n(), 
            On.Statin.Perc.by.Age.Pop = 100*sum(On.Statins == "Yes", na.rm=T)/Age.Pop,
            Age.Pop.w.LDL.Drawn = sum(Date.Last.LDL > "2015-02-12", na.rm=T),
            Age.Pop.On.Statins.w.LDL.Drawn = 
              sum(On.Statins == "Yes" & Date.Last.LDL > "2015-02-12", na.rm=T),
            LDL.Cat.Pop = 
              sum(LDL.130.Split == ">=130" & Date.Last.LDL > "2015-02-12", na.rm=T), 
            LDL.Cat.Pop.On.Statins = 
              sum(On.Statins == "Yes" & LDL.130.Split == ">=130" & 
                    Date.Last.LDL > "2015-02-12", na.rm=T),
            LDL.Cat.Perc.by.Age.Pop = 100*LDL.Cat.Pop/Age.Pop, 
            LDL.Cat.Perc.by.LDL.Drawn = 100*LDL.Cat.Pop/Age.Pop.w.LDL.Drawn, 
            On.Statin.Perc.by.LDL.Cat = 100*LDL.Cat.Pop.On.Statins/LDL.Cat.Pop) %>% 
  mutate(., LDL.Cat = ">=130", Last.LDL.Drawn = "<5 years")
))))))
))))))
))))))
))))))
)))


EPIC_ANALYSIS = EPIC_ANALYSIS %>% 
  select(., Total.Pop, Age.Group, Age.Pop, On.Statin.Perc.by.Age.Pop, Last.LDL.Drawn,
         Age.Pop.w.LDL.Drawn, Age.Pop.On.Statins.w.LDL.Drawn, LDL.Cat,  everything())

#write.csv(EPIC_ANALYSIS, "EPIC_ANALYSIS.csv", row.names = F)

EPIC_1075ANALYSIS = EPIC_ANALYSIS %>% 
  filter(., Age.Group != "<10" & Age.Group != "76+" )
#write.csv(EPIC_1075ANALYSIS, "EPIC_1075ANALYSIS.csv", row.names = F) 
#unique(EPIC_1075ANALYSIS$Age.Group)
##### FULL SUMMARY STATS ####

#install.packages("Hmisc")
#install.packages("psych")
#install.packages("pastecs")

library(Hmisc)
library(psych)
library(pastecs)
EPICdataLDLSummary = c("Last.LDL", summary(EPICselect$Last.LDL), describe(EPICselect$Last.LDL))
#unique(EPICselect$Age.Group)
EPICunder10 = EPICselect %>% filter(Age.Group=="<10")
EPIC1021 = EPICselect %>% filter(Age.Group=="10-21")
EPIC2239 = EPICselect %>% filter(Age.Group=="22-39")
EPIC4075 = EPICselect %>% filter(Age.Group=="40-75")
EPICover76 = EPICselect %>% filter(Age.Group=="76+")
EPICStatin = EPICselect %>% filter(On.Statins=="Yes")
EPICNOStatin = EPICselect %>% filter(On.Statins=="No" | is.na(On.Statins))
EPICunder10LDLSummary = c("Last.LDL", summary(EPICunder10$Last.LDL), describe(EPICunder10$Last.LDL))
EPIC1021LDLSummary = c("Last.LDL", summary(EPIC1021$Last.LDL), describe(EPICselect$Last.LDL))
EPIC2239LDLSummary = c("Last.LDL", summary(EPIC2239$Last.LDL), describe(EPIC1021$Last.LDL))
EPIC4075LDLSummary = c("Last.LDL", summary(EPIC4075$Last.LDL), describe(EPIC4075$Last.LDL))
EPICover76LDLSummary = c("Last.LDL", summary(EPICover76$Last.LDL), describe(EPICover76$Last.LDL))
EPICStatinLDLSummary = c("Last.LDL", summary(EPICStatin$Last.LDL), describe(EPICStatin$Last.LDL))
EPICNOStatinLDLSummary = c("Last.LDL", summary(EPICNOStatin$Last.LDL), describe(EPICNOStatin$Last.LDL))

EPICdataLDLSummaryALL = list(
  EPICdataLDLSummary, EPICunder10LDLSummary,  EPIC1021LDLSummary, EPIC2239LDLSummary, 
  EPIC4075LDLSummary, EPICover76LDLSummary, EPICStatinLDLSummary, EPICNOStatinLDLSummary)
install.packages("plyr")
EPICSummarySTATS = plyr::ldply(EPICdataLDLSummaryALL, data.frame)
EPICSummarySTATS = EPICSummarySTATS %>% mutate(., Age.Group = 
    c("ALL", "Under 10", "10-21", "22-39", "40-75", "76+", "On Statins", "Not On Statins"))
EPICSummarySTATS = EPICSummarySTATS %>% select(., Age.Group, everything())
write.csv(EPICSummarySTATS, "EPICSummarySTATS_LastLDL.csv", row.names = F)


#write.csv(EPICunder10LDLSummary,"EPICunder10LDLSummary.csv", row.names = FALSE)
#write.csv(EPIC1021LDLSummary,"EPIC1021LDLSummary.csv", row.names = FALSE)
#write.csv(EPIC2239LDLSummary,"EPIC2239LDLSummary.csv", row.names = FALSE)
#write.csv(EPIC4075LDLSummary,"EPIC4075LDLSummary.csv", row.names = FALSE)
#write.csv(EPICover76LDLSummary,"EPICover76LDLSummary.csv", row.names = FALSE)
# 
# EPICdataSummary = summary(EPICselect) 
# EPICdataDescribe = describe(EPICselect)
# EPICdataDescribe
# EPICdataSummary
# #write.csv(EPICselect,"EPICSelect_ALL_CLEANED_DATA.csv", row.names = FALSE)
# #write.csv(EPICdataDescribe,"EPICDataDescribe.csv", row.names = FALSE)
# #write.csv(EPICdataSummary,"EPICDataSummary.csv", row.names = FALSE)
# #write.csv(EPICdataLDLSummary,"EPICDataLDLSummary.csv", row.names = FALSE)
# 
# EPICdataOUTPUTSummary = summary(EPICdata) 
# EPICdataOUTPUTDescribe = describe(EPICdata)
# #write.csv(EPICdataOUTPUTDescribe,"EPICdataOUTPUTDescribe.csv", row.names = FALSE)
# #write.csv(EPICdataOUTPUTSummary,"EPICdataOUTPUTSummary.csv", row.names = FALSE)
# 
# # EPICdataTEST = 
# # EPICselect %>% 
# #   group_by(., Age.Group) %>% 
# #   summarise(., Total_Patients = n(), LDL.Perc = 100*n()/nrow(.),
# #     On.Statin.Perc = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group))) %>% 
# #   mutate(., LDL.Group = "All", Last.LDL.Drawn = "Ever") %>% 
# # union_all(
# #   EPICselect %>% 
# #   group_by(., LDL.Group, Age.Group) %>% 
# #   summarise(., Total_Patients = n(), LDL.Perc = 100*n()/nrow(.),
# #     On.Statin.Perc = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group))) %>% 
# #   mutate(., Last.LDL.Drawn = "Ever") %>% 
# # union_all(
# # EPICselect %>% 
# #   filter(!is.na(LDL.100.Split)) %>% 
# #   group_by(., Age.Group) %>% 
#   summarise(., Total_Patients = sum(LDL.100.Split == ">=100"), 
#             LDL.Perc = 100*sum(LDL.100.Split == ">=100")/nrow(.), 
#     On.Statin.Perc = 100*sum(On.Statins == "Yes" 
#               & LDL.100.Split == ">=100")/sum(!is.na(LDL.Group))) %>% 
#   mutate(., LDL.Group = ">=100", Last.LDL.Drawn = "Ever") %>% 
# union_all(
# EPICselect %>% 
#   filter(!is.na(LDL.130.Split)) %>% 
#   group_by(., Age.Group) %>% 
#   summarise(., Total_Patients = sum(LDL.130.Split == ">=130"), 
#             LDL.Perc = 100*sum(LDL.130.Split == ">=130")/nrow(.), 
#     On.Statin.Perc = 100*sum(On.Statins == "Yes" 
#               & LDL.130.Split == ">=130")/sum(!is.na(LDL.Group))) %>% 
#   mutate(., LDL.Group = ">=130", Last.LDL.Drawn = "Ever"))))
# EPICdataTEST = EPICdataTEST %>% filter(!is.na(Age.Group))
# EPICdataTEST

