library(dplyr)
library(ggplot2)
library(lubridate)
library(magrittr)
EPIC = read.csv("EPIC Last LDL and Statin 12 year from 2.12.20 
                    Pulled 5.11.20 FULLY DEIDENTIFIED.csv", 
               stringsAsFactors = F, na.strings = c("NA", " ", ""))
#head(EPIC)
names(EPIC) = make.names(names(EPIC), unique=T)
names(EPIC) = gsub("Plan..Insurance.", "Plan.Insurance", names(EPIC))
names(EPIC) = gsub("X2nd.to.last.A1C", "Second.to.last.A1C", names(EPIC))
names(EPIC) = gsub("Pt..Portal.Status", "Pt.Portal.Status", names(EPIC))
#colnames(EPIC)
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

EPIC$Last.A1C = gsub("%", "", EPIC$Last.A1C, fixed=T)
EPIC$Last.A1C = gsub(">14.0", "14.1", EPIC$Last.A1C, fixed=T)
EPIC$Last.A1C = gsub(">14", "14.1", EPIC$Last.A1C, fixed=T)
EPIC$Last.A1C = gsub("GREATER THAN 14.0", "14.1", EPIC$Last.A1C, fixed=T)
EPIC$Last.A1C = gsub("`", "", EPIC$Last.A1C, fixed=T)
EPIC$Last.A1C = gsub("929", "9.1", EPIC$Last.A1C)
EPIC$Last.A1C = gsub(",.*", "", EPIC$Last.A1C)
EPIC$Last.A1C = as.numeric(EPIC$Last.A1C)

EPIC$Second.to.last.A1C = gsub("0", "15", EPIC$Second.to.last.A1C)
EPIC$Second.to.last.A1C = gsub("202", "8", EPIC$Second.to.last.A1C)
EPIC$Second.to.last.A1C = as.numeric(EPIC$Second.to.last.A1C)

EPIC$Diab.Duration = gsub(" years", "", EPIC$Diab.Duration)
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


EPIC$Pref.Language = gsub("No", NA, EPIC$Pref.Language)
EPIC$Pref.Language = gsub("Other - please contact MI Department at x79800", 
                         "Other", EPIC$Pref.Language)
#unique(EPIC$Pref.Language)


EPIC$CGM.Type = gsub("Other - see comments", "Other", EPIC$CGM.Type)
#unique(EPIC$CGM.Type)


# unique(EPIC$Pump.Brand)
# 
# EPIC %>% filter(., Pump.Brand == "") %>% 
#   summarise(n())

unique(EPIC$Plan.Insurance) 
#Just need Medicare vs all AND medicare + medicaid vs all
EPIC = EPIC %>% 
  mutate(., Medicaid = 
           ifelse(grepl("Medicaid", Plan.Insurance, ignore.case=T), 
           "Medicaid", "Not Medicaid"))

# lapply(EPIC, class)
# unique(EPIC$Last.LDL)
EPIC$Last.LDL = gsub("TNP", NA, EPIC$Last.LDL, ignore.case = T)
EPIC$Last.LDL = gsub("Test not performed", NA, EPIC$Last.LDL, ignore.case = T)
EPIC$Last.LDL = gsub(">400", "401", EPIC$Last.LDL, fixed=T)
EPIC$Last.LDL = gsub("SEE COMMENT", NA, EPIC$Last.LDL, fixed=T)
EPIC$Last.LDL = gsub("SEE COMMENTS", NA, EPIC$Last.LDL, fixed=T)
EPIC$Last.LDL = gsub(" mg/dL", "", EPIC$Last.LDL, fixed=T, ignore.case = T)
EPIC$Last.LDL = gsub("N/A", NA, EPIC$Last.LDL, fixed=T, ignore.case = T)
EPIC$Last.LDL = gsub("96, 109", "103", EPIC$Last.LDL, fixed=T)
EPIC$Last.LDL = gsub("169, 148", "157", EPIC$Last.LDL, fixed=T)
EPIC$Last.LDL = gsub("169, 147", "158", EPIC$Last.LDL, fixed=T)
EPIC$Last.LDL = gsub("117, 102", "110", EPIC$Last.LDL, fixed=T)
EPIC$Last.LDL = gsub("5834", NA, EPIC$Last.LDL)
#EPIC$Last.LDL = gsub("1.67", NA, EPIC$Last.LDL)
EPIC$Last.LDL = gsub(",.*", "", EPIC$Last.LDL)
EPIC$Last.LDL = as.numeric(EPIC$Last.LDL)
#EPIC %>% filter(., Last.LDL<30) %>% group_by(., Last.LDL) %>%  summarise(n())
#unique(EPIC$On.Statins)


EPIC$Last.Cholesterol = gsub("TNP", NA, EPIC$Last.Cholesterol, ignore.case = T)
EPIC$Last.Cholesterol = gsub("ND", NA, EPIC$Last.Cholesterol, ignore.case = T)
EPIC$Last.Cholesterol = gsub(" mg/dL", "", EPIC$Last.Cholesterol, fixed=T)
EPIC$Last.Cholesterol = gsub("<100", "99", EPIC$Last.Cholesterol, fixed=T)
EPIC$Last.Cholesterol = gsub("3", NA, EPIC$Last.Cholesterol)
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
class(EPIC$Last.LDL)
EPIC = EPIC %>% 
  mutate(., Age.Group = 
               ifelse(Age<10, "<10",
               ifelse((Age >=10 & Age <22), "10-21",
               ifelse((Age >=22 & Age <40), "22-39",
               ifelse((Age >=40 & Age <=75), "40-75",
               ifelse(Age>75, "76+", NA))))),
            LDL.Group = 
               ifelse(Last.LDL<100, "<100",
               ifelse((Last.LDL >=100 & Last.LDL <130), "100-129",
               ifelse((Last.LDL >=130 & Last.LDL <159), "130-159",
               ifelse(Last.LDL>=160, "160+", NA)))),
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

EPICselect = EPIC %>% 
  select(., Patient_ID, Age, Age.Group, Diab.Duration, Race, Race.Simplified, 
         Race.Super.Simplified, Ethnicity, 
         Sex, Pref.Language, CGM.Type, Plan.Insurance, Medicaid, On.Statins, 
         Last.LDL, Date.Last.LDL, LDL.Group, LDL.100.Split, LDL.130.Split,
         Last.A1C, Date.Last.A1C, Second.to.last.A1C, A1C.Group,
         Last.Cholesterol, Date.Last.Cholesterol,
         Last.HDL, Date.Last.HDL, Last.Triglycerides, Date.Last.Triglycerides)
#head(EPICselect, 20)
#lapply(EPICselect, class)
unique(EPIC$Plan.Insurance)

EPICselect = EPICselect %>% 
  mutate(., Medicaid =
           ifelse(grepl("Medicaid", Plan.Insurance, ignore.case=T), 
                  "Medicaid", 
                  ifelse(is.na(Plan.Insurance), NA,
                  ifelse(Plan.Insurance == "None", NA, "Not Medicaid"))))
                  
EPICselect$Medicaid %>% filter(Plan.Insurance == "")


write.csv(EPICselect,"EPICselecttest.csv", row.names = FALSE)

#install.packages("xlsx")
library('xlsx')

EPIC1021 = EPICselect %>% 
  filter(., Age.Group == "10-21")

#####################################################################
######################### DEEP DIVE 10-21 ###########################
#####################################################################

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
write.csv(EPIC1021Sex,"EPIC1021Sex.csv", row.names = FALSE)

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
write.csv(EPIC1021A1C,"EPIC1021A1C.csv", row.names = FALSE)

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
write.csv(EPIC1021Race,"EPIC1021Race.csv", row.names = FALSE)

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
write.csv(EPIC1021Ethnicity,"EPIC1021Ethnicity.csv", row.names = FALSE)

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
write.csv(EPIC1021Lang,"EPIC1021Lang.csv", row.names = FALSE)

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
write.csv(EPIC1021Medicaid,"EPIC1021Medicaid.csv", row.names = FALSE)



# XLSX ATTEMPT - NOT QUITE WORKING
#options(java.parameters = "-Xmx6000m")
library(xlsx)
write.xlsx(EPIC1021, file="EPIC1021.xlsx", sheetName="Master", row.names=FALSE)
write.xlsx(EPIC1021A1C, file="EPIC1021.xlsx", sheetName="A1C", 
                append=TRUE, row.names=FALSE)
write.xlsx(EPIC1021Ethnicity, file="EPIC1021.xlsx", sheetName="Ethnicity", 
                append=TRUE, row.names=FALSE)
write.xlsx(EPIC1021Lang, file="EPIC1021.xlsx", sheetName="Pref_Lang", 
                append=TRUE, row.names=FALSE)
write.xlsx(EPIC1021Medicaid, file="EPIC1021.xlsx", sheetName="Medicaid", 
                append=TRUE, row.names=FALSE)
write.xlsx(EPIC1021Race, file="EPIC1021.xlsx", sheetName="Race", 
                append=TRUE, row.names=FALSE)
write.xlsx(EPIC1021Sex, file="EPIC1021.xlsx", sheetName="Sex", 
                append=TRUE, row.names=FALSE)

#######################################################################
########################## OTHER CALCS ################################
#######################################################################
library(magrittr)
library(tidyr)

EPICselect %>%
  group_by(Age.Group, .drop=FALSE) %>%
  group_by_drop_default()
EPICselect %>%
  group_by(LDL.Group, .drop=FALSE) %>%
  group_by_drop_default()

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
            LDL.Perc = 100*sum(LDL.100.Split == ">=100")/nrow(.), 
            On.Statin.Perc = 100*sum(On.Statins == "Yes" 
              & LDL.100.Split == ">=100")/sum(!is.na(LDL.Group))) %>% 
  mutate(., LDL.Group = ">=100", Last.LDL.Drawn = "3 years") %>% 
union_all(
EPICselect %>% 
  filter(!is.na(LDL.130.Split) & Date.Last.LDL > "2017-02-12") %>% 
  group_by(., Age.Group) %>% 
  summarise(., Total_Patients = sum(LDL.130.Split == ">=130"), 
            LDL.Perc = 100*sum(LDL.130.Split == ">=130")/nrow(.), 
    On.Statin.Perc = 100*sum(On.Statins == "Yes" 
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

EPICselect %>% 
  filter(Date.Last.LDL > "2015-02-12") %>% 
  group_by(., LDL.Group, Age.Group) %>% 
  summarise(., Total_Patients = n(), Age_Group_Perc = 100*n()/nrow(.),
  LDL.Perc = 100*sum(!is.na(Last.LDL))/n(),
    On.Statin.Perc = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group))) %>% 
  mutate(., Last.LDL.Drawn = "5 years") 


EPICdata = EPICdata %>% filter(!is.na(LDL.Group)) %>% 
  mutate(., Population.Perc = 100*Total_Patients/6724.0)
tail(EPICdata)
write.csv(EPICdata,"EPICDataALLAnalysis.csv", row.names = FALSE)

#install.packages("Hmisc")
#install.packages("psych")
install.packages("pastecs")
library(Hmisc)
library(psych)
EPICdataLDLSummary = c("Last.LDL", summary(EPICselect$Last.LDL), describe(EPICselect$Last.LDL))
unique(EPICselect$Age.Group)
EPICunder10 = EPICselect %>% filter(Age.Group=="<10")
EPIC1021 = EPICselect %>% filter(Age.Group=="10-21")
EPIC2239 = EPICselect %>% filter(Age.Group=="22-39")
EPIC4075 = EPICselect %>% filter(Age.Group=="40-75")
EPICover76 = EPICselect %>% filter(Age.Group=="76+")
EPICunder10LDLSummary = c("Last.LDL", summary(EPICunder10$Last.LDL), describe(EPICunder10$Last.LDL))
EPIC1021LDLSummary = c("Last.LDL", summary(EPIC1021$Last.LDL), describe(EPICselect$Last.LDL))
EPIC2239LDLSummary = c("Last.LDL", summary(EPIC2239$Last.LDL), describe(EPIC1021$Last.LDL))
EPIC4075LDLSummary = c("Last.LDL", summary(EPIC4075$Last.LDL), describe(EPIC4075$Last.LDL))
EPICover76LDLSummary = c("Last.LDL", summary(EPICover76$Last.LDL), describe(EPICover76$Last.LDL))

write.csv(EPICunder10LDLSummary,"EPICunder10LDLSummary.csv", row.names = FALSE)
write.csv(EPIC1021LDLSummary,"EPIC1021LDLSummary.csv", row.names = FALSE)
write.csv(EPIC2239LDLSummary,"EPIC2239LDLSummary.csv", row.names = FALSE)
write.csv(EPIC4075LDLSummary,"EPIC4075LDLSummary.csv", row.names = FALSE)
write.csv(EPICover76LDLSummary,"EPICover76LDLSummary.csv", row.names = FALSE)

EPICdataSummary = summary(EPICselect) 
EPICdataDescribe = describe(EPICselect)
EPICdataDescribe
EPICdataSummary
write.csv(EPICselect,"EPICSelect_ALL_CLEANED_DATA.csv", row.names = FALSE)
write.csv(EPICdataDescribe,"EPICDataDescribe.csv", row.names = FALSE)
write.csv(EPICdataSummary,"EPICDataSummary.csv", row.names = FALSE)
write.csv(EPICdataLDLSummary,"EPICDataLDLSummary.csv", row.names = FALSE)

EPICdataOUTPUTSummary = summary(EPICdata) 
EPICdataOUTPUTDescribe = describe(EPICdata)
write.csv(EPICdataOUTPUTDescribe,"EPICdataOUTPUTDescribe.csv", row.names = FALSE)
write.csv(EPICdataOUTPUTSummary,"EPICdataOUTPUTSummary.csv", row.names = FALSE)

# EPICdataTEST = 
# EPICselect %>% 
#   group_by(., Age.Group) %>% 
#   summarise(., Total_Patients = n(), LDL.Perc = 100*n()/nrow(.),
#     On.Statin.Perc = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group))) %>% 
#   mutate(., LDL.Group = "All", Last.LDL.Drawn = "Ever") %>% 
# union_all(
#   EPICselect %>% 
#   group_by(., LDL.Group, Age.Group) %>% 
#   summarise(., Total_Patients = n(), LDL.Perc = 100*n()/nrow(.),
#     On.Statin.Perc = 100*sum(On.Statins == "Yes")/sum(!is.na(LDL.Group))) %>% 
#   mutate(., Last.LDL.Drawn = "Ever") %>% 
# union_all(
# EPICselect %>% 
#   filter(!is.na(LDL.100.Split)) %>% 
#   group_by(., Age.Group) %>% 
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
