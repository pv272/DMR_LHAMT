#This is what aims to replace the data import part of DMR functions. At the moment I have only transferred what I have had to use (thus not the behaviours) 



# Connect to DB -----------------------------------------------------------

#this functions allows you to connect to the database from R
db_connect<-function(username,local=0){
  library(getPass)
  library(RMySQL)
  
  if(local == 0){
    hostname <- "kalahariresearch.org"
  }
  else{
    hostname <- "192.168.11.6"
  }
  
  con<-dbConnect(MySQL(), user = username,password = getPass(),  
                 dbname = 'Moleratdatabase', host = hostname)
  return(con)
}

#connect to DB, local = 1 if at project or local = 0 if AMS server  
con<-db_connect(username = 'philippev',local=0)



# Extract non-behavioural info from DB -----------------------------------------

#Extract membership from the database
MembershipBetweenV2 <- con %>%
  dbGetQuery ("SELECT 
    AnimalRef,
    AnimalID,
    DATE(MemberFrom) AS MemberFrom,
    DATE(MemberTo) AS MemberTo,
    MembershipBetweenV2.ColonyRef,
    MemberDays,
    MembershipBetweenV2.Colony,
    tblColonyCodes.ColonyLocation
    -- MembershipBetweenV2.CurrentPop -- Not the same as ColonyLocation, as it returns for all rows of membershipV2 where the animal currently is
FROM
    MoleratViews_Pending.MembershipBetweenV2
LEFT JOIN
    Moleratdatabase.tblColonyCodes ON MoleratViews_Pending.MembershipBetweenV2.ColonyRef = tblColonyCodes.ColonyRef
WHERE MembershipBetweenV2.Colony <> 'Exported_Nigel'
AND MembershipBetweenV2.Colony <> 'Exported_Conny'
-- AND MembershipBetweenV2.CurrentPop = 'L'") %>% #CurrentPop indicates where the animal is and not where it was. Therefore if one wants to return groupSize in the past
  mutate(MemberFrom=ymd(MemberFrom),MemberTo=ymd(MemberTo)) %>% 
  select(AnimalRef,AnimalID,MemberFrom,MemberTo,Colony,ColonyLocation)

#AnimalID
tblAnimalID <- con %>%
  dbGetQuery ("SELECT * FROM Moleratdatabase.tblAnimalID")

#Sex
tblSex<-con %>% 
  dbGetQuery("SELECT *
FROM Moleratdatabase.tblSex
") %>% 
  select(AnimalID,Sex)

#Weights
#includes escape weight
tblWeights<-con %>% 
  dbGetQuery("SELECT * FROM user_philippev.Weight_AnimalID") %>% 
  mutate(WeightDate=ymd(WeightDate)) %>% 
  select(AnimalID,WeightDate,Weight,WeightType) %>% 
  filter(!(is.na(Weight)))


#weight extract removing escape weight
# Weight_Extract<-con %>% 
#   dbGetQuery("SELECT * FROM user_philippev.Weight_AnimalID") %>% 
#   mutate(WeightDate=ymd(WeightDate)) %>% 
#   filter(WeightType != "E") %>% 
#   select(AnimalID,WeightDate,Weight,WeightType) %>% 
#   filter(!(is.na(Weight)))


#Birth dates
qry_BirthDate <- con %>%
  dbGetQuery ("SELECT * FROM MoleratViews.qry_BirthDate") %>% 
  rename(BirthDate = LabBirthdate) %>% 
  mutate(BirthDate = ymd(BirthDate))

#death dates
qry_DeathDate <- con %>%
  dbGetQuery ("SELECT * FROM MoleratViews.qry_DeathDate") %>% 
  mutate(DeathDate = ymd(DeathDate))

#tblPairing
#gives pairing date and colony
tblPairing <- con %>%
  dbGetQuery ("SELECT * FROM MR_MainData.tblPairing") %>% 
  mutate(PairingDate = case_when(AnimalID == "DRF015" & Colony == "Roms" | AnimalID == "LAM015" &Colony == "Roms" ~ "2019-03-14",
                                 AnimalID == "WEM011" & Colony == "Vunit" ~ "2019-05-04",
                                 TRUE ~ as.character(PairingDate))) %>% 
  mutate(PairingDate=ymd(PairingDate)) 
View(tblPairing)


#ColonyCodes
tblColonyCodes <- con %>%
  dbGetQuery ("SELECT * FROM Moleratdatabase.tblColonyCodes") %>% 
  select(Colony,ColonyOrigin)


#Litter code
tblLitterCode <- con %>%
  dbGetQuery ("SELECT LitterRef,
MotherID,
DATE(DateOfBirth) AS BirthDate,
Exact
FROM Moleratdatabase.tblLitterCode") %>% 
  mutate(BirthDate = ymd(BirthDate)) %>% 
  filter(!(LitterRef %in% c(31,139))) %>%  #Field population animals
  filter(!(LitterRef %in% c(383,384,392))) #Litters with no date of birth and zero pups. Posted on Gitlab
View(tblLitterCode)


##############From userphilippev

#Experiments
Experiments_SubjectID <- con %>%
  dbGetQuery ("SELECT * FROM user_philippev.Experiments_SubjectID") %>% 
  mutate(ExperimentStartDate = ymd(ExperimentStartDate))


#parentage 
#updated until litter 536, should be updated
PV_Parentage <- con %>%
  dbGetQuery ("SELECT * FROM user_philippev.PV_Parentage") %>% 
  mutate(FatherID = case_when(FatherID == "Z1M007 " ~ "Z1M007",
                              FatherID == "G7M028 " ~ "G7M028",
                              LitterRef == 15 ~ "Z3M010",
                              MotherID == "HEF003" ~ "Unknown",
                              TRUE ~ FatherID))#To correct a wrong entry, will have to be corrected in next update
View(PV_Parentage)


#First Colony
#For Captive bred it will be equal to birth colony
FirstColony <- con %>%
  dbGetQuery ("SELECT AnimalID,
Colony AS FirstColony
FROM MoleratViews_Pending.FirstColony")


#to inform whether samples have already been exported or not
Samples_Exported <- con %>%
  dbGetQuery ("SELECT * FROM user_philippev.Samples_Exported") %>% 
  mutate(Date_Exported = ymd(Date_Exported))
View(Samples_Exported)


# #First Colony: I am not super happy with using that one. Provides way too much info 
# FirstColony <- con %>%
#   dbGetQuery ("SELECT * FROM user_philippev.FirstColony") %>% 
#   mutate(DateMoved = ymd(DateMoved))

#Extract individual characteristics. 
#Regular datacheck must be carried out on this essential query to make sure no animal is missing
# IDCharacteristic_Extract <-
#   con %>% 
#   dbGetQuery("SELECT * FROM user_philippev.ID_Characteristic") %>% 
#   mutate(BirthDate=ymd(BirthDate),DeathDate=ymd(DeathDate),Mother_FirstLitter=ymd(Mother_FirstLitter),Father_FirstLitter=ymd(Father_FirstLitter)) %>% 
#   select(AnimalID,Sex,Wildcaught,WildcaughtQueen,BirthDate,LitterRef,Mother_FirstLitter,Father_FirstLitter,DeathDate,ColonyOrigin)#not sure that death date will often be required



# Database MySQL queries --------------------------------------------------

#Collected Urine samples
Urine_Collected <- con %>%
  dbGetQuery ("SELECT 
Temp.Colony,
Moleratdatabase.tblAnimalID.AnimalID,
MR_MainData.tblUrineSamples.UrineNumber,
UrineCondition.Label,
Moleratdatabase.tblExpDescript.ExpName,
DATE (MR_MainData.tblUrineSamples.Date) AS UrineDate,
MR_MainData.tblUrineSamples.In,
MR_MainData.tblUrineSamples.Out,
TIME (MR_MainData.tblUrineSamples.Delay) AS UrineDelay,
MR_MainData.tblUrineSamples.UrineVolumeCollected AS VolumeCollected
FROM MR_MainData.tblUrineSamples
LEFT JOIN Moleratdatabase.tblAnimalID ON MR_MainData.tblUrineSamples.AnimalRef = tblAnimalID.RowRef
LEFT JOIN Moleratdatabase.tblExpDescript ON MR_MainData.tblUrineSamples.Experiment = tblExpDescript.ExptRef
LEFT JOIN (SELECT *
FROM Moleratdatabase.tblCodeList
WHERE CodeRef= 'UrineCondition') AS UrineCondition ON MR_MainData.tblUrineSamples.`Condition` = UrineCondition.`Value`
LEFT JOIN MoleratViews_Pending.`MemberShipBetween` AS `Temp` ON `MR_MainData`.`tblUrineSamples`.`Date` BETWEEN `Temp`.`MemberFrom` AND `Temp`.`MemberTo` AND `MR_MainData`.`tblUrineSamples`.`AnimalRef` = `Temp`.`AnimalRef`
ORDER BY UrineNumber") %>% 
  mutate(UrineDate = ymd(UrineDate)) %>% 
  mutate(SampleType = "Urine") %>% 
  filter(!(Label %in% c("Stress","Cort","Testo Stress","NebidoPilot","CortOralAppPilot","CortAlloPilotH21A","TSubPilot","CortSubPilot")),
         !(ExpName %in% c("NebidoPilot","CortOralAppPilot","CortAlloPilotH21A","TSubPilot","CortSubPilot")))#I know that I do not want to get urine from stress condition
View(Urine_Collected)



Plasma_Collected <- con %>%
  dbGetQuery ("SELECT 
Moleratdatabase.tblAnimalID.AnimalID,
Temp.Colony, -- return the colony the animal was part of
Moleratdatabase.tblPlasmaSamples.PlasmaNumber AS SampleID,
DATE (Moleratdatabase.tblPlasmaSamples.SampleDate) AS PlasmaDate,
TIME (Moleratdatabase.tblPlasmaSamples.Time) AS PlasmaTime,
Moleratdatabase.tblPlasmaSamples.Delay,
Moleratdatabase.tblPlasmaSamples.Volume AS `Plasma Volume`,
Moleratdatabase.tblExpDescript.ExpName AS PlasmaExp
FROM Moleratdatabase.tblPlasmaSamples 
LEFT JOIN Moleratdatabase.tblAnimalID ON Moleratdatabase.tblPlasmaSamples.AnimalID = tblAnimalID.AnimalID
LEFT JOIN Moleratdatabase.tblExpDescript ON Moleratdatabase.tblPlasmaSamples.Experiment = tblExpDescript.ExptRef
LEFT JOIN Moleratdatabase.tblTeloSample ON Moleratdatabase.tblPlasmaSamples.PlasmaNumber = tblTeloSample.TeloNumber
LEFT JOIN user_philippev.ID_Characteristic ON Moleratdatabase.tblAnimalID.AnimalID = ID_Characteristic.AnimalID
LEFT JOIN MoleratViews_Pending.`MembershipBetweenV2` AS `Temp` ON `Moleratdatabase`.`tblPlasmaSamples`.`SampleDate` BETWEEN `Temp`.`MemberFrom` AND `Temp`.`MemberTo` AND `Moleratdatabase`.`tblPlasmaSamples`.`AnimalID` = `Temp`.`AnimalID`
WHERE Moleratdatabase.tblPlasmaSamples.PlasmaNumber IS NOT NULL
ORDER BY Moleratdatabase.tblPlasmaSamples.PlasmaNumber") %>% 
  mutate(PlasmaDate = ymd(PlasmaDate)) %>% 
  mutate(SampleType = "Plasma") %>% 
  filter(!(PlasmaExp %in% c("NebidoPilot","CortOralAppPilot","CortAlloPilotH21A","TSubPilot","CortSubPilot")))
View(Plasma_Collected)



# Individual Info Generation ----------------------------------------------

nrow(qry_BirthDate)
#1678


#Developing Colony
DevelopingColony <- qry_BirthDate %>% 
  select(-AnimalRef) %>% 
  rename(Date = BirthDate) %>% 
  mutate(Date = Date + 40) %>%  #I assume that after 40 days no animals were moved out of group until they were adult 
  membership(.,MembershipBetweenV2) %>% 
  select(-ColonyLocation,-Date) %>% 
  rename(DevelopingColony = Colony)
View(DevelopingColony)
#1654 entries
#Some animals have no developing colonies a possibility being that they are still developing and birth date + 40 pushes them in the future for which colony is unknown.
  

#Birth Colony
BirthColony <- qry_BirthDate %>% 
  select(-AnimalRef) %>% 
  rename(Date = BirthDate) %>% 
  membership(.,MembershipBetweenV2) %>% 
  select(-ColonyLocation, -Date) %>% 
  filter(Colony != "DEAD") %>% 
  rename(BirthColony = Colony)
View(BirthColony)
#1678 entries, no loss of row


#FirstColony
#For Captive bred it will be equal to birth colony


#Generate AnimalInfo
Animal_Info <-tblAnimalID %>% 
  select(AnimalID,WildCaught,Queen,LitterRef) %>% #add WildCaught, Queen and LitterRef info
  left_join(.,tblSex) %>% 
  left_join(.,qry_BirthDate %>% 
              select(-AnimalRef)) %>% #add BirthDate%>% 
  left_join(.,BirthColony) %>% 
  left_join(.,FirstColony) %>% 
  left_join(.,DevelopingColony) %>% 
  left_join(.,qry_DeathDate %>% 
              select(-AnimalRef)) %>% #add death dates
  mutate(DeathAge = DeathDate-BirthDate) %>%  #add age of death
  relocate(Sex,.after=AnimalID) %>% 
  distinct(AnimalID,.keep_all = TRUE)#to remove duplicated row of animal ID due to animal changing group at the date defined to query developing group
View(Animal_Info)
#returns 3 more rows than in AnimalID



# Pairing Info Generation -------------------------------------------------------

Pairing_Info <- tblPairing %>%
  arrange(PairingDate) %>% 
  distinct(AnimalID,Colony,PairingDate,.keep_all = TRUE) %>% #there are duplicate entries (see below. Have asked Dave to correct)
  #GROUP BY ANIMALID
  group_by(AnimalID) %>% 
  mutate(Pairing_IndividualCount_Total = row_number()) %>% #How many time animals have been paired
  ungroup() %>% 
  #UNGROUP
  #GROUP BY ANIMALID, COLONY 
  group_by(AnimalID, Colony) %>% 
  mutate(Pairing_IndividualCount_Group= row_number()) %>% #How many times animals have been paired in that group
  ungroup() %>%   
  #UNGROUP
  #GROUP BY Colony, DATE
  group_by(Colony) %>% 
  mutate(Pairing_GroupCount = dense_rank(PairingDate)) %>% #How many pairing occured in that group
  ungroup() %>% 
  #UNGROUP
  rename(PairingColony = Colony) %>% 
  mutate(PairingDate = ymd(PairingDate))



# Litter Info Generation -------------------------------------------------------------


# LitterRef/MotherID/ParturitionDate
Litter_Female <- tblLitterCode %>% 
  mutate(MotherID = toupper(MotherID)) %>% 
  rename(AnimalID = MotherID) %>% 
  rename(ParturitionDate = BirthDate) %>% 
  select(-Exact)
View(Litter_Female)

View(Litter_Female %>% 
       filter(is.na(AnimalID)))
#No NA as animal ID


# LitterRef/FatherID/ParturitionDate
# Note there can be several father within a LitterRef
# Note that all litters may not be in PV_Parentage (30/09/2020 all litters since litterRef 533 born in originally WC colony are missing)
Litter_Male <- tblLitterCode %>% 
  inner_join(.,PV_Parentage,by=("LitterRef")) %>% #inner join to avoid creating NA for LitterRef not in PV_Parentage
  select(LitterRef,FatherID,BirthDate) %>% 
  rename(AnimalID = FatherID) %>%
  rename(ParturitionDate = BirthDate) %>% 
  select(LitterRef,AnimalID,ParturitionDate) %>% 
  filter(!is.na(LitterRef), !(AnimalID %in% c("Unknown ", "Unknown"))) %>% 
  distinct() #because 1 father often sire several offsping in a litter
View(Litter_Male)


#Bind rows of female and male LitterRef/AnimalID/ParturitionDate
Litter_MaleFemale <- bind_rows(Litter_Female,Litter_Male)
View(Litter_MaleFemale)
#1056 rows on the 05/10/2020



#Membership of breeders throughout their lives
Breeder_Membership <- Litter_MaleFemale %>%
  distinct(AnimalID) %>% 
  left_join(.,MembershipBetweenV2) %>% 
  left_join(.,tblSex)
View(Breeder_Membership)


#Membership of breeders on the day they produced a litter
Breeders_ParturitionMembership <- Litter_MaleFemale %>% 
  rename(Date = ParturitionDate) %>% 
  membership(.,MembershipBetweenV2) %>% 
  select(-ColonyLocation) %>% 
  left_join(.,tblSex) %>% 
  rename(ParturitionColony = Colony) %>% 
  arrange(ParturitionColony,Date) %>% 
  distinct() %>% 
  filter(!(AnimalID == "BEF003" & Date == "2017-04-21" & ParturitionColony == "Virgin Islands"))
View(Breeders_ParturitionMembership)
#1056 rows, which is one row more. It is because parturition occurred on the day Virgin Island was formed which is annoying
#I decide that I will retain the row where the animal gave birth in BET001 to keep same row nmber


#Data check
#Males that had died at parturition
View(Breeders_ParturitionMembership %>% 
       filter(ParturitionColony == "DEAD"))


#Data check
#Check mismatch Litter_MaleFemale and Breeders_ParturitionMembership
View(anti_join(Litter_MaleFemale,Breeders_ParturitionMembership))
#All good


# Litters conception date
# use 88 as gestation length here
Litters_ConceptionDate <- Litter_MaleFemale %>%
  rename(Date = ParturitionDate) %>%
  conception(.,GestationLength = 88) %>%
  select(LitterRef,AnimalID,ConceptionDate)
View(Litters_ConceptionDate)


# Membership at conception (it still could be that they had emigrated)
Breeders_ConceptionMembership <- Litters_ConceptionDate %>% 
  rename(Date = ConceptionDate) %>% 
  membership(.,MembershipBetweenV2) %>% #get membership at conception
  select(-ColonyLocation) %>% 
  rename(ConceptionDate = Date) %>% 
  rename(ConceptionColony = Colony)
View(Breeders_ConceptionMembership)
#1054 rows loss of 1 row
#This is because L14F011, LitterRef 26 conceived in the field before entering the lab


View(anti_join(Litters_ConceptionDate,Breeders_ConceptionMembership))
#This is because L14F011, LitterRef 26 conceived in the field before entering the lab


#query Pairing date in groups that led females to produce a litter
#I assumed that the gestation length here should be of at least 80 days 
PairingConception <- Litter_MaleFemale %>% 
  left_join(.,Pairing_Info %>% 
              select(AnimalID,PairingDate,PairingColony)) %>% 
  mutate(PairingParturition_DayDiff = ParturitionDate - PairingDate) %>% 
  filter(PairingParturition_DayDiff > 80) %>% #for a pairing to lead to parturition I assumed 80 days at least would have to be passed (could actually be more 85)
  # GROUP BY LITTER REF, ANIMALID
  group_by(LitterRef,AnimalID) %>% 
  slice(which.min(PairingParturition_DayDiff))
ungroup()
# UNGROUP

View(PairingConception)

#################################Join all information
# Sex
# Litter Ref
# Conception date 
# ConceptionMembership 
# LitterRef
# Parturition date 
# Parturition Membership 
# Conception number total (which is equivalent to toal parturion since I neglected abortion)
# Conception number in that group (which is equvalent to parturition in that group)

Litter_Info <- Litter_MaleFemale %>% #Parturition date & LitterRef
  left_join(.,tblSex) %>% #Sex
  left_join(.,PairingConception) %>% #add info about pairing that led to conception. Add about 147 rows, unsure why
  left_join(.,Litters_ConceptionDate) %>% #Conception date
  left_join(.,Breeders_ConceptionMembership) %>% #Conception colony
  #GROUP BY ANIMALID
  group_by(AnimalID) %>% 
  mutate(IndividualConceptionCount_Total = row_number()) %>% #Conception number since first conception
  ungroup() %>% 
  #UNGROUP
  #GROUP BY ANIMALID, CONCEPTION COLONY
  group_by(AnimalID,ConceptionColony) %>% 
  mutate(IndividualConceptionCount_Colony = row_number()) %>% #Conception number since arrival in colony
  ungroup() %>% 
  #UNGROUP
  #GROUP BY CONCEPTION COLONY
  group_by(ConceptionColony) %>% 
  mutate(ColonyConceptionCount_Colony = dense_rank(ConceptionDate)) %>% #Conception number since arrival in colony
  ungroup() %>% 
  #UNGROUP
  left_join(.,Breeders_ParturitionMembership %>% #Parturition Colony
              rename(ParturitionDate = Date) %>% 
              select(-Sex)) %>% #change name to allow join; one row added! To check
  relocate(Sex,.after = AnimalID) %>% 
  relocate(ParturitionDate,.before=ParturitionColony) %>% 
  distinct()
View(Litter_Info)


View(Breeder_Info %>% 
       filter(AnimalID == "G1M026"))



# Urine/Plasma Samples Info Generation ------------------------------------

#Format urine collected to bind with plasma
#Does not contain level = "Stress"
Urine_AnimalID_Date  <- Urine_Collected %>% 
  select(Colony,AnimalID,SampleType,UrineDate,UrineNumber) %>% 
  rename(CollectionDate = UrineDate, SampleID = UrineNumber)
View(Urine_AnimalID_Date)

#Format plasma collected to bind with urine
Plasma_AnimalID_Date <- Plasma_Collected %>% 
  select(Colony,AnimalID,SampleType,PlasmaDate,SampleID) %>% 
  rename(CollectionDate = PlasmaDate)
View(Plasma_AnimalID_Date)


#Join plasma and urine collected 
PlasmaUrine_Collected <- bind_rows(Urine_AnimalID_Date,Plasma_AnimalID_Date) %>% 
  #GROUP BY ANIMALID, COLLECTION DATE, SAMPLE TYPE
  group_by(AnimalID,CollectionDate,SampleType,) %>% 
  mutate(SampleRepeat = row_number()) %>% #generate a number of repeat for that day and sample type
  ungroup()
#UNGROUP
View(PlasmaUrine_Collected)
#35'308 samples collected on the 05/10/2020, after exclusion of stress samples


#Membership of AnimalID on days plasma and urine samples were collected
PlasmaUrine_Membership <- membership(PlasmaUrine_Collected %>% 
                                       distinct(AnimalID,CollectionDate) %>% 
                                       rename(Date = CollectionDate), MembershipBetweenV2) %>% 
  select(-ColonyLocation) %>% 
  rename(Colony_Queried = Colony,
         CollectionDate = Date)
View(PlasmaUrine_Membership)


#Plasma and urine collected on day animals changed groups (thus double Membership)
PlasmaUrine_DoubleMembership <- PlasmaUrine_Membership %>% 
  #GROUP BY 
  group_by(AnimalID,CollectionDate) %>% 
  mutate(ColonyCount = n_distinct(Colony_Queried)) %>% 
  filter(ColonyCount > 1) %>% 
  ungroup() %>% 
#UNGROUP
  mutate(GroupChangeDay = "Yes") %>% 
  select(-ColonyCount,-Colony_Queried)
View(PlasmaUrine_DoubleMembership)
#470 rows (235 animalID/date combinations) for which there is a double entry, meaning sample was collected when animal changed group
#This would cascade in errors in group compositions on the day of sampling


#combine 
PlasmaUrine <- PlasmaUrine_Collected %>% 
  left_join(.,PlasmaUrine_Membership) %>% 
  left_join(.,PlasmaUrine_DoubleMembership) %>% 
  replace_na(list(GroupChangeDay = "No")) %>% 
  relocate(Colony_Queried, .after = Colony) %>% 
  relocate(CollectionDate, .after = AnimalID) %>% 
  relocate(GroupChangeDay, .after = CollectionDate) %>% 
  relocate(SampleID,.after = SampleRepeat)
  

View(PlasmaUrine)




