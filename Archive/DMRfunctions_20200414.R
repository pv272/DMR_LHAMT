
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



# Extract individual info from DB -----------------------------------------

#Extract membership from the database
Membership_Extract <- con %>%
  dbGetQuery ("SELECT 
    AnimalRef,
    AnimalID,
    DATE(MemberFrom) AS MemberFrom,
    DATE(MemberTo) AS MemberTo,
    MemberShipBetween.ColonyRef,
    MemberDays,
     MemberShipBetween.Colony AS QueriedColony,
    tblColonyCodes.ColonyOrigin
FROM
    MoleratViews_Pending.MemberShipBetween
LEFT JOIN
    Moleratdatabase.tblColonyCodes ON MoleratViews_Pending.MemberShipBetween.ColonyRef = tblColonyCodes.ColonyRef
WHERE MemberShipBetween.ColonyRef <> 120 
AND MemberShipBetween.Colony <> 'Exported_Nigel'") %>% 
  mutate(MemberFrom=ymd(MemberFrom),MemberTo=ymd(MemberTo)) %>% 
  select(AnimalRef,AnimalID,MemberFrom,MemberTo,QueriedColony,ColonyOrigin)

#Extract Sex from DB
sex_extract<-con %>% 
  dbGetQuery("SELECT *
FROM Moleratdatabase.tblSex
") %>% 
  select(AnimalID,Sex)

#Extract weight 
Weight_Extract<-con %>% 
  dbGetQuery("SELECT * FROM user_philippev.Weight_AnimalID") %>% 
  mutate(WeightDate=ymd(WeightDate)) %>% 
  select(AnimalID,WeightDate,Weight,WeightType) %>% 
  filter(!(is.na(Weight)))

#weight extract removing escape weight
Weight_Extract<-con %>% 
  dbGetQuery("SELECT * FROM user_philippev.Weight_AnimalID") %>% 
  mutate(WeightDate=ymd(WeightDate)) %>% 
  filter(WeightType != "E") %>% 
  select(AnimalID,WeightDate,Weight,WeightType) %>% 
  filter(!(is.na(Weight)))

#Extract individual characteristics. 
#Regular datacheck must be carried out on this essential query to make sure no animal is missing
IDCharacteristic_Extract <-
  con %>% 
  dbGetQuery("SELECT * FROM user_philippev.ID_Characteristic") %>% 
  mutate(BirthDate=ymd(BirthDate),DeathDate=ymd(DeathDate),Mother_FirstLitter=ymd(Mother_FirstLitter),Father_FirstLitter=ymd(Father_FirstLitter)) %>% 
  select(AnimalID,Sex,Wildcaught,WildcaughtQueen,BirthDate,LitterRef,Mother_FirstLitter,Father_FirstLitter,DeathDate,ColonyOrigin)#not sure that death date will often be required



# Extract scan behaviour info from DB ------------------------------------------

#Extract behaviours that are recorded during scan part of the scan
#drop food was not recorded from the beginning and I am noyt sure since when it has been consistent. Therefore some zeros there will be wrong zeros
Scan_Behav_Cont_Name<-con %>%
  dbGetQuery("SELECT * FROM Moleratdatabase.tblCodeList
WHERE CodeRef = 'ScanContBehav'
ORDER BY `Value`") %>% 
  select(Label) %>% 
  rename(Behaviour=Label)
View(Scan_Behav_Cont_Name)

#Extract scan session details
#I d need to add the start day for day and night observation
 Scan_SessionDetails<- con %>%
  dbGetQuery("SELECT * FROM user_philippev.Scan_SessionDetails") %>% 
   rename(ObsDate=ScanDate) %>% 
   rename(ObsRef=ScanRef) %>% 
   mutate(ObsDate=ymd(ObsDate)) %>% 
  mutate(Colony=ifelse(Colony=="AllColonies","Gemsbok 13",Colony))#This will have to be changed in the database
 View(Scan_SessionDetails)


#Exctract Instantaneous behaviours
 Scan_Behav_Inst<- con %>%
  dbGetQuery("SELECT * FROM user_philippev.Scan_Behaviour_Inst") %>%
  dplyr::select(ScanRef,AnimalID,Label,BehaviourCount) %>% 
  rename(ObsRef=ScanRef) %>% 
  rename(Behaviour=Label)
 names(Scan_Behav_Inst)

 
#Extract continuous behaviours
Scan_Behav_Cont<-con %>% 
  dbGetQuery("SELECT * FROM user_philippev.Scan_Behaviour_Cont") %>% 
  rename(ObsRef=ScanRef)



# Extract Focal Behaviour info from DB ------------------------------------

#Extract behaviours that are recorded during the focal Observation
#There are names and / and " " in the name. Leave like this for now
#drop food was not recorded from the beginning and I am noyt sure since when it has been consistent. Therefore some zeros there will be wrong zeros

Focal_Behav_Name<-con %>%
  dbGetQuery("SELECT tblCodeList.* FROM Moleratdatabase.tblCodeList
WHERE CodeRef = 'FocalBehav'
ORDER BY 'Value'") %>% 
  select(Label) %>% 
  rename(Behaviour=Label) %>% 
  mutate(BehaviourType = ifelse(Behaviour %in% c("Dig","Kick","Eat", "Gnaw", "Groom", "Huddeling", "Miscellaneous", "Nest Building", "Rest", "Self groom", "Sex forplay", "Sparr", "Sweep", "Carry food", "Carry NM", "Beg/Suckle","Out of sight"), "State","Point")) #this has to be checked from the data. Also it looks like to me that drop food is absent. Is it actually in the database or?
  

# Behaviours with duration: "Back Sparr", "Eat", "Gnaw", "Groom", "Huddeling", "Miscellaneous", "Nest Building", "Rest", "Self groom", "Sex forplay", "Sparr", "Sweep", "Carry food", "Carry NM", "Beg/Suckle"

#"Out of sight" to deal with separately and remove from the duration length 


# Pump

#Pass Interesting for network analyses, perhaps makes distinction for the type of call

#Sniff

#Submiss call interesting for networ analyses

#Bite Interesting for network analyses

#Pull tail Interesting for network analyses

#Retreat Interesting for network analyses or dominance

#Chase Interesting for network analyses or dominance

#Overt aggression # Interesting for network analyses

#Copulation interesting for network analyses, perhaps we want make a distinction between succesful and not succesful 

#Begging Call not really interesting

#Unknown call not really interesting

#Carry pup I think for this I shall use pup carry test anyway



#Extract Focal session details
#as a note there are quite a lot of focal for which Observed duration is null and they are still shown here. I will have to deal with them separately
Focal_SessionDetails<-con %>%
  dbGetQuery("SELECT * FROM user_philippev.Focal_SessionDetails") %>% 
  rename(ObsRef=FocalRef) %>% 
  mutate(ObsDate=ymd(ObsDate)) %>% 
  mutate(OutOfSight=as.integer(replace_na(OutOfSight,0))) %>% #to assign zero to animals that were not seen out of sight
  mutate(TotalSeen = ObservedDuration - OutOfSight)#return NA if the ObservedDuration was null


#Extract Focal Behaviours
Focal_Behav<-con %>%
  dbGetQuery("SELECT * FROM user_philippev.Focal_Behaviour") %>% 
  select(-AnimalID,-Colony,-StartDate,-StartTime) %>%  #remove all the information that are redundant with Focal Session details 
  rename(Behaviour = Label) %>% 
  rename(Behav_Duration=TotalDuration) %>% 
  rename(ObsRef=FocalRef) %>% 
  inner_join(.,Focal_Behav_Name)#to add whether the behaviour is state or point


#Huddeling, groom and suckle are 3 state events that can also be received?
View(Focal_Behav %>% 
  filter(!is.na(Behav_Duration),!is.na(Received)) %>% 
    distinct(Label))

#need check whether there are any state events that have been recorded but that have no duration
#There are 3 focals that have state behaviours with no duration and they should be excluded from analyses 
View(Poop %>% filter(is.na(Behav_Duration)))




# Function to query Animal and Group Characteritics -----------------------

#Get the colony an animal was part of at any given date 
#DF1 is a dataframe with 2 column. The first column is AnimalId and the second is Date (must be spelt like that)
#DF2 is the membership table from the DB 
#will return two colonies if an animal was part of 2 colonies on the same day
membership <- function(DF1, DF2) {
  inner_join(DF1 %>% distinct (AnimalID,Date) #one only wants one colony for each day as individual cannot be measure in two colonies simultaneously
             , DF2, by = "AnimalID") %>%
    filter(Date >= MemberFrom & Date <= MemberTo)%>% 
    select(-c(MemberFrom,MemberTo,AnimalRef))
}


#get_groupcomp(), extract the group composition of the animals which colony has been established 
#DF1 is the dataframe where a date and a colony are provided and that has has been obtained by Colony()
#DF2 is Membership object obtained from the database
groupcomp<-function(DF1,DF2){
  inner_join(DF1 %>% 
  distinct(Date,Colony),DF2) %>% 
  filter(Date >= MemberFrom & Date <=MemberTo) %>% 
  select(Colony,AnimalID,Date)
}


#get_closestweight() returns the closest weight of all individuals present in a colony at a given time. It will only returns a row if a date was collected because a time difference cannot be computed without a date of weight
#DF1 is a dataframe with a column Date and a column AnimalId. will return the closest weight of all group members at a given date if use output groupcomp()
#DF2 is the weight extracted from the database

#TO BE CHANGED the function should return the date weight if possible but that may be difficult if several date of weight, Perhaps should split the function in two: weight after and weight before?
closestweight<-function(DF1,DF2){
inner_join(DF1 ,DF2, by="AnimalID") %>%
mutate(DayDiff = abs(round(difftime(WeightDate,Date,units="days")))) %>% 
group_by(AnimalID,Date) %>% 
filter(DayDiff == min(DayDiff)) %>%
ungroup() %>% 
group_by(AnimalID,Date,DayDiff) %>% 
summarise(Weight=mean(Weight)) %>% 
ungroup() %>% 
rename(WeightDayDiff=DayDiff) %>% 
arrange(AnimalID,Date)
}

closestweight_after<-function(DF1,DF2){
  inner_join(DF1 ,DF2, by="AnimalID") %>%
    mutate(DayDiff = round(difftime(WeightDate,Date,units="days"))) %>% 
    filter(DayDiff >= 0) %>% 
    group_by(AnimalID,Date) %>% 
    filter(DayDiff == min(DayDiff)) %>%
    ungroup() %>% 
    group_by(AnimalID,Date,DayDiff) %>% 
    summarise(Weight=mean(Weight)) %>% 
    ungroup() %>% 
    rename(WeightDayDiff=DayDiff) %>% 
    arrange(AnimalID,Date)
}

closestweight_before<-function(DF1,DF2){
  inner_join(DF1 ,DF2, by="AnimalID") %>%
    mutate(DayDiff = round(difftime(WeightDate,Date,units="days"))) %>% 
    filter(DayDiff <= 0) %>% 
    group_by(AnimalID,Date) %>% 
    filter(DayDiff == max(DayDiff)) %>%
    ungroup() %>% 
    group_by(AnimalID,Date,DayDiff) %>% 
    summarise(Weight=mean(Weight)) %>% 
    ungroup() %>% 
    rename(WeightDayDiff=DayDiff) %>% 
    arrange(AnimalID,Date)
}





#IDinfo() extract relevant information from an individual at a given date
#DF1 provides the AnimalID and dates. will return the info of all group members at a given date if use output groupcomp() as function input
#DF2 is the individual info extracted from the database
#the breeding status has yet to include the paternity of the wild-caught colony. 
#Perhaps input shall be Date, AnimalId and Colony as this may be required to know whether a male is breeding?
#One could also make use of the pairing table if one want to use paired as a status
#I must check the validity of the breeding status function

idinfo<-function(DF1,DF2,GestationLength = 105){
  inner_join(DF1,DF2) %>% 
    ###AGE
    mutate(Age=round(difftime(Date,BirthDate,units="days")),
           DeathAge=round(difftime(DeathDate,BirthDate,units="days"))) %>% 
    ### BREEDING STATUS
    mutate(BreedingStatus = ifelse((WildcaughtQueen == 1 |
                                      (!is.na(Mother_FirstLitter) & ((Mother_FirstLitter - GestationLength) < Date))|
                                      (!is.na(Father_FirstLitter) & ((Father_FirstLitter - GestationLength) < Date))),
                                   "Breeder",
                                   "Helper")) %>% #This rely on the update of PV_Parentage in the DB since it is where Father_FirstLitter come from
    select(AnimalID,Date,Sex,Wildcaught,LitterRef,Age,BreedingStatus,DeathAge)
}



#gestation() returns the conception dat
#DF1 is a list of animalID and Date of birth
conception<-function(DF1, GestationLength = 105){DF1 %>%
    muatate(ConceptionDate = Date -GestationLength)
}





#GroupID_info() gets additional information on group composition 
#DF1 is the output from IDInfo()
#I could probably use a if condition as shown just after  

idgroup_info<-function(DF1){DF1 %>%
  #GROUP BY COLONY AND DATE FOR FOLLOWING MUTATE CALLS: GET INDIVIDUAL AND GROUP CHARACTERISTIC 
  group_by(Colony,Date) %>% 
  mutate(GroupSize=n()) %>% # group size
  mutate(WeightRank=min_rank(desc(Weight)), 
         AgeRank=min_rank(desc(Age))) %>% # Weight rank, check what it does with NA
  mutate(CompNB_5=sapply(Weight, function(x) sum(abs(x-Weight)<5)-1),
         CompNB_10=sapply(Weight, function(x) sum(abs(x-Weight)<10)-1),
         CompNB_15=sapply(Weight, function(x) sum(abs(x-Weight)<15)-1), 
         CompNB_20=sapply(Weight, function(x) sum(abs(x-Weight)<20)-1)) %>% 
  mutate(PupNB=sum(Age<1, na.rm= TRUE)) %>% 
  mutate(PupPresence=ifelse(PupNB==0,"No","Yes")) %>% 
  mutate(MinAge=min(Age,na.rm= TRUE)) %>% 
  ungroup() %>% 
  # UNGROUP
  # GROUP BY DATE, COLONY AND SEX FOR FOLLOWING MUTATE CALLS
  group_by(Date,Colony,Sex) %>% 
  mutate(QueueSize=n()) %>% #queue size, that is number of males and females
  mutate(WeightRank_Queue=min_rank(desc(Weight))) %>% #Weight rank
  mutate(CompNB_5_Queue=sapply(Weight, function(x) sum(abs(x-Weight)<5)-1),
         CompNB_10_Queue=sapply(Weight, function(x) sum(abs(x-Weight)<10)-1),
         CompNB_15_Queue=sapply(Weight, function(x) sum(abs(x-Weight)<15)-1), 
         CompNB_20_Queue=sapply(Weight, function(x) sum(abs(x-Weight)<20)-1)) %>% 
  ungroup() 
  # UNGROUP
}





# Function to query count in scan obs -------------------------------------


#scan_behaviour_count() returns the count of behaviours in the scan observation for both the instantaneous and the continuous part. It doesn't show the details of social interactions but simply generate a data summary

#Function input: DF is a list of ScanRef. The function also requires the "Scan_Behav_Inst" and "Scan_Behav_Cont" from user_philippev

#There may be an issue with the way I counted the behavioursin the continuous part because i) every interactions (regardless of direction) generates a count of 2 (one animal give and the other one receives, or the account in the total of the two animal)
#Pass, sparr, sex forplay?
#pump., coprophagy, nest build, drop fs, have no modifier AnimalID, thus only use G_

#The function should only return the behaviour and their count. The scan session details as they will be joined on Scan_Session_Info


scan_behaviour_count<-function(DF){
  
  # generate combination ScanRef/ID/Cont_Behaviour -------------------------------
  
  Scan_Behav_Cont_Combination<-merge(inner_join(DF,Scan_Behav_Inst) %>% distinct(ObsRef,AnimalID),Scan_Behav_Cont_Name) %>% 
    mutate(ObsType="ScanCont") %>% 
    arrange(ObsRef,ObsType,AnimalID,Behaviour) %>% 
    select(ObsRef,ObsType,AnimalID,Behaviour) %>% 
    arrange(ObsRef,AnimalID,Behaviour)
  
  
  # Generate Count Instantaneous Part ---------------------------------------
  
  Scan_Behav_Inst_Count<-inner_join(DF,Scan_Behav_Inst) %>%
    mutate(ObsType="ScanInst") %>% #Type of obs
    mutate(Received="G") %>% #Behaviour given
    select(ObsRef,ObsType,AnimalID,Behaviour,Received,BehaviourCount) %>% 
    #GROUP BY EVERYTHING BUT COUNT to have total of social interactions
    group_by_at(names(.)[-grep("BehaviourCount", names(.))])%>% #This is to group by all BUT
    summarise(Count=sum(BehaviourCount)) %>% 
    ungroup() %>% 
    #UNGROUP
    #GROUP BY OBSREF and ANIMALID
    group_by(ObsRef,AnimalID) %>% 
    spread(.,Behaviour,Count,fill = 0) %>% 
    ungroup() %>% 
    #UNGROUP
    rename_all(list(~make.names(.))) %>% #replace space by . in column names. Not sure how to do that with underescore
    mutate(Locomotion=Locomotion-LocoWork) %>% #this is because in the datasummary, locomotion includes loco work
    mutate(Help.Burrow=Dig+Sweep+Kick+LocoWork) %>% 
    mutate(Active.NoHelp=Gnaw+Other+Pump+Self.Groom+Sniff+Locomotion+Social.Interact+Eat) %>% #I have included Gnaw here. Pay attention as there may be discrepancies with functions in other DMR functions files in other R directoru
    mutate(Help.Total=Dig+Sweep+Kick+Food.Carry+Nest.Material+LocoWork+Pup.Carry) %>%
    mutate(TotActive= Dig+Eat+Food.Carry+Gnaw+Kick+Locomotion+LocoWork+Nest.Material+Other+Pump+Pup.Carry+Self.Groom+Sniff+Social.Interact+Sweep) %>% #I had to add the behaviours since I did not want to join to have the total number of scan
    mutate(TotalScanNumber = Gnaw+Other+Pump+Self.Groom+Sniff+Locomotion+Social.Interact+Eat+Dig+Sweep+Kick+Food.Carry+Nest.Material+LocoWork+Pup.Carry+Rest+Huddeling) %>% #TotalScanNumber must be computed and not taken from Scan sessions because in scan session the scan where no behaviours may have been recorded are included
    gather("Behaviour","Count",c(Dig:TotActive)) %>% 
    arrange(ObsRef,AnimalID,Behaviour)
  
  
  # Generate Count of Continuous Part ------------------------------------------
  
  #Behaviour for which animal was recorded as Focal
  Scan_Behav_Cont_Focal_Count<-inner_join(DF,Scan_Behav_Cont) %>% 
    mutate(ObsType="ScanCont") %>%
    #GROUP BY ScanRef, AnimalID, BehaviourName
    group_by(ObsRef,ObsType,AnimalID,Behaviour) %>% 
    summarise(G_Count=sum(BehaviourCount)) %>% 
    ungroup() 
  #UNGROUP
  
  #Behaviour for which animal was recorded as modifier 
  Scan_Behav_Cont_Partner_Count<- inner_join(DF,Scan_Behav_Cont) %>% 
    select(-AnimalID) %>% 
    filter(!is.na(PartnerID)) %>% 
    rename(AnimalID=PartnerID) %>% 
    mutate(ObsType="ScanCont") %>%
    #GROUP BY ScanRef, PartnerID, BehaviourName
    group_by(ObsRef,ObsType,AnimalID,Behaviour) %>% 
    summarise(R_Count=sum(BehaviourCount)) %>% 
    ungroup()
  #UNGROUP
  
  
  #  Join counts of Focal and Modifier of continuous -----------------
  
  Scan_Behav_Cont_Count<-full_join(Scan_Behav_Cont_Focal_Count,Scan_Behav_Cont_Partner_Count) %>% replace_na(list(G_Count = 0, R_Count = 0)) %>% #replace the NA with a 0
    mutate(T_Count=G_Count+R_Count) %>% 
    select(ObsRef,ObsType,AnimalID,Behaviour,G_Count,R_Count,T_Count)
  
  
  # Generate zero of continuous Behaviour -----------------------------------
  
  Scan_Behav_Cont_Count_WithZero<-left_join(Scan_Behav_Cont_Combination,Scan_Behav_Cont_Count) %>% 
    replace_na(list(G_Count = 0, R_Count = 0,T_Count=0))
  
  
  
  # Generate long format of Continuous Behaviour count ----------------------
  
  Scan_Behav_Cont_Give<-Scan_Behav_Cont_Count_WithZero %>%
    select(-R_Count,-T_Count) %>% 
    mutate(Received="G") %>% 
    select(ObsType,ObsRef,AnimalID,Behaviour,Received,G_Count) %>% 
    rename(Count=G_Count)
  
  
  Scan_Behav_Cont_Receive<-Scan_Behav_Cont_Count_WithZero %>%
    select(-G_Count,-T_Count) %>% 
    mutate(Received="R") %>% 
    select(ObsType,ObsRef,AnimalID,Behaviour,Received,R_Count) %>% 
    rename(Count=R_Count)
  
  
  Scan_Behav_Cont_Total<-Scan_Behav_Cont_Count_WithZero %>%
    select(-G_Count,-R_Count)%>% 
    mutate(Received="T") %>% 
    select(ObsType,ObsRef,AnimalID,Behaviour,Received,T_Count) %>% 
    rename(Count=T_Count) 
  
  Scan_Behav_Cont_Long<-bind_rows(Scan_Behav_Cont_Give,Scan_Behav_Cont_Receive,Scan_Behav_Cont_Total)
  
  # Bind count of inst and cont Behaviour in long format -------------------
  
  Scan_Behav_InstCont_Long<-bind_rows(Scan_Behav_Inst_Count %>% select(-TotalScanNumber),Scan_Behav_Cont_Long)
  
  
  #this is to make a separate object with the number of scan recorded per individuals
  #the total scan number has been computed for each
  TotalScanNumber<-Scan_Behav_Inst_Count %>% distinct(ObsRef,AnimalID,TotalScanNumber)
  
  #Here, at the difference than for WL analyses, I do not join to the scan session details 
  
  Scan_Behav_Count<-left_join(Scan_Behav_InstCont_Long,TotalScanNumber) %>% 
    select(ObsType,ObsRef,AnimalID,Behaviour,Received,Count,TotalScanNumber)
  
  return(Scan_Behav_Count)
}

View(Scan_Behav_Count)
names(Scan_SessionDetails)

# Function to query count and duration of state Focal Behav ---------------------

#the function take a list of FocalRef as input
#It requires the object Focal_Behav_Name, Focal_SessionDetails and Focal_Behav to be in the environment. This probably could be improved
#it returns the proportion and the count of every state behaviours for each focal observation provided in the list
#At the difference than scan observation, I have so far not made any categories of behaviours 

DF<-Focal

focal_behaviour_state_count<-function(DF){
  
# generate combination of FocalRef and StateBehav -------------------------
# sex foreplay should have a direction but for a long time could never be received 
Focal_Behav_State_Combination<-merge(DF,Focal_Behav_Name %>%
  filter(BehaviourType=="State") %>%
    select(Behaviour) %>% 
    rbind("Huddeling_R") %>% 
    rbind("Beg/Suckle_R") %>%
    rbind("Groom_R")) %>% 
  select(ObsRef,Behaviour)


# Generate count and total duration of state Behav ------------------------

#only retain state behaviours
Focal_Behav_State<-inner_join(DF,
   Focal_Behav %>% mutate(Behaviour = ifelse(Behaviour %in% c("Groom","Huddeling","Beg/Suckle") & Received == "Received", paste0(Behaviour,"_R"),Behaviour))) %>% #this modify the existing Behaviour and add a suffix to the received behaviour. This is conformed the combination we have created just above) %>%
    filter(!is.na(Behav_Duration)) %>% # to retain only state behaviour. an alternative is filter(BehaviourType=="State")
    mutate(ObsType="Focal_State")

   
#Make a count of state behaviours except Huddle that must be dealt separately (receive huddling would probably be teh case, but cannot be bothered rn)
Focal_Behav_State_NoHuddling_Count <- Focal_Behav_State %>% 
  filter(Behaviour != "Huddeling") %>% 
  #GROUP BY FOCALREF AND BEHAVIOUR 
  group_by(ObsRef,Behaviour) %>% 
  summarize(Behav_Duration=sum(Behav_Duration),Behav_Count=sum(BehaviourCount)) %>%  
  ungroup()
  #UNGROUP


#Make a count of huddling
Focal_Behav_State_Huddling_Count <- Focal_Behav_State %>% 
  filter(Behaviour == "Huddeling") %>% 
  distinct(FocalBehavRef,.keep_all = TRUE) %>% #to eliminate duplicate of huddling
  #GROUP BY FOCALREF AND BEHAVIOUR 
  group_by(ObsRef,Behaviour) %>% 
  summarize(Behav_Duration=sum(Behav_Duration),Behav_Count=sum(BehaviourCount)) %>%  
  ungroup()
#UNGROUP
  
#Append count of huddling and other behaviours 
Focal_Behav_State_Count<-bind_rows(Focal_Behav_State_NoHuddling_Count,Focal_Behav_State_Huddling_Count)


# join all combination ScanRef/AnimalID/BehaviourCont with count --------
Focal_Behav_State_Count_WithZero<-left_join(Focal_Behav_State_Combination, Focal_Behav_State_Count) %>% 
replace_na(list(Behav_Duration = 0, Behav_Count = 0)) 


# join to focal session details -------------------------------------------
# This is only necesaary to get DurationSeen and obtain frequency and duration.
#I leave animalID in Focal_SessionDetails to facilitate datacheck when subsequently joining to Focal_SessionInfo in my data prep files yet strictly speaking do not need it
#At the moment the proportion and frequency are calculated based on TotalSeen, whereas it would be better to be calculated on TotalSee_Active (after the exclusion of rest)

Focal_Behav_State_Count_SessionDetails<-inner_join(Focal_Behav_State_Count_WithZero,Focal_SessionDetails %>% select(ObsRef,AnimalID,TotalSeen)) %>% 
mutate(Frequency=Behav_Count/(TotalSeen/3600)) %>% #frequency per hour of observation
mutate(Proportion = (Behav_Duration/TotalSeen)*100)  #proportion of time spent doing an activity

return(Focal_Behav_State_Count_SessionDetails)

}





# Function to query count of point Focal Behav ----------------------------

# focal_behaviour_point_count () compute the count of point behaviours
#There are originally 17 point behaviours
#14 of these behaviour can either be received or given
#Pump, DropFS, Pass, have no direction thus one should only use the Given behaviour and exclude their received and total count
#I have added the subtypes of copulation (back/front) and pass (Over/Under/Sidewards; NoseNose/NoseTail/TailNose/TailTail )
#do I want to differentiate between different subtype of behaviours? or eliminate 

focal_behaviour_point_count<-function(DF){

# Generate new point Behav --------------------------------------------
Focal_Behav_Point<-inner_join(DF,Focal_Behav) %>% 
  filter(BehaviourType == "Point") %>%
  #filter(Received !="Received")
  mutate(Behaviour2=
      ifelse(Behaviour=="Copulation" & Copulation_Direction == "Front","CopulationF",
    ifelse(Behaviour=="Pass",paste0("Pass_",Pass_Direction,"_",Pass_Location),Behaviour))) #this create behaviours that contain the details of copulation and scan


# Generate count of given Behav -------------------------------------

Focal_Behav_Point_Count_Given <- Focal_Behav_Point %>% 
  filter(is.na (Received) | Received == "Initiated") %>% #this only retained the behaviour given or with no direction
  #GROUP BY FocalRef, Behaviour
  group_by(ObsRef,Behaviour2) %>% 
  summarise(G_Count=sum(BehaviourCount)) %>% 
  ungroup() %>% 
#UNGROUP
  rename(Behaviour = Behaviour2)


#Get the total of pass so I don't need compute it later
Focal_Behav_Pass<-Focal_Behav_Point %>% 
  filter(Behaviour == "Pass") %>% #this only retained the behaviour given or with no direction
  #GROUP BY FocalRef, Behaviour
  group_by(ObsRef,Behaviour) %>% 
  summarise(G_Count=sum(BehaviourCount)) %>% 
  ungroup() 
#UNGROUP


# Generate count of received Behav ----------------------------------
Focal_Behav_Point_Count_Received <- Focal_Behav_Point %>% 
  filter(Received == "Received") %>% #this only retained the behaviour given or with no direction
  #GROUP BY FocalRef, Behaviour
  group_by(ObsRef,Behaviour2) %>% 
  summarise(R_Count=sum(BehaviourCount)) %>% 
  ungroup() %>% 
#UNGROUP
  rename(Behaviour = Behaviour2)
 

# Join count of given and received Behav ----------------------------
#Pass give the total number of pass

Focal_Behav_Point_Count<-full_join(Focal_Behav_Point_Count_Given,Focal_Behav_Point_Count_Received) %>%
  full_join(.,Focal_Behav_Pass) %>% 
  replace_na(list(G_Count = 0, R_Count = 0)) %>% #replace the NA with a 0
mutate(T_Count=G_Count+R_Count)


# Generate combination FocalRef and Behav -----------------------------

#Create a list of behaviour that matches the one in Focal_Behav_Point_Count
Focal_Behav_Name_Point_All<- Focal_Behav_Name %>% filter (BehaviourType == "Point") %>% 
  select (-BehaviourType) %>% 
  filter(Behaviour != "Pass") %>%
  rbind("Pass") %>% #will be for total number of pass
  rbind("Pass_TailToNose_Under") %>% 
  rbind("Pass_NoseToNose_Sidewards") %>%
  rbind("Pass_NoseToNose_Over") %>%
  rbind("Pass_NoseToNose_Under") %>% 
  rbind("Pass_NoseToTail_Over") %>% 
  rbind("Pass_NoseToTail_Under") %>% 
  rbind("Pass_TailToTail_Under") %>% 
  rbind("Pass_TailToNose_Over") %>% 
  rbind("Pass_NoseToTail_Sidewards") %>% 
  rbind("Pass_TailToNose_Sidewards") %>% 
  rbind("Pass_TailToTail_Over") %>% 
  rbind("Pass_TailToTail_Sidewards") %>% 
  rbind("CopulationF") 

#create all combination of FocalRef and behaviours
Focal_Behav_Combination<-merge(DF,Focal_Behav_Name_Point_All)


# Generate zeros of given and received Behav  -------------------------

Focal_Behav_Point_Count_WithZero<-left_join(Focal_Behav_Combination,Focal_Behav_Point_Count) %>% 
replace_na(list(G_Count = 0, R_Count = 0,T_Count=0))


# Generate long format of all point Behav count ----------------------

Focal_Behav_Point_Give<-Focal_Behav_Point_Count_WithZero %>%
  select(-R_Count,-T_Count) %>% 
  mutate(Received="G") %>% 
  rename(Count=G_Count)
  
  
Focal_Behav_Point_Receive<-Focal_Behav_Point_Count_WithZero%>%
  select(-G_Count,-T_Count) %>% 
  mutate(Received="R") %>% 
  rename(Count=R_Count)


Focal_Behav_Point_Total<-Focal_Behav_Point_Count_WithZero %>%
  select(-G_Count,-R_Count)%>% 
  mutate(Received="T") %>% 
  rename(Count=T_Count) 

Focal_Behav_Cont_Long<-bind_rows(Focal_Behav_Point_Give,Focal_Behav_Point_Receive,Focal_Behav_Point_Total) %>% 
  select(ObsRef,Behaviour,Received,Count)


# Add frequency of behaviour ----------------------------------------------

# This is only necesaary to get DurationSeen and obtain frequency and duration.
#I leave animalID in Focal_SessionDetails to facilitate datacheck when subsequently joining to Focal_SessionInfo in my data prep files yet strictly speaking do not need it

Focal_Behav_Cont_Long_Count_SessionDetails<-inner_join(Focal_Behav_Cont_Long,Focal_SessionDetails %>% select(ObsRef,AnimalID,TotalSeen)) %>% 
mutate(Frequency=Count/(TotalSeen/3600)) #frequency per hour of observation


return(Focal_Behav_Cont_Long_Count_SessionDetails)

}


