library(dplyr)

load("../data/select2020.Rdata")
load("../data/select2021.Rdata")
load("../data/select2022.Rdata")
load("../data/select2023.Rdata")

## add unique patient identifier pid

select2020 <- select2020 %>%
  mutate(pid = row_number()) %>%
  select(pid, everything()) # reorder columns with pid first

select2021 <- select2021 %>%
  mutate(pid = row_number()) %>%
  select(pid, everything()) # reorder columns with pid first

select2022 <- select2022 %>%
  mutate(pid = row_number()) %>%
  select(pid, everything()) # reorder columns with pid first

select2023 <- select2023 %>%
  mutate(pid = row_number()) %>%
  select(pid, everything()) # reorder columns with pid first


### fill NA values of one column with values from a similar column with suffix
data2020f <- select2020 %>%
  mutate(birthWeight = coalesce(birthWeight...27, birthWeight...44),
         gender = coalesce(gender, gender...45),
         apgar1min = coalesce(apgar1min...29, as.numeric(apgar1min...46)),
         apgar5min = coalesce(apgar5min...30, as.numeric(apgar5min...47)),
         birthWeight = coalesce(as.numeric(birthWeight...27), as.numeric(birthWeight...44)),
         genderBin = coalesce(gender, gender...45),
         resuscitation = coalesce(resuscitation, resuscitation...31),
         
         bathBirthBin = case_when(
           bathBirth == "no" ~ 0,
           bathBirth == "yes" ~ 1,
           is.na(bathBirth) ~ 0
         ),
         bathBirthBin = as.numeric(bathBirthBin),
         
         fetalSurvCat = case_when(
           fetalSurveillance == "None" ~ "none",
           fetalSurveillance == "" ~ NA,
           is.na(fetalSurveillance) ~ NA,
           TRUE ~ fetalSurveillance
         ),
         
         # create GBS binary, set unknown as NA
         GBSCat = case_when(
           GBS == "negative" ~ 1,
           GBS == "positive" ~ 2,
           GBS == "unknown" ~ 3,
           is.na(GBS) ~ 1 #"negative"
         ),
         GBSCat = as.numeric(GBSCat),
         
         bloodLossCat = case_when(
           bloodLoss == "< 500 ml" ~ 1,
           bloodLoss == "500 - 1000 ml" ~ 2,
           bloodLoss == "> 1000 ml" ~ 3,
           TRUE ~ NA
         ),
         bloodLossCat = as.numeric(bloodLossCat),
         
         perineumCat = case_when(
           perineum == "1stDegreeTear" ~ "1DT",
           perineum == "2ndDegreeTear" ~ "2DT",
           perineum == "3rdDegreeTear" ~ "3DT",
           perineum == "4thDegreeTear" ~ "4DT",
           TRUE ~ perineum
         ),
         
         episiotomyBin = case_when(
           episiotomy == "no" ~ 0,
           episiotomy == "yes" ~ 1,
           is.na(episiotomy) ~  0 #"no"
         ),
         episiotomyBin = as.numeric(episiotomyBin),
         
         genderBin = case_when(
           genderBin == "girl" ~ 0,
           genderBin == "boy" ~ 1
         ),
         genderBin = as.numeric(genderBin),
         
         resuscitationCat = case_when(
           resuscitation == "none" ~ "none",
           resuscitation == "None" ~ "none",
           is.na(resuscitation) ~ "none",
           TRUE ~ resuscitation
         ),
         
         resuscitationBin = case_when(
           resuscitation == "none" ~ 0,
           resuscitation == "None" ~ 0,
           is.na(resuscitation) ~ 0, # "none"
          TRUE ~ 1
         ),
         resuscitationBin = as.numeric(resuscitationBin),
         
         prolongedMembranesRuptureAndGBSstatusCat = case_when(
           #ProlongedMembranesRuptureAndGBSstatus == "GBSunknown24hr" ~ NA,
           TRUE ~ ProlongedMembranesRuptureAndGBSstatus
         ),
         
         PMRGBS = case_when(
           #GBS == "unknown" ~ "unknown",
           GBS == "positive" & is.na(ProlongedMembranesRuptureAndGBSstatus) ~ NA,
           GBS == "negative" ~ "none",
           TRUE ~ ProlongedMembranesRuptureAndGBSstatus
         ),
         
         epiduralBin = case_when(
           epidural == "no" ~ 0,
           epidural == "yes" ~ 1,
          is.na(epidural) ~ 0 #"no"
         ),
         epiduralBin = as.numeric(epiduralBin),
         
         plannedDelivPlacCat = case_when(
           plannedDelivPlac == "birthCenter" ~ 1,
           plannedDelivPlac == "home" ~ 2,
           plannedDelivPlac == "hospitalWithMidwife" ~ 3,
           plannedDelivPlac == "hospitalWithObstetrician" ~ 4,
         ),
         plannedDelivPlacCat = as.numeric(plannedDelivPlacCat),
         
         nMiscarriage = as.numeric(nMiscarriage),
         nMiscarriage = case_when(
           is.na(nMiscarriage) ~ 0,
           TRUE ~ nMiscarriage
         ),
         nAbortion = as.numeric(nAbortion),
         nAbortion = case_when(
           is.na(nAbortion) ~ 0,
           TRUE ~ nAbortion
         ),
         
         birthPlaceCat = case_when(
           birthPlace == "birthCenter" ~ 1,
           birthPlace == "home" ~ 2,
           birthPlace == "hospital" ~ 3,
           birthPlace == "enroute" ~ 4,
         ),
         birthPlaceCat = as.numeric(birthPlaceCat),
         
         birthingResponsibilityBin = case_when(
           birthingResponsibility == "Obstetrician" ~ 0,
           birthingResponsibility == "midwife" ~ 1,
         ),
         birthingResponsibilityBin = as.numeric(birthingResponsibilityBin),
         
         pregnancyOnsetCat = case_when(
           pregnancyOnset == "spontaneous" ~ 1,
           pregnancyOnset == "KI" ~ 2,
           pregnancyOnset == "hormonal" ~ 3,
           pregnancyOnset == "IVF" ~ 4,
           pregnancyOnset == "ICSI" ~ 5,
         ),
         pregnancyOnsetCat = as.numeric(pregnancyOnsetCat),
         
         practiceCode = case_when(
           practiceCode == "NULL" ~ NA,
           TRUE ~ practiceCode
         ),
         practiceCode = as.numeric(practiceCode)
    )

(missing_counts <- colSums(is.na(data2020f)))

data2020fs <- data2020f %>%
  select(pid, bathBirthBin, birthingPosition, fetalSurvCat, GBSCat, placentalPhase, 
         bloodLossCat, perineumCat, episiotomyBin, birthWeight, genderBin,
         apgar1min, apgar5min, resuscitation, resuscitationBin, delivery,
         prolongedMembranesRuptureAndGBSstatusCat, PMRGBS,
         epiduralBin, plannedDelivPlac, birthPlace, birthPlaceCat, birthingResponsibilityBin,
         mothertongue, maternalAge, plannedDelivPlacCat, pregnancyOnsetCat,
         nMiscarriage, nAbortion, pregnancyOnset, practiceCode, zipcode, 
         referralDueToStagnation, reasonForUrgentReferral,
         reasonsForNonUrgentReferral)

write.csv(data2020fs, "../data/data2020fs.csv", row.names = FALSE)


data2021f <- select2021 %>%
  mutate(
    firstOrMultiParity = case_when(
      firstOrMultiParity == "first" ~ 0,
      firstOrMultiParity == "many" ~ 1,
    ),
    
    bathBirth = coalesce(bathBirth, bathBirth...19),
    bathBirthBin = case_when(
      bathBirth == "no" ~ 0,
      bathBirth == "yes" ~ 1,
      bathBirth == "NULL" ~ 0 #"no"
    ),
    
    fetalSurv = coalesce(fetalSurveillance, fetalSurveillance...54),
    fetalSurvBin = case_when(
      fetalSurv == "none" ~ 0,
      fetalSurv == "NULL" ~ NA, # true?
      is.na(fetalSurv) ~ NA,
      TRUE ~ 1
    ),
    
    GBS = coalesce(GBS, GBS...55),
    GBSCat = case_when(
      GBS == "negative" ~ 0,
      GBS == "positive" ~ 1,
      GBS == "unknown" ~ 2,
    ),
    GBSCat = as.numeric(GBSCat),
    
    placentalPhase = coalesce(placentalPhase...29, placentalPhase...60),
    placentalPhaseCat = case_when(
      placentalPhase == "unknown" ~ "unknown",
      placentalPhase == "NULL" ~ NA,
      TRUE ~ placentalPhase
    ),
    
    bloodLoss = coalesce(bloodLoss...30, bloodLoss...61),
    bloodLossCat = case_when(
      bloodLoss == "NULL" ~ NA,
      bloodLoss == "unknown" ~ NA,
      bloodLoss == "<500ml" ~ 1,
      bloodLoss == "500-1000ml" ~ 2,
      bloodLoss == "> 1000ml" ~ 3,
      bloodLoss == ">1000ml" ~ 3
    ),
    
    perineum = coalesce(perineum, Perineum...62),
    perineumCat = case_when(
      perineum == "NULL" ~ NA,
      perineum == "unknown" ~ NA,
      perineum == "1stDegreeTear" ~ "1DT",
      perineum == "2ndDegreeTear" ~ "2DT",
      perineum == "3rdDegreeTear" ~ "3DT",
      perineum == "4thDegreeTear" ~ "4DT",
      TRUE ~ perineum
    ),
    
    episiotomyCat = case_when(
      episiotomy == "NULL" ~ NA,
      #episiotomy == "no" ~ 0,
      episiotomy == "nee" ~ "no",
      #episiotomy == "yes" ~ 1,
      TRUE ~ episiotomy
    ),
    
    birthWeight = coalesce(birthWeight...36, birthWeight...64),
    birthWeight = case_when(
      birthWeight == "Onbekend" ~ NA,
      birthWeight == "NULL" ~ NA,
      TRUE ~ birthWeight
    ),
    birthWeight = as.numeric(birthWeight),
    
    gender = coalesce(gender...37, gender...65),
    genderBin = case_when(
      gender == "girl" ~ 0,
      gender == "boy" ~ 1,
      gender == "unknown" ~ NA
    ),
    
    apgar1min = coalesce(apgar1min...38, apgar1min...66),
    apgar1min = case_when(
      apgar1min == "NULL" ~ NA,
      TRUE ~ apgar1min
    ),
    apgar1min = as.numeric(apgar1min),
    
    apgar5min = coalesce(apgar5min...39, apgar5min...67),
    apgar5min =case_when(
      apgar5min == "NULL" ~ NA,
      TRUE ~ apgar5min
    ),
    apgar5min = as.numeric(apgar5min),
    
    resuscitation = coalesce(resuscitation, Resuscitation),
    resuscitationBin = case_when(
      resuscitation == "NULL" ~ NA,
      resuscitation == "none" ~ 0,
      resuscitation == NA ~ NA,
      TRUE ~ 1
    ),
    
    plannedDelivPlacCat = case_when(
      plannedDelivPlac == "birthCenter" ~ 1,
      plannedDelivPlac == "home" ~ 2,
      plannedDelivPlac == "hospitalWithMidwife" ~ 3,
      plannedDelivPlac == "hospitalWithObstetrician" ~ 4,
      plannedDelivPlac == "Midwifeledunit" ~ 5,
    ),
    
    actualDelivPlac = place...15,
    actualDelivPlacCat = case_when(
      actualDelivPlac == "birthCenter" ~ 1,
      actualDelivPlac == "home" ~ 2,
      actualDelivPlac == "hospital" ~ 3,
      actualDelivPlac == "enroute" ~ 4,
      actualDelivPlac == "Midwifeledunit" ~ 5,
    ),
    
    birthingResponsibilityBin = case_when(
      birthingResponsibility == "Obstetrician" ~ 0,
      birthingResponsibility == "midwife" ~ 1,
    ),
    
    pregnancyOnsetCat = case_when(
      pregnancyOnset == "NULL" ~ NA,
      pregnancyOnset == "spontaneously" ~ 1,
      pregnancyOnset == "spontaneous" ~ 1,
      pregnancyOnset == "KI" ~ 2,
      pregnancyOnset == "hormonal" ~ 3,
      pregnancyOnset == "IVF" ~ 4,
      pregnancyOnset == "ICSI" ~ 5,
      pregnancyOnset == "intraUterineInsemination" ~ 6
    ),
    pregnancyOnsetCat = as.numeric(pregnancyOnsetCat),
    
    maternalAge = case_when(
      maternalAge == "Onbekend" ~ NA,
      maternalAge == "NULL" ~ NA,
      TRUE ~ maternalAge
    ),
    maternalAge = as.numeric(maternalAge),
    
    nMiscarriage = as.numeric(nMiscarriage),
    nMiscarriage = case_when(
      is.na(nMiscarriage) ~ 0,
      TRUE ~ nMiscarriage
    ),
    nAbortion = as.numeric(nAbortion),
    nAbortion = case_when(
      is.na(nAbortion) ~ 0,
      TRUE ~ nAbortion
    ),
  )

data2021fs <- data2021f %>%
  select(pid, nParity, firstOrMultiParity, bathBirthBin, birthingPosition, fetalSurvBin, 
         GBS, GBSCat, placentalPhase, bloodLossCat, perineumCat, episiotomyCat,
         birthWeight, genderBin, apgar1min, apgar5min, resuscitationBin, 
         ProlongedMembranesRuptureAndGBSstatus, 
         plannedDelivPlacCat, actualDelivPlacCat, birthingResponsibilityBin, 
         mothertongue, maternalAge, pregnancyOnsetCat, nMiscarriage, nAbortion, 
         pregnancyOnsetCat, practiceCode, referralDueToStagnation,
         reasonForUrgentReferral, reasonForUrgentReferral)

write.csv(data2021fs, "../data/data2021fs.csv", row.names = FALSE)


data2022f <- select2022 %>%
  mutate(
    
    firstOrMultiParityBin = case_when(
      firstOrMultiParity == "first" ~ 0,
      firstOrMultiParity == "many" ~ 1,
    ),
    
    bathBirth = coalesce(bathBirth, BathBirth),
    bathBirthBin = case_when(
      bathBirth == "no" ~ 0,
      bathBirth == "yes" ~ 1,
      is.na(bathBirth) ~ 0 # "no"
    ),
    
    birthingPosition = coalesce(birthingPosition, BirthingPosition),
    birthingPositionCat = case_when(
      birthingPosition == "NULL" ~ NA,
      TRUE ~ birthingPosition
    ),
    
    fetalSurveillance = coalesce(fetalSurveillance...23, fetalSurveillance...57),
    fetalSurvCat = case_when(
      fetalSurveillance == "NULL" ~ NA,
      TRUE ~ fetalSurveillance
    ),
    
    GBS = coalesce(GBS...25, GBS...58),
    GBSBin = case_when(
      GBS == "negative" ~ 0,
      GBS == "positive" ~ 1,
      GBS == "unknown" ~ 3,
    ),
    
    placentalPhase = coalesce(placentalPhase...32, placentalPhase...63),
    placentalPhase = case_when(
      placentalPhase == "NULL" ~ NA,
      TRUE ~ placentalPhase
    ),
    
    bloodLoss = coalesce(bloodLoss...33, bloodLoss...64),
    bloodLossCat = case_when(
      bloodLoss == "NULL" ~ NA,
      bloodLoss == "<500ml" ~ 1,
      bloodLoss == "<500 ml" ~ 1,
      bloodLoss == "500-1000ml" ~ 2,
      bloodLoss == "> 1000 ml" ~ 3,
      bloodLoss == ">1000ml" ~ 3,
    ),
    
    birthWeight = coalesce(birthWeight...39, birthWeight...67),
    birthWeight = case_when(
      birthWeight == "NULL" ~ NA,
      TRUE ~ birthWeight
    ),
    birthWeight = as.numeric(birthWeight),
    
    perineum = coalesce(perineum...34, perineum...65),
    perineumCat = case_when(
      perineum == "NULL" ~ NA,
      perineum == "1stDegreeTear" ~ "1DT",
      perineum == "2ndDegreeTear" ~ "N2DTA",
      perineum == "3rdDegreeTear" ~ "3DT",
      perineum == "4thDegreeTear" ~ "4DT",
      TRUE ~ perineum
    ),
    
    episiotomyCat = case_when(
      episiotomy == "NULL" ~ "no",
      is.na(episiotomy) ~ "no",
      TRUE ~ episiotomy
    ),
    
    genderBin = coalesce(gender...40, gender...68),
    genderBin = case_when(
      genderBin == "NULL" ~ NA,
      genderBin == "girl" ~ 0,
      genderBin == "boy" ~ 1
    ),
    
    apgar1min = coalesce(apgar1min...41, apgar1min...69),
    apgar1min = case_when(
      apgar1min == "NULL" ~ NA,
      TRUE ~ apgar1min
    ),
    apgar1min = as.numeric(apgar1min),
    
    apgar5min = coalesce(apgar5min...42, apgar5min...70),
    apgar5min = case_when(
      apgar5min == "NULL" ~ NA,
      TRUE ~ apgar5min
    ),
    apgar5min = as.numeric(apgar5min),
    
    resuscitation = coalesce(resuscitation, Resuscitation),
    resuscitationCat = case_when(
      resuscitation == "NULL" ~ "none", 
      TRUE ~ resuscitation
    ),
    
    plannedDelivPlacCat = case_when(
      plannedDelivPlac == "birthCenter" ~ 1,
      plannedDelivPlac == "home" ~ 2,
      plannedDelivPlac == "hospitalWithMidwife" ~ 3,
      plannedDelivPlac == "hospitalWithObstetrician" ~ 4,
      plannedDelivPlac == "Midwifeledunit" ~ 5,
    ),
    
    actualDelivPlac = coalesce(place...18, place...53),
    actualDelivPlacCat = case_when(
      actualDelivPlac == "birthCenter" ~ 1,
      actualDelivPlac == "home" ~ 2,
      actualDelivPlac == "hospital" ~ 3,
      actualDelivPlac == "enroute" ~ 4,
      actualDelivPlac == "Midwifeledunit" ~ 5,
    ),
    
    birthingResponsibilityBin = case_when(
      birthingResponsibility == "Obstetrician" ~ 0, 
      birthingResponsibility == "midwife" ~ 1, 
    ),
    
    maternalAge = as.numeric(maternalAge),
    
    relationship = case_when(
      relationship == "NULL" ~ NA,
      TRUE ~ relationship
    ),
    
    highestEducation = case_when(
      highestEducation == "NULL" ~ NA,
      TRUE ~ highestEducation,
    ),
    
    nMiscarriage = case_when(
      nMiscarriage == "1 (+ een EUG)" ~ "1.0",
      nMiscarriage == "9w" ~ "1.0",
      is.na(nMiscarriage) ~ "0.0",
      TRUE ~ nMiscarriage
    ),
    nMiscarriage = as.numeric(nMiscarriage),
    
    nAbortion = case_when(
      nAbortion == "1 (owv hartafwijking)" ~ "1.0",
      nAbortion == "IMG" ~ "1.0",
      nAbortion == "11w" ~ "1.0",
      is.na(nAbortion) ~ "0.0",
      TRUE ~ nAbortion
    ),
    nAbortion = as.numeric(nAbortion),
    
    pregnancyOnsetCat = case_when(
      pregnancyOnset == "NULL" ~ NA,
      pregnancyOnset == "spontaneously" ~ 1,
      pregnancyOnset == "KI" ~ 2,
      pregnancyOnset == "hormonal" ~ 3,
      pregnancyOnset == "IVF" ~ 4,
      pregnancyOnset == "ICSI" ~ 5,
      pregnancyOnset == "intraUterineInsemination" ~ 6
    ),
    pregnancyOnsetCat = as.numeric(pregnancyOnsetCat),
    
    practiceCode = case_when(
      practiceCode == "NULL" ~ NA,
      TRUE ~ practiceCode
    ),
    practiceCode = as.numeric(practiceCode)
)

data2022fs <- data2022f %>%
  select(pid, nParity, firstOrMultiParityBin, bathBirthBin, birthingPositionCat,
         fetalSurveillance, GBSBin, placentalPhase, bloodLossCat, perineumCat, 
         episiotomyCat, birthWeight, genderBin, apgar1min, apgar5min,
         resuscitationCat, plannedDelivPlacCat, actualDelivPlacCat, 
         birthingResponsibilityBin, maternalAge, mothertongue,
         mothertongue, pregnancyOnsetCat, nMiscarriage,
         nAbortion, practiceCode, referralDueToStagnation, reasonForUrgentReferral,
         reasonForReferral...47)

write.csv(data2022fs, "../data/data2022fs.csv", row.names = FALSE)


data2023f <- select2023 %>%
  mutate(
    nParity = case_when(
      nParity == "NULL" ~ NA,
      TRUE ~ nParity
    ),
    nParity = as.numeric(nParity),
    
    firstOrMultiParityBin = case_when(
      firstOrMultiParity == "first" ~ 0,
      firstOrMultiParity == "many" ~ 1
    ),
    
    bathBirth = coalesce(bathBirth, BathBirth),
    bathBirthCat = case_when(
      bathBirth == "yes" ~ 1,
      bathBirth == "no" ~ 0,
      bathBirth == "NULL" ~ 0,
      TRUE ~ 2 # "NVT, sectio"
    ),
    
    birthingPosition = coalesce(birthingPosition, BirthingPosition),
    birthingPositionCat = case_when(
      birthingPosition == "NULL" ~ NA,
      TRUE ~ birthingPosition
    ),
    
    fetalSurveillance = coalesce(fetalSurveillance...24, fetalSurveillance...62),
    fetalSurveillance = case_when(
      fetalSurveillance == "NULL" ~ NA,
      TRUE ~ fetalSurveillance
    ),
    
    GBS = coalesce(GBS...26, GBS...63),
    GBSCat = case_when(
      GBS == "negative" ~ 0,
      GBS == "positive" ~ 1,
      GBS == "unknown" ~ 2,
    ),
    
    placentaPhase = coalesce(placentaPhase...34, placentalPhase...68),
    placentaPhaseCat = case_when(
      placentaPhase == "NULL" ~ NA,
      TRUE ~ placentaPhase
    ), 
    bloodLoss = coalesce(bloodLoss, estimatedBloodLoss),
    bloodLossCat = case_when(
      bloodLoss == "<500ml" ~ 1,
      bloodLoss == "500-1000ml" ~ 2,
      bloodLoss == ">1000ml" ~ 3,
      bloodLoss == "NULL" ~ NA
    ),
    
    perineum = coalesce(perineum...37, perineum...71),
    perineumCat = case_when(
      perineum == "NULL" ~ NA,
      perineum == "1stDegreeTear" ~ "1DT",
      perineum == "1st degree" ~ "1DT",
      perineum == "2ndDegreeTear" ~ "2DT",
      perineum == "2nd degree" ~ "2DT",
      perineum == "3rdDegreeTear" ~ "3DT",
      perineum == "3rd degree" ~ "3DT",
      perineum == "4thDegreeTear" ~ "4DT",
      perineum == "4th degree" ~ "4DT",
      TRUE ~ perineum
    ),
    
    episiotomyCat = case_when(
      episiotomy == "NULL" ~ NA,
      episiotomy == "unknown" ~ NA,
      episiotomy == NA ~ NA,
      TRUE ~ episiotomy
    ),
    
    birthWeight = coalesce(birthWeight...42, birthWeight...73),
    birthWeight = case_when(
      birthWeight == "NULL" ~ NA,
      TRUE ~ birthWeight
    ),
    birthWeight = as.numeric(birthWeight),
    
    gender = coalesce(gender...43, gender...74),
    genderBin = case_when(
      gender == "girl" ~ 0,
      gender == "boy" ~ 1,
      gender == "NULL" ~ NA,
    ),
    
    apgar1min = coalesce(apgar1min...44, apgar1min...75),
    apgar1min = case_when(
      apgar1min == "NULL" ~ NA,
      TRUE ~ apgar1min
    ),
    apgar1min = as.numeric(apgar1min),
    
    apgar5min = coalesce(apgar5min...45, apgar5min...76),
    apgar5min = case_when(
      apgar5min == "NULL" ~ NA,
      TRUE ~ apgar5min
    ),
    apgar5min = as.numeric(apgar5min),
    
    resuscitation = coalesce(resuscitation, resuscitation_more),
    resuscitationBin = case_when(
      is.na(resuscitation) ~ 0, # none
      resuscitation == "none" ~ 0,
      TRUE ~ 1
    ),
    
    prolongedMembranesRuptureAndGBSstatus = case_when(
      prolongedMembranesRuptureAndGBSstatus == "NULL" ~ NA,
      TRUE ~ prolongedMembranesRuptureAndGBSstatus
    ),
    
    plannedDelivPlacCat = case_when(
      plannedDelivPlac == "NULL" ~ NA,
      plannedDelivPlac == "birthCenter" ~ 1,
      plannedDelivPlac == "home" ~ 2,
      plannedDelivPlac == "hospitalWithMidwife" ~ 3,
      plannedDelivPlac == "hospitalWithObstetrician" ~ 4
      #plannedDelivPlac == "Midwifeledunit" ~ 5,
    ),
    
    actualDelivPlac = place,
    actualDelivPlacCat = case_when(
      actualDelivPlac == "birthCenter" ~ 1,
      actualDelivPlac == "home" ~ 2,
      actualDelivPlac == "hospital" ~ 3,
      actualDelivPlac == "enroute" ~ 4,
      actualDelivPlac == "midwifeLedUnit" ~ 5,
      actualDelivPlac == "maternityWard" ~ 6,
    ),
    
    birthingResponsibilityBin = case_when(
      birthingResponsibility == "obstetrician" ~ 0, 
      birthingResponsibility == "midwife" ~ 1, 
    ),
    
    mothertongue = case_when(
      mothertongue == "NULL" ~ NA,
      TRUE ~ mothertongue
    ),
    maternalAge = case_when(
      maternalAge == "NULL" ~ NA,
      TRUE ~ maternalAge
    ),
    maternalAge = as.numeric(maternalAge),
    
    relationship = case_when(
      relationship == "NULL" ~ NA,
      TRUE ~ relationship
    ),
    
    abortion = case_when(
      abortion == "NULL" ~ NA,
      TRUE ~ abortion
    ),
    
    pregnancyOnsetCat = case_when(
      pregnancyOnset == "NULL" ~ NA,
      TRUE ~ pregnancyOnset
    ),
    
    nMiscarriage = case_when(
      is.na(nMiscarriage) ~ 0,
      TRUE ~ nMiscarriage
    ),
    
    nAbortion = case_when(
      is.na(nAbortion) ~ 0,
      TRUE ~ nAbortion
    ),
    
  )


data2023fs <- data2023f %>%
  select(pid, nParity, firstOrMultiParityBin, bathBirth, bathBirthCat, 
         birthingPositionCat,fetalSurveillance, GBS, GBSCat, placentaPhase,
         bloodLossCat, perineumCat,
         episiotomyCat, birthWeight, genderBin, apgar1min, apgar5min,
         resuscitationBin, plannedDelivPlacCat, actualDelivPlacCat, 
         birthingResponsibilityBin, maternalAge, mothertongue, 
         pregnancyOnsetCat, nMiscarriage, nAbortion, practiceCode, province,
         reasonsForNonUrgentReferral, referralDueToStagnation,
         reasonForUrgentReferral
         )

write.csv(data2023fs, "../data/data2023fs.csv", row.names = FALSE)


colSums(is.na(data2020f))




