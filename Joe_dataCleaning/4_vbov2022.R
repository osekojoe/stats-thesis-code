library(dplyr)

load("../data/select2022.Rdata")

## add unique patient identifier pid

select2022 <- select2022 %>%
  mutate(pid = row_number()) %>%
  select(pid, everything()) # reorder columns with pid first

data2022fcoals <- select2022 %>%
  mutate(
    bathBirth = coalesce(bathBirth, BathBirth),
    birthingPosition = coalesce(birthingPosition, BirthingPosition),
    fetalSurv = coalesce(fetalSurveillance...23, fetalSurveillance...57),
    GBS = coalesce(GBS...25, GBS...58),
    placentalPhase = coalesce(placentalPhase...32, placentalPhase...63),
    bloodLoss = coalesce(bloodLoss...33, bloodLoss...64),
    birthWeight = coalesce(birthWeight...39, birthWeight...67),
    perineum = coalesce(perineum...34, perineum...65),
    gender = coalesce(gender...40, gender...68),
    apgar1min = coalesce(apgar1min...41, apgar1min...69),
    apgar5min = coalesce(apgar5min...42, apgar5min...70),
    resuscitation = coalesce(resuscitation, Resuscitation),
    actualDelivPlac = coalesce(place...18, place...53)
  )


data2022f <- data2022fcoals %>%
  mutate(
    bathBirth = case_when(
      bathBirth == "NULL" ~ "no",
      TRUE ~ bathBirth
    ),
    
    birthingPosition = case_when(
      birthingPosition == "NULL" ~ NA,
      TRUE ~ birthingPosition
    ),
    
    fetalSurv = case_when(
      fetalSurv == "NULL" ~ "none",
      TRUE ~ fetalSurv
    ),
    
    placentalPhase = case_when(
      placentalPhase == "NULL" ~ NA,
      TRUE ~ placentalPhase
    ),
    
    bloodLoss = case_when(
      bloodLoss == "<500 ml" ~ "<500ml",
      bloodLoss == "> 1000 ml" ~ ">1000ml",
      bloodLoss == "NULL" ~ "unknown",
      TRUE ~ bloodLoss
    ),
    
    birthWeight = case_when(
      birthWeight == "NULL" ~ NA,
      TRUE ~ birthWeight
    ),
    birthWeight = as.numeric(birthWeight),
    
    perineum = case_when(
      perineum == "1stDegreeTear" ~ "1DT",
      perineum == "2ndDegreeTear" ~ "2DT",
      perineum == "3rdDegreeTear" ~ "3DT",
      perineum == "4thDegreeTear" ~ "4DT",
      perineum == "abrasionWound, not stitched" ~ "abrasionWound",
      perineum == "NULL" ~ "intact",
      TRUE ~ perineum
    ),
    
    episiotomy = case_when(
      episiotomy == "NULL" ~ "no", 
      is.na(episiotomy) ~ "no",
      TRUE ~ episiotomy
    ),
    
    gender = case_when(
      gender == "NULL" ~ NA,
      TRUE ~ gender
    ),
    
    apgar1min = case_when(
      apgar1min == "NULL" ~ NA,
      TRUE ~ apgar1min
    ),
    apgar1min = as.numeric(apgar1min),
    
    apgar5min = case_when(
      apgar5min == "NULL" ~ NA,
      TRUE ~ apgar5min
    ),
    apgar5min = as.numeric(apgar5min),
    
    resuscitation = case_when(
      resuscitation == "NULL" ~ "none",
      TRUE ~ resuscitation
    ),
    
    PMRGBS = case_when(
      #GBS == "unknown" ~ "unknown",
      GBS == "positive" & is.na(ProlongedMembranesRuptureAndGBSstatus) ~ NA,
      GBS == "negative" ~ "none",
      #ProlongedMembranesRuptureAndGBSstatus == "GBSnegative18hr" ~ "GBSnegative18hr",
      #ProlongedMembranesRuptureAndGBSstatus == "GBSunknown24hr" ~ "GBSunknown24hr",
      #ProlongedMembranesRuptureAndGBSstatus == "GBSnegative24hr" ~ "GBSnegative24hr",
      #ProlongedMembranesRuptureAndGBSstatus == "GBSpositive12hr" ~ "GBSpositive12hr",
      TRUE ~ ProlongedMembranesRuptureAndGBSstatus
    ),
    
    maternalAge = case_when(
      maternalAge == "NULL" ~ NA,
      TRUE ~ maternalAge
    ),
    maternalAge = as.numeric(maternalAge),
    
    relationship = case_when(
      relationship == "NULL" ~ "single",
      TRUE ~ relationship
    ),
    
    highestEducation = case_when(
      highestEducation == "NULL" ~ NA,
      TRUE ~ highestEducation
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
    
    pregnancyOnset = case_when(
      pregnancyOnset == "spontaneously" ~ "spontaneous",
      pregnancyOnset == "NULL" ~ "spontaneous",
      TRUE ~ pregnancyOnset
    ),
    
    practiceCode = case_when(
      practiceCode == "NULL" ~ NA,
      TRUE ~ practiceCode
    ),
    practiceCode = as.numeric(practiceCode)
    
  )


data2022fs <- data2022f %>%
  select(
    pid, nParity, firstOrMultiParity, bathBirth, birthingPosition,
    fetalSurv, GBS, placentalPhase, bloodLoss, perineum, 
    episiotomy, birthWeight, gender, apgar1min, apgar5min,
    resuscitation, plannedDelivPlac, actualDelivPlac, PMRGBS,
    birthingResponsibility, maternalAge, mothertongue, relationship,
    pregnancyOnset, nMiscarriage, highestEducation, abortion,
    nAbortion, practiceCode, zipcode, expulsionPhase, referralDueToStagnation,
    reasonForUrgentReferral,reasonForReferral...47
  )


### exclude birthplace=enroute and postcode NOT belgie ---------
data2022fs <- data2022fs %>%
  filter(actualDelivPlac != "enroute") %>%
  filter(!zipcode %in% c("Dubai"))
### -----------------------------------------------------------


(missing_counts <- colSums(is.na(data2022fs)))

data2022fsimp <- data2022fs %>%
  mutate(
    
    mural = case_when(
      actualDelivPlac %in% c("birthCenter", "home") ~ "extramural",
      actualDelivPlac %in% c("hospital", "Midwifeledunit") ~ "intramural"
    ),
    
    perineum = case_when(
      perineum %in% c("intact", "vaginaWound",  "labia",
                      "NVT, sectio", "sectio", "abrasionWound") ~ "intact",
      TRUE ~ perineum
    ),
    
    #resuscitation = case_when(
    #  resuscitation %in% c() ~ ""
    #),
    
    fetalSurv = case_when(
      fetalSurv %in% c("intermittentCTG", "intermittentDoppler"
                       
                       ) ~ "intermittent",
      fetalSurv %in% c("STAN", "intermittentDopplerContinuCTGandSTAN",
                       "CTGdoptoneAndContinuCTG", "continuCTG", 
                       "intermittentDopplerContinuCTG", "CTGandDoptone",
                       "intermittentCTGandContinuCTG", "CTGandDoptoneAndContinuCTGandSTAN",
                       "CTGandDoptoneAndContinuCTG", "continuCTGandNFMS", 
                       "intermittentCTGcontinuCTGandSTAN", "intermittentCTGandSTAN",
                       "intermittentCTGcontinuCTGandSTANandLactateMonitoring",
                       "CTG + doptone, Continu CTG, STAN",
                       "CTGandDoptoneAndSTAN", "continuCTGandSTAN"
                       ) ~ "continuous",
      fetalSurv %in% c("none") ~ "none",
      TRUE ~ fetalSurv
    ),
    
    relationship = case_when(
      relationship %in% c("single") ~ "Single/never married",
      relationship %in% c("married", "actual cohabitation", "legal cohabitation",
                          "LAT-relationship") ~ "Married/living with partner",
      relationship %in% c("other relationship") ~ "other relationship",
      relationship %in% c("widowed") ~ "Widowed",
    ),
    
    educLevel = case_when(
      highestEducation %in% c("none") ~ "none",
      highestEducation %in% c("Secondary school", "lower secondary education",
                              "Upper secondary education") ~ "secondary",
      highestEducation %in% c("Bachelor", "Master", "Higher education: postgraduate, A2, HBO5",
                              "Doctorate", "Student", "Higher education: (bachelor, university, ...)"
                              ) ~ "tertiary",
      #highestEducation %in% c() ~ "other",
      TRUE ~ highestEducation
    ),
    
    birthingPosition = case_when(
      birthingPosition %in% c("lithotomyWithoutLegSupports", "onTheBack") ~ "LithotomyWithoutLegSupport",
      birthingPosition %in% c("lithotomyInLegSupports", 
                              "stoolSit, lyingOnTheSide, lithotomyWithoutLegSupports, lithotomyInLegSupports",
                              "squat, lithotomyWithoutLegSupports, lithotomyInLegSupports",
                              "lithotomy position") ~ "LithotomyInLegBraces",
      birthingPosition %in% c("handsKnees", "onMatNextToBed", "handsKneesAndWalkingPosture",
                              "verticalKneeSupport") ~ "HandsOnKnees",
      birthingPosition %in% c("lyingOnTheSide", "lyingBetweenSideLyingAndSupine") ~ "lyingOnSide",
      birthingPosition %in% c("standing") ~ "standing",
      birthingPosition %in% c("stoolSit", "sittingUpright", "sittingOnToilet",
                              "birthingStool", "LeaningOverSeat", "sittingOnPartnerLapOnToilet",
                              "sitting") ~ "birthingStool",
      birthingPosition %in% c("squat", "squat, lyingOnTheSide", "highRunnersLunge") ~ "squatting",
      birthingPosition %in% c("lyingInBath", "halfSitting/lyingInBath",
                              "leaningOnEdgeOfBath", "lyingInBath+handsKnees",
                              "halfSittingInBed") ~ "lyingInBath",
      TRUE ~ birthingPosition
    ),
    
    resusc = case_when(
      resuscitation %in% c("none") ~ "none",
      resuscitation %in% c("5Inflations", "2Inflations", "3Inflations"
                           ) ~ "inflation",
      resuscitation %in% c("ventilation", "5InflationsVentilation",
                           "5InflationsVentilationPEEPandCPAP", "ventilationPEEPandCPAP",
                           "5InflationsAndVentilation"
                           ) ~ "ventilation",
      resuscitation %in% c("PEEPandCPAP", "PEEP CPAP") ~ "PEEP CPAP",
      resuscitation %in% c("stimulationInOpenAir", "stimulation") ~ "tactileStimulation",
      resuscitation %in% c("5InflationsVentilationHeartMassagePEEPandCPAP"
                           ) ~ "inflationVentilationHeartMassage",
      resuscitation %in% c("yes") ~ "yes",
      TRUE ~ resuscitation
    ),
    
    placentalPhase = case_when(
      placentalPhase %in% c("spontaneous") ~ "spontaneous",
      placentalPhase %in% c("spontaneousFollowedByActivePolicy", 
                            "activePolicyImmediateSynto",
                            "Synto infusion during expulsion",
                            "activePolicyImmediateSynto + manualRevision"
                            ) ~ "activePolicy",
      placentalPhase %in% c("NVTandSectio") ~ "manualRevision",
      #placentalPhase %in% c("unknown") ~ "unknown",
      placentalPhase %in% c("NVTandSectio") ~ "NVTandSectio",
      TRUE ~ placentalPhase
    ),
    
    apgar1cat = case_when(
      apgar1min %in% c(0,1,2,3) ~ "low",
      apgar1min %in% c(4,5,6) ~ "medium",
      apgar1min %in% c(7,8,9,10) ~ "high",
    ),
    
    apgar5cat = case_when(
      apgar5min %in% c(0,1,2,3) ~ "low",
      apgar5min %in% c(4,5,6) ~ "medium",
      apgar5min %in% c(7,8,9,10) ~ "high",
    ),
    
    mothertongue = case_when(
      mothertongue %in% c("Dutch", "bilingual: french dutch") ~ "Dutch",
      mothertongue %in% c("French") ~ "French",
      mothertongue %in% c("English") ~ "English",
      mothertongue %in% c("German", "Russian", "Spanish", "Turkish", "Czech" ,
                            "Portuguese", "Ukrainian","Estonian","Polish","Danish",
                            "Romanian", "Lithuanian", "russian", "Italian", "Slovak",
                            "Greek", "Croatian", "spanish", "Hungarian", "Montenegrin",
                            "Arabic (Moroccan)", "Chinese", "Creole", "vietnamese",
                            "Brazilian", "Korean", "Hebrew") ~ "other",
      TRUE ~ mothertongue
    )
  )


### write CSV
write.csv(data2022fsimp, "../data/data2022fsimp.csv", row.names = FALSE)

### stats ----------------------------------------------------------------------

## -----perineum ---------------
(count <- data2022fsimp %>% # "intact" "2DT" "1DT" "episiotomy" "3DT" "4DT"
   count(perineum) )

(cross_tab <- data2022fsimp %>%
    count(perineum, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----fetalSurv ---------------
(count <- data2022fsimp %>%
   count(fetalSurv))

(cross_tab <- data2022fsimp %>%
    count(fetalSurv, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----resuscitation ---------------
(count <- data2022fsimp %>%
   count(resuscitation))

(cross_tab <- data2022fsimp %>%
    count(resuscitation, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----episiotomy ---------------
(count <- data2022fsimp %>%
   count(episiotomy))

(cross_tab <- data2022fsimp %>%
    count(episiotomy, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----apgar1min ---------------
(count <- data2022fsimp %>%
   count(apgar1min))

(cross_tab <- data2022fsimp %>%
    count(apgar1min, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----apgar5min ---------------
(count <- data2022fsimp %>%
   count(apgar5min))

(cross_tab <- data2022fsimp %>%
    count(apgar5min, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----bloodLoss ---------------
(count <- data2022fsimp %>%
   count(bloodLoss))

(cross_tab <- data2022fsimp %>%
    count(bloodLoss, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))


## -----GBS ---------------
(count <- data2022fsimp %>%
   count(GBS))

(cross_tab <- data2022fsimp %>%
    count(GBS, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----placentalPhase ---------------
(count <- data2022fsimp %>%
   count(placentalPhase))

(cross_tab <- data2022fsimp %>%
    count(placentalPhase, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----waterbirth ---------------
(count <- data2022fsimp %>%
   count(bathBirth))

(cross_tab <- data2022fsimp %>%
    count(bathBirth, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----gender ---------------
(count <- data2022fsimp %>%
   count(gender))

(cross_tab <- data2022fsimp %>%
    count(gender, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----mothertongue ---------------
(count <- data2022fsimp %>%
   count(mothertongue))

(cross_tab <- data2022fsimp %>%
    count(mothertongue, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----resuscitation ---------------
(count <- data2022fsimp %>%
   count(resuscitation))

(cross_tab <- data2022fsimp %>%
    count(resuscitation, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----relationship ---------------
(count <- data2022fsimp %>%
   count(relationship))

(cross_tab <- data2022fsimp %>%
    count(relationship, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----higestEducation ---------------
(count <- data2022fsimp %>%
   count(educLevel))

(cross_tab <- data2022fsimp %>%
    count(educLevel, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----firstOrMultiParity ---------------
(count <- data2022fsimp %>%
   count(firstOrMultiParity))

(cross_tab <- data2022fsimp %>%
    count(firstOrMultiParity, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))


## -----birthingPosition ---------------
(count <- data2022fsimp %>%
   count(birthingPosition))

(cross_tab <- data2022fsimp %>%
    count(birthingPosition, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))



## -----planned vs actual birth place  ---------------

(cross_tab <- data2022fsimp %>%
   count(plannedDelivPlac, actualDelivPlac) %>%
   pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))


## -----socio-demographic determinants --------------------------------------

## ---- mothertongue 
(count <- data2022fsimp %>%
   count(mothertongue))

(cross_tab <- data2022fsimp %>%
    count(mothertongue, plannedDelivPlac) %>%
    pivot_wider(names_from = mothertongue, values_from = n, values_fill = 0))

(cross_tab <- data2022fsimp %>%
    count(mothertongue, actualDelivPlac) %>%
    pivot_wider(names_from = mothertongue, values_from = n, values_fill = 0))

(cross_tab <- data2022fsimp %>%
    count(mothertongue, birthingResponsibility) %>%
    pivot_wider(names_from = mothertongue, values_from = n, values_fill = 0))



## ---- relationship 
(count <- data2022fsimp %>%
   count(relationship))

(cross_tab <- data2022fsimp %>%
    count(relationship, plannedDelivPlac) %>%
    pivot_wider(names_from = relationship, values_from = n, values_fill = 0))

(cross_tab <- data2022fsimp %>%
    count(relationship, actualDelivPlac) %>%
    pivot_wider(names_from = relationship, values_from = n, values_fill = 0))

(cross_tab <- data2022fsimp %>%
    count(relationship, birthingResponsibility) %>%
    pivot_wider(names_from = relationship, values_from = n, values_fill = 0))

## ---- educLevel 
(count <- data2022fsimp %>%
    count(educLevel))

(cross_tab <- data2022fsimp %>%
    count(educLevel, plannedDelivPlac) %>%
    pivot_wider(names_from = educLevel, values_from = n, values_fill = 0))

(cross_tab <- data2022fsimp %>%
    count(educLevel, actualDelivPlac) %>%
    pivot_wider(names_from = educLevel, values_from = n, values_fill = 0))

(cross_tab <- data2022fsimp %>%
    count(educLevel, birthingResponsibility) %>%
    pivot_wider(names_from = educLevel, values_from = n, values_fill = 0))


## -----Obstetric history --------------------------------------

## ---- pregnancyOnset 
(count <- data2022fsimp %>%
   count(pregnancyOnset))

(cross_tab <- data2022fsimp %>%
    count(pregnancyOnset, actualDelivPlac) %>%
    pivot_wider(names_from = pregnancyOnset, values_from = n, values_fill = 0))


