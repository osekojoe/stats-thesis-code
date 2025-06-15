
library(dplyr)

load("../data/select2021.Rdata")

select2021 <- select2021 %>%
  mutate(pid = row_number()) %>%
  select(pid, everything()) # reorder columns with pid first

data2021fcoalesce <- select2021 %>%
  mutate(
    bathBirth = coalesce(bathBirth, bathBirth...19),
    fetalSurv = coalesce(fetalSurveillance, fetalSurveillance...54),
    GBS = coalesce(GBS, GBS...55),
    placentalPhase = coalesce(placentalPhase...29, placentalPhase...60),
    bloodLoss = coalesce(bloodLoss...30, bloodLoss...61),
    perineum = coalesce(perineum, Perineum...62),
    birthWeight = coalesce(birthWeight...36, birthWeight...64),
    gender = coalesce(gender...37, gender...65),
    apgar1min = coalesce(apgar1min...38, apgar1min...66),
    apgar5min = coalesce(apgar5min...39, apgar5min...67),
    resuscitation = coalesce(resuscitation, Resuscitation),
    actualDelivPlac = place...15
  )

data2021f <- data2021fcoalesce %>%
  mutate(
    bathBirth = case_when(
      bathBirth == "NULL" ~ "no",
      TRUE ~ bathBirth
    ),
    
    fetalSurv = case_when(
      fetalSurv == "NULL" ~ "none",
      TRUE ~ fetalSurv
    ),
    
    placentalPhase = case_when(
      placentalPhase == "NULL" ~ NA,
      placentalPhase == "unknown" ~ "unknown",
      TRUE ~ placentalPhase
    ),
    
    bloodLoss = case_when(
      bloodLoss == "> 1000ml" ~ ">1000ml",
      bloodLoss == "NULL" ~ "unknown",
      bloodLoss == "unknown" ~ "unknown",
      TRUE ~ bloodLoss
    ),
    
    perineum = case_when(
      perineum == "1stDegreeTear" ~ "1DT",
      perineum == "2ndDegreeTear" ~ "2DT",
      perineum == "3rdDegreeTear" ~ "3DT",
      perineum == "4thDegreeTear" ~ "4DT",
      perineum == "NULL" ~ "intact",
      perineum == "unknown" ~ "intact",
      TRUE ~ perineum
    ),
    
    episiotomy = case_when(
      episiotomy == "NULL" ~ "no", 
      episiotomy == "nee" ~ "no",
      is.na(episiotomy) ~ "no",
      TRUE ~ episiotomy
    ),
    
    birthWeight = case_when(
      birthWeight == "Onbekend" ~ NA,
      birthWeight == "NULL" ~ NA,
      TRUE ~ birthWeight
    ),
    birthWeight = as.numeric(birthWeight),
    
    gender = case_when(
      gender == "unknown" ~ NA,
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
      TRUE ~ ProlongedMembranesRuptureAndGBSstatus
    ),
    
    mothertongue = case_when(
      mothertongue %in% c("French") ~ "French",
      mothertongue %in% c("Dutch", "FrenchAndDutch") ~ "Dutch",
      mothertongue %in% c("English") ~ "English",
      mothertongue %in% c("German", "Spanish", "Portuguese",
                          "Russian", "Italian", "Polish", "Bulgarian",
                          "Turkish", "Ukrainian", "Greek", "Hungarian", "Romanian", 
                          "Serbian", "Norwegian", "Lithuanian",
                          "Slovak", "Czech", "Japanese", "Hebrew", "Burundi", 
                          "Moroccan", "Chinese", "Hindi") ~ "other",
      is.na(mothertongue) ~ NA
    ),
    
    maternalAge = case_when(
      maternalAge == "Onbekend" ~ NA,
      maternalAge == "NULL" ~ NA,
      TRUE ~ maternalAge
    ),
    maternalAge = as.numeric(maternalAge),
    
    pregnancyOnset = case_when(
      pregnancyOnset == "spontaneously" ~ "spontaneous",
      pregnancyOnset == "NULL" ~ "spontaneous",
      TRUE ~ pregnancyOnset
    )
    
)


data2021fs <- data2021f %>%
  select(pid, nParity, bathBirth, birthingPosition, GBS, fetalSurv, 
         placentalPhase, bloodLoss, perineum, episiotomy,
         birthWeight, gender, apgar1min, apgar5min, resuscitation, 
         PMRGBS, plannedDelivPlac, actualDelivPlac, birthingResponsibility,
         mothertongue, maternalAge, abortion, nMiscarriage, nAbortion, 
         pregnancyOnset,practiceCode, zipcode, expulsionPhase,
         referralDueToStagnation,
         reasonForUrgentReferral, reasonForUrgentReferral)

(missing_counts <- colSums(is.na(data2021fs)))


### exclude birthplace=enroute and postcode NOT belgie ---------
data2021fs <- data2021fs %>%
  filter(actualDelivPlac != "enroute") %>%
  filter(!zipcode %in% c("Netherlands", "France", "NULL"))

### -----------------------------------------------------------

data2021fsimp <- data2021fs %>%
  mutate(
    
    mural = case_when(
      actualDelivPlac %in% c("birthCenter", "home") ~ "extramural",
      actualDelivPlac %in% c("hospital", "Midwifeledunit") ~ "intramural"
    ),
    
    perineum = case_when(
      perineum %in% c("intact", "labia", "NVTandSectio",
                      "intactLabia", "intactRuptureOfVaginalWallAndLiLabium",
                      "intactSmallRuptureOfInnerRightMucousMembrane", 
                      "intactInternalRuptureOfVagina",
                      "vaginaWound", "intactVaginalRuptureSmall",
                      "abrasionWound") ~ "intact",
      TRUE ~ perineum
    ),
    
    
    fetalSurv = case_when(
      fetalSurv %in% c("intermittentDoppler", "CTGandDoptone", "intermittentCTG"
                       
                       ) ~ "intermittent",
      fetalSurv %in% c("continuCTG", "intermittentDopplerContinuCTGandSTAN",
                       "ContinuCTGandCTGandDoptone", "STAN", "CTGandDoptoneContinuCTG",
                       "intermittentDopplerContinuCTG", "intermittentCTGandSTAN", 
                       "intermittentCTGandContinuCTG", "intermittentDopplerAndSTAN",
                       "CTGandDoptoneSTAN", "continueCTGandSTAN", "intermittentCTGandContinuCTGandSTAN",
                       "CTGandDoptoneContinuCTGandSTAN"
                       ) ~ "continuous",
      fetalSurv %in% c("none") ~ "none"
    ),
    
    birthingPosition = case_when(
      birthingPosition %in% c("lithotomyWithoutLegSupports") ~ "LithotomyWithoutLegSupport",
      birthingPosition %in% c("lithotomyInLegSupports", "lithotomy") ~ "LithotomyInLegBraces",
      birthingPosition %in% c("handsKnees", "squatHandsKnees") ~ "HandsOnKnees",
      birthingPosition %in% c("lyingOnTheSide") ~ "lyingOnSide",
      birthingPosition %in% c("standing") ~ "standing",
      birthingPosition %in% c("stoolSit", "sittingOnToilet", "birthingStoolHandsKnees",
                              "birthingStool") ~ "birthingStool",
      birthingPosition %in% c("halfSitting", "squat", "squatHandKnee") ~ "squatting",
      birthingPosition %in% c("inBath", "squatInBath", "sittingInBath",
                              "handsKneesSupineInBath") ~ "lyingInBath",
      TRUE ~ birthingPosition
    ),
    
    placentalPhase = case_when(
      placentalPhase %in% c("spontaneous") ~ "spontaneous",
      placentalPhase %in% c("spontaneousFollowedByActivePolicy", "activePolicyImmediateSynto",
                            "spontaneousThenActivePolicy") ~ "activePolicy",
      placentalPhase %in% c("manualRevision") ~ "manualRevision",
      placentalPhase %in% c("unknown") ~ "unknown",
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
    
    resusc = case_when(
      resuscitation %in% c("none") ~ "none",
      resuscitation %in% c("5Inflations", "1InflationAndOxygenMaintained",
                           "1UnnecessaryInflationThenGoodBreathing") ~ "inflation",
      resuscitation %in% c("5InflationsVentilation","5InflationsVentilationChestCompressions",
                           "ventilation", "ventilationChestCompressions", "5InflationVentilationPEEPandCPAP",
                           "5InflationsVentilationPEEPandCPAP"
                           ) ~ "ventilation",
      resuscitation %in% c("tactilestimulation", "3x5InflationsAndTactileStimulation",
                           "5InflationsAndTactileSimulation", "tactileSimulation"
                           ) ~ "tactileStimulation",
      resuscitation %in% c("5InflationVentilationPEEPandCPAPaspirationAndTactileSimulation"
                           ) ~ "inflationVentilationTactileSimulation",
      resuscitation %in% c("PEEPandCPAP") ~ "PEEP CPAP",
      resuscitation %in% c("5InflationVentilationHeartMassagePEEPandCPAP") ~ "inflationVentilationHeartMassage",
      TRUE ~ resuscitation
    )
    
  )

### write CSV
write.csv(data2021fsimp, "../data/data2021fsimp.csv", row.names = FALSE)


### stats ----------------------------------------------------------------------

## -----perineum ---------------
(count <- data2021fsimp %>% # "intact" "2DT" "1DT" "episiotomy" "3DT" "4DT"
   count(perineum) )

(count <- data2021fsimp %>% # "birthCenter" "home" "hospital" "enroute"
    filter(perineum == "intact", birthPlace == "birthCenter") %>%
    nrow())

(cross_tab <- data2021fsimp %>%
    count(perineum, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----fetalSurv ---------------
(count <- data2021fsimp %>%
   count(fetalSurv))

(cross_tab <- data2021fsimp %>%
    count(fetalSurv, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----resuscitation ---------------
(count <- data2021fsimp %>%
   count(resuscitation))

(cross_tab <- data2021fsimp %>%
    count(resuscitation, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----placentalPhase ---------------
(count <- data2021fsimp %>%
   count(placentalPhase))

(cross_tab <- data2021fsimp %>%
    count(placentalPhase, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----episiotomy ---------------
(count <- data2021fsimp %>%
   count(episiotomy))

(cross_tab <- data2021fsimp %>%
    count(episiotomy, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----apgar1min ---------------
(count <- data2021fsimp %>%
   count(apgar1min))

(cross_tab <- data2021fsimp %>%
    count(apgar1min, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----apgar5min ---------------
(count <- data2021fsimp %>%
   count(apgar5min))

(cross_tab <- data2021fsimp %>%
    count(apgar5min, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----bloodLoss ---------------
(count <- data2021fsimp %>%
   count(bloodLoss))

(cross_tab <- data2021fsimp %>%
    count(bloodLoss, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))


## -----GBS ---------------
(count <- data2021fsimp %>%
   count(GBS))

(cross_tab <- data2021fsimp %>%
    count(GBS, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))


## -----waterbirth ---------------
(count <- data2021fsimp %>%
   count(bathBirth))

(cross_tab <- data2021fsimp %>%
    count(bathBirth, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----gender ---------------
(count <- data2021fsimp %>%
   count(gender))

(cross_tab <- data2021fsimp %>%
    count(gender, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----mothertongue ---------------
(count <- data2021fsimp %>%
   count(mothertongue))

(cross_tab <- data2021fsimp %>%
    count(mothertongue, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----resuscitation ---------------
(count <- data2021fsimp %>%
   count(resuscitation))

(cross_tab <- data2021fsimp %>%
    count(resuscitation, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----birthingPosition ---------------
(count <- data2021fsimp %>%
   count(birthingPosition))

(cross_tab <- data2021fsimp %>%
    count(birthingPosition, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))


## -----placentalPhase ---------------
(count <- data2021fsimp %>%
   count(placentalPhase))

(cross_tab <- data2021fsimp %>%
    count(placentalPhase, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----planned vs actual birth place  ---------------

(cross_tab <- data2021fsimp %>%
   count(plannedDelivPlac, actualDelivPlac) %>%
   pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))



## -----socio-demographic determinants --------------------------------------

## ---- mothertongue 
(count <- data2021fsimp %>%
   count(mothertongue))

(cross_tab <- data2021fsimp %>%
    count(mothertongue, plannedDelivPlac) %>%
    pivot_wider(names_from = mothertongue, values_from = n, values_fill = 0))

(cross_tab <- data2021fsimp %>%
    count(mothertongue, actualDelivPlac) %>%
    pivot_wider(names_from = mothertongue, values_from = n, values_fill = 0))

(cross_tab <- data2021fsimp %>%
    count(mothertongue, birthingResponsibility) %>%
    pivot_wider(names_from = mothertongue, values_from = n, values_fill = 0))

## ----- relationship  ---------------
(cross_tab <- data2021fsimp %>%
   count(relationship, actualDelivPlac) %>%
   pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

(count <- data2021fsimp %>%
    count(mothertongue))

(cross_tab <- data2021fsimp %>%
    count(mothertongue, birthingResponsibility) %>%
    pivot_wider(names_from = birthingResponsibility, values_from = n, values_fill = 0))

(cross_tab <- data2021fsimp %>%
    count(, birthingResponsibility) %>%
    pivot_wider(names_from = birthingResponsibility, values_from = n, values_fill = 0))



## -----Obstetric history --------------------------------------

## ---- pregnancyOnset 
(count <- data2021fsimp %>%
   count(pregnancyOnset))

(cross_tab <- data2021fsimp %>%
    count(pregnancyOnset, actualDelivPlac) %>%
    pivot_wider(names_from = pregnancyOnset, values_from = n, values_fill = 0))

