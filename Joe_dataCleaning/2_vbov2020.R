library(dplyr)
library(tidyr)

load("../data/select2020.Rdata")

## add unique patient identifier pid

select2020 <- select2020 %>%
  mutate(pid = row_number()) %>%
  select(pid, everything()) # reorder columns with pid first

### fill NA values of one column with values from a similar column with suffix
data2020f <- select2020 %>%
  mutate(birthWeight = coalesce(birthWeight...27, birthWeight...44),
         gender = coalesce(gender, gender...45),
         apgar1min = coalesce(apgar1min...29, as.numeric(apgar1min...46)),
         apgar5min = coalesce(apgar5min...30, as.numeric(apgar5min...47)),
         birthWeight = coalesce(as.numeric(birthWeight...27), as.numeric(birthWeight...44)),
         gender = coalesce(gender, gender...45),
         resuscitation = coalesce(resuscitation, resuscitation...31),
         
         fetalSurv = case_when(
           fetalSurveillance == "None" ~ "none",
           fetalSurveillance == "" ~ NA,
           is.na(fetalSurveillance) ~ NA,
           TRUE ~ fetalSurveillance
         ),
         
         perineum = case_when(
           perineum == "1stDegreeTear" ~ "1DT",
           perineum == "2ndDegreeTear" ~ "2DT",
           perineum == "3rdDegreeTear" ~ "3DT",
           perineum == "4thDegreeTear" ~ "4DT",
           TRUE ~ perineum
         ),
         
         episiotomy = case_when(
           is.na(episiotomy) ~  "no",
           TRUE ~ episiotomy
         ),
         
         GBS = case_when(
           is.na(GBS) ~ "negative",
           TRUE ~ GBS
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
         
         resuscitation = case_when(
           resuscitation == "none" ~ "none",
           resuscitation == "None" ~ "none",
           is.na(resuscitation) ~ "none",
           TRUE ~ resuscitation
         ),
         
         epidural = case_when(
           is.na(epidural) ~ "no",
           TRUE ~ epidural
         ),
         
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
         
         practiceCode = case_when(
           practiceCode == "NULL" ~ NA,
           TRUE ~ practiceCode
         ),
         practiceCode = as.numeric(practiceCode)
  )



(missing_counts <- colSums(is.na(data2020f)))

data2020fs <- data2020f %>%
  select(pid, nParity, bathBirth, birthingPosition, GBS, fetalSurv, 
         placentalPhase, bloodLoss, delivery, perineum, episiotomy,
         birthWeight, gender, apgar1min, apgar5min, resuscitation, 
         PMRGBS, epidural, plannedDelivPlac, birthPlace, birthingResponsibility,
         mothertongue, maternalAge, nMiscarriage, nAbortion, pregnancyOnset,
         practiceCode, zipcode, referralDueToStagnation, reasonForUrgentReferral,
         reasonsForNonUrgentReferral)

(missing_counts <- colSums(is.na(data2020fs)))



### exclude birthplace=enroute and postcode NOT belgie ---------
data2020fs <- data2020fs %>%
  filter(birthPlace != "enroute") %>%
  filter(!is.na(zipcode))
### -----------------------------------------------------------


data2020fsimp <- data2020fs %>%
  mutate(
    
    mural = case_when(
      birthPlace %in% c("birthCenter", "home") ~ "extramural",
      birthPlace %in% c("hospital") ~ "intramural"
    ),
    
    perineum = case_when(
      perineum %in% c("intact", "labia") ~ "intact",
      TRUE ~ perineum
    ),
    
    fetalSurv = case_when(
      fetalSurv %in% c("intermittentDoppler", "intermittentCTG", 
                       "intermittentCTGandDoppler") ~ "intermittent",
      fetalSurv %in% c("continuousCTG") ~ "continuous",
      fetalSurv %in% c("none") ~ "none",
      TRUE ~ fetalSurv
    ),
    
    birthingPosition = case_when(
      birthingPosition %in% c("LithotomyWithoutLegSupport") ~ "LithotomyWithoutLegSupport",
      birthingPosition %in% c("LithotomyInLegBraces") ~ "LithotomyInLegBraces",
      birthingPosition %in% c("HandsKnees") ~ "HandsOnKnees",
      birthingPosition %in% c("sideLying") ~ "lyingOnSide",
      birthingPosition %in% c("standing") ~ "standing",
      birthingPosition %in% c("stoolChair") ~ "birthingStool",
      birthingPosition %in% c("squat") ~ "squatting",
      TRUE ~ birthingPosition
    ),
    
    placentalPhase = case_when(
      placentalPhase %in% c("spontaneous") ~ "spontaneous",
      placentalPhase %in% c("ActivePolicy", "spontaneousThenActivePolicy") ~ "activePolicy",
      placentalPhase %in% c("manualRevision") ~ "manualRevision",
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
      resuscitation %in% c("5Inflations") ~ "inflation",
      resuscitation %in% c("ventilation", "5InflationsAndVentilation") ~ "ventilation",
      resuscitation %in% c("PEEP CPAP") ~ "PEEP CPAP",
      TRUE ~ resuscitation
    )
  )

### write CSV
write.csv(data2020fsimp, "../data/data2020fsimp.csv", row.names = FALSE)


### stats ----------------------------------------------------------------------

(count <- data2020fsimp %>% 
   count(mural) )

(cross_tab <- data2020fsimp %>%
    count(mural, perineum) %>%
    pivot_wider(names_from = mural, values_from = n, values_fill = 0))

## -----delivery ---------------
(count <- data2020fsimp %>% # "spontaneously" "ventouse"  "sectio" "forceps"
  count(delivery))

(count <- data2020fsimp %>% # "birthCenter" "home" "hospital" "enroute"
  filter(delivery == "forceps", birthPlace == "hospital") %>%
  nrow())

(cross_tab <- data2020fsimp %>%
  count(birthPlace, delivery) %>%
  pivot_wider(names_from = birthPlace, values_from = n, values_fill = 0))

## -----perineum ---------------
(count <- data2020fsimp %>% 
  count(perineum) )

(count <- data2020fsimp %>% # "birthCenter" "home" "hospital" "enroute"
    filter(perineum == "intact", birthPlace == "birthCenter") %>%
    nrow())

(cross_tab <- data2020fsimp %>%
    count(perineum, birthPlace) %>%
    pivot_wider(names_from = birthPlace, values_from = n, values_fill = 0))

## -----fetalSurv ---------------
(count <- data2020fsimp %>%
  count(fetalSurv))

(cross_tab <- data2020fsimp %>%
    count(fetalSurv, birthPlace) %>%
    pivot_wider(names_from = birthPlace, values_from = n, values_fill = 0))

## -----epidural ---------------
(count <- data2020fsimp %>%
   count(epidural))

(cross_tab <- data2020fsimp %>%
    count(epidural, birthPlace) %>%
    pivot_wider(names_from = birthPlace, values_from = n, values_fill = 0))

(cross_tab <- data2020fsimp %>%
    count(epidural, mural) %>%
    pivot_wider(names_from = mural, values_from = n, values_fill = 0))

## -----episiotomy ---------------
(count <- data2020fsimp %>%
   count(episiotomy))

(cross_tab <- data2020fsimp %>%
    count(episiotomy, birthPlace) %>%
    pivot_wider(names_from = birthPlace, values_from = n, values_fill = 0))

## -----apgar1min ---------------
(count <- data2020fsimp %>%
   count(apgar1min))

(cross_tab <- data2020fsimp %>%
    count(apgar1min, birthPlace) %>%
    pivot_wider(names_from = birthPlace, values_from = n, values_fill = 0))

## -----apgar5min ---------------
(count <- data2020fsimp %>%
   count(apgar5min))

(cross_tab <- data2020fsimp %>%
    count(apgar5min, birthPlace) %>%
    pivot_wider(names_from = birthPlace, values_from = n, values_fill = 0))

## -----bloodLoss ---------------
(count <- data2020fsimp %>%
   count(bloodLoss))

(cross_tab <- data2020fsimp %>%
    count(bloodLoss, birthPlace) %>%
    pivot_wider(names_from = birthPlace, values_from = n, values_fill = 0))


## -----GBS ---------------
(count <- data2020fsimp %>%
   count(GBS))

(cross_tab <- data2020fsimp %>%
    count(GBS, birthPlace) %>%
    pivot_wider(names_from = birthPlace, values_from = n, values_fill = 0))


## -----waterbirth ---------------
(count <- data2020fsimp %>%
   count(bathBirth))

(cross_tab <- data2020fsimp %>%
    count(bathBirth, birthPlace) %>%
    pivot_wider(names_from = birthPlace, values_from = n, values_fill = 0))

## -----gender ---------------
(count <- data2020fsimp %>%
   count(gender))

(cross_tab <- data2020fsimp %>%
    count(gender, birthPlace) %>%
    pivot_wider(names_from = birthPlace, values_from = n, values_fill = 0))

## -----mothertongue ---------------
(count <- data2020fsimp %>%
   count(mothertongue))

(cross_tab <- data2020fsimp %>%
    count(mothertongue, birthPlace) %>%
    pivot_wider(names_from = birthPlace, values_from = n, values_fill = 0))

## -----resuscitation ---------------
(count <- data2020fsimp %>%
   count(resuscitation))

(cross_tab <- data2020fsimp %>%
    count(resuscitation, birthPlace) %>%
    pivot_wider(names_from = birthPlace, values_from = n, values_fill = 0))

## -----birthingPosition ---------------
(count <- data2020fsimp %>%
   count(birthingPosition))

(cross_tab <- data2020fsimp %>%
    count(birthingPosition, birthPlace) %>%
    pivot_wider(names_from = birthPlace, values_from = n, values_fill = 0))


## -----placentalPhase ---------------
(count <- data2020fsimp %>%
   count(placentalPhase))

(cross_tab <- data2020fsimp %>%
    count(placentalPhase, birthPlace) %>%
    pivot_wider(names_from = placentalPhase, values_from = n, values_fill = 0))

## -----planned vs actual birth place  ---------------

(cross_tab <- data2020fsimp %>%
   count(plannedDelivPlac, birthPlace) %>%
   pivot_wider(names_from = birthPlace, values_from = n, values_fill = 0))


## -----socio-demographic determinants --------------------------------------

## ---- mothertongue 
(count <- data2020fsimp %>%
   count(mothertongue))

(cross_tab <- data2020fsimp %>%
    count(mothertongue, plannedDelivPlac) %>%
    pivot_wider(names_from = mothertongue, values_from = n, values_fill = 0))

(cross_tab <- data2020fsimp %>%
    count(mothertongue, birthPlace) %>%
    pivot_wider(names_from = mothertongue, values_from = n, values_fill = 0))

(cross_tab <- data2020fsimp %>%
    count(mothertongue, birthingResponsibility) %>%
    pivot_wider(names_from = mothertongue, values_from = n, values_fill = 0))


## -----Obstetric history --------------------------------------

## ---- pregnancyOnset 
(count <- data2020fsimp %>%
   count(pregnancyOnset))

(cross_tab <- data2020fsimp %>%
    count(pregnancyOnset, birthPlace) %>%
    pivot_wider(names_from = pregnancyOnset, values_from = n, values_fill = 0))

## ----------------------------------------------------------------------------
## make levels numerical
data2020fsn <- data2020fs %>%
  mutate(
    bathBirth = as.character(bathBirth),
    bathBirthBin = case_when(
      bathBirth == "no" ~ 0,
      bathBirth == "yes" ~ 1,
      TRUE ~ NA_real_
    ),
    
    birthingPosition = birthingPosition,
    
    GBS = as.character(GBS),
    GBSCat = case_when(
      GBS == "negative" ~ 1,
      GBS == "positive" ~ 2,
      GBS == "unknown" ~ 3,
      TRUE ~ NA_real_
    ),
    
    fetalSurvCat = case_when(
      fetalSurv == "none" ~ 1,
      fetalSurv == "intermittentDoppler" ~ 2,
      fetalSurv == "continuousCTG" ~ 3,
      fetalSurv == "intermittentCTG" ~ 4,
      fetalSurv == "intermittentCTGandDoppler" ~ 5,
      TRUE ~ NA_real_
    ),
    
    placentPhaseCat = case_when(
      placentalPhase == "spontaneous" ~ 1,
      placentalPhase == "spontaneousThenActivePolicy" ~ 2,
      placentalPhase == "manualRevision" ~ 3,
      placentalPhase == "ActivePolicy" ~ 4,
      TRUE ~ NA_real_
    ),
    
    bloodLossCat = case_when(
      bloodLoss == "< 500 ml" ~ 1,
      bloodLoss == "500 - 1000 ml" ~ 2,
      bloodLoss == "> 1000 ml" ~ 3,
      TRUE ~ NA
    ),
    
    deliveryCat = case_when(
      delivery == "spontaneously" ~ 1,
      delivery == "ventouse" ~ 2,
      delivery == "sectio" ~ 3,
      delivery == "forceps" ~ 4,
      TRUE ~ NA_real_
    ),
    
    perineumCat = case_when(
      perineum == "1DT" ~ 1,
      perineum == "2DT" ~ 2,
      perineum == "3DT" ~ 3,
      perineum == "4DT" ~ 4,
      perineum == "intact" ~ 5,
      perineum == "episiotomy" ~ 6,
      perineum == "labia" ~ 7,
      TRUE ~ NA_real_
    ),
    
    episiotomyBin = case_when(
      episiotomy == "no" ~ 0,
      episiotomy == "yes" ~ 1,
      TRUE ~ NA_real_
    ),
    
    genderBin = case_when(
      gender == "girl" ~ 0,
      gender == "boy" ~ 1,
      TRUE ~ NA_real_
    ),
    
    resuscitationCat = case_when(
      resuscitation == "none" ~ 1,
      resuscitation == "5Inflations" ~ 2,
      resuscitation == "5InflationsAndVentilation" ~ 3,
      resuscitation == "ventilation" ~ 4,
      resuscitation == "PEEP CPAP" ~ 5,
      TRUE ~ NA_real_
    ),
    
    # PMRGBSCat = case_when()
    
    epiduralBin = case_when(
      epidural == "no" ~ 0,
      epidural == "yes" ~ 1,
      TRUE ~ NA_real_
    ),
    
    plannedDelivPlacCat = case_when(
      plannedDelivPlac == "birthCenter" ~ 1,
      plannedDelivPlac == "home" ~ 2,
      plannedDelivPlac == "hospitalWithMidwife" ~ 3,
      plannedDelivPlac == "hospitalWithObstetrician" ~ 4,
      TRUE ~ NA_real_
    ),
    
    actualBirthPlacCat = case_when(
      birthPlace == "birthCenter" ~ 1,
      birthPlace == "home" ~ 2,
      birthPlace == "hospital" ~ 3,
      birthPlace == "enroute" ~ 4,
      TRUE ~ NA_real_
    ),
    
    birthResponsBin = case_when(
      birthingResponsibility == "Obstetrician" ~ 0,
      birthingResponsibility == "midwife" ~ 1,
    ),
    
    mothertongueCat = case_when(
      mothertongue == "dutch" ~ 1,
      mothertongue == "french" ~ 2,
      mothertongue == "english" ~ 3,
      mothertongue == "others" ~ 4,
      TRUE ~ NA_real_
    ),
    
    pregnancyOnsetCat = case_when(
      pregnancyOnset == "spontaneous" ~ 1,
      pregnancyOnset == "KI" ~ 2,
      pregnancyOnset == "hormonal" ~ 3,
      pregnancyOnset == "IVF" ~ 4,
      pregnancyOnset == "ICSI" ~ 5,
    ),
    
    practiceCode = case_when(
      is.na(practiceCode) ~ NA_real_,
      TRUE ~ practiceCode
    )
  )

data2020fsnf <- data2020fsn %>%
  select(
    pid, bathBirthBin, GBSCat, fetalSurvCat, placentPhaseCat, bloodLossCat,
    deliveryCat, perineumCat, episiotomyBin, genderBin, resuscitationCat,
    epiduralBin, plannedDelivPlacCat, actualBirthPlacCat, birthResponsBin, 
    mothertongueCat, pregnancyOnsetCat, maternalAge, nMiscarriage, nAbortion, 
    practiceCode, zipcode
  )

(missing_counts <- colSums(is.na(data2020fsnf)))

write.csv(data2020fs, "../data/data2020fs.csv", row.names = FALSE)

