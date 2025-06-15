library(dplyr)

load("../data/select2023.Rdata")

## add unique patient identifier pid
select2023 <- select2023 %>%
  mutate(pid = row_number()) %>%
  select(pid, everything()) # reorder columns with pid first

data2023fcoals <- select2023 %>%
  mutate(
    bathBirth = coalesce(bathBirth, BathBirth),
    birthingPosition = coalesce(birthingPosition, BirthingPosition),
    fetalSurv = coalesce(fetalSurveillance...24, fetalSurveillance...62),
    GBS = coalesce(GBS...26, GBS...63),
    placentaPhase = coalesce(placentaPhase...34, placentalPhase...68),
    bloodLoss = coalesce(bloodLoss, estimatedBloodLoss),
    perineum = coalesce(perineum...37, perineum...71),
    birthWeight = coalesce(birthWeight...42, birthWeight...73),
    gender = coalesce(gender...43, gender...74),
    apgar1min = coalesce(apgar1min...44, apgar1min...75),
    apgar5min = coalesce(apgar5min...45, apgar5min...76),
    resuscitation = coalesce(resuscitation, resuscitation_more),
    actualDelivPlac = place
  )


data2023f <- data2023fcoals %>%
  mutate(
    
    actualDelivPlac = case_when(
      actualDelivPlac %in% c("maternityWard", "hospital") ~ "hospital",
      TRUE ~ actualDelivPlac
    ),
    
    nParity = case_when(
      nParity == "NULL" ~ NA,
      TRUE ~ nParity
    ),
    nParity = as.numeric(nParity),
    
    bathBirth = case_when(
      bathBirth == "NULL" ~ "no",
      bathBirth == "NVT, sectio" ~ "no",
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
    
    placentaPhase = case_when(
      placentaPhase == "NULL" ~ NA,
      placentaPhase == "Spontaneous" ~ "spontaneous",
      placentaPhase == "Spontaneous followed by active policy" ~ "spontaneously followed by active policy",
      placentaPhase == "Active policy (immediate synto)" ~ "active policy (immediate synto)",
      placentaPhase == "Manual revision" ~ "manual revision",
      placentaPhase == "NVT, sectio" ~ "sectio",
      placentaPhase == "section" ~ "sectio",
      TRUE ~ placentaPhase
    ),
    
    bloodLoss = case_when(
      bloodLoss == "NULL" ~ "unknown",
      TRUE ~ bloodLoss
    ),
    
    perineum = case_when(
      perineum == "1stDegreeTear" ~ "1DT",
      perineum == "1st degree" ~ "1DT",
      perineum == "2ndDegreeTear" ~ "2DT",
      perineum == "2nd degree" ~ "2DT",
      perineum == "3rdDegreeTear" ~ "3DT",
      perineum == "3rd degree" ~ "3DT",
      perineum == "4thDegreeTear" ~ "4DT",
      perineum == "4th degree" ~ "4DT",
      perineum == "NULL" ~ "intact",
      TRUE ~ perineum
    ),
    
    episiotomy = case_when(
      episiotomy == "NULL" ~ "no", 
      is.na(episiotomy) ~ "no",
      TRUE ~ episiotomy
    ),
    
    birthWeight = as.numeric(birthWeight),
    
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
        GBS == "NULL" ~ "none",
        GBS == "positive" & is.na(prolongedMembranesRuptureAndGBSstatus) ~ NA,
        GBS == "negative" ~ "none",
        TRUE ~ prolongedMembranesRuptureAndGBSstatus
      ),
      
    plannedDelivPlac = case_when(
      plannedDelivPlac == "NULL" ~ NA,
      TRUE ~ plannedDelivPlac
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
    
    abortion = case_when(
      abortion == "NULL" ~ "none",
      TRUE ~ abortion
    ),
    
    pregnancyOnset = case_when(
      pregnancyOnset == "Spontaneous" ~ "spontaneous",
      pregnancyOnset == "NULL" ~ "spontaneous",
      TRUE ~ pregnancyOnset
    ),
    
    practiceCode = case_when(
      practiceCode == "NULL" ~ NA,
      TRUE ~ practiceCode
    ),
    practiceCode = as.numeric(practiceCode),
    
    zipcode = case_when(
      zipcode == "NULL" ~ NA,
      TRUE ~ zipcode
    ),
    zipcode = as.numeric(zipcode),
    
)




data2023fs <- data2023f %>%
  select(pid, nParity, firstOrMultiParity, bathBirth, 
         birthingPosition,fetalSurv, GBS, placentaPhase,
         bloodLoss, perineum, PMRGBS, relationship,
         episiotomy, birthWeight, gender, apgar1min, apgar5min,
         resuscitation, plannedDelivPlac, actualDelivPlac, 
         birthingResponsibility, maternalAge, mothertongue, 
         pregnancyOnset, nMiscarriage, nAbortion, practiceCode,
         province, zipcode, expulsionPhase, reasonsForNonUrgentReferral, 
         referralDueToStagnation,reasonForUrgentReferral
  )

#write.csv(data2023fs, "../data/data2023fs.csv", row.names = FALSE)


### exclude birthplace=enroute and postcode NOT belgie ---------
data2023fs <- data2023fs %>%
  filter(actualDelivPlac != "enroute") %>%
  filter(!is.na(zipcode))
### -----------------------------------------------------------

colSums(is.na(data2023fs))


data2023fsimp <- data2023fs %>%
  mutate(
    
    mural = case_when(
      actualDelivPlac %in% c("birthCenter", "home") ~ "extramural",
      actualDelivPlac %in% c("hospital", "midwifeLedUnit") ~ "intramural"
    ),
    
    episiotomy = case_when(
      episiotomy %in% c("NVT", "NVT, sectio") ~ "NVT,sectio",
      episiotomy %in% c("no", "unknown") ~ "no",
      is.na(episiotomy) ~ "no",
      TRUE ~ episiotomy
    ),
    
    perineum = case_when(
      perineum %in% c("intact", "Labia", "intact, labia", "NVT, sectio",
                      "vaginaWound", "vaginal wall rupture", "labia",
                      "abrasionWound, not stitched",
                      "abrasion, stitched", "abrasion, not stitched",
                      "abrasionWound, stitched", "NVT") ~ "intact",
      perineum %in% c("episiotomy", "4th degree, episiotomy", "2nd degree, episiotomy",
                      "1st degree, episiotomy", "3rd degree, episiotomy" 
                      ) ~ "episiotomy",
      TRUE ~ perineum
    ),
    
    #resuscitation = case_when(
    #  resuscitation %in% c() ~ ""
    #),
    
    fetalSurv = case_when(
      fetalSurv %in% c("Intermittent Doppler", "Intermittent CTG", "CTG + doptone",
                       "Intermittent doppler", "intermittent pinard on request of parents"
                       ) ~ "intermittent",
      fetalSurv %in% c("Intermittent doppler, Continu CTG",
                       "CTG + doptone, Continu CTG", "Continu CTG",
                       "CTG Intermittent, STAN", "CTG + doptone, Continu CTG, STAN",
                       "CTG + doptone, STAN", "Intermittent Doppler, Continuous CTG",
                       "Continuous CTG", "Intermittent doppler, Continu CTG, STAN",
                       "Intermittent CTG, Continu CTG, STAN", 
                       "Intermittent CTG, Continu CTG", "STAN", 
                       "CTG + doptone, Continuous CTG", "CTG + doptone, continu CTG",
                       "CTG intermittent, CTG continuous", "continu CTG, STAN"
                       ) ~ "continuous",
      fetalSurv %in% c("none", "Unable to listen to FHT due to too fast delivery",
                       "None, baby born before arrival of midwife",
                       "MIU when listening to heart sounds for the first time"
                       ) ~ "none",
      TRUE ~ fetalSurv
    ),
    
    relationship = case_when(
      relationship %in% c("single") ~ "Single/never married",
      relationship %in% c("married", "actual cohabitation", "legal cohabitation",
                          "LAT-relationship", "cohabitation", "not cohabiting"
                          ) ~ "Married/living with partner",
      relationship %in% c("other relationship") ~ "other relationship",
      relationship %in% c("partner died during pregnancy") ~ "Widowed",
    ),
    
    birthingPosition = case_when(
      birthingPosition %in% c("lithotomyWithoutLegSupports", "Lithotomy without leg supports",
                              "Hands-knees, Side-lying, Lithotomy without leg supports",
                              "Hurk, Zijlig, Lithotomie zonder beensteunen",
                              "Hands-knees, Lithotomy without leg supports") ~ "LithotomyWithoutLegSupport",
      birthingPosition %in% c("Lithotomy in leg braces", "Side lying, Lithotomy in leg supports",
                              "lithotomyInLegSupports", "Lithotomy without leg braces, Lithotomy in leg braces"
                              ) ~ "LithotomyInLegBraces",
      birthingPosition %in% c("handsKnees", "Hands-knees") ~ "HandsOnKnees",
      birthingPosition %in% c("lyingOnTheSide", "Sideways") ~ "lyingOnSide",
      birthingPosition %in% c("standing") ~ "standing",
      birthingPosition %in% c("sectio", "NVT, sectio") ~ "NVT, sectio",
      birthingPosition %in% c("stoolSit", "sitting position", "semi-seated position",
                              "Birthing stool", "stool seat") ~ "birthingStool",
      birthingPosition %in% c("squat", "kneeling") ~ "squatting",
      birthingPosition %in% c("lyingInBath", "Hands and knees, Side lying, Lithotomy without leg rests, Lying in bath",
                              "inBath", "Lying in the bath") ~ "lyingInBath",
      TRUE ~ birthingPosition
    ),
    
    placentalPhase = case_when(
      placentaPhase %in% c("spontaneous") ~ "spontaneous",
      placentaPhase %in% c("active policy (immediate synto)",
                    "spontaneously followed by active policy"
        ) ~ "activePolicy",
      placentaPhase %in% c("manual revision") ~ "manualRevision",
      #placentaPhase %in% c("unknown") ~ "unknown",
      placentaPhase %in% c("sectio") ~ "NVTandSectio",
      TRUE ~ placentaPhase
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
      mothertongue %in% c("Dutch") ~ "Dutch",
      mothertongue %in% c("French") ~ "French",
      mothertongue %in% c("English") ~ "English",
      mothertongue %in% c("German", "Russian",  "Turkish", "Bulgarian",
                          "Arabic", "Hungarian", "Chinese", "Pakistani",
                          "Croatian", "Italian", "Kinyarwanda", "Spanish",
                          "Czech", "Bosnian", "Hebrew", "Polish", "Portuguese"
                          ) ~ "other",
      mothertongue %in% c("NULL") ~ NA,
      TRUE ~ mothertongue
    )
  )


### write CSV
write.csv(data2023fsimp, "../data/data2023fsimp.csv", row.names = FALSE)


### stats ----------------------------------------------------------------------

(count <- data2023fsimp %>% 
   count(mural) )

## -----perineum ---------------
(count <- data2023fsimp %>% 
   count(perineum) )

(cross_tab <- data2023fsimp %>%
    count(perineum, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----fetalSurv ---------------
(count <- data2023fsimp %>%
   count(fetalSurv))

(cross_tab <- data2023fsimp %>%
    count(fetalSurv, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----episiotomy ---------------
(count <- data2023fsimp %>%
   count(episiotomy))

(cross_tab <- data2023fsimp %>%
    count(episiotomy, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----apgar1min ---------------
(count <- data2023fsimp %>%
   count(apgar1min))

(cross_tab <- data2023fsimp %>%
    count(apgar1min, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----apgar5min ---------------
(count <- data2023fsimp %>%
   count(apgar5min))

(cross_tab <- data2023fsimp %>%
    count(apgar5min, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----bloodLoss ---------------
(count <- data2023fsimp %>%
   count(bloodLoss))

(cross_tab <- data2023fsimp %>%
    count(bloodLoss, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))


## -----GBS ---------------
(count <- data2023fsimp %>%
   count(GBS))

(cross_tab <- data2023fsimp %>%
    count(GBS, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----placentalPhase ---------------
(count <- data2023fsimp %>%
   count(placentalPhase))

(cross_tab <- data2023fsimp %>%
    count(placentalPhase, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----waterbirth ---------------
(count <- data2023fsimp %>%
   count(bathBirth))

(cross_tab <- data2023fsimp %>%
    count(bathBirth, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----gender ---------------
(count <- data2023fsimp %>%
   count(gender))

(cross_tab <- data2023fsimp %>%
    count(gender, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----mothertongue ---------------
(count <- data2023fsimp %>%
   count(mothertongue))

(cross_tab <- data2023fsimp %>%
    count(mothertongue, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

## -----resuscitation ---------------
(count <- data2023fsimp %>%
   count(resuscitation))

(cross_tab <- data2023fsimp %>%
    count(resuscitation, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))


## -----birthingPosition ---------------
(count <- data2023fsimp %>%
   count(birthingPosition))

(cross_tab <- data2023fsimp %>%
    count(birthingPosition, actualDelivPlac) %>%
    pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))


## -----planned vs actual birth place  ---------------

(cross_tab <- data2023fsimp %>%
   count(plannedDelivPlac, actualDelivPlac) %>%
   pivot_wider(names_from = actualDelivPlac, values_from = n, values_fill = 0))

(count <- data2023fsimp %>%
    count(mothertongue))

(cross_tab <- data2023fsimp %>%
    count(mothertongue, birthingResponsibility) %>%
    pivot_wider(names_from = birthingResponsibility, values_from = n, values_fill = 0))

(cross_tab <- data2023fsimp %>%
    count(, birthingResponsibility) %>%
    pivot_wider(names_from = birthingResponsibility, values_from = n, values_fill = 0))


## -----socio-demographic determinants --------------------------------------

## ---- mothertongue 
(count <- data2023fsimp %>%
    count(mothertongue))

(cross_tab <- data2023fsimp %>%
    count(mothertongue, plannedDelivPlac) %>%
    pivot_wider(names_from = mothertongue, values_from = n, values_fill = 0))

(cross_tab <- data2023fsimp %>%
    count(mothertongue, actualDelivPlac) %>%
    pivot_wider(names_from = mothertongue, values_from = n, values_fill = 0))

(cross_tab <- data2023fsimp %>%
    count(mothertongue, birthingResponsibility) %>%
    pivot_wider(names_from = mothertongue, values_from = n, values_fill = 0))

## ---- relationship 
(count <- data2023fsimp %>%
    count(relationship))

(cross_tab <- data2023fsimp %>%
    count(relationship, plannedDelivPlac) %>%
    pivot_wider(names_from = relationship, values_from = n, values_fill = 0))

(cross_tab <- data2023fsimp %>%
    count(relationship, actualDelivPlac) %>%
    pivot_wider(names_from = relationship, values_from = n, values_fill = 0))

(cross_tab <- data2023fsimp %>%
    count(relationship, birthingResponsibility) %>%
    pivot_wider(names_from = relationship, values_from = n, values_fill = 0))



## -----Obstetric history --------------------------------------

## ---- pregnancyOnset 
(count <- data2023fsimp %>%
   count(pregnancyOnset))

(cross_tab <- data2023fsimp %>%
    count(pregnancyOnset, actualDelivPlac) %>%
    pivot_wider(names_from = pregnancyOnset, values_from = n, values_fill = 0))

