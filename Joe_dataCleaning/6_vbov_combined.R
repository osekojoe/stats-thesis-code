library(tidyr)
library(dplyr)

data2020fsimp <- read.csv("../data/data2020fsimp.csv")
data2021fsimp <- read.csv("../data/data2021fsimp.csv")
data2022fsimp <- read.csv("../data/data2022fsimp.csv")
data2023fsimp <- read.csv("../data/data2023fsimp.csv")

data2020fsimp <- data2020fsimp %>%
  rename(actualDelivPlac = birthPlace)

data2021fsimp <- data2021fsimp %>%
  rename(delivery = expulsionPhase)

data2022fsimp <- data2022fsimp %>%
  rename(delivery = expulsionPhase)

data2023fsimp <- data2023fsimp %>%
  rename(delivery = expulsionPhase)

data20fcomb <- data2020fsimp %>%
  mutate(pid2 = 10000 + row_number() - 1) %>%
  select(pid2, mural, perineum, birthingPosition, fetalSurv, placentalPhase, 
         bloodLoss, episiotomy, gender, GBS, bathBirth, apgar1min, apgar5min, 
         apgar1cat, apgar5cat, plannedDelivPlac, actualDelivPlac,
         resuscitation, mothertongue, maternalAge, pregnancyOnset, practiceCode,
         birthWeight, birthingResponsibility, nMiscarriage, nAbortion, zipcode,
         referralDueToStagnation, reasonForUrgentReferral,
         reasonsForNonUrgentReferral)

data21fcomb <- data2021fsimp %>%
  mutate(pid2 = 20000 + row_number() - 1) %>%
  select(pid2, mural, perineum, birthingPosition, fetalSurv, placentalPhase, 
         bloodLoss, episiotomy, gender, GBS, bathBirth, birthingPosition,
         apgar1min, apgar5min, apgar1cat, apgar5cat, plannedDelivPlac, actualDelivPlac,
         resuscitation, mothertongue, maternalAge, pregnancyOnset, delivery,
         practiceCode, birthWeight, birthingResponsibility, nMiscarriage, 
         nAbortion, zipcode, referralDueToStagnation,
         reasonForUrgentReferral, reasonForUrgentReferral)

data22fcomb <- data2022fsimp %>%
  mutate(pid2 = 30000 + row_number() - 1) %>%
  select(pid2, mural, perineum, birthingPosition, fetalSurv, placentalPhase, 
         bloodLoss, episiotomy, gender, GBS, bathBirth, birthingPosition,
         apgar1min, apgar5min, apgar1cat, apgar5cat, plannedDelivPlac, actualDelivPlac,
         resuscitation, mothertongue, maternalAge, pregnancyOnset, practiceCode,
         delivery, birthWeight, birthingResponsibility, nMiscarriage, nAbortion,
         relationship, educLevel, zipcode, referralDueToStagnation,
         reasonForUrgentReferral,reasonForReferral...47)

data23fcomb <- data2023fsimp %>%
  mutate(pid2 = 40000 + row_number() - 1) %>%
  select(pid2, mural, perineum, birthingPosition, fetalSurv, placentalPhase, 
         bloodLoss, episiotomy, gender, GBS, bathBirth, birthingPosition,
         apgar1min, apgar5min, apgar1cat, apgar5cat, plannedDelivPlac, actualDelivPlac,
         resuscitation, mothertongue, maternalAge, pregnancyOnset, practiceCode,
         delivery, birthingResponsibility, nMiscarriage, nAbortion, relationship,
         zipcode, reasonsForNonUrgentReferral, 
         referralDueToStagnation,reasonForUrgentReferral)


# Combine two dataframes by columns
vbov_comb <- bind_rows(data20fcomb, data21fcomb, data22fcomb, data23fcomb)

vbovf <- vbov_comb %>%
  mutate(
    
    delivery = case_when(
      delivery %in% c("spontaneously", "Spontaneous", "Spontaneous, Fundus pressure",
                      "Spontaneous, Kristeller Maneuver") ~ "spontaneous",
      is.na(delivery) ~ "spontaneous",
      TRUE ~ "nonspontan"
    ),
    
    birthingPosition = case_when(
      birthingPosition %in% c("NVT, section", "NVT, sectio") ~ "NVTandSectio",
      TRUE ~ birthingPosition
    ),
    
    bloodLoss = case_when(
      bloodLoss %in% c("< 500 ml", "<500ml") ~ "<500ml",
      bloodLoss %in% c("500 - 1000 ml", "500-1000ml") ~ "500-1000ml",
      bloodLoss %in% c("> 1000 ml", ">1000ml") ~ ">1000ml",
      TRUE ~ bloodLoss
    ),
    
    episiotomy = case_when(
      episiotomy %in% c("NVTandSectio", "NVT, sectio" , "NVT,sectio") ~ "NVTandSectio",
      TRUE ~ episiotomy
    ),
    
    birthingPosition = case_when(
      birthingPosition %in% c("NVT, section", "NVT, sectio") ~ "NVTandSectio",
      TRUE ~ birthingPosition
    ),
    
    actual = case_when(
      actualDelivPlac %in% c("Midwifeledunit", "midwifeLedUnit") ~ "midwifeLedUnit",
      TRUE ~ actualDelivPlac
    ),
    
    planned = case_when(
      plannedDelivPlac %in% c("hospitalWithMidwife", "hospitalWithObstetrician") ~ "hospital",
      plannedDelivPlac %in% c("Midwifeledunit") ~ "midwifeLedUnit",
      TRUE ~ plannedDelivPlac
    ),
    
    resuscitation = case_when(
      resuscitation %in% c("none", "sorting control") ~ "none",
      is.na(resuscitation) ~ "none",
      resuscitation %in% c("5Inflations", "inflations", "5 inflations", "1 inflation", "3 x 5 inflations",
                           "1UnnecessaryInflationThenGoodBreathing", "5 inflations, PEEP CPAP",
                           "2 times 5 inflations", "5 inflations, aspiration",
                           "1x inflation upon arrival of midwife - immediate resistance so inflation stopped",
                           "2Inflations", "3Inflations"
                           ) ~ "inflation",
      resuscitation %in% c("5InflationsAndVentilation", "ventilation", "5InflationsVentilation",
                           "5InflationVentilationPEEPandCPAP", "5InflationsVentilationPEEPandCPAP",
                           "inflationsAndVentilation", "5 inflations, ventilation", 
                           "5 inflations, ventilation, PEEP CPAP", "5InflationsVentilationChestCompressions",
                           "5InflationVentilationHeartMassagePEEPandCPAP",
                           "ventilationChestCompressions", "5InflationsVentilationHeartMassagePEEPandCPAP",
                           "ventilationPEEPandCPAP"
                           ) ~ "ventil",
      resuscitation %in% c("PEEP CPAP", "PEEPandCPAP") ~ "PEEP CPAP",
      
      resuscitation %in% c("tactilestimulation", "3x5InflationsAndTactileStimulation",
                           "5InflationVentilationPEEPandCPAPaspirationAndTactileSimulation",
                           "5InflationsAndTactileSimulation", "tactileSimulation",
                           "tactile stimulation", "stimulation", "stimulationInOpenAir"
                           
                           ) ~ "stimulation",
      resuscitation %in% c("yes") ~ "yes",
      resuscitation %in% c("aspiration", "oxygen mask", "mask + oxygen", "mask",
                           "aspiration + oxygen", "aspiration + oxygen + ventilation",
                           "oxygen mask + aspiration", "1InflationAndOxygenMaintained",
                           "oxygen", "suctionCup") ~ "oxygen",
      #resuscitation %in% c() ~ "",
      #resuscitation %in% c() ~ "",
      TRUE ~ resuscitation
    ),
    
    pregnancyOnset = case_when(
      pregnancyOnset %in% c("intraUterineInsemination", "IUI") ~ "IUI",
      pregnancyOnset %in% c("Hormonal", "hormonal") ~ "hormonal",
      TRUE ~ pregnancyOnset
    ),
    birthingResponsibility = case_when(
      birthingResponsibility %in% c("Obstetrician", "obstetrician") ~ "obstetrician",
      TRUE ~ birthingResponsibility
    ),
    
    miscarriage = case_when(
      nMiscarriage %in% c(0) ~ 0,
      nMiscarriage %in% c(1,2,3,4,5,6,7) ~ 1,
      TRUE ~ nMiscarriage
    ),
    
    abortion = case_when(
      nAbortion %in% c(0) ~ 0,
      nAbortion %in% c(1,2,3) ~ 1,
      TRUE ~ nAbortion
    ),
    
    mothertongue = case_when(
      mothertongue %in% c("dutch", "Dutch") ~ "dutch",
      mothertongue %in% c("french", "French") ~ "french",
      mothertongue %in% c("english", "English") ~ "english",
      mothertongue %in% c("others", "other") ~ "other",
      TRUE ~ mothertongue
    ),
    
    referral = if_else(
      (!is.na(reasonForReferral) & reasonForReferral != "") |
        (!is.na(referralDueToStagnation) & referralDueToStagnation != "") |
        (!is.na(reasonForUrgentReferral) & reasonForUrgentReferral != "") |
        (!is.na(reasonsForNonUrgentReferral) & reasonsForNonUrgentReferral != ""),
      1,
      0
    )
  )


### exclude unknown from bloodLoss,  ---------
#vbovf <- vbovf %>%
#  filter(bloodLoss != "enroute") %>%
#  filter(!is.na(zipcode))


### write CSV
write.csv(vbovf, "../data/vbov_comb2.csv", row.names = FALSE)

## -----resuscitation ---------------

(cross_tab <- vbovf %>%
    count(birthingResponsibility, mural) %>%
    pivot_wider(names_from = mural, values_from = n, values_fill = 0))

## -----resuscitation ---------------
(count <- vbovf %>%
   count(referral))

(cross_tab <- vbovf %>%
    count(resuscitation, mural) %>%
    pivot_wider(names_from = mural, values_from = n, values_fill = 0))

## -----delivery ---------------
(cross_tab <- vbovf %>%
    count(delivery, mural) %>%
    pivot_wider(names_from = mural, values_from = n, values_fill = 0))


### perform symmetry test - Bowker's
library(vcd)

# Create the contingency table
birth_table <- matrix(c(455, 15, 312, 200,
                        8, 2148, 399, 1,
                        8, 178, 1239, 1,
                        0, 0, 84, 394),
                      nrow = 4, byrow = TRUE,
                      dimnames = list(Planned = c("birthCenter", "Home", "hospital", "MLU"),
                                      Actual = c("birthCenter", "Home", "hospital", "MLU")))

# Print the table
print(birth_table)

library(rcompanion)

# Perform Bowker's test
nominalSymmetryTest(birth_table,
                    digits = 3,
                    MonteCarlo = TRUE,
                    exact = TRUE,
                    ntrial = 100000)


mcnemar.test(birth_table)


