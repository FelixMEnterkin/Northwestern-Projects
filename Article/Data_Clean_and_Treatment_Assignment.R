# Data Clean and Treatment Assignment 

all_packages <-
  c(
    "ggplot2",
    "psych",
    "cowplot", 
    "readr",
    "modelsummary",
    "vtable",
    "broom",
    "flextable",
    "tidyverse",
    "moments",
    "lubridate",
    "jtools",
    "gt"
  )

lapply(all_packages, library, character.only = TRUE)

# assign data 
ESS <- df


### Cleaning ESS Data 

## Re-coding Outcome Variables 

ESS$Trust <- ifelse(ESS$trstplc > 10, NA, ESS$trstplc)



ESS$bplcdc <- ifelse(ESS$bplcdc > 10, NA, ESS$bplcdc)
ESS$doplcsy <- ifelse(ESS$doplcsy > 10, NA, ESS$doplcsy)
ESS$dpcstrb <- ifelse(ESS$dpcstrb > 10, NA, ESS$dpcstrb)

alpha(subset(ESS, select = c(bplcdc, doplcsy, dpcstrb)), check.keys =  TRUE)
ESS$Obligation <- (ESS$bplcdc + ESS$doplcsy + ESS$dpcstrb)/3



ESS$plcrgwr <- ifelse(ESS$plcrgwr > 5, NA, ESS$plcrgwr)
ESS$plcipvl <- ifelse(ESS$plcipvl > 5, NA, ESS$plcipvl)
ESS$gsupplc <- ifelse(ESS$gsupplc > 5, NA, ESS$gsupplc)
ESS$plcrgwr <- (6 - ESS$plcrgwr)
ESS$plcipvl <- (6 - ESS$plcipvl)
ESS$gsupplc <- (6 - ESS$gsupplc)
ESS$MoralAlignment <- (ESS$plcrgwr + ESS$plcipvl + ESS$gsupplc)/3  



ESS$plcrspc <- ifelse(ESS$plcrspc > 6, NA, ESS$plcrspc)
ESS$plcfrdc <- ifelse(ESS$plcfrdc > 5, NA, ESS$plcfrdc)
ESS$plcexdc <- ifelse(ESS$plcexdc > 5, NA, ESS$plcexdc)
ESS$ProceduralFairness <- (ESS$plcrspc + ESS$plcfrdc + ESS$plcexdc)/3 



ESS$plcpvcr <- ifelse(ESS$plcpvcr > 10, NA, ESS$plcpvcr)
ESS$Effectiveness <- ESS$plcpvcr


ESS$plccbrb <- ifelse(ESS$plccbrb > 10, NA, ESS$plccbrb)

# recoding scale 0-10 
ESS$plccbrb <- (10 - ESS$plccbrb)

ESS$Lawfulness <- ESS$plccbrb



##  Reforming Sociodemographic Covariates

ESS$Age <- ESS$agea
ESS$Age <- ifelse(ESS$Age == 999, NA, ESS$Age)


ESS$Gender <- ESS$gndr
ESS$Gender <- ifelse(ESS$Gender == 9, NA, ESS$Gender)
ESS$Gender[ESS$Gender == 1] <- 0
ESS$Gender[ESS$Gender == 2] <- 1


ESS$Education <- ESS$eduyrs
ESS$Education <- ifelse(ESS$Education > 70, NA, ESS$Education)


ESS$EthnicMinority <- ESS$blgetmg
ESS$EthnicMinority[ESS$EthnicMinority == 1] <- 0
ESS$EthnicMinority[ESS$EthnicMinority == 2] <- 1
ESS$EthnicMinority <- ifelse(ESS$EthnicMinority > 6, NA, ESS$EthnicMinority)


ESS$HouseholdIncome <- ESS$hinctnta
ESS$HouseholdIncome<- ifelse(ESS$HouseholdIncome > 70, NA, ESS$HouseholdIncome)


### Sub-setting datasets with NA 

ESS_with_NA <- ESS
ESSSE_with_NA <- ESS |> filter(ESS$cntry == "SE")
ESSRU_with_NA <- ESS |> filter(ESS$cntry == "RU")

## keep only complete cases of dependent variables and covariates

ESS <- ESS[complete.cases(
  ESS$Trust,
  ESS$Obligation,
  ESS$MoralAlignment,
  ESS$ProceduralFairness,
  ESS$Effectiveness,
  ESS$Lawfulness,
  ESS$Age,
  ESS$Gender,
  ESS$Education,
  ESS$HouseholdIncome
),]

### Treatment Assignment to Pre or Post Attack Groups

## Sweden Case Study 

attack_date_ESSSE <- as.Date("2010-12-11")  

ESSSE <- ESS |>  filter(cntry == "SE")

ESSSE$interviewstart <- as.Date(paste(ESSSE$inwyys, ESSSE$inwmms, ESSSE$inwdds, sep ="-"))
ESSSE$interviewend   <- as.Date(paste(ESSSE$inwyye, ESSSE$inwmme, ESSSE$inwdde, sep ="-"))
ESSSE <- ESSSE |>   filter(interviewstart == interviewend)
ESSSE$Date <- ESSSE$interviewstart

ESSSE$Treatment <- NA

ESSSE$Treatment[ESSSE$Date > as.Date(attack_date_ESSSE)] <- 1
ESSSE$Treatment[ESSSE$Date <= as.Date(attack_date_ESSSE)] <- 0
ESSSE <- ESSSE |> filter(Date != attack_date_ESSSE)


ESSSE$Runner <- as.integer(ESSSE$Date - as.Date(attack_date_ESSSE))
ESSSE$Bandwith <- abs(ESSSE$Runner)


## Russia Case Study 

attack_date_ESSRU <- as.Date("2011-01-24")  

ESSRU <- ESS |>  filter(cntry == "RU")

ESSRU$interviewstart <- as.Date(paste(ESSRU$inwyys, ESSRU$inwmms, ESSRU$inwdds, sep ="-"))
ESSRU$interviewend <- as.Date(paste(ESSRU$inwyye, ESSRU$inwmme, ESSRU$inwdde, sep ="-"))
ESSRU <- ESSRU |>   filter(interviewstart == interviewend)
ESSRU$Date <- ESSRU$interviewstart


ESSRU$Treatment <- NA
ESSRU$Treatment[ESSRU$Date > as.Date(attack_date_ESSRU)] <- 1
ESSRU$Treatment[ESSRU$Date <= as.Date(attack_date_ESSRU)] <- 0
ESSRU <- ESSRU |> filter(Date != attack_date_ESSRU)

ESSRU$Runner <- as.integer(ESSRU$Date - as.Date(attack_date_ESSRU))
ESSRU$Bandwith <- abs(ESSRU$Runner)

