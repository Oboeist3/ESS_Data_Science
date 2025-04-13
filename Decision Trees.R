library(MASS)

## Loading datasets

central <- read.csv("central_europe_clean_csv.csv", header = TRUE)
eastern <- read.csv("eastern_europe_clean_csv.csv", header = TRUE)
nordics <- read.csv("nordics_clean_csv.csv", header = TRUE)
southern <- read.csv("southern_europe_clean_csv.csv", header = TRUE)
uk <- eastern <- read.csv("uk_clean_csv.csv", header = TRUE)
west_central <- read.csv("wce_clean_csv.csv", header = TRUE)

## Making Copies for Analysis

central_2 <- central
eastern_2 <- eastern
nordics_2 <- nordics
southern_2 <- southern
uk_2 <- uk
west_central_2 <- west_central

## Low, Medium, High Partition for implvdm

central_2$dem_imp <- ifelse(central_2$implvdm < 5, "Low", 
                            ifelse(central_2$implvdm >= 8, "High", "Medium"))
eastern_2$dem_imp <- ifelse(eastern_2$implvdm < 5, "Low", 
                            ifelse(eastern_2$implvdm >= 8, "High", "Medium"))
nordics_2$dem_imp <- ifelse(nordics_2$implvdm < 5, "Low", 
                            ifelse(nordics_2$implvdm >= 8, "High", "Medium"))
southern_2$dem_imp <- ifelse(southern_2$implvdm < 5, "Low", 
                             ifelse(southern_2$implvdm >= 8, "High", "Medium"))
uk_2$dem_imp <- ifelse(uk_2$implvdm < 5, "Low", 
                       ifelse(uk_2$implvdm >= 8, "High", "Medium"))
west_central_2$dem_imp <- ifelse(west_central_2$implvdm < 5, "Low", 
                                 ifelse(west_central_2$implvdm >= 8, "High", "Medium"))

## Remove implvdm as factor
central_2$implvdm <- NULL
eastern_2$implvdm <- NULL
nordics_2$implvdm <- NULL
southern_2$implvdm <- NULL
uk_2$implvdm <- NULL
west_central_2$implvdm <- NULL

## Adding Mean Trust and Partitions (Non-Parametric)

central_numeric_data <- central[, sapply(central, is.numeric)]
central_numeric_data$implvdm <- NULL
central$mean_trust <- rowMeans(central_numeric_data)
central$trust_lvl <- ifelse(central$mean_trust < 5, "Low", 
                            ifelse(central$mean_trust >= 8, "High", "Medium"))

eastern_numeric_data <- eastern[, sapply(eastern, is.numeric)]
eastern_numeric_data$implvdm <- NULL
eastern$mean_trust <- rowMeans(eastern_numeric_data)
eastern$trust_lvl <- ifelse(eastern$mean_trust < 5, "Low", 
                            ifelse(eastern$mean_trust >= 8, "High", "Medium"))

nordics_numeric_data <- nordics[, sapply(nordics, is.numeric)]
nordics_numeric_data$implvdm <- NULL
nordics$mean_trust <- rowMeans(nordics_numeric_data)
nordics$trust_lvl <- ifelse(nordics$mean_trust < 5, "Low", 
                            ifelse(nordics$mean_trust >= 8, "High", "Medium"))

southern_numeric_data <- southern[, sapply(southern, is.numeric)]
southern_numeric_data$implvdm <- NULL
southern$mean_trust <- rowMeans(southern_numeric_data)
southern$trust_lvl <- ifelse(southern$mean_trust < 5, "Low", 
                             ifelse(southern$mean_trust >= 8, "High", "Medium"))

uk_numeric_data <- uk[, sapply(uk, is.numeric)]
uk_numeric_data$implvdm <- NULL
uk$mean_trust <- rowMeans(uk_numeric_data)
uk$trust_lvl <- ifelse(uk$mean_trust < 5, "Low", 
                       ifelse(uk$mean_trust >= 8, "High", "Medium"))

west_central_numeric_data <- west_central[, sapply(west_central, is.numeric)]
west_central_numeric_data$implvdm <- NULL
west_central$mean_trust <- rowMeans(west_central_numeric_data)
west_central$trust_lvl <- ifelse(west_central$mean_trust < 5, "Low", 
                                 ifelse(west_central$mean_trust >= 8, "High", "Medium"))

## Adding those columns to Region 2 dataframes
central_2$mean_trust <- central$mean_trust
eastern_2$mean_trust <- eastern$mean_trust
nordics_2$mean_trust <- nordics$mean_trust
southern_2$mean_trust <- southern$mean_trust
uk_2$mean_trust <- uk$mean_trust
west_central_2$mean_trust <- west_central$mean_trust

central_2$trust_lvl <- central$trust_lvl
eastern_2$trust_lvl <- eastern$trust_lvl
nordics_2$trust_lvl <- nordics$trust_lvl
southern_2$trust_lvl <- southern$trust_lvl
uk_2$trust_lvl <- uk$trust_lvl
west_central_2$trust_lvl <- west_central$trust_lvl

## Making scalers into factors
central_2$pplfair <- as.factor(central_2$pplfair)
eastern_2$pplfair <- as.factor(eastern_2$pplfair)
nordics_2$pplfair <- as.factor(nordics_2$pplfair)
southern_2$pplfair <- as.factor(southern_2$pplfair)
uk_2$pplfair <- as.factor(uk_2$pplfair)
west_central_2$pplfair <- as.factor(west_central_2$pplfair)

central_2$pplhlp <- as.factor(central_2$pplhlp)
eastern_2$pplhlp <- as.factor(eastern_2$pplhlp)
nordics_2$pplhlp <- as.factor(nordics_2$pplhlp)
southern_2$pplhlp <- as.factor(southern_2$pplhlp)
uk_2$pplhlp <- as.factor(uk_2$pplhlp)
west_central_2$pplhlp <- as.factor(west_central_2$pplhlp)

central_2$ppltrst <- as.factor(central_2$ppltrst)
eastern_2$ppltrst <- as.factor(eastern_2$ppltrst)
nordics_2$ppltrst <- as.factor(nordics_2$ppltrst)
southern_2$ppltrst <- as.factor(southern_2$ppltrst)
uk_2$ppltrst <- as.factor(uk_2$ppltrst)
west_central_2$ppltrst <- as.factor(west_central_2$ppltrst)

central_2$trstlgl <- as.factor(central_2$trstlgl)
eastern_2$trstlgl <- as.factor(eastern_2$trstlgl)
nordics_2$trstlgl <- as.factor(nordics_2$trstlgl)
southern_2$trstlgl <- as.factor(southern_2$trstlgl)
uk_2$trstlgl <- as.factor(uk_2$trstlgl)
west_central_2$trstlgl <- as.factor(west_central_2$trstlgl)

central_2$trstplc <- as.factor(central_2$trstplc)
eastern_2$trstplc <- as.factor(eastern_2$trstplc)
nordics_2$trstplc <- as.factor(nordics_2$trstplc)
southern_2$trstplc <- as.factor(southern_2$trstplc)
uk_2$trstplc <- as.factor(uk_2$trstplc)
west_central_2$trstplc <- as.factor(west_central_2$trstplc)

central_2$trstplt <- as.factor(central_2$trstplt)
eastern_2$trstplt <- as.factor(eastern_2$trstplt)
nordics_2$trstplt <- as.factor(nordics_2$trstplt)
southern_2$trstplt <- as.factor(southern_2$trstplt)
uk_2$trstplt <- as.factor(uk_2$trstplt)
west_central_2$trstplt <- as.factor(west_central_2$trstplt)

central_2$trstprt <- as.factor(central_2$trstprt)
eastern_2$trstprt <- as.factor(eastern_2$trstprt)
nordics_2$trstprt <- as.factor(nordics_2$trstprt)
southern_2$trstprt <- as.factor(southern_2$trstprt)
uk_2$trstprt <- as.factor(uk_2$trstprt)
west_central_2$trstprt <- as.factor(west_central_2$trstprt)

central_2$trstsci <- as.factor(central_2$trstsci)
eastern_2$trstsci <- as.factor(eastern_2$trstsci)
nordics_2$trstsci <- as.factor(nordics_2$trstsci)
southern_2$trstsci <- as.factor(southern_2$trstsci)
uk_2$trstsci <- as.factor(uk_2$trstsci)
west_central_2$trstsci <- as.factor(west_central_2$trstsci)