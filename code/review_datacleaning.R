library(tidyverse)
library(dplyr)
library(psych)
library(rio)


# Daten einlesen
data <- rio::import("data/tpack_review_new_csv.csv", 
                  fileEncoding="UTF-8-BOM")


# Kreuztabellen (2x2 Kontingenztabellen) für alle Kombinationen der tpACK subdomains

# kreuztabelle <- table(data$PCK, data$TPCK)
# summary(kreuztabelle)
# print(kreuztabelle)


# vektor mit vars anlegen
data_loop <- data[,c("TK",
                     "PK",
                     "CK",
                     "TPK",
                     "TCK",
                     "PCK",
                     "TPCK")]

# leeren data frame anlegen, der dann sukzessive gefüllt wird.
loop_results <- data.frame(var1 = as.character(),
                           var2 = as.character(),
                           phi = as.numeric())