library(tidyverse)
library(dplyr)
library(psych)
library(rio)


# Daten einlesen
data <- rio::import("data\\tpack_review_new_16092022.xlsx")


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

# schlaufen basteln
for (var1i in names(data_loop)) {
  for (var2i in names(data_loop)) {
    kreuztabelle <- table(data[,var1i], data[,var2i])
    
    loop_results <- loop_results %>%
      add_row(var1 = var1i,
              var2 = var2i,
              phi = phi(kreuztabelle))
  }
}

print(loop_results)   #Korellationstabelle (Phi Korelation, siehe Eid et al., S. 555)

# Verteilung nach Jahren
barplot(table(data$Year), xlab = "year", ylab="numbers of interventions")