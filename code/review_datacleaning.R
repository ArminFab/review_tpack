library(tidyverse)
library(dplyr)
library(psych)
library(rio)
library(gmodels)




# read data
data <- rio::import("data\\tpack_review_new_16092022.xlsx")


# new variable factor_sample consisting of each sample group type (pre-/in service & teacher educators)
data <- mutate(data, factor_sample = case_when(
                                  data$PST == 1 ~ "pre_service",
                                  data$'In-service' == 1 ~ "in_service",
                                  data$`teacher educators` == 1 ~ "educators"))

factor_sample <- as.factor(data$factor_sample)


# new variable "factor_countries" consisting of each country
data <- mutate(data, factor_country = case_when(data$Africa == 1 ~ "Africa",
                                                data$Australie == 1 ~ "Australia",
                                                data$`North-America` == 1 ~"North-America",
                                                data$`South-America`== 1 ~"South-America",
                                                data$Europe == 1 ~ "Europe",
                                                data$Asia == 1 ~ "Asia"))

data$factor_country <- as.factor(data$factor_country)

# new variable "factor_method" consisting of each methodological type (quant, qual, mixed)
data <- mutate(data, factor_method = case_when(data$mixed == 1 ~ "mixed",
                                                data$quantitative == 1 ~ "quantitative",
                                                data$qualitative == 1 ~"qualitative"))
data$factor_method <- as.factor(data$factor_method)

# new variable "factor_years" divided into the ranges 2005 - 2010, 2011 - 2016, 2017- 2020
data <- mutate(data, factor_years = case_when(data$Year <= 2010 ~ "early",
                                              data$Year >= 2016 ~ "recent",
                                              TRUE ~ "middle"))

sink("descriptive_results.txt")
# DESCRIPTIVE RESULTS...
#.... regarding countries
cat("Descriptiv results regarding \n")
cat("\n countries")
print(table(data$factor_country))
print(100*(prop.table(table(data$factor_country)))) # in percentage

#... regarding sample groups
cat("\n sample_groups")
print(table(data$factor_sample))
print(100*(prop.table(table(data$factor_sample))))

#... regarding method used
cat("\n methodological approach")
print(table(data$factor_method))
print(100*(prop.table(table(data$factor_method))))

#... regarding published years
cat("\n years of publication")
print(barplot(table(data$Year), xlab = "year", ylab="numbers of interventions"))

#... regarding subjects
cat("\n subjects")
factor_subjects <- select(data, math, social_studies, science_without_Math, languages, music, diverse, others)
lapply(factor_subjects, sum)

sink()

######### kontingenztafeln und chisq-tests to find dependencies among variables #####

sink("descriptive_results.txt", append = TRUE)
cat("\n correlations knowledge foci ~ subject-specific?")
#
for(j in c("TK", "PK", "CK", "TPK", "TCK", "PCK", "TPCK")) {
  print(j)
  print(table(data[,j], data$`subj_specific?`))
  print(chisq.test((table(data[,j], data$`subj_specific?`))))
}

cat("\n correlations knowledge foci ~ methodological approach")
for(j in c("TK", "PK", "CK", "TPK", "TCK", "PCK", "TPCK")) {
  print(j)
  print(table(data[,j], data$factor_method))
  print(chisq.test((table(data[,j], data$`factor_method?`))))
}

cat("\n correlations knowledge foci ~ sample group")
for(j in c("TK", "PK", "CK", "TPK", "TCK", "PCK", "TPCK")) {
  print(j)
  print(table(data[,j], data$factor_method))
  print(chisq.test((table(data[,j], data$factor_sample))))
}

cat("\n correlations knowledge foci ~ years")
for(j in c("TK", "PK", "CK", "TPK", "TCK", "PCK", "TPCK")) {
  print(j)
  print(table(data[,j], data$factor_years))
  print(chisq.test((table(data[,j], data$factor_years))))
}

for(j in c("TK", "PK", "CK", "TPK", "TCK", "PCK", "TPCK")) {
  print(j)
  print(table(data[,j], data$self_report))
  print(chisq.test((table(data[,j], data$self_report))))
}

sink()


                                                


##### descriptive 3-way contigency tables ######

# Analyse; Abhängigkeit TPCK, PCK und sample groups (3-way contingency table, https://www.r-bloggers.com/2020/12/contingency-tables-in-r/)
tab_1 <- xtabs(~data$PCK + data$TPCK + data$lvl_sample, data=data)

#  Analyse; Abhängigkeit TPCK, TPK und sample groups
tab_2 <- xtabs(~data$TPK + data$TPCK + data$lvl_sample, data=data)

#  Analyse; Abhängigkeit TPCK, PCK and self-reports
tab_3 <- xtabs(~data$PCK + data$TPCK + data$self_report, data=data)

# Abhängigkeit PCK, Self-Reports
tab_4 <- table(data$PCK, data$self_report)

# PCK und Performance measurements
tab_5 <- table(data$PCK, data$performance)

# PCK und sample group
tab_6 <- table(data$PCK, data$lvl_sample)





# PCK variable as factor



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
