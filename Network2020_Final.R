# Network Analysis  Project 
# Well-Being, Gender, and Conspiracy Thinking

rm(list=ls())
setwd("~/Desktop/Network Analysis/Final")
NA_2020_data <- read.csv("~/Desktop/Network Analysis/Final/NA_2020_data.csv")

# Data Organization
Data <- as.data.frame(cbind(NA_2020_data$gender, NA_2020_data$Q18, NA_2020_data$Q19, 
              NA_2020_data$Q20, NA_2020_data$Q68, NA_2020_data$Q69, 
              NA_2020_data$Q70, NA_2020_data$Q71, NA_2020_data$Q72, 
              NA_2020_data$Q73))
names(Data) <- c("Gender", "CB1", "CB2", "CB3", "Physical Health", "Sleep", 
                 "Optimism", "Apperance", "Personal Life", "Relationships")
Data$Gender <- as.numeric(Data$Gender)

Female_Data <- subset(Data, Data$Gender == 1)
Male_Data <- subset(Data, Data$Gender == 2)
Other_Data <- subset(Data, Data$Gender == 3)

# Sample Sizes
nrow(Female_Data)
nrow(Male_Data)
nrow(Other_Data)
10+194+297

# Inspect for Normality 
# Female Data
fem_var <- as.data.frame(Female_Data[ , 2:10])
fem_Shapiro <- list()
for (name in names(fem_var)){
  subset <- fem_var[[name]]
  fem_Shapiro[[name]] <- shapiro.test(subset)
}
print(fem_Shapiro)

# Male Data
mal_var <- as.data.frame(Male_Data[ , 2:10])
mal_Shapiro <- list()
for (name in names(mal_var)){
  subset <- mal_var[[name]]
  mal_Shapiro[[name]] <- shapiro.test(subset)
}
print(mal_Shapiro)

# All Shaprio-Wilk normalities tests were significant, indicating 
# that all variables significantly differed from normality
# However, Spearman correlations can still be used if data violates
# normality (canvas announcement).

# EBICglasso allows accomodates for missing data, so it does not need to 
# be adjusted/removed before constructing the network

# Constructing Female Network
library(bootnet)
Female_Network <- estimateNetwork(fem_var, 
                               default = "EBICglasso", 
                               corMethod= "spearman")
Female_Plot <- plot(Female_Network, 
                    layout = "spring", 
                    posCol = "#6D6875",
                    theme = "colorblind")
Layout <- Female_Plot$layout

# Constructing Male Network
Male_Network <- estimateNetwork(mal_var, 
                               default = "EBICglasso", 
                               corMethod= "spearman")
Male_Plot <- plot(Male_Network, 
                  layout = Layout, 
                  posCol = "#6D6875",
                  theme = "colorblind")

# Network Comparison Test
NCT <- NetworkComparisonTest::NCT(Female_Network, Male_Network, 
                                  binary.data = FALSE, 
                                  test.edges = TRUE, 
                                  edges = "all", 
                                  paired = TRUE)

# Female Bootstrapping
# non-parametric bootstrap
boot_nonparametric <- bootnet(Female_Network, nBoots = 1000, nCores = 8)
plot(boot_nonparametric, order = "sample", labels = FALSE)
#EBICglasso is regularized so split0=FALSE
plot(boot_nonparametric, plot = "difference", onlyNonZero = TRUE, order = "sample")

# Male Bootstrapping
boot_nonparametric_male <- bootnet(Male_Network, nBoots = 1000, nCores = 8)
 plot(boot_nonparametric_male, order = "sample", labels = FALSE)
#EBICglasso is regularized so split0=FALSE
plot(boot_nonparametric_male, plot = "difference", onlyNonZero = TRUE, order = "sample")

#' Conclusion: Conspiracy beliefs were not related to well-being 
#' & gender identity did not predict differences in network structure.


