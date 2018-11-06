library(tidyverse)
library(reshape2)
```{r}
#data
#user should update directory path
BYAREA = read.csv('data/BYAREA.TXT',header=T,sep = "|")

#Collecting list of cancer 
my_list = c("Female Breast","Colon and Rectum","Kidney and Renal Pelvis","Leukemias","Liver and Intrahepatic Bile Duct","Lung and Bronchus","Melanomas of the Skin","Ovary","Prostate","Oral Cavity and Pharynx")

#selecting records with given list of cancer
cancer_data = BYAREA[which(with(BYAREA, EVENT_TYPE == "Incidence" & SITE %in% my_list
                                & RACE == "All Races" & AREA != "Atlanta" & AREA != "Detroit" & AREA != "District of Columbia" 
                                & AREA != "East North Central" & AREA != "East South Central" & AREA != "Los Angeles" 
                                & AREA != "Middle Atlantic" & AREA != "Midwest" & AREA != "Mountain" & AREA != "New England" 
                                & AREA != "Northeast" & AREA != "Pacific" & AREA != "San Francisco-Oakland" & AREA != "San Jose-Monterey" 
                                & AREA != "Seattle-Puget Sound" & AREA != "South" & AREA != "South Atlantic" & AREA != "West" 
                                & AREA != "West North Central" & AREA != "West South Central")),]

#Checking for missing records and removing missing values
cancer_data$AGE_ADJUSTED_RATE[cancer_data$AGE_ADJUSTED_RATE == "~"] = NA
cancer_data = na.omit(cancer_data)
```{r}
#collecting data area, cancer and  Age-Adjusted Rates means
decision_matrix = data.frame("state" = cancer_data$AREA,"site" = cancer_data$SITE,"AGE_ADJUSTED_RATE" = as.numeric(as.character(cancer_data$AGE_ADJUSTED_RATE)))
decision_matrix = acast(decision_matrix, decision_matrix$state ~ decision_matrix$site, mean)
```{r}
#(1) Calculate the normalized decision matrix.
# All criterion are equally weighted
weights = rep(1/10,10) 
sqrt_col_sums = sqrt(colSums(decision_matrix^2))

normalized_weighted_matrix = matrix(nrow=50, ncol=10)
colnames(normalized_weighted_matrix) = colnames(decision_matrix)
rownames(normalized_weighted_matrix)= rownames(decision_matrix)

normalized_weighted_matrix = matrix(nrow=50, ncol=10)
colnames(normalized_weighted_matrix) = colnames(decision_matrix)
rownames(normalized_weighted_matrix)= rownames(decision_matrix)

for (col in 1:ncol(decision_matrix)) {
    normalized_weighted_matrix[,col] <- (decision_matrix[, col]/sqrt_col_sums[col]) * weights[col]
}
```{r}
#(3) Determine ideal best and ideal worst
ideal_best = apply(normalized_weighted_matrix, 2, min)
ideal_worst = apply(normalized_weighted_matrix, 2, max)

#(4) Calculate the separation measures, using the n-dimensional Euclidean distance
dist_worst = function(x) {sqrt( sum( (x - ideal_worst)^2) )}
dist_best = function(x) {sqrt( sum( (x - ideal_best)^2) )}

best = apply(normalized_weighted_matrix, 1, dist_best)
worst = apply(normalized_weighted_matrix, 1, dist_worst)
```{r}
scores = worst/(best + worst)
state_rankings = data.frame(scores = scores, rank = rank(-scores))
state_rankings = state_rankings[order(state_rankings$rank),]
state_rankings

MORTALITY RATE CODE 
library(tidyverse)
library(reshape2)
```{r}
#data
#user should update directory path
BYAREA = read.csv('data/BYAREA.TXT',header=T,sep = "|")

#Collecting list of cancer 
my_list = c("Female Breast","Colon and Rectum","Kidney and Renal Pelvis","Leukemias","Liver and Intrahepatic Bile Duct","Lung and Bronchus","Melanomas of the Skin","Ovary","Prostate","Oral Cavity and Pharynx")

#selecting records with given list of cancer
cancer_data = BYAREA[which(with(BYAREA, EVENT_TYPE == "Mortality" & SITE %in% my_list
                                & RACE == "All Races" & AREA != "Atlanta" & AREA != "Detroit" & AREA != "District of Columbia" 
                                & AREA != "East North Central" & AREA != "East South Central" & AREA != "Los Angeles" 
                                & AREA != "Middle Atlantic" & AREA != "Midwest" & AREA != "Mountain" & AREA != "New England" 
                                & AREA != "Northeast" & AREA != "Pacific" & AREA != "San Francisco-Oakland" & AREA != "San Jose-Monterey" 
                                & AREA != "Seattle-Puget Sound" & AREA != "South" & AREA != "South Atlantic" & AREA != "West" 
                                & AREA != "West North Central" & AREA != "West South Central")),]

#Checking for missing records and removing missing values
cancer_data$AGE_ADJUSTED_RATE[cancer_data$AGE_ADJUSTED_RATE == "~"] = NA
cancer_data = na.omit(cancer_data)
```{r}
#collecting data area, cancer and  Age-Adjusted Rates means
decision_matrix = data.frame("state" = cancer_data$AREA,"site" = cancer_data$SITE,"AGE_ADJUSTED_RATE" = as.numeric(as.character(cancer_data$AGE_ADJUSTED_RATE)))
decision_matrix = acast(decision_matrix, decision_matrix$state ~ decision_matrix$site, mean)
```{r}
#(1) Calculate the normalized decision matrix.
# All criterion are equally weighted
weights = rep(1/10,10) 
sqrt_col_sums = sqrt(colSums(decision_matrix^2))

normalized_weighted_matrix = matrix(nrow=50, ncol=10)
colnames(normalized_weighted_matrix) = colnames(decision_matrix)
rownames(normalized_weighted_matrix)= rownames(decision_matrix)

normalized_weighted_matrix = matrix(nrow=50, ncol=10)
colnames(normalized_weighted_matrix) = colnames(decision_matrix)
rownames(normalized_weighted_matrix)= rownames(decision_matrix)

for (col in 1:ncol(decision_matrix)) {
    normalized_weighted_matrix[,col] <- (decision_matrix[, col]/sqrt_col_sums[col]) * weights[col]
}
```{r}
#(3) Determine ideal best and ideal worst
ideal_best = apply(normalized_weighted_matrix, 2, min)
ideal_worst = apply(normalized_weighted_matrix, 2, max)

#(4) Calculate the separation measures, using the n-dimensional Euclidean distance
dist_worst = function(x) {sqrt( sum( (x - ideal_worst)^2) )}
dist_best = function(x) {sqrt( sum( (x - ideal_best)^2) )}

best = apply(normalized_weighted_matrix, 1, dist_best)
worst = apply(normalized_weighted_matrix, 1, dist_worst)
```{r}
scores = worst/(best + worst)
state_rankings = data.frame(scores = scores, rank = rank(-scores))
state_rankings = state_rankings[order(state_rankings$rank),]
state_rankings
