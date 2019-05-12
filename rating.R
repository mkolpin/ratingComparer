###########################################
### Define variables, adapt accordingly ###
###########################################
inputFile <- "exampleFiles/CIB_IRR.xlsx"
outputFile <- "exampleFiles/CIB_IRR_validation.xlsx"
goldStandard <- "Anna"
raters <- c("Leonie", "Tabea", "Katharina", "Tina")

#######################
### Function import ###
#######################
source("rating_fctn.R")

#########################################
### Actual execution, minimal example ###
#########################################
library(openxlsx)
data <- read.xlsx(xlsxFile = inputFile)

#arguments are: dataframe, name of gold standard rater, list of other raters, 
# list of variables by which to group (default: "id", "condition")
validation <- get_rating_valid(data, goldStandard, raters)

write.xlsx(validation, outputFile, asTable = TRUE )
