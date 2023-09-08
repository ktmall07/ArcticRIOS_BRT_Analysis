#Load libraries 
library(dismo)
library(ggplot2)
library(dplyr)

# Chemistry Data
df_chem <- read.csv('2016-2018_Abbott Synoptic.csv')
head(df_total)

# Watershed Characteristics
df_characteristics <- read.csv('Watershed_characteristics.csv')
head(df_characteristics)

# Change uniqueID to match Site.ID in Abbott Dataset
colnames(df_characteristics)[1] <- "Site.ID"

# Merge datasets by Chemistry and characteristics
df_total <- merge(df_chem, df_characteristics, by="Site.ID")

# Store Merged Dataset
write.csv(df_total, "Merged_Dataset.csv")

# Clean Data (column names + remove useless columns)
for ( col in 1:ncol(df_total)) {
  col
  colnames(df_total)[col] <- sub("\\.\\..*", "", colnames(df_total)[col])
}

# Select response variables of interest
df <- subset(df_total, select = c(
  "Acetate",
  "Br",
  "Cl",
  "Co",
  "F",
  "S",
  "DOC",
  "Al",
  "B",
  "Ba",
  "Ca",
  "Cd",
  "Cr",
  "Cu",
  "Fe",
  "K",
  "Li",
  "Mg",
  "Mn",
  "Mo",
  "Na",
  "Ni",
  "Pb",
  "Zn",
  "Ammonium",
  "Nitrite",
  "Sulfate",
  "Phosphate",
  "Formate",
  "TDN",
  "As",
  "Se",
  "Si",
  "Sr",
  "Ti",
  "V",
  "P",
  "PP",
  "SRP",
  "TDP",
  "Season",
  "area_km2", 
  "SL_60_MEAN", 
  "SP_1000_MEAN", 
  "NDVI2012_MEAN", 
  "Alpine_Non.Carbonate_Barrens", 
  "Alpine_Non.Carbonate_Dryas_Dwarf_Shrub", 
  "Lowland_Low_Birch.Willow_Shrub", 
  "Lowland_Water", 
  "Lowland_Wet_Sedge_Tundra", 
  "Upland_Birch.Aspen.Spruce_Forest", 
  "Upland_Dryas_Dwarf_Shrub_Tundra", 
  "Upland_Low_Birch.Willow_Shrub_Tundra",  
  "Upland_Moist_Sedge_Shrub", 
  "Upland_Shrubby_Tussock_Tundra", 
  "Riverine_Water_Shrub_Wet_Sedge_Barrens"
))

# Change season to be included in influence
df$Season[df$Season == 'Early'] <- 0
df$Season[df$Season == 'Late'] <- 1
df$Season <- as.numeric(df$Season) 

require(gbm)

# For Each Response variable
for ( col in 1:41) {
  # Select subset of data containing only samples above detection rate 
  df_temp <- subset(df, subset = colnames(df)[col] > 0)
  
  # Returns a GBM object
  results <- gbm.step(data=df_temp, gbm.x=41:56, gbm.y=col, family="gaussian", tree.complexity=5,
                      learning.rate=0.001, bag.fraction=0.5)
  
  summary(results)
  gbm.plot(results)
  
  # Write model results to CSV
  if (col == 1) {
    write.csv(colnames(df)[col], file='BRT_results.csv')
  } else {
    write.table(colnames(df)[col], file='BRT_results.csv', append=TRUE, sep=",")
  }
  write.table(results$contributions, file='BRT_results.csv', append=TRUE, sep=",")
}

