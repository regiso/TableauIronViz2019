# April 2019
# This script is to prepare data for the May 5 Iron Viz with Tableau
# It has 6 parts
# 1. Read in the data from 6 separate csv files extracted from the original data set
# 2. Eliminate columns, repair typos in Louisiana & rename Alaska ag areas
# 3. Merge 4 data csv with county csv
# 4. Rename variables with human friendly words
# 5. Correlation analysis with gender - not very interesting - did not include in viz
# 6. Write final csv for Tableau


library(tidyverse)  #to reshape & read & write csv

# 1. Read in csv files

df1 <- read_csv("county.csv")     
df2 <- read_csv("Economics.csv")     
df3 <- read_csv("Farms.csv")     
df4 <- read_csv("LivestockAnimals.csv")     
df5 <- read_csv("Operators.csv")     
dfv <- read_csv("variables.csv")     

#2. Streamline
# 2.1 keep only variables that are valueNumeric in df2 to df5

df2.1 <-  select(df2, FIPSTEXT, contains("valueNumeric"))
df3.1 <-  select(df3, FIPSTEXT, contains("valueNumeric"))
df4.1 <-  select(df4, FIPSTEXT, contains("valueNumeric"))
df5.1 <-  select(df5, FIPSTEXT, contains("valueNumeric"))

# 2.2 Isolate the key interest variables in d2 to df5
df2.2 <-  df2.1 %>%
  select(-c("y12_M015_valueNumeric":"y12_M032_valueNumeric")) %>%
  select(-c("y12_M052_valueNumeric":"y12_M074_valueNumeric")) %>%
  select(-c("y12_M267_valueNumeric":"y12_M373_valueNumeric"))
df3.2 <- df3.1 %>%
  select(-c("y12_M003_valueNumeric")) %>%
  select(-c("y12_M082_valueNumeric":"y12_M103_valueNumeric")) %>%
  select(-c("y12_M247_valueNumeric")) %>%
  select(-c("y12_M251_valueNumeric":"y12_M257_valueNumeric")) %>%
  select(-c("y12_M262_valueNumeric":"y12_M275_valueNumeric"))
df4.2 <- df4.1 %>%
  select(FIPSTEXT, y12_M298_valueNumeric)
df5.2 <- df5.1 %>%
  select(FIPSTEXT, y12_M126_valueNumeric)

# 2.3 Correct spelling of Louisiana & AK county names in df1
df1$StateName <- sub("Louisana","Louisiana", df1$StateName)
df1$CountyName <- sub(" Area", "", df1$CountyName)


# 3 Merge the 4 dfs 
df6 <- merge(df1, df2.2, by="FIPSTEXT", all.y=TRUE)
df6 <- merge(df6, df3.2, by="FIPSTEXT", all.y=TRUE)
df6 <- merge(df6, df4.2, by="FIPSTEXT", all.y=TRUE)
df6 <- merge(df6, df5.2, by="FIPSTEXT", all.y=TRUE)
# Put the columns in alphabetic order so it is easy to merge new column names
df6 <- df6[, order(names(df6))]




# 4. Rename merged data with friendlier names
# 4.1 Create a map between the variable codes & names
df6.cols <- as.data.frame(colnames(df6))
df6.cols <- df6.cols %>% rename(variables = `colnames(df6)`)
df6.cols$map <- str_extract(df6.cols$variables, "M\\d{3}")
dfv$mapnum   <- str_extract(dfv$MapID, "M\\d{3}")

# 4.2 merge variable df with the short codes
df7 <- merge(dfv, df6.cols, by.x="mapnum", by.y="map")
df7names <- c(df7$MAPTITLE)

#4.3 Rename merged dataframe columns
colnames(df6)[5:31] <- df7names



#5. Correlation analysis - results are not meaningful
library(Hmisc)
y <- df6[26]
x <- df6 %>% 
  select(-`Percent of Farms with Female Principal Operator:  2012`)%>%
  select(-c("FIPSTEXT":"StateName"))

cor(x,y)

#Correlation Analysis is not informative. Will not include it in viz


#6. Write variable extract

dest <- "ironvizextract.csv"
write.csv(df6, dest)

