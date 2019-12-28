suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(vegan)))

#create empty dataframe that will eventually hold all values for table
df <- data.frame(Filename = character(), Clonotypes = character(), Sum.Count = character())
#adds all .tsv files in /data to a list
filenames <- list.files("data", pattern="*.tsv", full.names=TRUE)
#for each name in the list of filenames
for (item in filenames)
{
  #quick housekeeping print
  print(item)
  #any time there is an Na value in the read_tsv it spams output with warnings, these two suppress the warnings while reading tsv into x
  suppressMessages(suppressWarnings(x <- read_tsv(item)))
  #removes Out sequence statuses and blank amino acid inputs
  x <- x %>% filter(sequenceStatus == 'In') %>% filter(is.na(aminoAcid) == F)
  #Stores numver of 'In' rows
  num.rows <- x %>% summarize(Clonotypes = n())
  #Stoes sum of reads in all 'In' rows
  sum.count <- x %>% summarize(Sum.Count = sum(`count (reads)`))
  #shannon diversity index
  Shannon.Diversity <- diversity(x$`count (reads)`, "simpson")
  #Makes a temporary single row df for each file
  row <- data.frame('Filename' = item, num.rows, sum.count, simp)
  #Adds temporaryd df to new dataframe as new row
  df<-rbind(df,row)
}
# Housekeeping print
print(df)

