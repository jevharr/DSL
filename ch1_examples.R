library(Hmisc)
library(funModeling)
library(tidyverse)
library(wrapr)
data("heart_disease")

#1.1.1
myDataStatus <- df_status(heart_disease, print_results = F)

    ##the "%>% .$<column_name>" means pipe the results of this and return just the results of <column_name> in a vector
varsToRemove <- myDataStatus %>% 
  filter(p_zeros > 60) %>% 
  .$variable
heart_disease_2 <- heart_disease %>% 
  select(-one_of(varsToRemove))
arrange(myDataStatus, -p_zeros) %>% 
  select(variable, q_zeros, p_zeros)
nrow(heart_disease)
ncol(heart_disease)
colnames(heart_disease)

#1.1.2
freq(data = heart_disease, input = qc(thal, chest_pain))
freq(data=heart_disease$thal, plot = FALSE, na.rm = T)

heart_disease_3 <- select(heart_disease, thal, chest_pain)
describe(heart_disease_3)

#1.1.3
max_ix<-function(d) 
{
  ix=which(!is.na(d))
  res=ifelse(length(ix)==0, NA, d[max(ix)])
  return(res)
}

data_world <- read_csv(file = 'https://goo.gl/2TrDgN', na = '..') %>% 
  filter(`Series Code`!="")

data_world <- data_world %>% 
  select(contains('YR')) %>% 
  rowwise() %>% 
  is.na() %>% 
  which()
  mutate(newest_value = max_ix(.))
