#read in data

library(tidyverse)
sample_data <- read.csv("~/Desktop/ontario-report/data/sample_data.csv")
View(sample_data)

summarise(sample_data, average_cells = mean(cells_per_ml))

#can use command+shift+M to write a pipe operator %>% 
sample_data %>%  
  summarise(average_cells = mean(cells_per_ml)) #put a new line after the pipe

################### filter data function ##################
sample_data %>% 
  filter(env_group == "Deep") %>%   
  # == means 'is eual to', so justifying if each sample is true or false for the criteria
  # != means 'not equal to'
  # you can also use > or < for numerical variables filtering
  # %in% c("criteria A", "B") --> keep rows where column matches the criteria A or B
  #in tidyverse, we dont use quatation mark when referring to a column, but for a specific value, we need quatation mark
  summarise(average_cells = mean (cells_per_ml))

sample_data %>% 
  filter(str_detect(env_group, "Shallow*"))
#filter based on a open ending criteria in certain string

######challenge: 
# 1) calculate the mean of chlorophyll
# 2) calculate the mean of chlorophyll in shallow september
#1) 
sample_data %>% 
  summarise(average_chlorophyll <- mean(chlorophyll))
# in the pipe function, it automatically pipe the dataset from step before to the next step, so no need to secify datasets in the following steps
#2) 
sample_data %>% 
  filter(env_group == "Shallow_September") %>% 
  summarise(average_chlorophyll_Shallow_Sep <- mean(chlorophyll))

# group_by
sample_data %>% 
  group_by(env_group) %>% 
  summarise(average_cells = mean(cells_per_ml), 
            min_cells = min(cells_per_ml)) %>% 
  summarise(avg_temp = mean(temperature))

#calculate the average temp per env_group
sample_data %>% 
  group_by(env_group) %>% 
  summarise(avg_temp = mean(temperature)) 

################ make new column by mutate function ##################
sample_data %>% 
  mutate(tn_tp_ratio = total_nitrogen / total_phosphorus)
#this new column is not added to the sample_data yet
# now we want a ratio grouped by factors
sample_data %>% 
  mutate(temp_is_hot = temperature > 8) %>%  
  #creates a logical column, true or false
  group_by(env_group, temp_is_hot) %>% 
  summarise(avg_temp = mean (temperature),
            avg_cells = mean (cells_per_ml))

#selecting only wanted columns in the dataset with select function
sample_data %>% 
  select(sample_id, depth)

sample_data %>% 
  select(-env_group) #remove certain columns

sample_data %>% 
  select(sample_id:temperature) #select columns from sample id to temperature column

sample_data %>% 
  select(starts_with('total'))

#challenge: create a data frame with only sample_id, env_group, temp, and cells_per_ml
sample_data %>% 
  select(sample_id:temperature)
#OR
sample_data %>% 
  select(sample_id, env_group, depth, temperature, cells_per_ml)
#OR
sample_data %>% 
  select(1:5) #in select, you can use number to present the column
#OR
sample_data %>% 
  select(-(total_nitrogen:chlorophyll))

###my own question for FeM
sample_data %>% 
  summarise(across(temperature:chlorophyll, mean))
#to apply the function to all columns selected, here in the example of 'mean' function

##################### clean dataset for bacterial taxon#######################
taxon_clean <- read_csv('data/taxon_abundance.csv', skip = 2) %>% # means skipping 2 rows on top of the table
  select(-...10) %>% #remove the last empty column
  rename(sequencer = ...9) %>%
  select(-Lot_Number, -sequencer)
  
#removed the emty, lot number and sequencer columns
#assign this all to an object called 'taxon_clean"
taxon_long <- taxon_clean %>% 
  pivot_longer(cols = Proteobacteria:Cyanobacteria, 
               names_to = 'Phylum',
               values_to = 'Abundance')

taxon_long %>% 
  group_by(Phylum) %>% 
  summarise(avg_abund = mean(Abundance))

taxon_long %>% 
  ggplot()+
  aes(x=sample_id,
      y=Abundance,
      fill = Phylum)+
  geom_col()
  theme(axis.text = element_text(angle = 90))

#making long table
taxon_long %>% 
  pivot_wider(names_from = 'Phylum',
              values_from = 'Abundance')

#joining data frames (combining table for same samples)
head(sample_data)
head(taxon_clean)

#for joining, we have: 
#inner_join (join only smaples has value in both tables)
#full_join (join all samples from both sides together, if missing value, use NA)
#directional join (left_join 1,2 = right_join 2,1; left join:example bringing new column from table B to table A when keeping every column and rows in A)
#anti_join (find the unmatches, where tables differ)

inner_join(sample_data, taxon_clean, by = 'sample_id') #be explicit to tell R join tables based on which column that exist in both
anti_join(sample_data, taxon_clean, by = 'sample_id')

sample_data$sample_id
taxon_clean$sample_id

taxon_clean_goodSep <- taxon_clean %>% 
  mutate(sample_id = str_replace(sample_id, pattern = 'Sep', replacement = 'September'))

inner_join(sample_data, taxon_clean_goodSep, by = 'sample_id')

#save the cleaned and sorted data table
sample_and_taxon <- inner_join(sample_data, taxon_clean_goodSep, by = 'sample_id')
write_csv(sample_and_taxon, file = 'data/sample_and_taxon.csv')

#make a plot
#ask where does chlorofelxi like to live?

install.packages("ggpubr")
library("ggpubr")

sample_and_taxon %>% 
  ggplot()+
  aes(x=depth, 
      y=Chloroflexi)+
  geom_point()+
  labs(x= 'Depth (m)', 
       y= 'Chloroflexi Relative Abundance')+
  geom_smooth(method = 'lm') +
  #stat_regline_equation() +
  stat_cor()+
  annotate(geom = 'text',
           x=25, y=0.3, 
           label='This is a text label')
    
##what is the average abundance and standard deviation of chloroflexi in our three groups?
sample_and_taxon %>% 
  group_by(env_group) %>%   
  summarise(avg_abun_chloroflexi = mean (Chloroflexi),
            sd_abun_chloroflexi = sd(Chloroflexi))
