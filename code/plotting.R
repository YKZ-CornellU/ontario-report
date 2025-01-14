install.packages("tidyverse")
library(tidyverse)

sample_data <- read.csv("~/Downloads/ontario-report/sample_data.csv")
View(sample_data)

#when naming a new variable, never start the name with a number, see the example:
1example <- 8
#when working with variable names, remember it's case sensitive


# use the comment function:
Sys.Date() # output the current date
getwd() # outputs the current working directory
sum() # add numbers
read_csv(fie='sample_data.csv') #reads in csv file

#create first plot
ggplot(data = sample_data) + 
  aes(x=temperature) + 
  labs(x="Temperature (C)") +
  aes(y=cells_per_ml) +
  labs(y="Cells per mL") +
  geom_point() +
  labs(title = "Does tempurature affect microbial abundance?") +
  aes(color = env_group) +
  aes(size = chlorophyll) +
  aes(shape = env_group) +
  labs (size = 'Chlorophyll (ug/L)',
        color = 'Environment Group',
        shape = 'Environment Group')

#Combine neater code
ggplot(data = sample_data) + 
  aes(x=temperature,
      y=cells_per_ml/1000000,
      color=env_group,
      size=chlorophyll) +
geom_point() +
labs(x="Temperature (C)",
     y="Cells per mL",
     title= "Does tempurature affect microbial abundance?",
     size='Chlorophyll (ug/mL)',
     color= 'Environment Group')
# in ggplot, you can seperate data by using color, size, and shape
# use the 'theme' function to change every single element, font size on the figure

######importing datasets
buoy_data <- read_csv("buoy_data.csv")
View(buoy_data)
dim(buoy_data) #dimension of the table
head(buoy_data) #see begining of the datatable
tail(buoy_data) #see the last several entries of the dataset


#####plot more!
## to seperate buoy into different plots
#introduce facets: facet wrap
ggplot(data = buoy_data)+
  aes(x = day_of_year,
      y = temperature,
      group = sensor, 
      color = depth) +
geom_line() +
facet_wrap(~buoy, scales = 'free_y') #ever single graph has its own optimal y-axis limit


#introduce facets: facet grid
ggplot(data = buoy_data)+
  aes(x = day_of_year,
      y = temperature,
      group = sensor, 
      color = depth) +
  geom_line() +
  facet_grid(rows = vars(buoy)) 
    #tell R to seperate by buoy, either use 'vars()' or use quotation marks arund the variable for seperation

#structure of datasets
str(buoy_data)

##discrete plots
#box plot
ggplot(data = sample_data) +
  aes(x = env_group,
      y = cells_per_ml) +
  geom_boxplot() +
  geom_jitter(aes(size = chlorophyll)) #plot individual value as points, random position each dots so no overlap of dots, do after the boxplot so dots on top of the boxplot, otherwise the boxplot hide some plots

#box plot, color version
ggplot(data = sample_data) +
  aes(x = env_group,
      y = cells_per_ml) +
  geom_boxplot(aes(fill= env_group)) +
scale_fill_manual(values = c('pink', 'tomato', 'papayawhip')) #mannually select the color for groups

#scale fill brewer
ggplot(data = sample_data) +
  aes(x = env_group,
      y = cells_per_ml) +
  geom_boxplot(aes(fill= env_group)) +
  scale_fill_brewer(palette = 'Set3') #refer to the saved image for R color brewer list

##custom palette time
install.packages("wesanderson")
library('wesanderson')
ggplot(data = sample_data) +
  aes(x = env_group,
      y = cells_per_ml) +
  geom_boxplot(aes(fill= env_group)) +
  scale_fill_manual(values = wes_palette('Cavalcanti1'))

install.packages('MetBrewer')
library('MetBrewer')

ggplot(data = sample_data) +
  aes(x = env_group,
      y = cells_per_ml) +
  geom_boxplot(aes(fill= env_group)) +
  scale_fill_manual(values=met.brewer ('NewKingdom'))

##box plot
#change transparency
ggplot(data = sample_data) +
  aes(x = env_group,
      y = cells_per_ml) +
  geom_boxplot(fill='darkblue', alpha = 0.2) #alpha is the color intensity here

#univariate plots
ggplot(sample_data) +
  aes(x=cells_per_ml)+
  geom_density(aes(fill = env_group), alpha = 0.5)+
  theme_bw()

##box plot
#rotate x axis labels
box_plot <- 
  ggplot(data = sample_data) +
    aes(x = env_group,
        y = cells_per_ml) +
    geom_boxplot()+
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
box_plot

#saving plots
ggsave('awesome_plot.jpg', width = 6, height = 4, dpi = 600)
  
#add changes to the plot  
box_plot + theme_bw()
box_plot <- box_plot+ theme_bw()  
box_plot

ggsave('awesome_box_plot.jpg', plot = box_plot, width = 6, height = 4, dpi = 600)

