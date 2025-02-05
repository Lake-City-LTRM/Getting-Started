
#### install packages ####
# install.packages("tidyverse") # run this line if you need to install tidyverse
install.packages("styler") # run this line if you need to install styler

#### Load Packages ####
library(tidyverse) # load tidyverse package
library(ggthemes) # contains colorblind friendly color palettes

#### Load Data ####
fishdata <- read_csv("ltrm_fish_data.csv") # Read In Fish Data
glimpse(fishdata) # take a peek at the data

#### Tidy Data ####

#### Visualize Data ####
ggplot(data = fishdata,
       mapping = aes(x=sdate,))