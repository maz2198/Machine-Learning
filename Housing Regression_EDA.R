##### GROUP B ML MODEL
library(pacman)
p_load(janitor,naniar)
p_load(dplyr)
p_load(data.table)
#### LOADING DATA ####
dt_loc <- read.csv(file.choose())
glimpse(dt_loc)
house <- dt_loc
house <- setDT(house)
glimpse(house)
class(house)
### Eliminating all the commas changing price from factor to character -> numeric ###
house$Price <- as.character(house$Price)
house$Price <- as.numeric(gsub(",", "",house$Price))

house$m2 <- as.character(house$m2)
house$m2 <- as.numeric(gsub(",", "",house$m2))

house$Plot <- as.character(house$Plot)
house$Plot <- as.numeric(gsub(",", "",house$Plot))
###
glimpse(house)

###
colSums(is.na(house))
gg_miss_var(house)
gg_miss_upset(house)

#### naming conventions ####
clean_names(house);

#### simple ##### plots ####
p_load(ggplot2)
ggplot(data=house[!is.na(house$Price),], aes(x=Price)) +
  geom_histogram(fill="blue",bins = 175) + xlim(c(0,0.5*10^7))
  #scale_x_continuous(breaks= seq(0, 20000, by=100000))

#### outliers ######
p_load(grDevices)
house$Price[which(house$Price %in% boxplot.stats(house$Price)$out)]
boxplot(house$Price, main = "Boxplots for Station Data",ylab = "Solar Energy")
?grDevices
p_load(EnvStats)
rosnerTest(house$Price, k = 560, warn = F)

glimpse(house)
str(house)
