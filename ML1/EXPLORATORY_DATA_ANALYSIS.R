##### [1] Packages loading #####
library(pacman)
p_load(dplyr,data.table)
p_load(DAAG,e1071,forcats,leaps,lmtest,Rmisc,xgboost,caret,car)
p_load(naniar,Amelia,corrplot,corrgram,esquisse,FactoMineR)
p_load(ggplot2,GGally,ggvis,ggthemes,leaflet,leaflet.extras)
p_load(kableExtra,htmltools,DataExplorer)
p_load(rpart,randomForest,caTools,leaps,DataExplorer,glmnet)
p_load(gridExtra,rpart.plot,ipred,randomForest,lubridate)

# Note: ensure the factoextra is installed in order to run PCA
install.packages("rlang",type="win.binary")
install.packages("devtools")
install.packages("fs")
devtools::install_github("kassambara/ggpubr")
devtools::install_github("kassambara/factoextra")


#### [Load Data] ####
dt_sol <- readRDS(file.choose())
dt_sol <- setDT(dt_sol)
dt_loc <- fread(file.choose())
dt_loc <- setDT(dt_loc)
dt_add <- readRDS(file.choose())
dt_add <- setDT(dt_add)

#### [Variable checks] ####
class(dt_sol)
class(dt_loc)
class(dt_add)

head(dt_sol)
str(dt_sol)
summary(dt_sol)
class(dt_add)

head(dt_loc)
str(dt_loc)
summary(dt_loc)

head(dt_add,1)
#### Data type conversion ####
dt_sol$Date <- as.Date(dt_sol$Date,format = "%Y%m%d")# to reasonable Dateformat
dt_add$Date <- as.Date(dt_add$Date,format = "%Y%m%d")# to reasonable Dateformat
dt_sol_Year <- year(dt_sol$Date)# extract year using lubridate library
dt_sol_Month <- month(dt_sol$Date) # extract month using lubridate library
class(dt_sol_Year)

#### [Summary Statistics] ####
new_dt_sol <- dt_sol[1:5113,2:99] ## new dataset for summary stat
Sol_Mean <- new_dt_sol[,sapply(.SD,mean),,.SDcols = c(1:98)]
Sol_Min <- new_dt_sol[,sapply(.SD,min),,.SDcols = c(1:98)]
Sol_Max <- new_dt_sol[,sapply(.SD,max),,.SDcols = c(1:98)]
Sol_Median <- new_dt_sol[,sapply(.SD,median),,.SDcols = c(1:98)]
Sol_IQr <- new_dt_sol[,sapply(.SD,IQR),,.SDcols = c(1:98)]
Sol_StD <- new_dt_sol[,sapply(.SD,sd),,.SDcols = c(1:98)]

Main.Stats <- as.data.frame(Sol_Mean)
Main.Stats <- cbind(Main.Stats,Sol_Min);Main.Stats <- cbind(Main.Stats,Sol_Max);Main.Stats <- cbind(Main.Stats,Sol_Median);Main.Stats <- cbind(Main.Stats,Sol_IQr);Main.Stats <- cbind(Main.Stats,Sol_StD);
round(Main.Stats, digits = 2)
Main.Stats <- as.data.frame(Main.Stats);
head(Main.Stats,5);

#### [Outliers] ####
install.packages("grDevices")
library(grDevices)
boxplot(dt_sol[,2:99], main = "Boxplots for Station Data",ylab = "Solar Energy")
# There are no outliers found

#### [Check for Missing values] ####
gg_miss_var(dt_sol[,1:99],show_pct = TRUE)
gg_miss_upset(dt_sol[,1:99], nsets = 8, nintersects = NA) # missing values are as stated
#gg_miss_fct(dt_sol[,1:99],fct = as.factor(dt_sol$Date[1:511]))

# check for missing values in addtional variables
gg_miss_var(dt_add[,2:100],show_pct = TRUE)
gg_miss_upset(dt_add[,1:50])

# Webpage with crude summary of missing variables
create_report(dt_sol[,2:99])
create_report(dt_add[,2:99])


### [Principal Component Analysis] ####
# Produce PCA component analysis of the station solar energy

PCA_DATA <- prcomp(na.omit(dt_sol[,2:99]),scale = TRUE) # PCA of known data
PCA_DATA;
# % PC contribution according to station
PCA_cont_plot <- fviz_contrib(PCA_DATA,choice="var",axes = 1, top = 10) +
  labs(title = "Station PC contributions")
PCA_cont_plot; # Station PC contributions
str(PCA_DATA)
# Get the Eigenvalue and contribution percentage
eig.val_sol_stat <- get_eigenvalue(PCA_DATA)
sol.top_6_PCA <- head(eig.val,6);
sol.top_6_PCA;# Get Top 6 PCA components

# The proportion of variation explained by each eigenvalue is given in the second column. 
# For example, 84.407 divided by 100 equals 0.84407, or, about 84.41% of the variation 
# is explained by this first eigenvalue. 
# The cumulative percentage explained is obtained by adding the 
# successive proportions of variation explained to obtain the running total. 
# For instance, 84.407% plus 5.1401% equals 89.54795%, and so forth. 
# Therefore, about 93.735% of the variation is explained by the first 4 eigenvalues together.
PCAFactor.map <- fviz_pca_ind(PCA_DATA,col.ind = "cos2",geom = "point") +
  labs(title ="Individual Factor Map: PCA", x = "PC1", y = "PC2") +
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint=0.6)+ theme_minimal()
PCAFactor.map; # PCA Factor Map

# PCA by year
PCA_cont_by_year <- fviz_pca_ind(PCA_DATA, label="none", habillage=as.factor(year(dt_sol$Date[1:5113])))
PCA_cont_by_year;
PCA_cont_by_month <- fviz_pca_ind(PCA_DATA, label="none", habillage=as.factor(month(dt_sol$Date[1:5113])))
PCA_cont_by_month; # Monthly changes -> 1 - Jan; 2 - Feb, 3 - Mar etc...
PCA_cont_by_quarter <- fviz_pca_ind(PCA_DATA, label="none", habillage=as.factor(`quarter<-`(dt_sol$Date[1:5113],with_year = FALSE,fiscal_start = 3))); 
PCA_cont_by_quarter; # Seasonal changes # Group 1 - Spring; 2 - Summer; 3 - Autumn; 4 - Winter;

# Same analysis as before

# Analysis of the already calculated PC components for the variables

## Analyis of PC components
PC_calc <- prcomp(na.omit(dt_sol[,100:456]),scale = TRUE) # pcof
PC_cont_plot <- fviz_contrib(PC_calc,choice="var",axes = 1, top = 20) +
  labs(title = "Station PC contributions")
PC_cont_plot;
eig.val_PC_stat <- get_eigenvalue(PC_calc)
sol.top_6_PC_calc <- head(eig.val,15);
sol.top_6_PC_calc;

# Get Top 6 PCA components
PC_calc.map <- fviz_pca_ind(PC_calc,col.ind = "cos2",geom = "point") +
  labs(title ="Individual Factor Map: PCA", x = "PC1", y = "PC2") +
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint=0.6)+ theme_minimal()
PC_calc.map; # PCA Factor Map

# PCA by year
PC_calc_cont_by_year <- fviz_pca_ind(PC_calc, label="none", habillage=as.factor(year(dt_sol$Date)))
PC_calc_cont_by_year;
PC_calc_cont_by_month <- fviz_pca_ind(PC_calc, label="none", habillage=as.factor(month(dt_sol$Date)))
PC_calc_cont_by_month; # by month
PC_calc_cont_by_quarter <- fviz_pca_ind(PC_calc, label="none", habillage=as.factor(`quarter<-`(dt_sol$Date,with_year=FALSE,fiscal_start = 3))))                                                                                                                                                                     labels=c("Spring", "Summer", "Autumn","Winter"))
PC_calc_cont_by_quarter;# season 

head(dt_sol[,2:99],2)
cor.mtest(na.omit(dt_sol[,2:99]),na.rm = T)
# Solar_Data
sol_miss_fields.names <- miss_var_which(dt_sol) # Checking which fields have missing data
# We see that all data is missing
sol_miss_tabl <- miss_case_table(dt_sol) # 
sol_miss_summ <- miss_summary(dt_sol,order = TRUE)
sol_miss_pct <- miss_var_table(dt_sol)
sol_miss_case <- miss_case_summary(dt_sol)

## Add variables
add_miss_fields.names <- miss_var_which(dt_add)
add_miss_fields.names;# Checking which fields have missing data
# We see that all data is missing
add_miss_tabl <- miss_case_table(dt_add)
add_miss_tabl;# 
add_miss_summ <- miss_summary(dt_add,order = TRUE)
add_miss_summ
add_miss_pct <- miss_var_table(dt_add)
add_miss_pct;
add_miss_case <- miss_case_summary(dt_add)
add_miss_case;
heatmap(dt_loc)
mis_rows <- which(is.na(dt_sol[,99:ncol(dt_sol)]))           # count all the mrows where there are missing values
dt_sol1<- dt_sol[which(is.na(dt_sol[,99:ncol(dt_sol)])),1];
mis_final <- cbind(dt_sol1, dt_sol[5114:nrow(dt_sol), 2:98])
miss_plot <- mis_final[1:15,1:15];

miss_plot %>% 
  kable(escape = F) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F);

#### [Station Location Analysis] ####
#### 
pal <- colorNumeric(palette = "RdYlBu",domain = dt_loc$elev,reverse = TRUE)
leaflet(dt_loc) %>%
  addTiles() %>%
  addCircles(lng = ~elon, lat = ~nlat,radius = ~(elev*0.5),weight = 10,popup = ~as.character(elev),
             label = ~htmlEscape(stid),color = ~pal(elev),opacity = 0.9,labelOptions = labelOptions(noHide = F,direction = "bottom", textsize = "15px",style = list(
               "color" = "red",
               "font-family" = "serif",
               "font-style" = "italic"
             )
             )) %>% addLegend(
               position = "bottomleft",
               pal = pal,
               values = ~elev,
               group = "circles",
               title = "Elevation",
               opacity = 1,
               labFormat = labelFormat(suffix = " ft"),
             )

#### Variable Correlations ####
cor.mtest(dt_sol[,2:99],na.rm = T)
cor(dt_sol[1:5113,2:99],dt_sol[1:5113,100:456]) # correlation between stations and PCs
#### [PCA statistical analysis] ####

PC_Mean <-dt_sol[,sapply(.SD,mean),,.SDcols = -c(1:99)]
PC_Min <- dt_sol[,sapply(.SD,min),,.SDcols = -c(1:99)]
PC_Max <- dt_sol[,sapply(.SD,max),,.SDcols = -c(1:99)]
PC_Median <- dt_sol[,sapply(.SD,median),,.SDcols = -c(1:99)]
PC_IQr <- dt_sol[,sapply(.SD,IQR),,.SDcols = -c(1:99)]
PC_StD <- dt_sol[,sapply(.SD,sd),,.SDcols = -c(1:99)]


#### Variable Correlations ####
## Having seen the contributions from the GUTH station, Shaw, we'll plot some basic plots.

# plot for GUTH STATTION (PRIMARY), KENT (HIGHEST), IDAB (LOWEST)
solar_dat_old <- dt_sol %>% filter(Date <= '2007-12-31')
solar_dat_old <- solar_dat_old %>% select(1:99)
# Normal plots of solar energy over time
par(mfrow = c(3,1))
plot(solar_dat_old$Date, solar_dat_old$KENT,xlab="Date",ylab="Solar Energy",type = "l",main = "Kent - Highest Station",col = "red")
plot(solar_dat_old$Date, solar_dat_old$IDAB,xlab="Date",ylab="Solar Energy",type = "l",main = "IDAB - Lowest Station",col = "blue")
plot(solar_dat_old$Date, solar_dat_old$GUTH,xlab="Date",ylab="Solar Energy",type = "l",main = "GUTH Station",col = "green")

# Distribution plots
p1 <- ggplot(solar_dat_old,aes(x=solar_dat_old$KENT)) + geom_histogram(aes(binwidth = 100,color = "red")) + labs(title = "KENT SOLAR ENERGY",x = "Solar Energy") + theme_clean()
p2 <- ggplot(solar_dat_old,aes(x=solar_dat_old$IDAB)) + geom_histogram(aes(binwidth = 100,color ="green")) + labs(title = "IDAB SOLAR ENERGY",x = "Solar Energy") + theme_clean()
p3 <- ggplot(solar_dat_old,aes(x=solar_dat_old$GUTH)) + geom_histogram(aes(binwidth = 100,color = "purple")) + labs(title = "GUTH SOLAR ENERGY",x = "Solar Energy") + theme_clean()
p4 <- ggplot(solar_dat_old,aes(x=solar_dat_old$ACME)) + geom_histogram(aes(binwidth = 100,color ="blue")) + labs(title = "ACME SOLAR ENERGY",x = "Solar Energy") + theme_clean()

grid.arrange(p1,p2,p3,p4)

#
# We understand from the analysis that the GUTH station with regards to relative importance, has the highest explained 
# variance. Moreover as we increase the elevation, so we increase the solar energy data.

# We