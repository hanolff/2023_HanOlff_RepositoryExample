##### study information and libraries #####
#' Fire experiment Loliondo 2023 F Ledidi & H Olff, University of Groningen \
#' contact: h.olff@rug.nl \
#' vegetation composition in 2x2 m plots \
#' developed in R version 4.3.1 \
#' Database used: 2023_FireExperiment_Ledidi \
#' You may want to inspect the database used (remove # in next line, will open in browser) \
# browseURL("https://docs.google.com/spreadsheets/d/1ie3Z00nFTmcYx_UTa0kaRmsZ8FUkSR4CMFdyJ15kijM/edit?usp=sharing") 

#' clear workspace
rm(list = ls()) 

#' restore the libraries to the version used to develop the script
renv::restore()

#' load required libraries
library(tidyverse)
library(emmeans) # for tukey test
library(multcomp) # for tukey test
library(multcompView) # for tukey test
library(performance) # For check_model to check assumptions
 
##### read and merge the data #####
#' Show the tables in  the database with explanation of contents
tables_link<-"https://docs.google.com/spreadsheets/d/e/2PACX-1vSZ62o9ummnSZOYOZ2nwYNT6-xk9fngVpFhdYAIB2DRXhaOh-iKblWPmUjkPMJXMvqAmXFvcWBPYLdA/pub?gid=138313207&single=true&output=csv"
tables<-read_csv(tables_link,show_col_types = F) 
tables

#' show variables in the dataset, their explanation
variables_link<-"https://docs.google.com/spreadsheets/d/e/2PACX-1vSZ62o9ummnSZOYOZ2nwYNT6-xk9fngVpFhdYAIB2DRXhaOh-iKblWPmUjkPMJXMvqAmXFvcWBPYLdA/pub?gid=115450686&single=true&output=csv"
variables<-read_csv(variables_link, show_col_types = F)
variables
#' show validated relations between tables in the database, that can be used in, e.g. left_join()
relations<-variables |>
  dplyr::filter(relational=="yes") |>
  dplyr::arrange(Variable_ID,Table_ID)
relations

#' Read the relevant data tables from the database
DimExpPlot_link<-as.character(tables[tables$Table_ID=="DimExpPlot","CSV_link"])
DimExpPlot<- read_csv(DimExpPlot_link,show_col_types = F) 
DimExpPlot

FactVegStruc_link<-as.character(tables[tables$Table_ID=="FactVegStruc","CSV_link"])
FactVegStruc<- read_csv(FactVegStruc_link, show_col_types = F) |>
  mutate(Date=lubridate::dmy(Date),
         FirstDate=lubridate::dmy(FirstDate))

#' show the unique dates in the dataset, and sort these by date
unique(sort(FactVegStruc$FirstDate))

# merge the data tables based on the database relations (common variables)
VegStruct <- left_join(FactVegStruc,DimExpPlot, by="ExpPlot_ID") |>
  mutate(Treatment=factor(Treatment,levels=c("G","SG","FB","SFB")),
         Treatment=dplyr::recode_factor(Treatment,"G"="grazed,shrubs remained","SG"="grazed,shrubs cleared",
                                        "FB"="ungrazed, shrubs remained","SFB"="ungrazed,shrubs cleared"),
         SoilCat=dplyr::recode_factor(factor(SoilCat),"black"="black soil","red"="red soil"))
names(VegStruct) # show all variable names in the combined table
str(VegStruct)

##### summarize and plot the data #####
# Summarize and plot the mean vegetation height per treatment over time
fig01<-VegStruct %>%  
  group_by(ExpPlot_ID, SoilCat,Treatment,FirstDate) |>
  # calculate first the mean height per plot per block
  summarize(VegHgt_cm=mean(VegHgt_cm,na.rm=T)) |>
  group_by(SoilCat,Treatment,FirstDate) |>
  # calcuate the mean height per treatment and soil category
  summarize(mean=mean(VegHgt_cm,na.rm=T),
            sd=sd(VegHgt_cm,na.rm=T),
            n=n(),
            se=sd/sqrt(n)) |>
  arrange(FirstDate,SoilCat,Treatment) |>
  ggplot(data=_, aes(x=FirstDate,y=mean,col=Treatment)) +
  #     scale_fill_manual(values=c("#757575","#ef756a")) +
  geom_line(linewidth=0.5)  +
  geom_point(size=1.5) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),
                                width=0.5,linewidth=0.5) + 
  xlab('Date') +
  ylab('vegetation height (cm)') +
  ylim(0,NA) + 
  ggtitle('Vegetation height development') +
  facet_wrap(~SoilCat) +
  theme(text = element_text(size=10)) 
fig01 # show resulting figure

#' save the figure to a png file at good resolution
ggsave(filename="./figures/fig01.png",plot=fig01,width=1920, height=1080, units='px')

# show dates
unique(VegStruct$FirstDate)

##### statistical tests ##### 
#' test the significance of differences between the treatments on the last date
#' using a three-way ANOVA 
model01<-VegStruct |>
  filter(FirstDate=="2023-07-25") |>
  lm(VegHgt_cm~SoilCat*Grazed*BushCleared,data=_)
anova(model01)

#### test the differences with a Tukey test
#' calculate a variable representing the groups to be compared
VegStruct <- VegStruct |>
  mutate(groups=interaction(SoilCat,Treatment))
#' compute one-way anova on this variable
model02<-VegStruct |>
  filter(FirstDate=="2023-07-25") |>
  lm(VegHgt_cm~groups,data=_)
anova(model02)

#' calculate tukey tests
tukey <- emmeans::emmeans(model02, "groups", type = "response")
multcomp::cld(tukey, Letter="abcdefg")
#' this means that four groups are different, while bush clearing within 
#' grazing and bush clearing within ungrazed is not significantly different 

# add the letters to the figure
# where means with the same letter are not significantly different 
# (to be completed)


# Are we allowed to install new packages?
