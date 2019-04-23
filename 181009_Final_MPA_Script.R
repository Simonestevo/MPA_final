
# Using R version 3.3.2

#####################

# IMPORTANT - ALL INPUT FILES CAN BE ACCESSED FROM: https://figshare.com/account/home#/projects/35711

#####################

# STEP 1 - CALCULATE PRESSURE VALUES FOR DIFFERENT AREAS (MPAs, UNPROTECTED AND EEZs)

#Load libraries

library(plyr)
library(dplyr)
library(readr)
library(data.table)
library(raster)
library(rgdal)
library(ff)
library(ffbase)
library(magrittr)

#set working directory

basedir <- "#Your working directory filepath here"

setwd(basedir)

#create folder for current date to store output 

currentDate <- Sys.Date()

output_dir <- paste(currentDate,"_output_files",sep="")

dir.create(output_dir)

dirs <- list.dirs()

output_file_path <- dirs[ grepl(currentDate, dirs) ]

# Read all files from directory and place in a list 

directory_files<-function(directory){
  
  files<-list.files(directory,full.names = TRUE)
  tmp<-lapply(files,read_csv)
  
}

#load processed area and pressure files - save files in folders names (1_area_rasters/ & 2_pressure_rasters) as below in the working directory

area_files <- list.files(path='2018_MPA_Input_files/1_area_rasters/', 
                         pattern =".tif$", full.names=TRUE)

area_list<-lapply(area_files,raster)


pressure_files <- list.files(path='2018_MPA_Input_files/2_pressure_rasters/', 
                             pattern =".tif$", full.names=TRUE)

pressure_list<-lapply(pressure_files,raster)


# names for pressure dataframes

pressure_names<-c("acid", "art", "dd", "dndhbc", "dndlbc", "fert", 
                  "inorg", "invas", "night", "oil", "pest", "phbc",
                  "plbc", "poll", "pop", "ship", "slr", "sst", "uv")


#Extract_vals function calculates pressure statistics for specified area file (e.g. ia, ib etc)

extract_vals<-function(x) {
  
  eez_id <- ff(vmode='short',getValues(areas), length = ncell(areas))
  wh <- ffwhich(eez_id, !is.na(eez_id)) %>% as.ram
  completed <- data.table(indices = wh, id = eez_id[wh], active = FALSE)
 
  pressure <- pressure_list[[x]]  
  pressure_vals <- ff(vmode = "double", getValues(pressure), length =  ncell(pressure))
  pressure_area <- data.table(completed,pressure_vals=pressure_vals[wh])
  
  out<- ddply(pressure_area,'id',summarise,
              pressure_mean=mean(pressure_vals,na.rm=TRUE),
              pressure_max=max(pressure_vals,na.rm=TRUE),
              pressure_min=min(pressure_vals,na.rm=TRUE),
              pressure_sum=sum(pressure_vals,na.rm=TRUE),
              pressure_CV=sd(pressure_vals,na.rm=TRUE)/mean(pressure_vals,na.rm=TRUE))
  
  colnames(out) <- paste(pressure_names[[x]], colnames(out), sep = "_")
  
  out
  
}


### SLOW CODE ~ loop takes around 5 hours on high performance server ### 

#user   system  elapsed 
#11569.06  5173.22 17646.04 

#Loop to calculate pressure statistics for all 19 pressures in each of the 9 areas using extract_vals function.  
#Output should be parent list of 9 objects, where each object in the parent list is a child list of 19 dataframes

parent_area_list <- list()

area_pressure_list <- list()

for (j in 1:9) {
  
  areas <- area_list[[j]]
  
  for (i in 1:19) {
    
    area_pressure_list[[i]] <- extract_vals(i)
    
  }
  
  parent_area_list[[j]] <- area_pressure_list
  
}


objectname <- paste(currentDate,"_parent_area_list",".rda",sep="")
save(parent_area_list, file=paste(output_file_path,objectname, sep = "/" ))

#name elements of the area list according to their area 

area_names<-c("ia", "ib", "ii", "iii","iv", "v", "vi","unprotected","eez")

names(parent_area_list) <- area_names


## STEP 2 - ATTACH ATTRIBUTES, AGGREGATE, CLEAN AND SCALE DATA

##Prepare attribute tables for areas to merge with the value dataframes 
# - need to do EEZ, Unprotected and MPA attributes separately

#NOTE - if you don't have enough computing power to complete the above steps, can begin here by loading the parent area list .rda file

load('2018_MPA_Input_files/5_processed_data/2018-08-17_parent_area_list.rda')

#read in raw attribute table pulled from shapefile in ArcGIS (make sure you have saved files in 3_area_attribute_tables as downloaded)

eez_attributes<-read.csv('2018_MPA_Input_files/3_area_attribute_tables/9_2013_attributes_eez.csv')

#subset relevant columns and add some to make it match the columns in the mpa files

eez_attributes_clean <- eez_attributes %>%
  dplyr::select(FID, eez_ID, EEZ) %>%
  mutate(category = "EEZ", marine = 0) %>%
  cbind(eez_attributes$Area,eez_attributes$ISO_3digit,eez_attributes$parentiso3) 

#rename columns

setnames(eez_attributes_clean, old = colnames(eez_attributes_clean), 
         new = c("matchid", "uniqueID", "name", "category", "marine", "area", "iso3", "parentiso3"))

##Make unprotected attribute table, change category 

unprotected_attributes_clean <- eez_attributes_clean
  
unprotected_attributes_clean$category[unprotected_attributes_clean$category == "EEZ"] <- "unprotected"

##FOR MPA FILES (can disregard warnings about column classes)

mpa_attributes_list<- directory_files('2018_MPA_Input_files/4_mpa_attribute_tables')

# Subset the raw attribute tables and rename the columns, save as a new list

clean_attributes <- function(df) {
  
  attributes_subset<- df %>%
  dplyr::select(FID, WDPAID, NAME, IUCN_CAT, MARINE,GIS_M_AREA, ISO3, PARENT_ISO) 
  setnames(attributes_subset, old = colnames(attributes_subset), 
  new = c("matchid", "uniqueID", "name", "category", "marine", "area", "iso3", "parentiso3"))
  
}

clean_mpa_list<-lapply(mpa_attributes_list, clean_attributes)

#combine mpa, unprotected and eez attribute dataframes into one list

clean_list<- c(clean_mpa_list, list(unprotected_attributes_clean), list(eez_attributes_clean))

area_attr_names<-c("ia_att", "ib_att", "ii_att", "iii_att","iv_att", "v_att", "vi_att","unprotected_att","eez_att")

names(clean_list) <- area_attr_names

#Function to subset large attribute dataframes to match smaller number of observations in pressure dataframes

subset_by_row<- function (large_df, small_df) {
  
  selectedRows <- (large_df$matchid %in% small_df$acid_id)
  
  matching_df<-large_df[selectedRows,]
  
}

remove_junk_cols<-function(dataframe) {
  
    dataframe %>%
    dplyr::select(-ends_with("_id")) 

  }

#Aggregate_pressures function attaches the attributes of the areas to the first pressure dataframe (acid), 
#and then aggregates the attributes and list of individual pressure dataframes into a single dataframe 
#for each area category

aggregate_pressures <- function(attribute_df, area_list) {
  
  attributes_subset <- subset_by_row(attribute_df,area_list[[1]])
  
  unlisted_df <- do.call(cbind,area_list)
  
  colnames(unlisted_df)[colnames(unlisted_df) == "acid_id"] <- "matchid"
  
  area_all_pressures <- merge(attributes_subset, unlisted_df, by = "matchid")
  
  area_all_pressures <- remove_junk_cols(area_all_pressures)
  
}

#Loop to aggregate individual pressure dataframes for all categories at once, 
#should end up with 9 dataframes (one for each area, 7 MPA levels, unprotected areas and eezs)
#each df should have 103 columns (attributes plus pressures)

final_areas_list <- list()

for (i in 1:9) {
  
  final_areas_list[[i]] <- aggregate_pressures(clean_list[[i]], parent_area_list[[i]])

}


#separate eez data from the other areas so it can be used to scale mpa values later on

eez_pressures_df <- final_areas_list[[9]]

#combine MPA and unprotected pressure dataframes (still excluding eez) into one master dataframe

area_pressures_df <- ldply(final_areas_list[1:8],rbind)


# Only need the mean values so extract them from master dataframe.  
# Also remove oil rig pressure - b/c raw data doesn't look accurate

subset_means <- function(data) {
  
                dplyr::select(data, -ends_with("max"),
                -ends_with("min"), -ends_with("sum"), 
                -ends_with("CV"),-starts_with("oil"))
  
}

eez_means <- subset_means(eez_pressures_df)
area_means <- subset_means(area_pressures_df)

#add territory column to the EEZ means dataframe and remove eez rows that are territories (can disregard warnings re: colclasses) 

territories <- read_csv('2018_MPA_Input_files/3_area_attribute_tables/2013_territory_list.csv')

eez_territories_join <- left_join(eez_means, territories)

eez_parent <- eez_territories_join %>%
              filter(territory == '0'|territory == '2')


#function to remove all contested zones, incomplete rows etc.

complete_cases_only <- function(data) {
    
    data %>%
    filter(!parentiso3 =='conflict/contested zone') %>%
    filter(complete.cases(.)) %>%
    distinct()
    
}

#apply to areas and eez dataframes - should match now (i.e. shouldn't be any MPAs with EEZs that have no values)

eez_means_complete <- complete_cases_only(eez_parent)

area_means_complete <- complete_cases_only(area_means)

#check there are eez values for every mpa, otherwise subsequent loop will fail

area_parentiso3 <- area_means_complete$parentiso3
eez_parentiso3 <- eez_means_complete$parentiso3

new <- area_parentiso3[!area_parentiso3 %in% eez_parentiso3]

#remove MPAs without matching eez values

area_means_matched <- subset(area_means_complete, !parentiso3 %in% new)

#adds M column and assigns 1 to protected sites, 0 to unprotected sites

area_means_matched <-mutate(area_means_matched,M=ifelse(grepl("unprotected",category),0,1))

#create list of full pressure names

pressures <- c("Ocean acidification", "Artisanal fishing", "Destructive demersal fishing", 
               "High bycatch demersal fishing", "Low bycatch demersal fishing", "Fertiliser pollution",
               "Inorganic pollution", "Invasive species", "Night light pollution", "Pesticide pollution",           "High bycatch pelagic fishing", "Low bycatch pelagic fishing", 
               "Ocean pollution (ships and ports)", "Human impacts", "Commercial Shipping", "Sea level rise",      "Sea surface temperature anomalies", "Ultraviolet radiation anomalies")


# scale area values by their parent EEZ values - takes around 14 seconds


area_scaled <- data.frame()

for(i in 1 : nrow(area_means_matched)){
  
  for(j in 9:26){
  
        area_scaled[i, j-8] <- area_means_matched[i, j] / 
        eez_means_complete[grep(area_means_matched$parentiso3[i], eez_means_complete$parentiso3), j]
  
      }
}


#add attribute columns back to scaled dataframe

colnames(area_scaled)<- colnames(area_means_matched[9:26])
mpa_df<-cbind(area_means_matched[1:8],area_scaled, area_means_matched[27])

# Loop produces NA values when the EEZ and MPA pressure value is zero because dividing by zero,
# need to turn them back to zero

mpa_df[is.na(mpa_df)]<- 0

# Remove values that returned Inf (this is because of a parent/territory MPA mismatch - only a few so remove them)

no.inf <- apply(mpa_df[,9:26],1,function(row) "Inf" %in% row)

mpa_df <- mpa_df[!no.inf,]

# Save final scaled dataframe

objectname <- paste(currentDate,"_scaled_by_eez_mpa_df",".rda",sep="")
save(mpa_df, file=paste(output_file_path,objectname, sep = "/" ))

## STEP 3 - STATISTICS, TABLES AND FIGURES

##IF STARTING FROM HERE, LOAD MPA DF

load('2018_MPA_Input_files/5_processed_data/2019-04-23_scaled_by_eez_mpa_df.rda')

#libraries for stats, figures and tables

library(MASS)
library(ggplot2)
library(RColorBrewer)
library(readr)
library(viridis)
library(dplyr)
library(stringr)

### Create dataframe which identifies which pressures are manageable & updates parameter codes


manageable <- c("Not-manageable", "Manageable", "Manageable", 
                "Manageable", "Manageable","Not-manageable", 
                "Not-manageable", "Manageable", "Not-manageable", "Not-manageable", 
                "Manageable", "Manageable", 
                "Not-manageable", "Not-manageable", "Manageable", "Not-manageable",
                "Not-manageable", "Not-manageable")

parameters <- colnames(mpa_df[,9:26])

pressure_df <- cbind(pressures, manageable, parameters)

#Create boxplots comparing raw protected and unprotected data

## Create boxplot dataframe

raw_boxplot_df <- area_means_matched

id_variables <- colnames(raw_boxplot_df[c(1:8, 27)])

raw_melted_boxplot_df <- melt(raw_boxplot_df, id.vars = id_variables)

colnames(raw_melted_boxplot_df)[colnames(raw_melted_boxplot_df) == 'variable'] <- 'parameters'

raw_melted_boxplot_df <- merge(raw_melted_boxplot_df, pressure_df, by = 'parameters')

raw_melted_boxplot_df <- mutate(raw_melted_boxplot_df, status = ifelse(grepl("unprotected",category),"Unprotected", "Protected"))

#Set up tiff to save figure S1 boxplots

objectname <- paste(currentDate,"_figure_S1",".tiff",sep="")
tiff(file = (paste(output_file_path,objectname, sep = "/")), units="in", width=10, height=5, res=400)


figure_s1 <- ggplot(raw_melted_boxplot_df, aes (x = pressures, y = value),  ylim = c(-8, 8)) +
                   geom_boxplot(outlier.colour="black", outlier.shape = 16,
                   outlier.size=0.10, notch=FALSE) +
                   labs(x = "Human induced pressures", y = "Mean pressure value") +
                   theme (panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black"),
                   axis.text.x=element_text(angle=90,hjust=1))+
                   geom_hline(yintercept = 0, colour = "red") +
                   ylim(-20,20)


figure_s1 <- figure_s1 + facet_wrap(~status, nrow = 2) + 
                          aes(fill = status, alpha = 0.5) + 
                          scale_fill_manual(values=c("#014636", "#800026")) +
                          theme(legend.position="none") 



figure_s1

#turn off devices

dev.off()

#Pearson's correlation test for multicollinearity

table_s2 <- as.data.frame(cor(mpa_df[,9:26], method = "pearson"))

names(table_s2) <- parameters
rownames(table_s2) <- parameters

#Return combinations of variables that exceed the 
#correlation threshold of 0.7 (see Dormann etal. 2013)

threshold <- 0.6

over.threshold.df <- data.frame()

for (i in 1:nrow(table_s2)) {
  
  for (j in 1:ncol(table_s2)) {
    
    over.threshold.df[i,j] <- (table_s2[i,j] > threshold) & #Which pairs exceed 0.7
                  (colnames(table_s2[j]) != rownames(table_s2[i,])) #That aren't the same variable (i.e. fert * fert)
    
    }

}

names(over.threshold.df) <- parameters #Add column names
rownames(over.threshold.df) <- parameters #Add row names

correlated <- which(over.threshold.df == TRUE,arr.ind = T) #Return matrix of variables that exhibit multicollinearity
corr_vars <- correlated[,1]


#Correlated matrix shows following pairs exhibit collinearity:
#Inorganic & Pesticide
#Fertiliser & Pesticide
#Fertiliser & Inorganic
#Human impacts & Night light pollution

#Inorganic, fertiliser and pesticide pollution distribution were all modeled
#using the same methods and are highly correlated, therefore we can drop inorganic
#and pesticide, and leave fertiliser with the assumption its distribution is
#an adequate representation of all land-based runoff pollution.

#Likewise, night light pollution and human impacts are both directly related
#to population density.  Given night light pollution is a function of human
#settlements, we can remove it and include only human impacts as representative.

#Subset corr_vars to only those variables we want to remove 
#(inorganic, pesticide & night light pollution)

corr_remove <- corr_vars[c(1,3,5)]
table_s2_2 <- dplyr::select(table_s2, -corr_remove)
table_s3 <- table_s2_2[-c(6,7,9),]

#Rename columns and rows for table_s2 and s3

pressures_2 <- pressures[-c(6,7,9)]

names(table_s2) <- pressures_2
rownames(table_s2) <- pressures_2
names(table_s3) <- pressures_2
rownames(table_s3) <- pressures_2

# Save collinearity tables S2 & s3

objectname <- paste(currentDate,"_table_S2",".csv",sep="")
write.csv(table_s2, file=paste(output_file_path,objectname, sep = "/" ))

objectname <- paste(currentDate,"_table_S3",".csv",sep="")
write.csv(table_s3, file=paste(output_file_path,objectname, sep = "/" ))

##Transform covariates##

#log transformation

mpa_df[, 9:23] <- log(mpa_df[,9:23]+1)

#scale data

mpa_df[,c(9:26)]<-scale(mpa_df[,c(9:26)])

#multiply everything by ten (brings model estimates onto the same scale)

mpa_df[,c(9:26)] <- apply((mpa_df[,c(9:26)]), 2,"*", 10) 

#mpa_df$pest_pressure_mean <- mpa_df$pest_pressure_mean*10
#mpa_df$fert_pressure_mean <- mpa_df$fert_pressure_mean*10
#mpa_df$uv_pressure_mean <- mpa_df$uv_pressure_mean*10



objectname <- paste(currentDate,"_mpa_df_transformed",".rda",sep="")
write.csv(mpa_df, file=paste(output_file_path,objectname, sep = "/" ))


#Create new boxplots comparing transformed protected and unprotected data

## Create boxplot dataframe

transformed_boxplot_df<-mpa_df

id_variables <-colnames(transformed_boxplot_df[c(1:8, 27)])

transformed_melted_boxplot_df <- melt(transformed_boxplot_df, id.vars = id_variables)

colnames(transformed_melted_boxplot_df)[colnames(transformed_melted_boxplot_df) == 'variable'] <- 'parameters'

transformed_melted_boxplot_df <- merge(transformed_melted_boxplot_df, pressure_df, by = 'parameters')

transformed_melted_boxplot_df <- mutate(transformed_melted_boxplot_df, status = ifelse(grepl("unprotected",category),"Unprotected", "Protected"))

#Set up tiff to save figure s2 - transformed boxplots

objectname <- paste(currentDate,"_figure_S2",".tiff",sep="")
tiff(file = (paste(output_file_path,objectname, sep = "/")), units="in", width=10, height=5, res=400)


figure_s2 <- ggplot(transformed_melted_boxplot_df, aes (x = pressures, y = value),  ylim = c(-8, 8)) +
                   geom_boxplot(outlier.colour="black", outlier.shape = 16,
                   outlier.size=0.10, notch=FALSE) +
                   labs(x = "Human induced pressures", y = "Transformed pressure value") +
                   theme (panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(fill = "grey97"),
                   axis.line = element_line(colour = "black"),
                   axis.text.x=element_text(angle=90,hjust=1))+
                   geom_hline(yintercept = 0, colour = "red") +
                   ylim(-20,20)


figure_s2 <- figure_s2 + facet_wrap(~status, nrow = 2) + 
                  aes(fill = status, alpha = 0.5) + 
                  scale_fill_manual(values=c("#014636", "#800026")) +
                  theme(legend.position="none") 

figure_s2

#turn off device

dev.off()

### SLOW CODE ### may take a minute or so

#GLM for general model - all protected v unprotected only

#using step AIC for best model


mpa_glm_result<-glm(M~1,family=binomial,data=mpa_df)

mpa_step_glm <- stepAIC(mpa_glm_result,scope = list(upper= ~acid_pressure_mean+art_pressure_mean+dd_pressure_mean+
                                                      dndhbc_pressure_mean+dndlbc_pressure_mean+inorg_pressure_mean+
                                                      invas_pressure_mean+phbc_pressure_mean+plbc_pressure_mean+
                                                      pop_pressure_mean+ship_pressure_mean+ #poll_pressure_mean+
                                                      slr_pressure_mean+sst_pressure_mean+
                                                      uv_pressure_mean,lower=~1))


#######

### SLOW CODE ### may take a minute or so

# Separate GLMs for each IUCN category (7 in total)

temp_categories <-  unique(mpa_df$category)

mpa_outputs <- list()

cat_mods <-list()

for (i in 1:7){
 
  temp <- mpa_df[c(which(mpa_df$category==temp_categories[i]),which(mpa_df$category==temp_categories[8])),]
  
  mpa.cat <- glm(M~1,family=binomial,data=temp)
  
  cat_mods[[i]] <- stepAIC(mpa.cat,scope = list(upper = ~acid_pressure_mean+art_pressure_mean+dd_pressure_mean+
                                                  dndhbc_pressure_mean+dndlbc_pressure_mean+inorg_pressure_mean+
                                                  invas_pressure_mean+phbc_pressure_mean+plbc_pressure_mean+
                                                  pop_pressure_mean+ship_pressure_mean+ #poll_pressure_mean+
                                                  slr_pressure_mean+sst_pressure_mean+
                                                  uv_pressure_mean,lower = ~1))
  
  
  
  mpa_outputs[[i]] <- list(category=temp_categories[i], mods=cat_mods)
  
}


#add all outputs into one list

all_mods <- c(list(mpa_step_glm), cat_mods)

lapply(all_mods, vif)

#Save models as r data file if needed

objectname <- paste(currentDate,"_all_models",".rda",sep="")
save(all_mods, file=paste(output_file_path,objectname, sep = "/" ))

mod_names <- c("General protection model", "Level Ia model", "Level Ib model",
                     "Level II model", "Level III model", "Level IV model", "Level V model",
                     "Level VI model")

## To create Table 1

#extract parameters, coefficients and stats into a dataframe 

extract_coefficients <- function (model,name) {   
    
  model_summary <- summary(model)
  
  coefficient_df <- as.data.frame(model_summary$coefficients)
  
  parameters <- row.names(coefficient_df)
  
  confidence_intervals <- as.data.frame(confint(model))

  category <- rep(name, length(parameters))
  
  out <- cbind(category, parameters, coefficient_df, confidence_intervals)
  
  out <- setNames(out, c("category", "parameters", "Estimate", "Std_Error", "Z_value", "P_value", "Lower_CI", "Upper_CI"))
  
   }


coefficient_list <- list()

for (i in seq(along = all_mods)) {
  
  coefficient_list[[i]] <- extract_coefficients(all_mods[[i]],mod_names[[i]])

  }


# extract formulas etc. for each best model and store in table 1

  extract_table_1_vals <- function (model, coefficients) {
  
  out <- list()
    
  {
  
  category <- mod_names[[i]]
    
  f <- model$formula

  formula_name <- paste(f[2],f[3],sep='~')
  
  model_summary <- summary(model)
  
  intercept <- coefficients[1,5]
  
  intercept_p_value <- coefficients[1,6]
  
  AIC <- model$aic
  
  n <- length(model$data$matchid)
  
  
  R_sq <- 1 - (model_summary$deviance/model_summary$null.deviance)
  
  }
  
  numeric_cols <- as.numeric(c(intercept, intercept_p_value, AIC, R_sq, n), round = 2)
  
  out <- c(list(category,formula_name), numeric_cols)
  
  out
  
}

table_1_list <- list()


for (i in seq(along = all_mods)) {
  
    table_1_list[[i]] <- extract_table_1_vals(all_mods[[i]],coefficient_list[[i]])
    
}

table_1 <- as.data.frame(do.call(cbind,table_1_list))

table_1 <- as.data.frame(sapply(table_1, unlist))

rownames(table_1)<- c("Model","Formula", "Intercept", "Intercept_p_value", "AIC", "R_squared", "n")

table_1 <- as.data.frame(t(table_1))

rownames(table_1) <- c()

#Save Table 1

objectname <- paste(currentDate,"_table_1",".csv",sep="")
write.csv(table_1, file=paste(output_file_path,objectname, sep = "/" ))

##To create dataframe for Figure 1

#view all colours
display.brewer.all()

#view palette colours
display.brewer.pal(9,"PuBuGn")

#view palette names
brewer.pal(9,"PuBuGn")

figure_1_df <- as.data.frame(do.call(rbind,coefficient_list))

figure_1_df <- figure_1_df %>%
               filter(!parameters == "(Intercept)") %>%
               mutate(significant = ifelse((P_value > 0.05 | Lower_CI < 0 & Upper_CI >0), 
               "FALSE", "TRUE")) %>%
               mutate(direction = ifelse(Z_value < 0,"negative","positive")) 


figure_1_df <- merge(pressure_df, figure_1_df, by = 'parameters')

figure_1_df <- mutate(figure_1_df, manageable = ifelse (manageable == "Manageable", TRUE, FALSE))

figure_1_df <- arrange(figure_1_df, category, Z_value, manageable)

figure_1_df <- mutate(figure_1_df,shape_code=ifelse(manageable == TRUE,1,6))

figure_1_df$shape_code <- as.factor(figure_1_df$shape_code)

figure_1_df <- mutate(figure_1_df, 
               colour = ifelse(significant == TRUE,
                               ifelse(manageable == TRUE,"#253494","#41B6C4"),
                               "#737373"))

figure_1_df <- mutate(figure_1_df, 
                      legend = ifelse(significant == TRUE,
                                      ifelse(manageable == TRUE,"Manageable, 
                                             significant effect","Not-manageable,
                                             significant effect"),
                                      "Not significant"))

figure_1_df <- mutate(figure_1_df, y_axis_cols = ifelse(manageable == TRUE,
                                                        "#253494", "#41B6C4"))

#Save figure 1 dataframe if needed

objectname <- paste(currentDate,"_figure_1_df",".rda",sep="")
save(figure_1_df, file=objectname)

## Plot Figure 1

#set up tiff to save hi res plot (only need to do when figure is complete)

objectname <- paste(currentDate,"_figure_1",".tiff",sep="")
tiff(paste(output_file_path,objectname, sep = "/"), units="in", width=10, height=5, res=400)


#make caterpillar plot

figure_1 <- qplot(data = figure_1_df, x = reorder(pressures, Estimate), y = Estimate,
                    ymin = Lower_CI, ymax = Upper_CI,
                    ylab = "Estimate with 95% confidence intervals", 
                    xlab = "Human Induced Pressures", geom = "blank", group = manageable) +
                    geom_hline(yintercept = 0) +
                    geom_linerange(data = figure_1_df, colour = figure_1_df$colour) +
                    theme (axis.text=element_text(size= 9),
                    axis.title=element_text(size= 9),
                    axis.text.y = element_text(),
                    axis.text.x = element_text(),
                    legend.text = element_text(size = 9),
                    legend.title = element_text(size = 9),
                    legend.key.size = unit(0.4,"cm"),
                    legend.position = "bottom",
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_rect(fill = "#DEDEDE"),
                    axis.line = element_line(colour = "black")) 
                   
                    


figure_1 <- figure_1 + facet_wrap(~ category, nrow = 2, scales = "free_x") + 
            coord_flip() + geom_point(aes(x = pressures, y = Estimate,                                                                                           
            shape = manageable, colour = colour, fill = colour), size = 1) + 
            scale_shape_manual(values = c(16,0))+
            scale_colour_manual(values=c("#253494","#41B6C4", "#737373")) +
            scale_fill_manual(values=c("#41B6C4","#253494","#737373")) +
            theme(strip.text.x = element_text(size = 11), 
            strip.background = element_rect(fill ="#B3B3B3"),
            panel.spacing = unit(1, "lines")) +
            labs(x = "") 
            

figure_1 <- figure_1 + theme(legend.position ="none")

figure_1


#activate code below when making tiff when plot is finished

dev.off()  


### Create supplementary materials model selection tables S3 to S10

#Create vector of names for models for use in formulas

short_mod_names <- c("MPA_all", "MPA_ia", "MPA_ib", "MPA_ii", "MPA_iii", "MPA_iv", "MPA_v", "MPA_vi")

#Function creates a base table from model output

make_table <-function(model) {
  
  model <- model
  
  df <- model$anova
  
  length <- length(df$Step)
  
  k <- sort(seq(from = length, to = 1, by = - 1))
  
  df <- cbind(df,k)
  
  df <- mutate(df, Log_likelihood = k - (df$AIC/2))
  
}

#loop to make tables for each model - should have output list of 8 base tables

table_list <- list()

for (i in 1:length(all_mods)){
  
  table_list[[i]] <- make_table(all_mods[[i]])
  
}

#name the tables

names(table_list) <- short_mod_names

#function to create formulas for top ten candidate models for each model category - input data is tables from previous function.

drop_parameters <- function(data, mod_name, drop) {
  
  mod_pt1 <- paste(mod_name, "~" )
  
  final_mod <- paste(data$Step, collapse = " ")
  
  final_mod <- str_replace_all(final_mod, "_pressure_mean","")
  
  final_mod <- substring(final_mod, 4)
  
  number <- (str_count(final_mod, boundary("word")))
  
  num_sequence <- seq(from = 0, to = (number*2), by = 2)
  
  num_doubled <- (number * 2 )-1
  
  remove <- (num_doubled - num_sequence[[drop]])
  
  next_mod <- word(final_mod, 1:remove)
  
  mod_pt2 <- paste(next_mod, collapse = " ")
  
  Model <- paste(mod_pt1, mod_pt2, collapse = " ")
  
  Model
  
}

#Nested loop - creates list of top ten candidate model formulas for each of the 8 models 
#(output is a list of 8 character vectors, each containing 10 model formulas)

#Create index for loop

dropindex <- c(1:10)


category_formulas <- list()
all_formulas<- list()


for (j in 1:8) {
  
  data <- table_list[[j]]
  
  for (i in 1:10) {
    
    category_formulas[[i]] <- unlist(drop_parameters(data, short_mod_names[[j]], dropindex[[i]]), use.names = FALSE)
    
  }
  
  all_formulas[[j]] <- unlist(category_formulas, use.names = FALSE)
  
}

#Subset the original base tables created so they only contain top ten models and relevant variables.  Output should be a list of 8 smaller tables.

subset_table_list <-list()


for (i in 1:8) {
  
    subset_table_list[[i]] <-   table_list[[i]] %>%
    dplyr::select(k, AIC, Log_likelihood) %>%
    arrange(AIC) %>%
    slice(1:10)
  
}


#function to calculate deltaAIC column for one model table (using subset tables as data input)

calculate_deltaAIC <- function(data, row) {
  
  best_AIC <- data$AIC[1]
  
  delta <- data$AIC[row] - best_AIC
  
  delta
  
}

#Loop to calculate deltaAIC for each of the 8 subset tables


delta.list<- list()
all.deltas.list<- list()

for (j in 1:8) {
  
  data <- subset_table_list[[j]]
  
  for (i in 1:10) {
    
    delta.list[[i]] <- calculate_deltaAIC(data, dropindex[[i]])
    
  }
  
  all.deltas.list[[j]] <- unlist(delta.list, use.names = FALSE)
  
}


#Create model ID object

Model_id <- c(1:10)

#Loop to bind formulas, ids, other columns and delta AIC values for 
#all 8 models (output should be list of 8 dataframes)

all.tables.list <- list()

for (i in 1:8){
  
  all.tables.list[[i]] <- cbind(all_formulas[[i]],Model_id,subset_table_list[[i]],all.deltas.list[[i]])
  
  names(all.tables.list[[i]]) <- c("Candidate model", "Model id","No. of parameters", "AIC", "Log likelihood", "Delta AIC")
  
}

#Name tables

table.names <- c("Table S3","Table S4","Table S5","Table S6","Table S7","Table S8","Table S9","Table S10")

names(all.tables.list) <- table.names

#Save tables as csv files 

for (i in seq_along(all.tables.list)) {
  
  filename = paste(currentDate,names(all.tables.list)[i], ".csv")
  write.csv(all.tables.list[[i]], file=paste(output_file_path,filename, sep = "/" ))
  
}



