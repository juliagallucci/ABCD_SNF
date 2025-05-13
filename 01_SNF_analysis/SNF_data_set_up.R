### Setting up data for SNF Run

library(ggplot2)
library(knitr)
library(ggcorrplot)
library(tidyverse)
library(devtools)
library(reshape2)
library(mice)
#install.packages("impute")
library(impute)
library(remotes)
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS=TRUE)
library(devtools)
install_github("jfortin1/neuroCombat_Rpackage")
library(neuroCombat)
library(impute)

## SET THE OUTPUT DIRECTORY
Output_directory="code/snf/input/"

#
#
#
#
#
#
#

setwd("/mnt/tigrlab/projects/tsecara/ABCD_julia/data/5.1/abcd-general/")

## site data for combat
site <- read.csv("abcd_y_lt.csv", header=TRUE)
site <- site[which(site$eventname == "baseline_year_1_arm_1"), ]

###### scanner - don't actually need this
setwd("/mnt/tigrlab/projects/tsecara/ABCD_julia/data/5.1/imaging/")
scanner <- read.csv("mri_y_adm_info.csv",  header=TRUE) # 22939
scanner <- scanner[which(scanner$eventname == "baseline_year_1_arm_1"), ] #11771
scanner <- scanner[ , c("src_subject_id", "mri_info_manufacturer")]

# imaging data
setwd("/mnt/tigrlab/projects/jgallucci/abcd/")
source("code/setting_up_data/imaging_data.R") 
resting_state[, 2:339] <- apply(resting_state[, 2:339], 2, function(x) as.numeric(x))
resting_state <- na.omit(resting_state) #9363

participants <- as.data.frame(resting_state[ , c("src_subject_id")])
colnames(participants) <- "src_subject_id"
vol_dk <- merge(vol_dk, participants, by="src_subject_id") #9363
ct_dk <- merge(ct_dk, participants, by="src_subject_id")


# cognitive data
setwd("/mnt/tigrlab/projects/jgallucci/abcd/")
source("code/setting_up_data/Cognitive_scores.R") #11865
missing <- as.data.frame(sapply(cognition, function(x) sum(is.na(x))))
cognition <- na.omit(cognition) #10936
complete <- merge(resting_state, cognition, by="src_subject_id") #8678

participants <- complete[ , 1:2]
participants[ ,2] <- NULL

# environmental data - should exclude measures that have more than 10% missing after excluding for the above criteria
setwd("/mnt/tigrlab/projects/jgallucci/abcd/")
source("code/setting_up_data/environmental_factors.R")
environment <- merge(environment, participants, by="src_subject_id")

# calculating percentage of missing points for participants that have complete cognitive & environmental data
missing <- as.data.frame(sapply(environment, function(x) sum(is.na(x))))
missing$percent <- missing[ ,1]/8678*100

to_be_removed <- missing[which(missing$percent > 10), ] # 5
to_be_removed <- row.names(to_be_removed) # 5 variables removed
environment_removed <- environment[,!(names(environment) %in% to_be_removed)]
environment_removed$na_count <- apply(environment_removed, 1, function(x) sum(is.na(x)))
environment_removed <- environment_removed[which(environment_removed$na_count <5), ] # max 10% missing data
environment_removed$na_count <- NULL
#42 variables left


# imputing the missing values with the median
environment_removed[, 2:42] <- apply(environment_removed[, 2:42], 2, function(x) as.numeric(x))
env_imputed <- environment_removed
env_imputed$src_subject_id <- NULL
env_imputed <- impute::impute.knn(as.matrix(env_imputed), k = 15, rowmax = 0.5, colmax = 0.8, maxp = 3474, rng.seed=362436069)
env_imputed <- as.data.frame(env_imputed[1])

env_imputed$src_subject_id <- environment_removed$src_subject_id
complete <- merge(env_imputed, complete, by="src_subject_id") #7965


## Family and ethnicity data to remove siblings
# participants belonging to the same family share a family ID
setwd("/mnt/tigrlab/projects/tsecara/ABCD_julia/data/5.1/abcd-general/")
relationship_1 <- read.csv("abcd_y_lt.csv", header=TRUE)
relationship_2 <- read.csv("abcd_p_demo.csv", header=TRUE)
relationship_1 <- relationship_1[which(relationship_1$eventname == "baseline_year_1_arm_1"), ] #11868
relationship_2 <- relationship_2[which(relationship_2$eventname == "baseline_year_1_arm_1"), ] #11868
relationship <- merge(relationship_1, relationship_2, by = "src_subject_id")
relationship <- relationship[ ,c("src_subject_id", "rel_family_id", "interview_age",  "demo_sex_v2")]
relationship$interview_age <- as.numeric(relationship$interview_age)
relationship$demo_sex_v2 <- as.factor(relationship$demo_sex_v2)


#########################################################################
# Prodromal Questionnaire Brief, child report - 21 questions
# 6 point scale if weighted by distress
setwd("/mnt/tigrlab/projects/tsecara/ABCD_julia/data/5.1/mental-health//")
psychosis <- read.csv("mh_y_pps.csv", header=TRUE)
psychosis <- psychosis[which(psychosis$eventname == "baseline_year_1_arm_1"), ]
psychosis[, 3:78] <- apply(psychosis[, 3:78], 2, function(x) as.numeric(x))

# If there is no distress score, value is made 0 or 1 and if there is a distress score that score is added to 1 indicating that that score is experienced
for (i in 1:21){
  psychosis[[paste0("prodromal_",i, "_y")]] <- ifelse(is.na(psychosis[[paste0("prodromal_",i, "b_y")]]), psychosis[[paste0("prodromal_",i, "_y")]], psychosis[[paste0("prodromal_",i, "b_y")]]+psychosis[[paste0("prodromal_",i, "_y")]])
}

psychosis <- psychosis %>% select(src_subject_id, starts_with("prodromal_") & !ends_with("b_y"))

# distress score of 1-5,~20%, which would really be 2-6
psychosis$distressed <- 0
for (i in 1:21){
  psychosis$distressed <- ifelse(psychosis[[paste0("prodromal_",i, "_y")]] > 3, 1, psychosis$distressed)
}

psychosis <- psychosis %>% select(src_subject_id, distressed)

#### Merging Everything

complete_sample <- merge(resting_state, cognition, by="src_subject_id") #8678
complete_sample <- merge(env_imputed, complete_sample, by="src_subject_id") #7965
complete_sample <- merge(vol_dk, complete_sample, by="src_subject_id") 
complete_sample <- merge(surface_area, complete_sample, by = "src_subject_id")
complete_sample <- merge(ct_dk, complete_sample, by="src_subject_id") 
complete_sample <- merge(relationship, complete_sample, by="src_subject_id") 
complete_sample$has_a_sibling <- duplicated(complete_sample$rel_family_id)
# creating a list of family ids with multiple siblings (all are twins or triplets)
sibling_list <- complete_sample[complete_sample$has_a_sibling == "TRUE", ]
sibling_list <- sibling_list %>% distinct(sibling_list$rel_family_id) #995
# creating a column that designates where someone has a sibling or not
complete_sample$has_a_sibling <- ifelse(complete_sample$rel_family_id %in% sibling_list[,1], "yes", "no")
# removing participants so that there is only one sibling
complete_sample <- complete_sample[!duplicated(complete_sample$rel_family_id), ] #6941
complete_sample <- complete_sample[which(complete_sample$Sample == "Main"), ]
complete_sample <- merge(psychosis, complete_sample, by="src_subject_id") 
table(complete_sample$New_Sample, complete_sample$Sample)

complete_sample <- complete_sample[which(complete_sample$distressed == 1), ]


### Combat harmonization
#https://github.com/Jfortin1/ComBatHarmonization/tree/master/R

imaging <- complete_sample %>% select( src_subject_id, starts_with("rsfmri"), interview_age, demo_sex_v2)

# Removing duplicated cortical-cortical connectivity measures based on duplicated measures
dups <- as.data.frame(t(imaging[1, ]))
dups$duplicated <- duplicated(dups[, 1])
dups <- dups[which(dups$duplicated == "FALSE"), ]
dups <- row.names(dups)

imaging <- imaging[which(colnames(imaging) %in% dups)]

# number of cort-cort 341

imaging$interview_age <- as.numeric(imaging$interview_age)
imaging$demo_sex_v2 <- as.factor(imaging$demo_sex_v2)

site <- site[ , c("src_subject_id", "site_id_l")]
imaging <- merge(site, imaging, by="src_subject_id")

combat_input <- imaging %>% select(starts_with("rsfmri"))
names <- colnames(combat_input)
combat_input[, 1:length(combat_input)] <- apply(combat_input[, 1:length(combat_input)], 2, function(x) as.numeric(x))
#missing_env <- as.data.frame(sapply(combat_input, function(x) sum(is.na(x))))

combat_input <- t(combat_input)
combat_input <- apply(combat_input, 2 ,as.numeric)


batch <- imaging$site_id_l # can be numeric or character
sex <- imaging$demo_sex_v2
age <- imaging$interview_age

mod <- model.matrix(~age+sex)

# not using empirical Bayes because it's a large sample size and the number of features < participants
# To run the ComBat model without empirical Bayes, which boils down to fitting a location/shift (L/S) model for each feature separately
imaging.harmonized <- neuroCombat(dat=combat_input, batch=batch, parametric=FALSE, eb=FALSE, mod=mod)

# harmonized data
imaging.harmonized <- as.data.frame(t(imaging.harmonized$dat.combat))
colnames(imaging.harmonized) <- names


#### cortical thickness

cortthick <- complete_sample %>% select(starts_with("smri_thick_cdk"), src_subject_id, interview_age, demo_sex_v2)
cortthick$interview_age <- as.numeric(cortthick$interview_age)
cortthick$demo_sex_v2 <- as.factor(cortthick$demo_sex_v2)

site <- site[ , c("src_subject_id", "site_id_l")]
cortthick <- merge(site, cortthick, by="src_subject_id")

combat_input <- cortthick %>% select(starts_with("smri_thick_cdk"))
names <- colnames(combat_input)
combat_input[, 1:68] <- apply(combat_input[, 1:68], 2, function(x) as.numeric(x))
#missing_env <- as.data.frame(sapply(combat_input, function(x) sum(is.na(x))))

combat_input <- t(combat_input)
combat_input <- apply(combat_input, 2 ,as.numeric)

batch <- cortthick$site_id_l # can be numeric or character
sex <- cortthick$demo_sex_v2 
age <- cortthick$interview_age
mod <- model.matrix(~age+sex)

# not using empirical Bayes because it's a large sample size and the number of features < participants
# To run the ComBat model without empirical Bayes, which boils down to fitting a location/shift (L/S) model for each feature separately
cortthick.harmonized <- neuroCombat(dat=combat_input, batch=batch, eb=FALSE, mod=mod)

# harmonized data
cortthick.harmonized <- as.data.frame(t(cortthick.harmonized$dat.combat))
colnames(cortthick.harmonized) <- names


#### surface areea
# keep total SA because going to divide each value by it
surfarea <- complete_sample %>% select(src_subject_id, interview_age, demo_sex_v2, starts_with("smri_area_cdk"))
surfarea$interview_age <- as.numeric(surfarea$interview_age)
surfarea$demo_sex_v2 <- as.factor(surfarea$demo_sex_v2)

# dividing by total SA
surfarea[, 4:72] <- apply(surfarea[, 4:72], 2, function(x) as.numeric(x))
surfarea[, 4:72] <- apply(surfarea[, 4:72], 2, function(x) x/surfarea$smri_area_cdk_total)
surfarea$smri_area_cdk_total <- NULL

site <- site[ , c("src_subject_id", "site_id_l")]
surfarea <- merge(site, surfarea, by="src_subject_id")

combat_input <- surfarea %>% select(starts_with("smri_area_cdk"))
combat_input[, 1:length(combat_input)] <- apply(combat_input[, 1:length(combat_input)], 2, function(x) as.numeric(x))

names <- colnames(combat_input)


#missing_env <- as.data.frame(sapply(combat_input, function(x) sum(is.na(x))))

combat_input <- t(combat_input)
combat_input <- apply(combat_input, 2 ,as.numeric)

batch <- surfarea$site_id_l # can be numeric or character
sex <- surfarea$demo_sex_v2 
age <- surfarea$interview_age

mod <- model.matrix(~age+sex)

# not using empirical Bayes because it's a large sample size and the number of features < participants
# To run the ComBat model without empirical Bayes, which boils down to fitting a location/shift (L/S) model for each feature separately
surfarea.harmonized <- neuroCombat(dat=combat_input, batch=batch, eb=FALSE, mod=mod)

# harmonized data
surfarea.harmonized <- as.data.frame(t(surfarea.harmonized$dat.combat))
colnames(surfarea.harmonized) <- names



### volume

volume <- complete_sample %>% select(smri_vol_scs_ltventriclelh:smri_vol_scs_vedcrh, smri_vol_scs_intracranialv, src_subject_id, interview_age, demo_sex_v2)
volume$interview_age <- as.numeric(volume$interview_age)
volume$demo_sex_v2 <- as.factor(volume$demo_sex_v2)

volume[, 1:27] <- apply(volume[, 1:27], 2, function(x) as.numeric(x))
volume[, 1:26] <- apply(volume[, 1:26], 2, function(x) x/volume$smri_vol_scs_intracranialv)
volume$smri_vol_scs_intracranialv <- NULL

site <- site[ , c("src_subject_id", "site_id_l")]
volume <- merge(site, volume, by="src_subject_id")

combat_input <- volume %>% select(starts_with("smri_vol_scs"))
names <- colnames(combat_input)

#missing_env <- as.data.frame(sapply(combat_input, function(x) sum(is.na(x))))

combat_input <- t(combat_input)
combat_input <- apply(combat_input, 2 ,as.numeric)


batch <- volume$site_id_l # can be numeric or character
sex <- volume$demo_sex_v2
age <- volume$interview_age

mod <- model.matrix(~age+sex)

# not using empirical Bayes because it's a large sample size and the number of features < participants
# To run the ComBat model without empirical Bayes, which boils down to fitting a location/shift (L/S) model for each feature separately
volume.harmonized <- neuroCombat(dat=combat_input, batch=batch, eb=FALSE, mod=mod)

# harmonized data
volume.harmonized <- as.data.frame(t(volume.harmonized$dat.combat))
colnames(volume.harmonized) <- names


# saving imaging files
write.csv(imaging.harmonized, file=paste(Output_directory, "restingstate_discovery_input.csv", sep=""), row.names = FALSE)
write.csv(volume.harmonized, file=paste(Output_directory, "volume_discovery_input.csv", sep=""), row.names = FALSE)
write.csv(surfarea.harmonized, file=paste(Output_directory, "surfarea_discovery_input.csv", sep=""), row.names = FALSE)
write.csv(cortthick.harmonized, file=paste(Output_directory, "cortthick_discovery_input.csv", sep=""), row.names = FALSE)

site <- imaging[ , c("subjectkey", "site_id_l")]
write.csv(site, file=paste(Output_directory, "subjects_site.csv", sep=""), row.names = FALSE)


colnames(complete_sample) <- gsub("data.", "", colnames(complete_sample))

environment <- complete_sample %>% select(parental_education:discrim)
cognition <- complete_sample %>% select(nihtbx_flanker_uncorrected:lmt_scr_num_correct)

# need to divide into discovery and replication
write.csv(environment, file=paste(Output_directory, "environment_input_discovery.csv", sep=""), row.names = FALSE)
write.csv(cognition, file=paste(Output_directory, "cognition_input_discovery.csv", sep=""), row.names = FALSE)




