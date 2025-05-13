### Applying ComBat to ABCD diffusion data to harmonize across sites

setwd("/mnt/tigrlab/projects/tsecara/ABCD_julia/data/5.1/abcd-general/")
relationship_1 <- read.csv("abcd_y_lt.csv", header=TRUE)
relationship_2 <- read.csv("abcd_p_demo.csv", header=TRUE)
relationship_1 <- relationship_1[which(relationship_1$eventname == "baseline_year_1_arm_1"), ] #11868
relationship_2 <- relationship_2[which(relationship_2$eventname == "baseline_year_1_arm_1"), ] #11868
relationship <- merge(relationship_1, relationship_2, by = "src_subject_id")
relationship <- relationship[ ,c("src_subject_id", "rel_family_id", "interview_age",  "demo_sex_v2")]
relationship$interview_age <- as.numeric(relationship$interview_age)
relationship$demo_sex_v2 <- as.factor(relationship$demo_sex_v2)

library(neuroCombat)

## site data for combat
site <- read.csv("abcd_y_lt.csv", header=TRUE)
site <- site[which(site$eventname == "baseline_year_1_arm_1"), ]

###### scanner - don't actually need this
setwd("/mnt/tigrlab/projects/tsecara/ABCD_julia/data/5.1/imaging/")
scanner <- read.csv("mri_y_adm_info.csv",  header=TRUE) # 22939
scanner <- scanner[which(scanner$eventname == "baseline_year_1_arm_1"), ] #11771
scanner <- scanner[ , c("src_subject_id", "mri_info_manufacturer")]

# included participants
subjects <- complete_sample$src_subject_id

## Diffusion imaging data
setwd("/mnt/tigrlab/projects/tsecara/ABCD_julia/data/5.1/imaging/")
diffusion_all <- read.csv("mri_y_dti_fa_fs_at.csv", header=TRUE)
diffusion_all <- diffusion_all[which(diffusion_all$eventname == "baseline_year_1_arm_1"), ]
diffusion_all <- merge(diffusion_all, complete_sample, by="src_subject_id")


FA <-diffusion_all %>%
  select(src_subject_id, starts_with("dmdtifp1_"))

FA <- merge(FA, site, by="src_subject_id")
FA <- merge(FA, relationship, by="src_subject_id")
#FA <- FA %>% mutate_all(na_if,"")
FA <-FA %>%
  select(src_subject_id, starts_with("dmdtifp1_"), site_id_l,demo_sex_v2,interview_age)
FA <- na.omit(FA)



md_diffusion_all <- read.csv("mri_y_dti_md_fs_at.csv", header=TRUE)
md_diffusion_all <- md_diffusion_all[which(md_diffusion_all$eventname == "baseline_year_1_arm_1"), ]
md_diffusion_all <- merge(md_diffusion_all, complete_sample, by="src_subject_id")

MD <- md_diffusion_all %>%
  select(src_subject_id, starts_with("dmdtifp1_"))
MD <- merge(MD, site, by="src_subject_id")
MD <- merge(MD, relationship, by="src_subject_id")
MD <-MD %>%
  select(src_subject_id, starts_with("dmdtifp1_"), site_id_l,demo_sex_v2,interview_age)
MD <- na.omit(MD)

### Combat harmonization
#https://github.com/Jfortin1/ComBatHarmonization/tree/master/R

combat_input <- FA %>% select(starts_with("dmdtifp"))
names <- colnames(combat_input)
combat_input[, 1:length(combat_input)] <- apply(combat_input[, 1:length(combat_input)], 2, function(x) as.numeric(x))
combat_input <- na.omit(combat_input)
combat_input <- t(combat_input)
combat_input <- apply(combat_input, 2 ,as.numeric)

batch <- FA$site_id_l # can be numeric or character
sex <- FA$demo_sex_v2
age <- FA$interview_age
mod <- model.matrix(~age+sex)

# not using empirical Bayes because it's a large sample size and the number of features < participants
# To run the ComBat model without empirical Bayes, which boils down to fitting a location/shift (L/S) model for each feature separately
imaging.harmonized <- neuroCombat(dat=combat_input, batch=batch, mod=mod)

# harmonized data
imaging.harmonized <- as.data.frame(t(imaging.harmonized$dat.combat))
colnames(imaging.harmonized) <- names

FA_harmonized <- imaging.harmonized
FA_harmonized$src_subject_id <- FA$src_subject_id


########### mean difusivity
combat_input <- MD %>% select(starts_with("dmdtifp"))
names <- colnames(combat_input)
combat_input[, 1:length(combat_input)] <- apply(combat_input[, 1:length(combat_input)], 2, function(x) as.numeric(x))
combat_input <- na.omit(combat_input)
combat_input <- t(combat_input)
combat_input <- apply(combat_input, 2 ,as.numeric)

batch <- MD$site_id_l # can be numeric or character
sex <- MD$demo_sex_v2
age <- MD$interview_age
mod <- model.matrix(~age+sex)

# not using empirical Bayes because it's a large sample size and the number of features < participants
# To run the ComBat model without empirical Bayes, which boils down to fitting a location/shift (L/S) model for each feature separately
imaging.harmonized <- neuroCombat(dat=combat_input, batch=batch, parametric=FALSE, eb=FALSE, mod=mod)

# harmonized data
imaging.harmonized <- as.data.frame(t(imaging.harmonized$dat.combat))
colnames(imaging.harmonized) <- names

MD_harmonized <- imaging.harmonized
MD_harmonized$src_subject_id <- MD$src_subject_id

## saving files
write.csv(FA_harmonized, file="/projects/jgallucci/abcd/data/Diff_FA_harmonized.csv", row.names = FALSE)
write.csv(MD_harmonized, file="/projects/jgallucci/abcd/data/Diff_MD_harmonized.csv", row.names = FALSE)
