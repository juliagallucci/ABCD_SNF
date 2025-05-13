###Organizing Imaging ABCD data and qcing it based off of recommendations from ABCD including Damien Fair's presentation
#https://abcd-repronim.github.io/materials/week-4/ 

##### Loading all necessary data
setwd("/mnt/tigrlab/projects/tsecara/ABCD_julia/data/5.1/imaging/")

# MRI Raw Quality Control
mriqc_1 <- read.csv("mri_y_qc_raw_smr_t1.csv",  header=TRUE)
mriqc_1 <- mriqc_1[which(mriqc_1$eventname == "baseline_year_1_arm_1"), ]
mriqc_1 <- mriqc_1[ , c("src_subject_id", "iqc_t1_ok_ser")] 

mriqc_2 <- read.csv("mri_y_qc_raw_rsfmr.csv",  header=TRUE)
mriqc_2 <- mriqc_2[which(mriqc_2$eventname == "baseline_year_1_arm_1"), ]
mriqc_2 <- mriqc_2[ , c("src_subject_id", "iqc_rsfmri_ok_ser")] 

#merge raw MRI and fMRI quality control
mriqc <- merge(mriqc_1, mriqc_2, by = "src_subject_id")


## Visual qc of freesurfer - consensus from two trained raters
freesurfer <- read.csv("mri_y_qc_man_fsurf.csv", header=TRUE)
freesurfer <- freesurfer[which(freesurfer$eventname == "baseline_year_1_arm_1"), ]
freesurfer <- freesurfer[ , c("src_subject_id", "fsqc_qc")]


## Auto postqc
auto_postpro_qc <- read.csv("mri_y_qc_auto_post.csv",header=TRUE)
auto_postpro_qc <- auto_postpro_qc[which(auto_postpro_qc$eventname == "baseline_year_1_arm_1"), ]
auto_postpro_qc <- auto_postpro_qc[ , c("src_subject_id", "apqc_fmri_bounwarp_flag", "apqc_fmri_regt1_rigid", "apqc_fmri_fov_cutoff_dorsal", "apqc_fmri_fov_cutoff_ventral")]
auto_postpro_qc[, 2:5] <- apply(auto_postpro_qc[, 2:5], 2, function(x) as.numeric(x))


## Manual post-processing QC 
manual_qc <- read.csv("mri_y_qc_man_post_fmr.csv",  header=TRUE)
manual_qc <- manual_qc[which(manual_qc$eventname == "baseline_year_1_arm_1"), ]
manual_qc <- manual_qc[ ,c("src_subject_id", "fmri_postqc_qc")]

## Imaging inclusion summary
inclusion_summary <- read.csv("mri_y_qc_incl.csv", header=TRUE)
inclusion_summary <- inclusion_summary[which(inclusion_summary$eventname == "baseline_year_1_arm_1"), ]
inclusion_summary <- inclusion_summary[ ,c("src_subject_id", "imgincl_rsfmri_include")]
inclusion_summary <- inclusion_summary[which(inclusion_summary$imgincl_rsfmri_include == "1"), ]

# Gordon Network network correlations
# QCing based on Hagler et al, NeuroImage, 2019
rs_net <- read.csv("mri_y_rsfmr_cor_gp_gp.csv", header=TRUE)
rs_net <- rs_net[which(rs_net$eventname == "baseline_year_1_arm_1"), ]

qc_motion <- read.csv("mri_y_qc_motion.csv", header=TRUE)
qc_motion <- qc_motion[which(qc_motion$eventname == "baseline_year_1_arm_1"), ]

# Gordon Network to subcortical ROI correlations
# QCing based on Hagler et al, NeuroImage, 2019
rs_sub <- read.csv("mri_y_rsfmr_cor_gp_aseg.csv", header=TRUE)
rs_sub <- rs_sub[which(rs_sub$eventname == "baseline_year_1_arm_1"), ]
#rs_sub <- rs_sub[, c(4, 23:269)]

# # cortical thickness & surface area
ct_dk <- read.csv("mri_y_smr_thk_dsk.csv", header=TRUE)
ct_dk <- ct_dk[which(ct_dk$eventname == "baseline_year_1_arm_1"), ]
ct_dk <- ct_dk %>% select(src_subject_id,starts_with("smri_thick_cdk"))

sa_dk <- read.csv("mri_y_smr_area_dsk.csv", header=TRUE)
sa_dk <- sa_dk[which(sa_dk$eventname == "baseline_year_1_arm_1"), ]
sa_dk <- sa_dk %>% select(src_subject_id,starts_with("smri_area_cdk"))

# subcortical volume
vol_dk <- read.csv("mri_y_smr_vol_aseg.csv", header=TRUE)
vol_dk <- vol_dk[which(vol_dk$eventname == "baseline_year_1_arm_1"), ]
vol_dk <- vol_dk %>% select(src_subject_id,starts_with("smri_vol_scs"))

###############################################
# Merging quality control data for structure
mriqc$iqc_t1_ok_ser <- as.numeric(mriqc$iqc_t1_ok_ser) # Number of series that are complete and passed QC
mriqc$iqc_rsfmri_ok_ser <- as.numeric(mriqc$iqc_rsfmri_ok_ser) # Number of series that are complete and passed QC
freesurfer <- merge(freesurfer, mriqc, by="src_subject_id", all.x = TRUE)

# structural T1 weighted measures
t1_dk <- merge(ct_dk, sa_dk, by="src_subject_id")
t1_dk <- merge(t1_dk, vol_dk, by="src_subject_id")
t1_dk <- merge(t1_dk, freesurfer, by="src_subject_id") # 11728
t1_dk <- t1_dk[which(t1_dk$fsqc_qc != "0" | is.na(t1_dk$fsqc_qc)), ] # passed freesurfer quality control 11277
t1_dk <- t1_dk[which(t1_dk$iqc_t1_ok_ser > 0), ] # 11273


write.csv(t1_dk, "/projects/jgallucci/abcd/data/t1_dk.csv")

# Quality control for resting-state fmri
rs_net <- merge(rs_net, freesurfer, by="src_subject_id", all.x = TRUE) # 11225 
rs_net <- merge(rs_net, auto_postpro_qc, by="src_subject_id", all.x = TRUE) # 11225
rs_net <- merge(rs_net, manual_qc, by="src_subject_id", all.x = TRUE) # 11225
rs_net <- merge(rs_net, qc_motion, by="src_subject_id", all.x = TRUE) # 11225

rs_net <- rs_net[which(rs_net$iqc_t1_ok_ser > 0), ] #11221
rs_net <- rs_net[which(rs_net$iqc_rsfmri_ok_ser > 0), ] #11221
rs_net <- rs_net[which(rs_net$fsqc_qc != "0" | is.na(rs_net$fsqc_qc)), ] #10829
rs_net <- rs_net[which(rs_net$fmri_postqc_qc != "0" | is.na(rs_net$fmri_postqc_qc)), ] #10442
# Number of frames after excluding outlier frames (based on standard deviation across ROIs)
rs_net$rsfmri_ntpoints <- as.numeric(rs_net$rsfmri_ntpoints) 
rs_net <- rs_net[which(rs_net$rsfmri_ntpoints > 375), ] #9381
rs_net <- rs_net[which(rs_net$apqc_fmri_bounwarp_flag == "1"), ] #9379
rs_net <- rs_net[which(rs_net$apqc_fmri_regt1_rigid < 19), ] #9379
rs_net <- rs_net[which(rs_net$apqc_fmri_fov_cutoff_dorsal < 65), ] #9379
rs_net <- rs_net[which(rs_net$apqc_fmri_fov_cutoff_ventral < 60), ] #9377

# Number of frames after excluding outlier frames (based on standard deviation across ROIs)
#Time points with FD greater than 0.2 mm are excluded from thevariance and correlation calculations. \
#Note that this is a slightly moreconservative threshold than that used for the regression step. 
#Time pe-riods with fewer thanfive contiguous, sub-threshold time points are alsoexcluded. 
#The effects of head motion can potentially linger for severalseconds after an abrupt head motion, for example
#due to spin-history orT1relaxation effects (Friston et al., 1996), so an additional round ofcensoring is applied 
#based on detecting time points that are outliers withrespect to spatial variation across the brain. 
#SD across ROIs is calculatedfor each time point, and outlier time points, defined as having an SDvalue more than 
#three times the median absolute deviation (MAD) belowor above the median SD value, are excluded from variance and correla-tion calculations.

#rs_net <- rs_net[ , c(1, 23:191)]
resting_state <- merge(rs_net, rs_sub, by="src_subject_id")

write.csv(resting_state, "/projects/jgallucci/abcd/data/resting_state.csv")



freesurfer <- NULL
relationship <- NULL
mriqc_1 <- NULL
rs_net <- NULL
rs_sub <- NULL
#vol_dk <- NULL
#ct_dk <- NULL
auto_postpro_qc <- NULL
manual_qc <- NULL
inclusion_summary <- NULL

