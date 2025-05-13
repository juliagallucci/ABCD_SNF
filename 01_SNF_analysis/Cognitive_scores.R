## Organizing ABCD cognitive scores

# Want to include raw scores - interested in sex and age, don't want to normalize that information out
#### Uncorrected standard scores 
# NIH Toolbox Flanker Inhibitory Control and Attention Test - Measure of Cognitive Control/Attention  
# NIH Toolbox List Sorting Working Memory Test - Working Memory; Categorization; Information Processing    
# NIH Toolbox Dimensional Change Card Sort Test - Flexible thinking; concept formation; set shifting    
# NIH Toolbox Pattern Comparison Processing Speed Test - Processing Speed; Information Processing    
# NIH Toolbox Picture Sequence Memory Test - Visuospatial sequencing & memory      
# NIH Toolbox Oral Reading Recognition Test - Reading Ability; Language; Academic Achievement  
# NIH Toolbox Picture Vocabulary Test - Language; Verbal intellect  
# WISC-V Matrix Reasoning Total Scaled Score - Fluid Reasoning; Visuospatial ability; Part-whole reasoning; Visual sequencing  
# Rey Auditory Verbal Learning Test Total Correct Responses across trials - Verbal encoding; learning; memory  
# Little Man Task Total Correct & average reaction time - Visuospatial attention; Perspective-taking; mental rotation

setwd("/mnt/tigrlab/projects/tsecara/ABCD_julia/data/5.1/neurocognition/")

# NIH TB Summary Scores
# Measures included in NIH TB Summary Scores: NIH TBX Picture Vocabulary; NIH Tbx Flanker Inhibitory Control and Attention; NIH Tbx List Sorting Working Memory; NIH Tbx Dimensional Change Card Sort; NIH Tbx Pattern Comparison Processing Speed;NIH Tbx Picture Sequence Memory; NIH Tbx Oral Reading Recognition
NIH <- read.csv("nc_y_nihtb.csv", header=TRUE)
NIH <- NIH[which(NIH$eventname == "baseline_year_1_arm_1"), ]
NIH <- NIH[, c(1,5:120)]

# WISC-V Matrix reasoning
# Measures included in WISC-V: Fluid Reasoning; Visuospatial ability; Part-whole reasoning; Visual sequencing
WISCV <- read.csv("nc_y_wisc.csv", header = TRUE)
WISCV <- WISCV[which(WISCV$eventname == "baseline_year_1_arm_1"), ]
WISCV <- WISCV[, c(1,3:38)]

# Pearson scores
# Measures included in Pearson Scores: Rey Auditory Verbal Learning Test, 
# Matrix Reasoning Test, and Rey Delayed Recall Test
pearson <- read.csv("nc_y_ravlt.csv",header=TRUE)
pearson <- pearson[which(pearson$eventname == "baseline_year_1_arm_1"), ]
pearson <- pearson[, c(1, 3:29)] 

## Little Man Task
little <- read.csv("nc_y_lmt.csv", header=TRUE)
little <- little[which(little$eventname == "baseline_year_1_arm_1"), ]
little <- little[, c(1, 3:56)]

all <- merge(pearson, NIH, by="src_subject_id", all = TRUE)
all <- merge(all, WISCV, by="src_subject_id", all = TRUE)
all <- merge(all, little, by="src_subject_id", all = TRUE)


# making pearson columns numeric
all[, c("pea_ravlt_sd_trial_i_tc", "pea_ravlt_sd_trial_ii_tc", "pea_ravlt_sd_trial_iii_tc", 
        "pea_ravlt_sd_trial_iv_tc", "pea_ravlt_sd_trial_v_tc", "pea_ravlt_sd_trial_vi_tc", 
        "pea_ravlt_sd_listb_tc", "pea_ravlt_sd_trial_vii_tc")] <- 
  lapply(c("pea_ravlt_sd_trial_i_tc", "pea_ravlt_sd_trial_ii_tc", "pea_ravlt_sd_trial_iii_tc", 
           "pea_ravlt_sd_trial_iv_tc", "pea_ravlt_sd_trial_v_tc", "pea_ravlt_sd_trial_vi_tc", 
           "pea_ravlt_sd_listb_tc", "pea_ravlt_sd_trial_vii_tc"), function(x) as.numeric(all[[x]]))

# adding total correct score
all$rey_all_correct <- rowSums(all[, c("pea_ravlt_sd_trial_i_tc", "pea_ravlt_sd_trial_ii_tc", 
                                       "pea_ravlt_sd_trial_iii_tc", "pea_ravlt_sd_trial_iv_tc", 
                                       "pea_ravlt_sd_trial_v_tc", "pea_ravlt_sd_trial_vi_tc", 
                                       "pea_ravlt_sd_listb_tc")])
# summarizing scores
Cog_scores <- all %>% select(src_subject_id, nihtbx_flanker_uncorrected, nihtbx_list_uncorrected, nihtbx_cardsort_uncorrected, nihtbx_pattern_uncorrected, nihtbx_picture_uncorrected, nihtbx_reading_uncorrected,
         nihtbx_picvocab_uncorrected, pea_wiscv_trs, rey_all_correct, lmt_scr_num_correct, lmt_scr_rt_correct) %>%
  gather(Test, Score, -src_subject_id)

# cleaning up values
Cog_scores$Score <- as.numeric(Cog_scores$Score)
Cog_scores$Score <- ifelse(is.na(Cog_scores$Score), NA, Cog_scores$Score)

# making all scores numeric
all$nihtbx_flanker_uncorrected <- as.numeric(all$nihtbx_flanker_uncorrected)
all$nihtbx_list_uncorrected <- as.numeric(all$nihtbx_list_uncorrected)
all$nihtbx_cardsort_uncorrected <- as.numeric(all$nihtbx_cardsort_uncorrected)
all$nihtbx_pattern_uncorrected <- as.numeric(all$nihtbx_pattern_uncorrected)
all$nihtbx_picture_uncorrected <- as.numeric(all$nihtbx_picture_uncorrected)
all$nihtbx_reading_uncorrected <- as.numeric(all$nihtbx_reading_uncorrected)
all$nihtbx_picvocab_uncorrected <- as.numeric(all$nihtbx_picvocab_uncorrected)
all$pea_wiscv_trs <- as.numeric(all$pea_wiscv_trs)
all$rey_all_correct <- as.numeric(all$rey_all_correct)
all$lmt_scr_num_correct <- as.numeric(all$lmt_scr_num_correct)

# narrowing down data frame to measures
cognition <- all[ ,c("src_subject_id", "nihtbx_flanker_uncorrected", "nihtbx_list_uncorrected", 
                 "nihtbx_cardsort_uncorrected", "nihtbx_pattern_uncorrected", 
                 "nihtbx_picture_uncorrected", "nihtbx_reading_uncorrected",
                 "nihtbx_picvocab_uncorrected", "pea_wiscv_trs", "rey_all_correct",
                 "lmt_scr_num_correct")]

# table(cognition$nihtbx_flanker_uncorrected, exclude = NULL) #150
# table(cognition$nihtbx_list_uncorrected, exclude = NULL) #193
# table(cognition$nihtbx_cardsort_uncorrected, exclude = NULL) #149
# table(cognition$nihtbx_pattern_uncorrected, exclude = NULL) #168
# table(cognition$nihtbx_picture_uncorrected, exclude = NULL) #156
# table(cognition$nihtbx_reading_uncorrected, exclude = NULL) #158
# table(cognition$nihtbx_picvocab_uncorrected, exclude = NULL) #144
# table(cognition$pea_wiscv_trs, exclude = NULL) #240
# table(cognition$rey_all_correct, exclude = NULL) #331
# table(cognition$lmt_scr_num_correct, exclude = NULL) #334


all <- NULL
little <- NULL
NIH <- NULL
pearson <- NULL

write.csv(cognition , "/projects/jgallucci/abcd/cognition.csv")
