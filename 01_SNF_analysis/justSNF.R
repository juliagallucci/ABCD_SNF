# Check if the argument C is passed; if not, set default value
args <- commandArgs(trailingOnly = TRUE)
C <- ifelse(length(args) > 0, as.integer(args[1]), 3)  # Default to 3 if not provided

print(paste("Number of Clusters (C):", C))

#######Install packages
setwd("/projects/jgallucci/abcd/")

library(SNFtool, lib.loc="code/R_packages")

#install.packages("remotes")
#remotes::install_version("SNFtool", version = "2.3.1", repos = "https://cloud.r-project.org/")
#install.packages("devtools")
#devtools::install_github("maxconway/SNFtool")

#remotes::install_version("psych", version = "2.4.6.26", repos = "https://cloud.r-project.org/")
library(psych,lib.loc="code/R_packages")

#remotes::install_version("dunn.test", version = "1.3.6", repos = "https://cloud.r-project.org/")
library(dunn.test,lib.loc="code/R_packages")

#remotes::install_version("ggplot2", version = "3.5.1", repos = "https://cloud.r-project.org/")
library(ggplot2,lib.loc="code/R_packages")

#remotes::install_version("dplyr", version = "1.1.4", repos = "https://cloud.r-project.org/")
library(dplyr,lib.loc="code/R_packages")

#remotes::install_version("tidyverse", version = "1.3.1", repos = "https://cloud.r-project.org/")
library(tidyr,lib.loc="code/R_packages")

#remotes::install_version("broom", version = "1.0.6", repos = "https://cloud.r-project.org/")
library(broom,lib.loc="code/R_packages")

#remotes::install_version("fossil", version = "0.4.0", repos = "https://cloud.r-project.org/")
library(fossil,lib.loc="code/R_packages")


#remotes::install_version("corrplot", version = "0.95", repos = "https://cloud.r-project.org/")
library(corrplot,lib.loc="code/R_packages")

#remotes::install_version("cluster", version = "2.1.6", repos = "https://cloud.r-project.org/")
library(cluster,lib.loc="code/R_packages")

#remotes::install_version("MASS", version = "7.3-61", repos = "https://cloud.r-project.org/")
library(MASS,lib.loc="code/R_packages")

#remotes::install_version("ggsignif", version = "0.6.4", repos = "https://cloud.r-project.org/")
library(ggsignif,lib.loc="code/R_packages")

library(fpc, lib.loc = "code/R_packages")
# source code for function to determine data integration and clustering across re-sampling using SNF and spectral clustering
source("/projects/tsecara/SPINS_SNF/R_code/robust_core-clustering_function.R")


print("####################PACKAGES INSTALLED####################")

#Load in data
#replace split 1 with split 2 if running replication

subjects <- read.csv("/projects/jgallucci/abcd/code/snf/input/complete_sample.csv", header=TRUE)
CT <- read.csv("/projects/jgallucci/abcd/code/snf/input/cortthick_input.csv", header=TRUE)
SA <- read.csv("/projects/jgallucci/abcd/code/snf/input/surfarea_input.csv", header=TRUE)
VOL <- read.csv("/projects/jgallucci/abcd/code/snf/input/volume_input.csv", header=TRUE)
RS <- read.csv("/projects/jgallucci/abcd/code/snf/input/restingstate_input.csv", header=TRUE)
COG <- read.csv("/projects/jgallucci/abcd/code/snf/input/cognition_input_discovery.csv", header=TRUE)
ENV <- read.csv("/projects/jgallucci/abcd/code/snf/input/environment_input_discovery.csv", header=TRUE)
train_subs <- read.csv("/projects/jgallucci/abcd/code/snf/input/split_1.csv")
train_subs <- train_subs$x

#Put back subject ids to each df
CT$src_subject_id = subjects$src_subject_id
SA$src_subject_id = subjects$src_subject_id
VOL$src_subject_id = subjects$src_subject_id
RS$src_subject_id = subjects$src_subject_id
COG$src_subject_id = subjects$src_subject_id
ENV$src_subject_id = subjects$src_subject_id

#subset training data only
train_CT <- CT[CT$"src_subject_id" %in% train_subs, ]
train_SA <- SA[SA$"src_subject_id" %in% train_subs, ]
train_VOL <- VOL[VOL$"src_subject_id" %in% train_subs, ]
train_RS <- RS[RS$"src_subject_id" %in% train_subs, ]
train_COG <- COG[COG$"src_subject_id" %in% train_subs, ]
train_ENV <- ENV[ENV$"src_subject_id" %in% train_subs, ]

train_CT_input = train_CT %>% dplyr::select(-src_subject_id)
train_SA_input = train_SA %>%dplyr::select(-src_subject_id)
train_VOL_input = train_VOL %>%dplyr::select(-src_subject_id)
train_RS_input = train_RS %>%dplyr::select(-src_subject_id)
train_COG_input = train_COG %>%dplyr::select(-src_subject_id)
train_ENV_input = train_ENV %>%dplyr::select(-src_subject_id)

# normalizing measures within each data type using a function from the SNF package
train_CT_input = standardNormalization(train_CT_input)
train_SA_input = standardNormalization(train_SA_input)
train_VOL_input = standardNormalization(train_VOL_input)
train_RS_input = standardNormalization(train_RS_input)
train_COG_input = standardNormalization(train_COG_input)
train_ENV_input = standardNormalization(train_ENV_input)
# setting the parameters (finalized after comparisons using Running_parameter_iterations.r )
K = 44;		# number of neighbors, usually (10~30), Metacluster C
alpha = 0.7;  	# hyperparameter, usually (0.3~0.8) - Metacluster C
t = 20; 	# Number of Iterations, usually (10~20) 

# creating participant distance matrices using euclidean distances
Dist_CT = SNFtool::dist2(as.matrix(train_CT_input),as.matrix(train_CT_input));
Dist_SA = SNFtool::dist2(as.matrix(train_SA_input),as.matrix(train_SA_input));
Dist_VOL = SNFtool::dist2(as.matrix(train_VOL_input),as.matrix(train_VOL_input));
Dist_RS = SNFtool::dist2(as.matrix(train_RS_input),as.matrix(train_RS_input));
Dist_COG = SNFtool::dist2(as.matrix(train_COG_input),as.matrix(train_COG_input));
Dist_ENV = SNFtool::dist2(as.matrix(train_ENV_input),as.matrix(train_ENV_input));

# creating participant affinity matrices for each data type
AM_CT = affinityMatrix(Dist_CT,K,alpha)
AM_SA = affinityMatrix(Dist_SA,K,alpha)
AM_VOL = affinityMatrix(Dist_VOL,K,alpha)
AM_RS = affinityMatrix(Dist_RS,K, alpha)
AM_COG = affinityMatrix(Dist_COG,K, alpha)
AM_ENV = affinityMatrix(Dist_ENV,K, alpha)

C = C  # Using the value of C passed as a parameter

# calling function to integrate data types using SNF and cluster participants using spectral clustering 
#across resampling 80% of participants 1000 times
print("####################SNF RUNNING####################")
robust.W = RobustCoreClusteringMatrix(feature.affinity.mat.list = list(AM_CT, AM_SA, AM_VOL, AM_RS, AM_COG, AM_ENV),exp.num.samples = 1000, num.clusts = C, seed = 123)
#Two matrices - Dense Core Cluster Matrix and Sparse Core Cluster Matrix
dense <- robust.W[1]
dense <- matrix(unlist(dense), ncol = 858, byrow = TRUE)

# displaying clusters
# Save the heatmap plot directly while running the script
output_filename <- paste0(C, "_Split1__clust_Heatmap.png")

# Start the PNG device to save the plot
png(output_filename, width = 1200, height = 800, res = 100)

displayClustersWithHeatmap(dense, spectralClustering(dense, C))
#displayClustersWithHeatmap(sparse, spectralClustering(sparse, C))

# Close the PNG device (this saves the plot)
dev.off()
print("####################CLUSTER MAP####################")

# calculating normalized mutual information (NMI) based off of the original data types and the clustering similarity matrix
SNF_NMIScores <- rankFeaturesByNMI(list(train_CT_input, train_SA_input, train_VOL_input, train_RS_input, train_COG_input, train_ENV_input), dense)
# separating and organizing scores
CT_scores <- as.data.frame(SNF_NMIScores[[1]][1])
SA_scores <- as.data.frame(SNF_NMIScores[[1]][2])
VOL_scores <- as.data.frame(SNF_NMIScores[[1]][3])
RS_scores <- as.data.frame(SNF_NMIScores[[1]][4])
COG_scores <- as.data.frame(SNF_NMIScores[[1]][5])
ENV_scores <- as.data.frame(SNF_NMIScores[[1]][6])
names(CT_scores) <- c("NMI")
names(SA_scores) <- c("NMI")
names(VOL_scores) <- c("NMI")
names(RS_scores) <- c("NMI")
names(COG_scores) <- c("NMI")
names(ENV_scores) <- c("NMI")

CT_scores <- t(as.data.frame(CT_scores))
colnames(CT_scores) = colnames(train_CT_input)
SA_scores <- t(as.data.frame(SA_scores))
colnames(SA_scores) = colnames(train_SA_input)
VOL_scores <- t(as.data.frame(VOL_scores))
colnames(VOL_scores) = colnames(train_VOL_input)
RS_scores <- t(as.data.frame(RS_scores))
colnames(RS_scores) = colnames(train_RS_input)
COG_scores <- t(as.data.frame(COG_scores))
colnames(COG_scores) = colnames(train_COG_input)
ENV_scores <- t(as.data.frame(ENV_scores))
colnames(ENV_scores) = colnames(train_ENV_input)

all_scores <- cbind(CT_scores, SA_scores)
all_scores <- cbind(all_scores, VOL_scores)
all_scores <- cbind(all_scores, RS_scores)
all_scores <- cbind(all_scores, ENV_scores)
all_scores <- cbind(all_scores, COG_scores)
all_scores <- t(all_scores)

directory <- "/projects/jgallucci/abcd/output/"
#saving csv of NMI scores and clustering similarity matrix
write.csv(all_scores, paste0("/projects/jgallucci/abcd/output/Split1_ALLINPUTS_all_scores_", C, "_clust_k44_0.7_1000perms_transposed.csv"))
print("####################NMI SCORES####################")
write.matrix(dense, file = file.path(directory, paste0("Split1_clustering_matrix_", C, "_clust_k44_0.7_1000perms")))
print("####################SIMILARITY MATRIX####################")

# Find cluster labels of individuals using the robust clustering similarity matrix
robust.groups.df = RobustCoreClusteringClusters(core.clustering.list = robust.W,num.clusts = C,verbose = T)

train_subjects <- subjects[subjects$"src_subject_id" %in% train_subs, ]
clusters <- cbind(train_subjects, robust.groups.df)
table(clusters$groups)

#Reorganize and re-integrating demographics file with projected cluters 
clusters$id <- NULL #Removing ID row as this is not necessary 

ori_demo <- read.csv("/projects/jgallucci/abcd/code/snf/input/complete_sample.csv", header=TRUE)
CT_anal <- read.csv("/projects/jgallucci/abcd/code/snf/input/cortthick_input.csv", header=TRUE)
SA_anal <- read.csv("/projects/jgallucci/abcd/code/snf/input/surfarea_input.csv", header=TRUE)
VOL_anal <- read.csv("/projects/jgallucci/abcd/code/snf/input/volume_input.csv", header=TRUE)
RS_anal <- read.csv("/projects/jgallucci/abcd/code/snf/input/restingstate_input.csv", header=TRUE)
COG_anal <- read.csv("/projects/jgallucci/abcd/code/snf/input/cognition_input_discovery.csv", header=TRUE)
ENV_anal <- read.csv("/projects/jgallucci/abcd/code/snf/input/environment_input_discovery.csv", header=TRUE)

#Put back subject ids to each df
CT_anal$src_subject_id = subjects$src_subject_id
SA_anal$src_subject_id = subjects$src_subject_id
VOL_anal$src_subject_id = subjects$src_subject_id
RS_anal$src_subject_id = subjects$src_subject_id
COG_anal$src_subject_id = subjects$src_subject_id
ENV_anal$src_subject_id = subjects$src_subject_id

#Combing the two together 
compare_df <- inner_join(clusters, ori_demo, by = "src_subject_id")
compare_df <- inner_join(compare_df, SA_anal)
compare_df <- inner_join(compare_df, CT_anal)
compare_df <- inner_join(compare_df, VOL_anal)
compare_df <- inner_join(compare_df, RS_anal)
compare_df <- inner_join(compare_df, ENV_anal)
compare_df <- inner_join(compare_df, COG_anal)


write.csv(compare_df, paste0("/projects/jgallucci/abcd/output/Split1_RESULTS_", C, "_clust_k44_0.7.csv"))
print("####################CLUSTER ASSIGNMENT####################")

## calculating silouette width for each participant and the silhouette plot
dissim <- 1 - dense
dissim <- as.matrix(dissim)
clusters$groups <- as.integer(clusters$groups)
sil <- silhouette(clusters$groups, dmatrix = dissim)
# Convert groups column to numeric
clusters$groups <- as.numeric(as.character(clusters$groups))
# Check if there are any non-numeric values
non_numeric <- sum(is.na(clusters$groups))

# saving the silhouette plot
# Dynamically create a color vector based on the value of C
colors <- switch(as.character(C),
                 "2" = c(adjustcolor("#F8766D", alpha.f = 0.5), 
                         adjustcolor("#A3A500", alpha.f = 0.5)),
                 "3" = c(adjustcolor("#F8766D", alpha.f = 0.5), 
                         adjustcolor("#A3A500", alpha.f = 0.5), 
                         adjustcolor("#00BF7D", alpha.f = 0.5)),
                 "4" = c(adjustcolor("#F8766D", alpha.f = 0.5), 
                         adjustcolor("#A3A500", alpha.f = 0.5), 
                         adjustcolor("#00BF7D", alpha.f = 0.5), 
                         adjustcolor("#00B0F6", alpha.f = 0.5)),
                 "5" = c(adjustcolor("#F8766D", alpha.f = 0.5), 
                         adjustcolor("#A3A500", alpha.f = 0.5), 
                         adjustcolor("#00BF7D", alpha.f = 0.5), 
                         adjustcolor("#00B0F6", alpha.f = 0.5), 
                         adjustcolor("#E76BF3", alpha.f = 0.5)),
                 "6" = c(adjustcolor("#F8766D", alpha.f = 0.5), 
                         adjustcolor("#A3A500", alpha.f = 0.5), 
                         adjustcolor("#00BF7D", alpha.f = 0.5), 
                         adjustcolor("#00B0F6", alpha.f = 0.5), 
                         adjustcolor("#E76BF3", alpha.f = 0.5), 
                         adjustcolor("#D89000", alpha.f = 0.5))
)

# Saving the silhouette plot with dynamic colors
name = paste0(C, "Split1_clust_Silhouette_plot_0.7_44.png")
png(name, width = 1200, height = 800, res = 100)

plot(sil, col = colors, border = NA, lwd = 1)

dev.off()
print("####################SILHOUETTES####################")


cluster_stats <-fpc::cluster.stats(d=dissim, clusters$groups, alt.clustering = NULL, noisecluster = FALSE, silhouette = TRUE,wgap = TRUE, sepindex = TRUE)
cluster_stats <- as.data.frame(unlist(cluster_stats))
write.csv(cluster_stats, file = paste0(directory, "Split1_Cluster_fit_stats_", C, ".csv"), row.names = TRUE)
print("####################FIT STATISTICS####################")
