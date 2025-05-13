#!/usr/bin/env Rscript

### Script to run resampling stability analysis

library(corrplot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape)

insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

#change split2 to split1 when running discovery vs replication
subjects <- read.csv("/projects/jgallucci/abcd/data/split2_subs.csv", header=TRUE)
subjects < subjects$src_subject_id
CT <- read.csv("/projects/jgallucci/abcd/data/Split2_Input/CT.csv", header=TRUE)
VOL <- read.csv("/projects/jgallucci/abcd/data/Split2_Input/VOL.csv", header=TRUE)
SA <- read.csv("/projects/jgallucci/abcd/data/Split2_Input/SA.csv", header=TRUE)
ENV <-  read.csv("/projects/jgallucci/abcd/data/Split2_Input/ENV.csv", header=TRUE)
RS <-  read.csv("/projects/jgallucci/abcd/data/Split2_Input/RS.csv", header=TRUE)
COG <-  read.csv("/projects/jgallucci/abcd/data/Split2_Input/COG.csv", header=TRUE)

directory <- ("/projects/jgallucci/abcd/output/Split2_Stability_C5")

source("/projects/jgallucci/abcd/code/bootstrapping.r")
numboot=1000
nsub=858
K=44
alpha=0.7
t=20
bootsize=0.8
clusters=5

# Setting up which participants will be included in each permutation
permutation_matrix <- bootstrapping_SNF(numboot=numboot, nsub=nsub, bootsize=bootsize)
# Getting clustering solutions for all the permuatations of sampled participants using SNF 
clus_sil <- clustering(perms=permutation_matrix, bootsize=bootsize, K=K, t=t, alpha=alpha, clusters=clusters, CT=CT, SA=SA, VOL=VOL, RS=RS, ENV=ENV, COG=COG)
# Dividing output matrix into clusters and silhouette widths
clus_out <- clus_sil[1:(numboot), ]
silhouette_width <- clus_sil[(numboot+1):(numboot*2), ]

# getting the adjusted rand index between all clustering solutions
list_randindex <- stability(clus_out=clus_out, perms=permutation_matrix) # returns brandindex:adjusted rand index
# Calculate how often each participant is clusted together and the probability that they will be clustered together
percent_agree <- percent_agree(clus_out=clus_out)

write.csv(percent_agree, file=file.path(directory, paste("Percent_agree_5c_1000perms.csv", sep="")))
write.csv(list_randindex, file=file.path(directory, paste("Rand_indices_5c_1000perms.csv", sep="")))

write.csv(clus_out, file=file.path(directory, paste("Adj_rand_indices_5c_1000perms.csv", sep="")))
write.csv(permutation_matrix, file=file.path(directory, paste("permutation_matrix_5c_1000perms.csv", sep="")))