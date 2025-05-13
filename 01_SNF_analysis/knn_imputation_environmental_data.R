## Using K nearest neighbours to compute missing values for environmental data

# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("impute")

library(impute)

env <- read.csv("output/SNF_input/environment_input_discovery_missingvalues.csv")
missing_env <- as.data.frame(sapply(env, function(x) sum(is.na(x))))

# check <- env
# check$row <- seq(1, 3474)
# check <- check[which(is.na(check$total_caregiver_support)), ]
# check$row 

# maxp = The largest block of genes imputed using the knn algorithm inside impute.knn (default 1500); 
#larger blocks are divided by two-means clustering (recursively) prior to imputation. 
#If maxp=p, only knn imputation is done.
env_imputed <- impute.knn(as.matrix(env) ,k = 15, rowmax = 0.5, colmax = 0.8, maxp = 3474, rng.seed=362436069)
env_imputed <- as.data.frame(env_imputed[1])

write.csv(env_imputed, file="output/SNF_input/environment_input_discovery_knn.csv", row.names = FALSE)
write.csv(env_imputed, file="code/Running_SNF/Discovery_Input/environment_input_discovery_knn.csv", row.names = FALSE)


#impute.knn uses -nearest neighbors in the space of genes to impute missing expression values. 
#For each gene with missing values, we find the  nearest neighbors using a Euclidean metric, 
#confined to the columns for which that gene is NOT missing. Each candidate neighbor might be 
#missing some of the coordinates used to calculate the distance. In this case we average the distance 
#from the non-missing coordinates. Having found the k nearest neighbors for a gene, we impute the 
#missing elements by averaging those (non-missing) elements of its neighbors. This can fail if 
#ALL the neighbors are missing in a particular element. In this case we use the overall column 
#mean for that block of genes. Since nearest neighbor imputation costs  operations per gene, where  
#is the number of rows, the computational time can be excessive for large p and a large number of 
#missing rows. Our strategy is to break blocks with more than maxp genes into two smaller blocks using
#two-mean clustering. This is done recursively till all blocks have less than maxp genes. 
#For each block, -nearest neighbor imputation is done separately. We have set the default value of 
#maxp to 1500. Depending on the speed of the machine, and number of samples, this number might be
#increased. Making it too small is counter-productive, because the number of two-mean clustering 
#algorithms will increase.



