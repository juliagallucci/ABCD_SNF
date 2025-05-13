# SNF analysis

This folder contains R and shell scripts for data processing, integration, and clustering using cognitive, environmental, structural, and functional input modalities.

## Script Summaries
- **metaSNF_5050_split.Rmd** script to identify hyperparameters (number of nearest neighbors (k), the normalization parameter (alpha)) for SNF data integration using grid search
- **justSNF.R** script to run data integration using the hyperparameters from metaSNF and spectral clustering to generate cluster assignments
- **run_cluster.sh** script to run SNF in parallel by cluster number (2-6)
- **cluster_comparisons.Rmd** Comparing consensus clustering to determine the optimal cluster number
