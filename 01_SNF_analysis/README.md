# SNF analysis

This folder contains R and shell scripts for data processing, integration, and clustering using cognitive, environmental, structural, and functional input modalities.

## Script Summaries

- **Cognitive_scores.R**: Extracts, merges, and formats raw ABCD cognitive test scores for downstream integration in multimodal clustering.
- **imaging_data.R**: Loads, QC-filters, and extracts structural and functional MRI data from the ABCD study for inclusion in multimodal clustering.
- **COMBAT_diffusion_data.R**: Applies ComBat harmonization to out-of-model diffusion MRI (FA and MD) data to correct for site-related variability before clustering.
- **environmental_factors.R**: Processes and summarizes ABCD environmental, sociodemographic, developmental, and psychosocial variables for clustering input.
- **knn_imputation_environmental_data.R**: Performs k-nearest neighbor imputation to address missing values in environmental data prior to SNF integration.
- **metaSNF_5050_split.Rmd** script to identify hyperparameters (number of nearest neighbors (k), the normalization parameter (alpha)) for SNF data integration using grid search
- **justSNF.R** script to run data integration using the hyperparameters from metaSNF and spectral clustering to generate cluster assignments
- **run_cluster.sh** script to run SNF in parallel by cluster number (2-6)
- **cluster_comparisons.Rmd** Comparing consensus clustering to determine the optimal cluster number
