# Stability testing
This folder contains R Markdown and R scripts for stability testing of clustering, both in the discovery and replication samples.
*Code adapted from https://github.com/gracejacobs/SNF-NDD-Clustering*

## Script Summaries
- **bootstrap.R** Defines functions to perform bootstrap resampling, clustering via SNF, and calculate stability metrics such as NMI scores, adjusted Rand indices, and participant agreement matrices.

- **Running_bootstrapping.R** Calls the functions from bootstrapping.R to perform the full resampling analysis for cluster stability on your data.

- **post-bootstrapping_visualizations.Rmd** Generates visualizations of clustering stability and agreement based on bootstrap resampling results, including participant agreement matrices, and adjusted Rand Index distributions.
