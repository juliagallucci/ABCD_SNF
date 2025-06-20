# Out of model
This folder contains R Markdown scripts for out-of-model analyses, including subgroup differences in held-out (white matter + psychopathology) and longitudinal symptom data.

## Script Summaries

- **COMBAT_diffusion_data.R**: Applies ComBat harmonization to out-of-model diffusion MRI (FA and MD) data to correct for site-related variability before cluster comparison.
- **Out-of-model.Rmd**: ANOVAs group-comparisons across held-out features; diffusion and CBCL 
- **Longitudinal_Analysis_PLE.Rmd**
Performs longitudinal trajectory modeling of Total PLE scores across baseline, Year 1, and Year 2 using LCGA and GMM.
