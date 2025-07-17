# Vignettes

This package includes comprehensive vignettes that demonstrate all the functionality of the `rwa` package.

## Available Vignettes

### 1. Introduction to RWA
Main tutorial covering the basics of Relative Weights Analysis.

### 2. Bootstrap Confidence Intervals  
Advanced guide to statistical significance testing with bootstrap methods.

## Accessing the Vignettes

After installing the package, you can access the vignettes in several ways:

### 1. From R Console
```r
# View available vignettes
vignette(package = "rwa")

# Open specific vignettes
vignette("introduction-to-rwa", package = "rwa")
vignette("bootstrap-confidence-intervals", package = "rwa")
```

### 2. From RStudio

- Go to the **Help** menu
- Select **Vignettes**
- Look for the `rwa` package

### 3. Online Documentation
The vignettes are also available in the package documentation website.

## Vignette Contents

### Introduction to RWA
1. **Basic methodology** - Understanding when and why to use Relative Weights Analysis
2. **Simple examples** - Step-by-step examples with interpretation  
3. **Real-world applications** - Diamond price analysis example
4. **Best practices** - Guidelines for proper use and troubleshooting

### Bootstrap Confidence Intervals
1. **Statistical significance** - Why and how to test relative weight significance
2. **Bootstrap methods** - BCA, percentile, and basic bootstrap confidence intervals
3. **Advanced features** - Comprehensive analysis and focal variable comparisons
4. **Best practices** - Sample size, interpretation, and reporting guidelines

## Building Vignettes Locally

If you're developing the package, you can build vignettes locally:

```r
# Build vignettes during package installation
devtools::install(build_vignettes = TRUE)

# Or build vignettes separately
devtools::build_vignettes()
```

Note: Building vignettes requires the suggested packages (`knitr`, `rmarkdown`) to be installed.
