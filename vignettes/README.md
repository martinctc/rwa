# Vignettes

This package includes a comprehensive vignette that demonstrates all the functionality of the `rwa` package.

## Accessing the Vignette

After installing the package, you can access the vignette in several ways:

### 1. From R Console
```r
# View available vignettes
vignette(package = "rwa")

# Open the main vignette
vignette("introduction-to-rwa", package = "rwa")
```

### 2. From RStudio
- Go to the **Help** menu
- Select **Vignettes**
- Look for the `rwa` package

### 3. Online Documentation
The vignette is also available in the package documentation website.

## Vignette Contents

The main vignette covers:

1. **Introduction to RWA methodology** - Understanding when and why to use Relative Weights Analysis
2. **Basic usage examples** - Simple examples with interpretation
3. **Advanced features** - Bootstrap confidence intervals, signs, visualization
4. **Real-world examples** - Practical applications with diamonds dataset
5. **Best practices** - Guidelines for proper use and interpretation
6. **Troubleshooting** - Common issues and solutions

## Building Vignettes Locally

If you're developing the package, you can build vignettes locally:

```r
# Build vignettes during package installation
devtools::install(build_vignettes = TRUE)

# Or build vignettes separately
devtools::build_vignettes()
```

Note: Building vignettes requires the suggested packages (`knitr`, `rmarkdown`) to be installed.
