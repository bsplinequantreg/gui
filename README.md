# BsplineQuantRegGui

[![CRAN status](https://www.r-pkg.org/badges/version/BsplineQuantRegGui)](https://cran.r-project.org/package=BsplineQuantRegGui)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/BsplineQuantRegGui)](https://cran.r-project.org/package=BsplineQuantRegGui)
[![R](https://img.shields.io/badge/R>=4.6-blue.svg)](https://cran.r-project.org/)
[![License: GPL-3](https://img.shields.io/badge/License-GPL--3-blue.svg)]

## Overview
BsplineQuantRegGui is an interactive Shiny interface for the BsplineQuantReg package, providing a user-friendly way to perform quantile regression using B-splines with shape constraints.

The package is available on CRAN. 

The underlying BsplineQuantReg package provides the core regression methods for B-spline quantile regression with Karlin-Studden polynomial sign characterization, including degrees 1 to 4. It provides a small set of robust functions in pure R for polynomial calculation, piecewise polynomial and B-spline manipulation, including differentiation, B-spline coefficients calculations and transformation to PP-form.

## Features

This GUI application makes it easy to:

- Load and visualize data (from multiple data sources: custom function, CSV files)
- Interactive plotting: zoom, pan, and add knots by clicking on the plot
- Configure B-spline parameters (degree 1 to 4)
- Plot multiple curves with color management
- Apply shape constraints (monotonicity, convexity, third derivative)
- Manage constraints per region interactively
- Run quantile regression with various solvers
- Export reproducible R code
- Run demo presets directly from the interface (according to degree)
- Type of regression : quantile or mean-square (for 'BsplineQuantReg' version >=0.2.3) 

## Installation

### From CRAN
```
install.packages("BsplineQuantRegGui")
```

### From GitHub
```R
install.packages("remotes")
remotes::install_github("alexandreabbes/BsplineQuantRegGui")
```

## Quick Start
```R
library(BsplineQuantRegGui)
# Launch the application
runGui()
```

The GUI will open in your default web browser.

## Docker Deployment

A Docker image is available for easy deployment:
```bash
docker pull ghcr.io/alexandreabbes/bsplinequantreggui:latest
docker run -p 3838:3838 ghcr.io/alexandreabbes/bsplinequantreggui:latest
```

Then open http://localhost:3838 in your browser.

## Dependencies

This package depends on:

| Package | Purpose |
|---------|---------|
| BsplineQuantReg (>= 0.2.2) | Core regression functions |
| shiny | Interactive web framework |
| plotly | Interactive graphics |
| DT | Interactive tables |
| shinythemes | UI themes |
| shinyjs | Enhanced JavaScript capabilities |
| colourpicker | Color selection widget |
| ECOSolveR| A good solver that causes no problem to compile |


## Usage Guide

### 1. Data Selection
- Click Test to generate sample data
- Click Temp to load the global temperature dataset (1880-1992)
- Click CSV to import your own data file
- Use the Custom function field to define your own data generator

### 2. Spline Configuration
- Select Degree (1 to 4)
- Adjust Auto knots count
- Click Add knot to enter manual knot placement mode, then click on the plot
- Click Clear knots to reset to automatic knots

### 3. Constraints
- Choose Uniform constraints to apply the same constraint everywhere
- Choose Per region to define different constraints in specific regions
- Click Select to enter region selection mode
- Drag a rectangle on the plot to define the region
- Adjust monotonicity, convexity, and third derivative constraints
- Click Add region to apply the constraint
- View/remove all active region constraints in the Regions tab

### 4. Run Analysis
- Click Run to execute the regression with your chosen solver and type of regression ('BsplineQuantReg' >= '0.2.3')
- View results: the curve and the Information. 
- View Coefficients on the B-spline Basis and on polynomial basis (local/canonical)
- View verbose output in the Console tab if verbose = TRUE is selected
- "Data" tab: Summary and table of the data
- R Code: Generated code to reproduce the analysis

### 5. Run Demos
Click on any demo to run it. The degree of the spline is that selected in the GUI.
You see the result in the "demo" tab.

## Citation

citation("BsplineQuantRegGui")

If you use this package in your research, please cite:

@Article{Abbes2025,
  author  = {Alexandre Abbes},
  title   = {Quantile Regression with Cubic Polynomial Splines under Shape Constraints with Applications},
  year    = {2025},
  doi     = {10.5281/zenodo.17427913}
}

@Manual{Abbes2026R,
  author  = {Alexandre Abbes},
  title   = {BsplineQuantReg: Quantile Regression with B-Splines of Degrees 1 to 4},
  year    = {2026},
  note    = {R package version 0.2.0},
  url     = {https://cran.r-project.org/package=BsplineQuantReg}
}

## License

GPL-3 (c) Alexandre Abbes (2026)

## References

- Abbes, A. (2025). Quantile Regression with Cubic Polynomial Splines under Shape Constraints with Applications. Zenodo. doi:10.5281/zenodo.17427913
- Abbes, A. (2026). BsplineQuantReg: R Implementation of B-Spline Quantile Regression. https://cran.r-project.org/package=BsplineQuantReg
- Abbes, A. (2026). BsplineQuantRegpy: Python Implementation of B-Spline Quantile Regression. https://pypi.org/project/BsplineQuantRegpy/
