# RiverLoad
Load Estimation of River Compounds with Different Methods<br><br>
The `'RiverLoad'` package implements several of the most popular load estimation procedures, including averaging methods, ratio estimators and regression methods. The package provides an easy-to-use tool to rapidly calculate the load for various compounds and to compare different methods. The package also supplies additional functions to easily organize and analyze the data.<br><br>

## Installation
You can install a stable version of RiverLoad with:<br><br>
```r
install.packages("RiverLoad")
```

## Usage
First the package must be loaded into R:
```r
library(RiverLoad)
```

You can view a list of functions available in rLakeAnalyzer with:
```r
help(package = "RiverLoad")
```

An example of application is reported below:

```r
# Merge the flow records with the concentration data
union1 <- db.union(flow.data1, conc.data1)

#Estimate the load with the first 7 methods
met1 <- method1(union1, 2)
met2 <- method2(union1, 2)
met3 <- method3(union1, 2)
met4 <- method4(union1, 2)
met5 <- method5(union1, 2)
met6 <- method6(union1, 2)
met7 <- beale.ratio(union1, 2)
```

## Citation
Veronica Nava, Martina Patelli, Marco Rotiroti, Barbara Leoni (2019).
  An R package for estimating river compound load using different
  methods. Environmental Modelling and Software, SUBMITTED.


## Contributors
Veronica Nava, Barbara Leoni, Martina Patelli, Marco Rotiroti
