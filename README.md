
# messy.cats

<!-- badges: start -->
<!-- badges: end -->

The goal of messy.cats is to make cleaning messy categorical easier. User inputted character categorical data often suffers from messiness than can be complicated and time consuming to clean up. When inputting data, users often make typos or formatting errors than cause stubborn, hard to detect issues in their data. By leveraging string distance measurement tools, messy.cats allows users to automate many of the steps involved with cleaning categorical data. This enables users to spend less time fiddling around with inconsistent categorical data with less effort. 

## Installation

You can install the released version of messy.cats from [GITHUB](https://github.com) with:

``` r
if(!require(devtools)){
 install.packages("devtools")
}
devtools::install_github("hkarp1/messy.cats")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(messy.cats)
plant_categories = c("tree", "bush", "herb", "grass")
messy_plant_categories = c("green tree", "red bush", "new herb", "old grass", "young tree", "small bush", "20 herbs", "the grass", "a tree", 
"bushes", "herbs", "tall grass")

cat_match(plant_categories, messy_plant_categories)
```

