# autobin: Discretization for Binary Classification 

`autobin` will help you to discretize a continuous variable with binary target 
variable. It searches cut point which maximize the current information value in 
each step, while performing a fisher test on the parts divided, to guarantee 
the cut point is essentially significant.

## Installation

First, please ensure the package `devtools` installed on your R system. If not, 
install it with the following statement.

```
install.packages('devtools')
```

Then you can install this package by

```
devtools::install_github('siyuany/autobin')
```

## Examples