# dataplotr

This package mainly contains wrapper functions for ggplot2.

## Installation
```
devtools::install_github("aotearoastats/dataplotr")
```

## plot_barplot
Plot_barplot plots simple barplots with the option to add a grid.

```
library(tidyverse)
iris.plot <- iris %>% 
      group_by(Species) %>% 
      summarize(Sepal.Length=mean(Sepal.Length))

plot_barplot(iris.plot,
             x_var = "Species",
             y_var = "Sepal.Length")
```
