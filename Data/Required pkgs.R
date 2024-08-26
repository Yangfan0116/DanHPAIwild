list.of.packages <- c("raster", "dplyr", "sf", "tmap", "sp", "ggplot2", "tibble", "viridis", "stringr", "gplots", "tidyverse", "plotly", "ggthemes", "RColorBrewer", "readxl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)