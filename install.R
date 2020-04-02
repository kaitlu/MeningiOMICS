packages <- c("ggplot2", 
              "dplyr", 
              "car", 
              "shiny", 
              "stringr", 
              "plotly", 
              "shinythemes", 
              "DT", 
              "heatmaply")

for (i in packages) {
    if (!requireNamespace(i, quietly = TRUE))
    install.packages(i)
}
