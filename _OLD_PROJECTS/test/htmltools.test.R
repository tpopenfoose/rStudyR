library(htmltools)
library(data.table)
library(htmlTable)
library(magrittr)
tab <- data.table(X = 1:3, Y = 4:6, Z = c('a', 'b', 'c$'))
tab %>% htmlTable %>% str


file.create('./abc.html')

text <- tagList(tags$html('abc'), tags$h1('efg'))

writeLines(as.character(text), file('./abc.html'))