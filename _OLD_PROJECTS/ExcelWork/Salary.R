rm(list = ls())

library(XLConnect)

source('FUN.R', encoding = 'UTF-8')

# files <- dir()
file <- './Files/异动/1月异动.xls'

wb <- loadWorkbook(file)

wb.sheets <- getSheets(wb)

sheet.2 <- c('投二考勤架构', '投二异动')


ws.2Structure <- readWorksheet(wb, sheet.2[1], header = FALSE)