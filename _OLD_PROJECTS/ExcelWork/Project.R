rm(list = ls())

library(XLConnect)
# 
# Path.Data <- './Data'
# 
# dir(Path.Data)
# file <- 'C:/Users/Accretion/Documents/R_WorkSpace/SalaryExcel/Data/异动/1月异动.xls'
file <- 'E:/USER/Documents/WORKSPACE/R/PRIVATES/SalaryExcel/Data/异动/1月异动.xls'

wb <- loadWorkbook(file)

wb.sheet.2Structure

# writeWorksheet()