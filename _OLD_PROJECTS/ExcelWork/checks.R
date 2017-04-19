file.check <- 'E:/USER/Documents/WORKSPACE/R/PRIVATES/SalaryExcel/Data/考勤/2016.1财富二/2016-天元港1月份考勤(完整版）(2).xlsx'

wb.check <- loadWorkbook(file.check)

wb.check.sheets <- getSheets(wb.check)

wb.sheet.2Check.df <- readWorksheet(wb.check, '李总直管', header = FALSE)
