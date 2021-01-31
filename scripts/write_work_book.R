
# stlye -------------------------------------------------------------------

headerStyle <- createStyle(fontSize = 12, 
                           fontColour = "#FFFFFF",
                           halign = "center",
                           valign = "center",
                           fontName = "Arial Narrow",
                           textDecoration = "bold",
                           fgFill = "#ee5859",
                           border = "TopBottomLeftRight ",
                           borderColour = "#fafafa",
                           wrapText = T,numFmt = "DATE"
)
bodyStyle <- createStyle(fontSize = 11, 
                         fontName = "Arial Narrow",
                         border = "TopBottomLeftRight ",
                         borderColour = "#4F81BD",
                         valign = "center",
                         halign = "left",
)

sty_date_time <- createStyle(numFmt = "dd/mm/yyyy hh:mm",
                   fontSize = 11, 
                   fontName = "Arial Narrow",
                   border = "TopBottomLeftRight ",
                   borderColour = "#4F81BD",
                   valign = "center",
                   halign = "left")
sty_date <- createStyle(numFmt = "dd/mm/yyyy",
                             fontSize = 11, 
                             fontName = "Arial Narrow",
                             border = "TopBottomLeftRight ",
                             borderColour = "#4F81BD",
                             valign = "center",
                             halign = "left")


# work book ---------------------------------------------------------------

wb <- createWorkbook("Hydro-incidentdataset")

addWorksheet(wb, sheetName = "Hydro-Incident DB (daily)", gridLines = FALSE)
addWorksheet(wb, sheetName = "Weather Gauges (hourly)", gridLines = FALSE)
addWorksheet(wb, sheetName = "Rain Gauges (sub-hourly)", gridLines = FALSE)
addWorksheet(wb, sheetName = "Raw Incident Records", gridLines = FALSE)

writeData(wb, sheet = 1, full_data2, rowNames = F)
writeData(wb, sheet = 2, hourly_summary, rowNames = F)
writeData(wb, sheet = 3, precipitation_interval_and_rendered, rowNames = F)
writeData(wb, sheet = 4, final_for_report_raw_incident, rowNames = F)

addFilter(wb,sheet =  1, row = 1, cols = 1:ncol(full_data2))
addFilter(wb,sheet =  2, row = 1, cols = 1:ncol(hourly_summary))
addFilter(wb,sheet =  3, row = 1, cols = 1:ncol(precipitation_interval_and_rendered))
addFilter(wb,sheet =  4, row = 1, cols = 1:ncol(final_for_report_raw_incident))

  
freezePane(wb, sheet = 1, firstCol = TRUE, firstRow = T)
freezePane(wb, sheet = 2, firstCol = TRUE, firstRow = T)
freezePane(wb, sheet = 3, firstCol = TRUE, firstRow = T)
freezePane(wb, sheet = 4, firstCol = TRUE, firstRow = T)

addStyle(wb, sheet = 1, headerStyle, rows = 1, cols = 1:ncol(full_data2), gridExpand = TRUE)
addStyle(wb, sheet = 1, bodyStyle, rows = 1:nrow(full_data2)+1, cols = 1:ncol(full_data2), gridExpand = TRUE)

addStyle(wb, sheet = 2, headerStyle, rows = 1, cols = 1:ncol(hourly_summary), gridExpand = TRUE)
addStyle(wb, sheet = 2, bodyStyle, rows = 1:nrow(hourly_summary)+1, cols = 1:ncol(hourly_summary), gridExpand = TRUE)

addStyle(wb, sheet = 3, headerStyle, rows = 1, cols = 1:ncol(precipitation_interval_and_rendered), gridExpand = TRUE)
addStyle(wb, sheet = 3, bodyStyle, rows = 1:nrow(precipitation_interval_and_rendered)+1, cols = 1:ncol(precipitation_interval_and_rendered), gridExpand = TRUE)

addStyle(wb, sheet = 4, headerStyle, rows = 1, cols = 1:ncol(final_for_report_raw_incident), gridExpand = TRUE)
addStyle(wb, sheet = 4, bodyStyle, rows = 1:nrow(final_for_report_raw_incident)+1, cols = 1:ncol(final_for_report_raw_incident), gridExpand = TRUE)


setColWidths(wb, 1, cols = 1:ncol(full_data2), widths = 25) 
setColWidths(wb, 2, cols = 1:ncol(hourly_summary), widths = 25) 
setColWidths(wb, 3, cols = 1:ncol(precipitation_interval_and_rendered), widths = 25) 
setColWidths(wb, 4, cols = 1:ncol(final_for_report_raw_incident), widths = 25) 


# fix_date_formatting -----------------------------------------------------------

addStyle(wb, 1, style = sty_date, rows = 1:nrow(full_data2)+1, cols = 1:1, gridExpand = F)
addStyle(wb, 2, style = sty_date_time, rows = 1:nrow(hourly_summary)+1, cols = 1:1, gridExpand = F)
addStyle(wb, 3, style = sty_date_time, rows = 1:nrow(precipitation_interval_and_rendered)+1, cols = 1:1, gridExpand = F)
addStyle(wb, 4, style = sty_date, rows = 1:nrow(final_for_report_raw_incident)+1, cols = 1:1, gridExpand = F)






# read styleing  ----------------------------------------------------------

# wb2 <- loadWorkbook(file = "outputs/compile_dataset/format.xlsx")
# style_1 <- getStyles(wb2)[1]
# data<- read.xlsx("outputs/formatting/other_tab_info.xlsx",sheet = "Readme")
# 
# writeData(wb, sheet = 5 , data, rowNames = F,borderStyle =  style_1)
# 
# saveWorkbook(wb2, file = paste0("outputs/compile_dataset/",str_replace_all(Sys.Date(),"-",""),"_","hydromatrdgical_dataset",".xlsx"), overwrite = TRUE)

###############################################################

saveWorkbook(wb, file = paste0("outputs/compile_dataset/",str_replace_all(Sys.Date(),"-",""),"_","hydrometeological_dataset",".xlsx"), overwrite = TRUE)
  
  