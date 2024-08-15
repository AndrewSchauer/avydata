#' Write the AHI input data to an Excel workbook.
#'
#' @param input_data The input avalanche record. This format must comply to the DHA avalanche database standards in order for the function to work.
#' @param n_frequency The minimum number of events required to include the avalanche path as a 'Major' avalanche path. Default is 4. Value will be ignored if 'major_paths' argument is specified.
#' @param major_paths Optional vector of path names, if you want to look at a specific list of paths.
#' @param outputPath Desired path to save Excel Workbook
#'
#' @return This will save the AHI data to a workbook that is similar to the format used by Hamre et al.
#'
#'
#' @name Write_AHI_inputs
#' @export


Write_AHI_inputs <- function(input_data, n_frequency = 4, major_paths = NULL, outputPath) {
  library(openxlsx)
  library(avydata)

  # Custom function to add hyperlinks to PathName column
  add_hyperlinks <- function(wb, sheet, df) {
    for (i in 1:nrow(df)) {
      sheet_name <- df$PathName[i]
      if (!is.na(sheet_name) && sheet_name != "") {
        writeFormula(wb, sheet, x = sprintf('HYPERLINK("#\'%s\'!A1", "%s")', sheet_name, sheet_name), startCol = 1, startRow = i + 1)
      }
    }
  }

  AHI_sum <- AHI_summarizer(input_data = input_data, n_frequency = n_frequency, major_paths = major_paths)
  path_tables <- AHI_sum$PathTables

  wb <- createWorkbook()
  addWorksheet(wb, sheetName = "Summary")

  # Write the Summary data
  summary_data <- AHI_sum$Summary
  writeData(wb, sheet = "Summary", summary_data)

  # Add hyperlinks to PathName column
  add_hyperlinks(wb, "Summary", summary_data)

  for (i in seq_along(path_tables)) {
    sheet_name <- path_tables[[i]]$Name
    addWorksheet(wb, sheetName = sheet_name)
    writeData(wb, sheet = sheet_name, path_tables[[i]]$Table)
  }

  saveWorkbook(wb, file = outputPath, overwrite = TRUE)
}

# Example usage:
# Write_AHI_inputs(input_data, n_frequency = 4, major_paths = NULL, outputPath = "output.xlsx")

