#' Write the AHI input data to an Excel workbook.
#'
#' @param input_data The input avalanche record. This format must comply to the DHA avalanche database PathNames in order for the function to work.
#' @param n_frequency The minimum number of events required to include the avalanche path as a 'Major' avalanche path. Default is 4. Value will be ignored if 'major_paths' argument is specified.
#' @param major_paths Optional vector of path names, if you want to look at a specific list of paths.
#' @param WADT Traffic volume?
#' @param percent_cars percentage of traffic volume that is cars. percentage of trucks calcualted automatically based on this value (1-percent_cars)
#' @param length_car_m average car length
#' @param length_truck_m average truck length
#' @param Lave_light Adjustment estimate for average length on road for light events. Default 0.3 from Schaerer, 1989
#' @param Lave_deep Adjustment estimate for average length on road for deep events. Default 0.7 from Schaerer, 1989
#' @param Lave_plunging Adjustment estimate for average length on road for plunging events. Default 0.5 from Schaerer, 1989
#' @param reaction_time Driver's time to react in seconds. Used to calculate stopping distance. Default is 2.5.
#' @param missing_seasons Optional number of seasons missing from record. Used to calculate Record Length for return intervals.
#' @param speed_limit Speed limit in km/h. Default 48 km/h
#' @param road_grade Grade of road in percentage as a decimal. Negative values indicate downhill slope. Default is -0.1.
#' @param coeff_friction the coefficient of friction between the tires and the road. It is typically assumed to be equal to 0.7 on a dry road and in the range from 0.3 to 0.4 on a wet road. Default is 0.2.
#' @param Q_cars_powder Q factor for cars for powder events. Default = 0
#' @param Q_cars_light Q factor for cars for light events. Default = 3
#' @param Q_cars_deep Q factor for cars for deep events. Default = 10
#' @param Q_cars_plunging Q factor for cars for plunging events. Default = 12
#' @param Q_trucks_powder Q factor for trucks for powder events. Default = 0
#' @param Q_trucks_light Q factor for trucks for light events. Default = 3
#' @param Q_trucks_deep Q factor for trucks for deep events. Default = 10
#' @param Q_trucks_plunging Q factor for trucks for plunging events. Default = 12
#' @param powder_RI Return Interval to use for powder events if no events occur in the record. Default is 100.
#' @param light_RI Return Interval to use for light events if no events occur in the record. Default is 100.
#' @param deep_RI Return Interval to use for deep events if no events occur in the record. Default is 100.
#' @param plunging_RI Return Interval to use for deep events if no events occur in the record. Default is 1000.
#' @param path_info Optional data.frame with columns "path_count", "PathName",'length', "Jminus", "Jplus", and "missing_seasons", defining the road length of each path and space between previous and next path, in meters, and the number of seasons missing from the entire record for each path.
#' @param outputPath Desired path to save Excel Workbook
#' @param Ps Probability of a second avalanche running in path J with waiting traffic. Default 0.05 after Schaere, 1989.
#' @param Ps_prime Probability of an avalanche releasing in path adjacent to J with waiting traffic. Default 0.3 after Schaerer, 1989.
#' @param wait_time Time in hours for waiting traffic. Default = 1.

#' @return This will save the AHI data to a workbook that is similar to the format used by Hamre et al.
#'
#'
#' @name Write_AHI_template
#' @export


Write_AHI_template <- function(input_data, n_frequency = 1, major_paths = NULL, WADT = 7000,
                             percent_cars = 0.98, length_car_m = 15, length_truck_m = 30,
                             reaction_time = 2.5, speed_limit = 48, road_grade = -0.1,
                             coeff_friction = 0.2, Q_cars_powder = 0, Q_trucks_powder = 0,
                             Q_cars_light = 3, Q_trucks_light = 3, Q_cars_deep = 10,
                             Q_trucks_deep = 10, Q_cars_plunging = 12, Q_trucks_plunging = 12,
                             powder_RI = 100, light_RI = 100, deep_RI = 100, plunging_RI = 1000,
                             path_info = NULL, outputPath, wait_time = 1, Ps = 0.05, Ps_prime = 0.3, Lave_light = 0.3,
                             Lave_deep = 0.7, Lave_plunging = 0.5, missing_seasons = 0) {
  library(openxlsx)
  library(avydata)


  AHI_sum <- AHI_template(input_data = input_data,
                            n_frequency = n_frequency,
                            major_paths = major_paths,
                            WADT = WADT,
                            percent_cars = percent_cars,
                            length_car_m = length_car_m,
                            length_truck_m = length_truck_m,
                            reaction_time = reaction_time,
                            speed_limit = speed_limit,
                            road_grade = road_grade,
                            coeff_friction = coeff_friction,
                            Q_cars_powder = Q_cars_powder,
                            Q_trucks_powder = Q_trucks_powder,
                            Q_cars_light = Q_cars_light,
                            Q_trucks_light = Q_trucks_light,
                            Q_cars_deep = Q_cars_deep,
                            Q_trucks_deep = Q_trucks_deep,
                            Q_cars_plunging = Q_cars_plunging,
                            Q_trucks_plunging = Q_trucks_plunging,
                            powder_RI = powder_RI,
                            light_RI = light_RI,
                            deep_RI = deep_RI,
                            plunging_RI = plunging_RI,
                            path_info = path_info,
                            wait_time = wait_time,
                            missing_seasons = missing_seasons)
  AHI_tables <- list("Powder" = AHI_sum$AHI_moving_powder,
                      "Light" = AHI_sum$AHI_moving_light,
                      "Deep" = AHI_sum$AHI_moving_deep,
                      "Plunging" = AHI_sum$AHI_moving_plunging)

  sheet_names <- c("Powder", "Light", "Deep", "Plunging")

  #Create workbook
  wb <- createWorkbook()

  #write sheet for AHI totals
  addWorksheet(wb, sheetName = "AHI Totals")
  writeData(wb, sheet = "AHI Totals", AHI_sum$Overall_AHI)

  # write sheet for Input Parameters
  addWorksheet(wb, sheetName = "Input Parameters")
  writeData(wb, sheet = "Input Parameters", AHI_sum$input_params)

  # Write sheet for plots
  plot_path <- "plot.png"
  ggsave(plot_path, plot = AHI_sum$plots, width = 9, height = 8, dpi = 300)
  addWorksheet(wb, "AHI Plots")
  insertImage(wb, "AHI Plots", plot_path, startRow = 2, startCol = 2, width = 9, height = 8)

  #Write Sheet for Overall Hazard
  addWorksheet(wb, sheetName = "Overall Hazard")
  writeData(wb, sheet = "Overall Hazard", AHI_sum$Overall_Hazard)

  #Write sheets for moving AHI values
  for (i in seq_along(AHI_tables)) {
    sheet_name <- sheet_names[i]
    addWorksheet(wb, sheetName = sheet_name)
    writeData(wb, sheet = sheet_name, AHI_tables[[i]])
  }

  # Write sheet for waiting AHI
  addWorksheet(wb, sheetName = "Waiting Traffic")
  writeData(wb, sheet = "Waiting Traffic", AHI_sum$AHI_waiting)

  # Write sheet for Probability of second impact
  addWorksheet(wb, sheetName = "Probability Waiting")
  writeData(wb, sheet = "Probability Waiting", AHI_sum$Probability_waiting)

  # Write sheet for Probability Matrix
  addWorksheet(wb, sheetName = "Probability Second Event")
  writeData(wb, sheet = "Probability Second Event", AHI_sum$Probability_event)

  # Write sheet for Calculated Parameters
  addWorksheet(wb, sheetName = "Calculated Parameters")
  writeData(wb, sheet = "Calculated Parameters", AHI_sum$calc_params)

  # Write the Summary data
  addWorksheet(wb, sheetName = "Summary")
  summary_data <- AHI_sum$Summary
  writeData(wb, sheet = "Summary", summary_data)

  # Write the avalanche records
  addWorksheet(wb, sheetName = "Events")
  road_hits <- AHI_sum$hits
  writeData(wb, sheet = "Events", road_hits)
  saveWorkbook(wb, file = outputPath, overwrite = TRUE)

  file.remove(plot_path)
}

# Example usage:
# Write_AHI_template(input_data, n_frequency = 4, major_paths = NULL, outputPath = "output.xlsx")

