#' Create the AHI template based on the excel workbook used by Hamre et al.
#'
#' @param coeff_friction The coefficient of friction between the tires and the road. It is typically assumed to be equal to 0.7 on a dry road and in the range from 0.3 to 0.4 on a wet road. Default is 0.2.
#' @param deep_RI Return Interval to use for deep events if no events occur in the record. Default is 100.
#' @param input_data The input avalanche record. This format must comply to the DHA avalanche database PathNames in order for the function to work.
#' @param length_car_m Average car length.
#' @param length_truck_m Average truck length.
#' @param light_RI Return Interval to use for light events if no events occur in the record. Default is 100.
#' @param Lave_light Adjustment estimate for average length on road for light events. Default 0.3 from Schaerer, 1989
#' @param Lave_deep Adjustment estimate for average length on road for deep events. Default 0.7 from Schaerer, 1989
#' @param Lave_plunging Adjustment estimate for average length on road for plunging events. Default 0.5 from Schaerer, 1989
#' @param major_paths Optional vector of path names, if you want to look at a specific list of paths.
#' @param missing_seasons Optional number of seasons missing from record. Used to calculate Record Length for return intervals.
#' @param n_frequency The minimum number of events required to include the avalanche path as a 'Major' avalanche path. Default is 4. Value will be ignored if 'major_paths' argument is specified.
#' @param path_info Path to optional .csv with columns "path_count", "PathName",'length', "Jminus", "Jplus", and "missing_info" defining the road length of each path and space between previous and next path, in meters, and the number of seasons missing from the entire record for each path..
#' @param percent_cars Percentage of traffic volume that is cars. Percentage of trucks calculated automatically based on this value (1-percent_cars).
#' @param plunging_RI Return Interval to use for deep events if no events occur in the record. Default is 1000.
#' @param powder_RI Return Interval to use for powder events if no events occur in the record. Default is 100.
#' @param Ps Probability of a second avalanche running in path J with waiting traffic. Default 0.05 after Schaerer, 1989.
#' @param Ps_prime Probability of an avalanche releasing in path adjacent to J with waiting traffic. Default 0.15 after Schaerer, 1989.
#' @param Q_cars_deep Q factor for cars for deep events. Default = 10.
#' @param Q_cars_light Q factor for cars for light events. Default = 3.
#' @param Q_cars_plunging Q factor for cars for plunging events. Default = 12.
#' @param Q_cars_powder Q factor for cars for powder events. Default = 0.
#' @param Q_trucks_deep Q factor for trucks for deep events. Default = 10.
#' @param Q_trucks_light Q factor for trucks for light events. Default = 3.
#' @param Q_trucks_plunging Q factor for trucks for plunging events. Default = 12.
#' @param Q_trucks_powder Q factor for trucks for powder events. Default = 0.
#' @param reaction_time Driver's time to react in seconds. Used to calculate stopping distance. Default is 2.5.
#' @param road_grade Grade of road in percentage as a decimal. Negative values indicate downhill slope. Default is -0.1.
#' @param speed_limit Speed limit in km/h. Default 48 km/h.
#' @param WADT Traffic volume?
#' @param wait_time Time in hours for waiting traffic. Default = 1.
#'
#' @return The tables used in the AHI template
#'
#' \item{MajorPaths}{A character vector of the major paths.}
#'
#' @name AHI_template
#' @export

AHI_template <- function (input_data, n_frequency = 1, major_paths = NULL, WADT = 7000,
          percent_cars = 0.98, length_car_m = 15, length_truck_m = 30,
          reaction_time = 2.5, speed_limit = 48, road_grade = -0.1,
          coeff_friction = 0.2, Q_cars_powder = 0, Q_trucks_powder = 0,
          Q_cars_light = 3, Q_trucks_light = 3, Q_cars_deep = 10,
          Q_trucks_deep = 10, Q_cars_plunging = 12, Q_trucks_plunging = 12,
          powder_RI = 100, light_RI = 100, deep_RI = 100, plunging_RI = 1000,
          path_info = NULL, wait_time = 1, Ps = 0.05, Ps_prime = 0.15, Lave_light = 0.3,
          Lave_deep = 0.7, Lave_plunging = 0.5, missing_seasons = 0)
{
  library(dplyr)
  library(tidyr)
  library(reshape2)
  library(ggplot2)

  if(!is.null(path_info)){
    path_info <- read.csv(path_info, header = TRUE)
    major_paths <- path_info$PathName
    #MajorPaths <- input_data %>%
    #  filter((RoadLength >= 1 | RoadDepth >= 1) |
    #           (RoadHit == TRUE | RoadHit == 1)) %>%
    #  filter(PathName %in% major_paths, !is.na(PathName), PathName != "") %>%
    #  mutate(PathName = factor(PathName, levels = major_paths)) %>%
    #  group_by(PathName) %>%
    #  summarise(count = n()) %>%
    #  filter(count >= 1) %>%
    #  mutate(PathName = as.character(PathName)) %>%
    #  pull(PathName)
    MajorPaths <- major_paths


  }
  else{
  if (is.null(major_paths) &
      is.null(path_info) ) {
    PathCounts <- data.frame(table(input_data$PathName))
    RO <- which(PathCounts$Freq >= n_frequency)
    MajorPaths <- PathCounts[RO, "Var1"]
    MajorPaths <- input_data %>% filter(PathName %in% MajorPaths,
                                        !is.na(PathName), PathName != "") %>% group_by(PathName) %>%
      summarise(count = n()) %>% filter(count >= n_frequency) %>%
      pull(PathName)
  }
  else {
    MajorPaths <- input_data %>%
      filter((RoadLength >= 1 | RoadDepth >= 1) |
               (RoadHit == TRUE | RoadHit == 1)) %>%
      filter(PathName %in% major_paths, !is.na(PathName), PathName != "") %>%
      mutate(PathName = factor(PathName, levels = major_paths)) %>%
      group_by(PathName) %>%
      summarise(count = n()) %>%
      filter(count >= 1) %>%
      mutate(PathName = as.character(PathName)) %>%
      pull(PathName)
  }
  }
  if (length(MajorPaths) == 0) {
    stop("No major paths found.")
  }


  stopping_dist = (0.278 * reaction_time * speed_limit) +
    ((speed_limit^2)/(254 * (coeff_friction + road_grade)))
  percent_trucks = 1 - percent_cars
  n_cars = round(percent_cars * WADT, 0)
  n_trucks = round(percent_trucks * WADT, 0)
  Lw <- ((WADT*length_car_m*wait_time/24/2)*percent_cars) + ((WADT*length_truck_m*wait_time/24/2)*percent_trucks)


  WorkingPaths <- which(input_data$PathName %in% MajorPaths)
  WorkingData <- input_data[WorkingPaths, ]
  WorkingData[, "xSection_area"] <- WorkingData[, "RoadDepth"] *
    WorkingData[, "RoadLength"]
  WorkingData$PathName[WorkingData$PathName == ""] <- "Not Recorded"
  MajorPaths[MajorPaths == ""] <- "Not Recorded"
  date_separated <- separate(WorkingData, col = Date, into = c("Month",
                                                               "Day", "Year"), sep = "/")
  WorkingData[, "Month"] <- as.numeric(date_separated[, "Month"])
  WorkingData[, "Day"] <- as.numeric(date_separated[, "Day"])
  WorkingData[, "Year"] <- as.numeric(date_separated[, "Year"])

  for(ob in 1:nrow(WorkingData)){
    WorkingData[ob, "Season"] <- ifelse(WorkingData[ob, "Month"] %in% c(1:6),
                                        WorkingData[ob, "Year"],
                                        WorkingData[ob, "Year"] + 1)

  }

  storage <- data.frame(PathName = character(), n_events = integer(),
                        n_naturals = integer(), n_artificial = integer(), firstdate = integer(),
                        lastdate = integer(), recordlength = integer(), n_hits = integer(),
                        n_roadopen = integer(), mitigation_effectiveness = numeric(),
                        residual_risk = numeric(), open_naturals = integer(),
                        closed_naturals = integer(), n_calc_events = integer(), n_powder = integer(),
                        n_light = integer(), n_deep = integer(), n_plunging = integer(),
                        frequency = numeric(), powder_frequency = numeric(), light_frequency = numeric(),
                        deep_frequency = numeric(), plunging_frequency = numeric(),
                        light_percentage = numeric(), deep_percentage = numeric(),
                        plunging_percentage = numeric(), ave_length_powder_ft = numeric(), ave_length_light_ft = numeric(),
                        ave_length_deep_ft = numeric(), ave_length_plunging_ft = numeric(), ave_length_powder_m = numeric(),
                        ave_length_light_m = numeric(), ave_length_deep_m = numeric(),
                        ave_length_plunging_m = numeric(), stringsAsFactors = FALSE)
  powder <- data.frame(path_count = character(), PathName = character(),
                       AnnualFreq = numeric(), ReturnPeriod = numeric(), AveLength_m = numeric(),
                       Lmax_m = numeric(), Encounter_car = numeric(), Encounter_truck = numeric(),
                       Encounter_total = numeric(), Q_cars = numeric(), Q_trucks = numeric(),
                       Moving_AHI = numeric(), Car_Ratio = numeric(), Truck_Ratio = numeric())
  light <- deep <- plunging <- powder

  waiting <- data.frame(path_count = numeric(),
                        PathName = character(),
                        j_minus = numeric(),
                        j_plus = numeric(),
                        Lmax = numeric(),
                        light_return = numeric(),
                        ave_cars_light = numeric(),
                        deep_return = numeric(),
                        ave_cars_deep = numeric())

  station_list <- list()
  road_hits <- list()
  for (ob in 1:length(MajorPaths)) {
    events <- which(WorkingData$PathName == MajorPaths[ob] &
                      (WorkingData[, "RoadDepth"] > 0 |
                         WorkingData[, "RoadHit"] %in% c("TRUE", 1) |
                         WorkingData[, "RoadLength"] > 0 |
                         WorkingData[, "Size"] >= 4))

    missing_seasons <- ifelse(!is.na(path_info[ob, "missing_seasons"]),
                              path_info[ob, "missing_seasons"],
                              missing_seasons)
    powder_events <- which(WorkingData[events, "AirBlast"] ==
                             "J" & WorkingData[events, "RoadDepth"] == 0)
    light_events <- which(WorkingData[events, "RoadDepth"] <=
                            3)
    deep_events <- which(WorkingData[events, "RoadDepth"] >
                           3 & WorkingData[events, "RoadDepth"] < 20)
    plunging_events <- which(WorkingData[events, "RoadDepth"] >=
                               20)
    full_record <- which(WorkingData$PathName == MajorPaths[ob])
    if (length(events) == 0 || length(full_record) == 0){
      storage[ob, "PathName"] <- MajorPaths[ob]
      storage[ob, c("n_events",
                    "n_naturals",
                    "n_artificial",
                    "n_hits",
                    "n_roadopen",
                    "mitigation_effectiveness",
                    "residual_risk",
                    "open_naturals",
                    "closed_naturals",
                    "n_powder",
                    "n_light",
                    "n_deep",
                    "n_plunging",
                    "frequency",
                    "powder_frequency",
                    "light_frequency",
                    "deep_frequency",
                    "plunging_frequency",
                    "n_calc_events",
                    "light_percentage",
                    "deep_percentage",
                    "plunging_percentage")] <- 0
      full_record <- which(WorkingData$PathName == MajorPaths[ob])
      storage[ob, "firstdate"] <- min(WorkingData[full_record,
                                                  "Season"], na.rm = TRUE)
      storage[ob, "lastdate"] <- max(WorkingData[full_record,
                                                 "Season"], na.rm = TRUE)
      full_length <- (max(WorkingData[, "Season"], na.rm = TRUE) - min(WorkingData[, "Season"], na.rm = TRUE))
      storage[ob, "recordlength"] <- full_length - missing_seasons + 1
      storage[ob, "ave_length_powder_ft"] <- 1/.3048
      storage[ob, "ave_length_light_ft"] <- ifelse(!is.null(path_info), Lave_light*path_info[ob, "length"]/.3048, 1/.3048)
      storage[ob, "ave_length_deep_ft"] <- ifelse(!is.null(path_info), Lave_deep*path_info[ob, "length"]/.3048, 1/.3048)
      storage[ob, "ave_length_plunging_ft"] <- ifelse(!is.null(path_info), Lave_plunging*path_info[ob, "length"]/.3048, 1/.3048)
      storage[ob, "ave_length_powder_m"] <- round(storage[ob, "ave_length_powder_ft"] * 0.3048, 0)
      storage[ob, "ave_length_light_m"] <- round(storage[ob, "ave_length_light_ft"] * 0.3048, 0)
      storage[ob, "ave_length_deep_m"] <- round(storage[ob, "ave_length_deep_ft"] * 0.3048, 0)
      storage[ob, "ave_length_plunging_m"] <- round(storage[ob,"ave_length_plunging_ft"] * 0.3048, 0)

    }
    else{
    storage[ob, "PathName"] <- MajorPaths[ob]
    storage[ob, "n_events"] <- length(events)
    storage[ob, "n_naturals"] <- length(which(WorkingData[events,
                                                          "Trigger"] == "N"))
    storage[ob, "n_artificial"] <- length(which(!(WorkingData[events,
                                                              "Trigger"] %in% c("N", ""))))
    storage[ob, "firstdate"] <- min(WorkingData[full_record,
                                                "Season"], na.rm = TRUE)
    storage[ob, "lastdate"] <- max(WorkingData[full_record,
                                               "Season"], na.rm = TRUE)
    full_length <- (max(WorkingData[, "Season"], na.rm = TRUE) - min(WorkingData[, "Season"], na.rm = TRUE))
    storage[ob, "recordlength"] <- full_length - missing_seasons + 1
                                        #ifelse(storage[ob, "lastdate"] ==
                                            #storage[ob, "firstdate"], 1, storage[ob, "lastdate"] -
                                            #storage[ob, "firstdate"] + 1)
    storage[ob, "n_hits"] <- length(which(WorkingData[events,
                                                      "RoadDepth"] > 0 | WorkingData[events, "RoadHit"] %in%
                                            c(TRUE) | WorkingData[events, "RoadLength"] > 0))
    storage[ob, "n_roadopen"] <- length(which(WorkingData[events,
                                                          "RoadOpen"] %in% c("1", TRUE) & (WorkingData[events,
                                                                                                       "RoadHit"] %in% c("1", TRUE) | WorkingData[events,
                                                                                                                                                  "RoadDepth"] > 0 | WorkingData[events, "RoadLength"] >
                                                                                             0)))
    storage[ob, "mitigation_effectiveness"] <- 1 - (storage[ob,
                                                            "n_roadopen"]/storage[ob, "n_hits"])
    storage[ob, "residual_risk"] <- (storage[ob, "n_roadopen"]/storage[ob,
                                                                       "n_hits"])
    storage[ob, "open_naturals"] <- length(which(WorkingData[events,
                                                             "RoadOpen"] %in% c("1", TRUE) & WorkingData[events,
                                                                                                         "Trigger"] == "N"))
    storage[ob, "closed_naturals"] <- length(which(WorkingData[events,
                                                               "RoadOpen"] %in% c("0", FALSE) & WorkingData[events,
                                                                                                            "Trigger"] == "N"))
    storage[ob, "n_powder"] <- length(which(WorkingData[events, "AirBlast"] == "J" &
                                              WorkingData[events, "RoadDepth"] == 0))
    storage[ob, "n_light"] <- length(which(WorkingData[events,
                                                       "RoadDepth"] <= 3 & WorkingData[events, "RoadDepth"] >
                                             0))
    storage[ob, "n_deep"] <- length(which(WorkingData[events,
                                                      "RoadDepth"] > 3 & WorkingData[events, "RoadDepth"] <
                                            20))
    storage[ob, "n_plunging"] <- length(which(WorkingData[events,
                                                          "RoadDepth"] >= 20))
    storage[ob, "frequency"] <- storage[ob, "n_hits"]/storage[ob,
                                                              "recordlength"]
    storage[ob, "powder_frequency"] <- storage[ob, "n_powder"]/storage[ob, "recordlength"]
    storage[ob, "light_frequency"] <- storage[ob, "n_light"]/storage[ob,
                                                                     "recordlength"]
    storage[ob, "deep_frequency"] <- storage[ob, "n_deep"]/storage[ob,
                                                                   "recordlength"]
    storage[ob, "plunging_frequency"] <- storage[ob, "n_plunging"]/storage[ob,
                                                                           "recordlength"]
    storage[ob, "n_calc_events"] <- length(which(WorkingData[events,
                                                             "RoadDepth"] > 0 & WorkingData[events, "RoadLength"] >
                                                   0))
    light_forPercent <- length(which(WorkingData[events,
                                                 "RoadDepth"] <= 3 & WorkingData[events, "RoadDepth"] >
                                       0 & WorkingData[events, "RoadLength"] > 0))
    deep_forPercent <- length(which(WorkingData[events,
                                                "RoadDepth"] > 3 & WorkingData[events, "RoadDepth"] <
                                      20 & WorkingData[events, "RoadLength"] > 0))
    plunging_forPercent <- length(which(WorkingData[events,
                                                    "RoadDepth"] >= 20 & WorkingData[events, "RoadLength"] >
                                          0))
    total_events <- light_forPercent + deep_forPercent +
      plunging_forPercent
    if (total_events > 0) {
      storage[ob, "light_percentage"] <- light_forPercent/total_events
      storage[ob, "deep_percentage"] <- deep_forPercent/total_events
      storage[ob, "plunging_percentage"] <- plunging_forPercent/total_events
    }
    else {
      storage[ob, "light_percentage"] <- 0
      storage[ob, "deep_percentage"] <- 0
      storage[ob, "plunging_percentage"] <- 0
    }
    storage[ob, "ave_length_powder_ft"] <- round(ifelse(length(powder_events) > 0,
                                                  ifelse(all(is.na(WorkingData[powder_events, "RoadLength"])),
                                                         1/.3048,
                                                         mean(WorkingData[powder_events, "RoadLength"], na.rm = TRUE)),
                                                  1/.3048), 0)
    storage[ob, "ave_length_light_ft"] <- round(ifelse(length(light_events) > 0,
                                                  ifelse(all(is.na(WorkingData[light_events, "RoadLength"])),
                                                         ifelse(!is.null(path_info), Lave_light*path_info[ob, "length"]/.3048, 1/.3048),
                                                         mean(WorkingData[light_events, "RoadLength"], na.rm = TRUE)),
                                                 ifelse(!is.null(path_info), Lave_light*path_info[ob, "length"]/.3048, 1/.3048)), 0)
    storage[ob, "ave_length_deep_ft"] <- round(ifelse(length(deep_events) > 0,
                                                  ifelse(all(is.na(WorkingData[deep_events, "RoadLength"])),
                                                         ifelse(!is.null(path_info), Lave_deep*path_info[ob, "length"]/.3048, 1/.3048),
                                                         mean(WorkingData[deep_events, "RoadLength"], na.rm = TRUE)),
                                                ifelse(!is.null(path_info), Lave_deep*path_info[ob, "length"]/.3048, 1/.3048)), 0)
    storage[ob, "ave_length_plunging_ft"] <- round(ifelse(length(plunging_events) > 0,
                                                  ifelse(all(is.na(WorkingData[plunging_events, "RoadLength"])),
                                                         ifelse(!is.null(path_info), Lave_plunging*path_info[ob, "length"]/.3048, 1/.3048),
                                                         mean(WorkingData[plunging_events, "RoadLength"], na.rm = TRUE)),
                                                  ifelse(!is.null(path_info), Lave_plunging*path_info[ob, "length"]/.3048, 1/.3048)), 0)
    storage[ob, "ave_length_powder_m"] <- ifelse(is.null(path_info),
                                                 round(storage[ob, "ave_length_powder_ft"] * 0.3048, 0),
                                                 min(round(storage[ob, "ave_length_powder_ft"] * 0.3048, 0), path_info[ob, "length"])
                                                 )
    storage[ob, "ave_length_light_m"] <- ifelse(is.null(path_info),
                                                round(storage[ob, "ave_length_light_ft"] * 0.3048, 0),
                                                min(round(storage[ob, "ave_length_light_ft"] * 0.3048, 0), path_info[ob, "length"])
                                                )
    storage[ob, "ave_length_deep_m"] <- ifelse(is.null(path_info),
                                               round(storage[ob, "ave_length_deep_ft"] * 0.3048, 0),
                                               min(round(storage[ob, "ave_length_deep_ft"] * 0.3048, 0), path_info[ob, "length"])
    )
    storage[ob, "ave_length_plunging_m"] <- ifelse(is.null(path_info),
                                                   round(storage[ob, "ave_length_plunging_ft"] * 0.3048, 0),
                                                   min(round(storage[ob, "ave_length_plunging_ft"] * 0.3048, 0), path_info[ob, "length"])
    )
    }

    # Check and assign powder events
      powder[ob, "path_count"] <- ob
      powder[ob, "PathName"] <- MajorPaths[ob]
      powder[ob, "AnnualFreq"] <- storage[ob, "powder_frequency"]
      powder[ob, "ReturnPeriod"] <- ifelse(powder[ob, "AnnualFreq"] == 0, powder_RI, 1/storage[ob, "powder_frequency"])
      powder[ob, "AveLength_m"] <- storage[ob, "ave_length_powder_m"]
      if (!is.null(path_info) && powder[ob, "PathName"] %in% path_info$PathName) {
        powder[ob, "Lmax_m"] <- path_info[path_info$PathName == powder[ob, "PathName"], "length"]
      } else {
        powder[ob, "Lmax_m"] <- ifelse(length(which(WorkingData[powder_events, "RoadLength"] > 0) > 0),
                                       max(WorkingData[powder_events, "RoadLength"], na.rm = TRUE),
                                       1)
      }
      powder[ob, "AveLength_m"] <- min(storage[ob, "ave_length_powder_m"], powder[ob, "Lmax_m"])
      powder[ob, "Encounter_car"] <- (n_cars * (powder[ob, "AveLength_m"] + stopping_dist))/(powder[ob, "ReturnPeriod"] * speed_limit * 24000)
      powder[ob, "Encounter_truck"] <- (n_trucks * (powder[ob, "AveLength_m"] + stopping_dist))/(powder[ob, "ReturnPeriod"] * speed_limit * 24000)
      powder[ob, "Encounter_total"] <- powder[ob, "Encounter_car"] + powder[ob, "Encounter_truck"]
      powder[ob, "Q_cars"] <- Q_cars_powder
      powder[ob, "Q_trucks"] <- Q_trucks_powder
      powder[ob, "Moving_AHI"] <- ((powder[ob, "Encounter_car"] * Q_cars_powder) * percent_cars) + ((powder[ob, "Encounter_truck"] * Q_trucks_powder) * percent_trucks)
      powder[ob, "Car_Ratio"] <- percent_cars
      powder[ob, "Truck_Ratio"] <- percent_trucks
    # Check and assign light events
      light[ob, "path_count"] <- ob
      light[ob, "PathName"] <- MajorPaths[ob]
      light[ob, "AnnualFreq"] <- storage[ob, "light_frequency"]
      light[ob, "ReturnPeriod"] <- ifelse(light[ob, "AnnualFreq"] == 0, light_RI, 1/storage[ob, "light_frequency"])
      light[ob, "AveLength_m"] <- storage[ob, "ave_length_light_m"]
      if (!is.null(path_info) && light[ob, "PathName"] %in% path_info$PathName) {
        light[ob, "Lmax_m"] <- path_info[path_info$PathName == light[ob, "PathName"], "length"]
      } else {
        light[ob, "Lmax_m"] <- ifelse(length(which(WorkingData[light_events, "RoadLength"] > 0) > 0),
                                       max(WorkingData[light_events, "RoadLength"], na.rm = TRUE),
                                       1)
      }
      light[ob, "AveLength_m"] <- min(storage[ob, "ave_length_light_m"], light[ob, "Lmax_m"])
      light[ob, "Encounter_car"] <- (n_cars * (light[ob, "AveLength_m"] + stopping_dist))/(light[ob, "ReturnPeriod"] * speed_limit * 24000)
      light[ob, "Encounter_truck"] <- (n_trucks * (light[ob, "AveLength_m"] + stopping_dist))/(light[ob, "ReturnPeriod"] * speed_limit * 24000)
      light[ob, "Encounter_total"] <- light[ob, "Encounter_car"] + light[ob, "Encounter_truck"]
      light[ob, "Q_cars"] <- Q_cars_light
      light[ob, "Q_trucks"] <- Q_trucks_light
      light[ob, "Moving_AHI"] <- ((light[ob, "Encounter_car"] * Q_cars_light) * percent_cars) + ((light[ob, "Encounter_truck"] * Q_trucks_light) * percent_trucks)
      light[ob, "Car_Ratio"] <- percent_cars
      light[ob, "Truck_Ratio"] <- percent_trucks
    # Check and assign deep events
      deep[ob, "path_count"] <- ob
      deep[ob, "PathName"] <- MajorPaths[ob]
      deep[ob, "AnnualFreq"] <- storage[ob, "deep_frequency"]
      deep[ob, "ReturnPeriod"] <- ifelse(deep[ob, "AnnualFreq"] == 0, deep_RI, 1/storage[ob, "deep_frequency"])
      deep[ob, "AveLength_m"] <- storage[ob, "ave_length_deep_m"]
      if (!is.null(path_info) && deep[ob, "PathName"] %in% path_info$PathName) {
        deep[ob, "Lmax_m"] <- path_info[path_info$PathName == deep[ob, "PathName"], "length"]
      } else {
        deep[ob, "Lmax_m"] <- ifelse(length(which(WorkingData[deep_events, "RoadLength"] > 0) > 0),
                                       max(WorkingData[deep_events, "RoadLength"], na.rm = TRUE),
                                       1)
      }
      deep[ob, "AveLength_m"] <- min(storage[ob, "ave_length_deep_m"], deep[ob, "Lmax_m"])
      deep[ob, "Encounter_car"] <- (n_cars * (deep[ob, "AveLength_m"] + stopping_dist))/(deep[ob, "ReturnPeriod"] * speed_limit * 24000)
      deep[ob, "Encounter_truck"] <- (n_trucks * (deep[ob, "AveLength_m"] + stopping_dist))/(deep[ob, "ReturnPeriod"] * speed_limit * 24000)
      deep[ob, "Encounter_total"] <- deep[ob, "Encounter_car"] + deep[ob, "Encounter_truck"]
      deep[ob, "Q_cars"] <- Q_cars_deep
      deep[ob, "Q_trucks"] <- Q_trucks_deep
      deep[ob, "Moving_AHI"] <- ((deep[ob, "Encounter_car"] * Q_cars_deep) * percent_cars) + ((deep[ob, "Encounter_truck"] * Q_trucks_deep) * percent_trucks)
      deep[ob, "Car_Ratio"] <- percent_cars
      deep[ob, "Truck_Ratio"] <- percent_trucks
    # Check and assign plunging events
      plunging[ob, "path_count"] <- ob
      plunging[ob, "PathName"] <- MajorPaths[ob]
      plunging[ob, "AnnualFreq"] <- storage[ob, "plunging_frequency"]
      plunging[ob, "ReturnPeriod"] <- ifelse(plunging[ob, "AnnualFreq"] == 0, plunging_RI, 1/storage[ob, "plunging_frequency"])
      plunging[ob, "AveLength_m"] <- storage[ob, "ave_length_plunging_m"]
      if (!is.null(path_info) && plunging[ob, "PathName"] %in% path_info$PathName) {
        plunging[ob, "Lmax_m"] <- path_info[path_info$PathName == plunging[ob, "PathName"], "length"]
      } else {
        plunging[ob, "Lmax_m"] <- ifelse(length(which(WorkingData[plunging_events, "RoadLength"] > 0) > 0),
                                       max(WorkingData[plunging_events, "RoadLength"], na.rm = TRUE),
                                       1)
      }
      plunging[ob, "AveLength_m"] <- min(storage[ob, "ave_length_plunging_m"], plunging[ob, "Lmax_m"])
      plunging[ob, "Encounter_car"] <- (n_cars * (plunging[ob, "AveLength_m"] + stopping_dist))/(plunging[ob, "ReturnPeriod"] * speed_limit * 24000)
      plunging[ob, "Encounter_truck"] <- (n_trucks * (plunging[ob, "AveLength_m"] + stopping_dist))/(plunging[ob, "ReturnPeriod"] * speed_limit * 24000)
      plunging[ob, "Encounter_total"] <- plunging[ob, "Encounter_car"] + plunging[ob, "Encounter_truck"]
      plunging[ob, "Q_cars"] <- Q_cars_plunging
      plunging[ob, "Q_trucks"] <- Q_trucks_plunging
      plunging[ob, "Moving_AHI"] <- ((plunging[ob, "Encounter_car"] * Q_cars_plunging) * percent_cars) + ((plunging[ob, "Encounter_truck"] * Q_trucks_plunging) * percent_trucks)
      plunging[ob, "Car_Ratio"] <- percent_cars
      plunging[ob, "Truck_Ratio"] <- percent_trucks

      if(!is.null(path_info)){
      waiting[ob, "path_count"] <- ob
      waiting[ob, "PathName"] <-  MajorPaths[ob]
      waiting[ob, "j_minus"] <- path_info[ob, "Jminus"]
      waiting[ob, "j_plus"] <- path_info[ob, "Jplus"]
      waiting[ob, "Lmax"] <- path_info[ob, "length"]
      waiting[ob, "light_return"] <- light[ob, "ReturnPeriod"]
      waiting[ob, "ave_cars_light"] <- round(light[ob, "AveLength_m"]/((length_car_m*percent_cars)+(length_truck_m*percent_trucks)), 0)
      waiting[ob, "deep_return"] <- deep[ob, "ReturnPeriod"]
      waiting[ob, "ave_cars_deep"] <- round(deep[ob, "AveLength_m"]/((length_car_m*percent_cars)+(length_truck_m*percent_trucks)), 0)
      waiting[ob, "plunging_return"] <- plunging[ob, "ReturnPeriod"]
      waiting[ob, "ave_cars_plunging"] <- round(plunging[ob, "AveLength_m"]/((length_car_m*percent_cars)+(length_truck_m*percent_trucks)), 0)
      }else{
        waiting[ob, "path_count"] <- ob
        waiting[ob, "PathName"] <-  MajorPaths[ob]
        waiting[ob, "j_minus"] <- "NA"
        waiting[ob, "j_plus"] <- "NA"
        waiting[ob, "Lmax"] <- "NA"
        waiting[ob, "light_return"] <- light[ob, "ReturnPeriod"]
        waiting[ob, "ave_cars_light"] <- round(light[ob, "AveLength_m"]/((length_car_m*percent_cars)+(length_truck_m*percent_trucks)), 0)
        waiting[ob, "deep_return"] <- deep[ob, "ReturnPeriod"]
        waiting[ob, "ave_cars_deep"] <- round(deep[ob, "AveLength_m"]/((length_car_m*percent_cars)+(length_truck_m*percent_trucks)), 0)
        waiting[ob, "plunging_return"] <- plunging[ob, "ReturnPeriod"]
        waiting[ob, "ave_cars_plunging"] -> round(plunging[ob, "AveLength_m"]/((length_car_m*percent_cars)+(length_truck_m*percent_trucks)), 0)
      }

      # Assign final PathInfo and update station_list
      #PathInfo <- WorkingData[events, ] %>% select_if(~any(!is.na(.) & . != ""))
      PathInfo <- WorkingData[events, ]
      station_list[[ob]] <- list(Name = MajorPaths[ob], Table = PathInfo)
  }

  for (ob in seq_along(station_list)) {
    # Append each PathInfo table to the list
    road_hits[[ob]] <- station_list[[ob]]$Table
  }

  #create Waiting matrix
  n <- nrow(waiting)
  # Create an empty matrix to store the results
  result_matrix <- matrix(0, nrow = n, ncol = 2 * n - 1)
  colnames(result_matrix) <- c(paste0("J-", (n-1):1), "J", paste0("J+", 1:(n-1)))
  rownames(result_matrix) <- waiting$PathName

  j_plus <- as.numeric(waiting$j_plus)
  j_minus <- as.numeric(waiting$j_minus)
  Lmax <- as.numeric(waiting$Lmax)
  for (i in 1:n) {
    for (j in 1:(2*n-1)) {
      if ((j + i <= n) | (j + i > (2*n))) {
        result_matrix[i,j] <- 0
      } else if (j == n) {
        result_matrix[i,j] <- Ps
      } else if (j == (n+1) & (j-i > 1)) {
        sum_j <- j_plus[i]
        result_matrix[i, j] <- ifelse(Lw > sum_j, Ps_prime, 0)
      } else if (j == (n-1) & (j+i > n)) {
        sum_j <- j_minus[i]
        result_matrix[i, j] <- ifelse(Lw > sum_j, Ps_prime, 0)
      } else if (j < (n-1) & (j+i > n)) {
        sum_j <- sum(j_minus[max(1, i-(n-j)+1):i], na.rm = TRUE) + sum(Lmax[max(1, i-(n-j)+1):(i-1)], na.rm=TRUE)
        result_matrix[i, j] <- ifelse(Lw > sum_j, Ps_prime, 0)
      } else if (j > (n+1) & (j + i) <= (2*n)) {
        # Ensure valid indices for summation
        start_idx <- i
        end_idx <- min(i+j-n-1, n)

        # Calculate the correct sum
        sum_j <- sum(j_plus[start_idx:end_idx], na.rm = TRUE) + sum(Lmax[(start_idx+1):end_idx], na.rm = TRUE)

        result_matrix[i, j] <- ifelse(Lw > sum_j, Ps_prime, 0)
      }
    }
  }


  #Ps_matrix <- apply(result_matrix, 2, as.numeric)
  Ps_matrix <- result_matrix


  # Create an empty matrix to store the results for Pw_matrix
  Pw_matrix <- matrix(0, nrow = n, ncol = 2 * n - 1)
  colnames(Pw_matrix) <- c(paste0("J-", (n-1):1), "J", paste0("J+", 1:(n-1)))
  rownames(Pw_matrix) <- waiting$PathName

  ave_cars_light <- waiting$ave_cars_light
  light_return <- waiting$light_return
  ave_cars_deep <- waiting$ave_cars_deep
  deep_return <- waiting$deep_return
  ave_cars_plunging <- waiting$ave_cars_plunging
  plunging_return <- waiting$plunging_return

  # Fill the Pw_matrix
  for (i in 1:n) {
    for (j in 1:(2 * n - 1)) {
      if(i + j <= n |
         i + j > 2*n) {Pw_matrix[i, j] <- 0}
      else
        #if (i + j > n &
         # i + j <= 2*n)
        {
        lookup_row = i - (n-j)
        lookup_column = j
        Pw_matrix[i, j] <- (Q_cars_light * (ave_cars_light[lookup_row] / light_return[lookup_row]) * Ps_matrix[i, lookup_column]) +
          (Q_cars_deep * (ave_cars_deep[lookup_row] / deep_return[lookup_row]) * Ps_matrix[i, lookup_column]) +
          (Q_cars_plunging * (ave_cars_plunging[lookup_row] / plunging_return[lookup_row]) * Ps_matrix[i, lookup_column])
      }
    }
  }

  # Convert the Pw_matrix to numeric if necessary
  Pw_matrix <- apply(Pw_matrix, 2, as.numeric)
  # Round the Pw_matrix to one decimal place
  Pw_matrix <- round(Pw_matrix, 1)
  #Calculate row sums of the matrix
  Waiting_AHI <- rowSums(Pw_matrix, na.rm = TRUE)

  #define function to calculate AHI_2
  diagonal_sum <- function(mat) {
    n <- nrow(mat)
    middle_col <- n  # Middle column index for the first row
    result <- numeric(n)

    for (i in 0:(n-1)) {  # We start at the middle column and move rightwards
      sum_elements <- 0
      for (j in 0:(n-1)) {  # Diagonally move down and to the left
        row_index <- 1 + j
        col_index <- middle_col + i - j

        # Ensure indices are within bounds
        if (row_index <= n && col_index > 0 && col_index <= ncol(mat)) {
          sum_elements <- sum_elements + mat[row_index, col_index]
        }
      }
      result[i+1] <- sum_elements  # Store the result
    }

    return(result)
  }
  #Create Summary Table
  num_rows <- length(MajorPaths)
  SumTab <- data.frame(PathName = rep("", num_rows),
                         Powder_moving = rep(0, num_rows),
                         Light_moving = rep(0, num_rows),
                         Deep_moving = rep(0, num_rows),
                         Plunging_moving = rep(0, num_rows),
                         Moving_AHI = rep(0, num_rows),
                         Waiting_AHI = rep(0, num_rows),
                         Waiting_AHI_2 = rep(0, num_rows),
                         Total_AHI = rep(0, num_rows),
                         Total_AHI_2 = rep(0, num_rows),
                         Mitigation_Effectiveness = rep(0, num_rows),
                         Residual_Risk = rep(0, num_rows),
                         Residual_AHI = rep(0, num_rows))

  SumTab$PathName <- MajorPaths
  SumTab$Powder_moving <- round(powder[,"Moving_AHI"], 1)
  SumTab$Light_moving <- round(light[, "Moving_AHI"], 1)
  SumTab$Deep_moving <- round(deep[, "Moving_AHI"], 1)
  SumTab$Plunging_moving <- round(plunging[, "Moving_AHI"], 1)
  SumTab$Moving_AHI <- rowSums(SumTab[, c("Powder_moving", "Light_moving", "Deep_moving", "Plunging_moving")])
  SumTab$Waiting_AHI <- Waiting_AHI
  SumTab$Waiting_AHI_2 <- diagonal_sum(Pw_matrix)
  SumTab$Total_AHI <- round(SumTab$Moving_AHI + SumTab$Waiting_AHI, 2)
  SumTab$Total_AHI_2 <- round(SumTab$Moving_AHI + SumTab$Waiting_AHI_2, 2)
  SumTab$Mitigation_Effectiveness <- round(storage[, "mitigation_effectiveness"], 2)
  SumTab$Residual_Risk <- round(1-SumTab$Mitigation_Effectiveness, 2)
  SumTab$Residual_AHI <- round(SumTab$Residual_Risk*SumTab$Total_AHI_2, 1)

  #Table of road hits
  combined_road_hits <- do.call(rbind, road_hits) %>% select_if(~any(!is.na(.) & . != ""))

  input_parameters <- data.frame("Parameter" = c("WADT", "Percent Cars", "Percent Trucks", "Car Length", "Truck Length", "Reaction Time", "Speed Limit", "Road Grade", "Friction Coeff", "Waiting Time", "Ps", "Ps_prime", "Lave_light", "Lave_deep", "Lave_plunging"),
                             "Value" = c(WADT, percent_cars, percent_trucks, length_car_m, length_truck_m, reaction_time, speed_limit, road_grade, coeff_friction, wait_time, Ps, Ps_prime, Lave_light, Lave_deep, Lave_plunging))
  calc_parameters <- data.frame("Parameter" = c("n_cars", "n_trucks", "stopping_dist", "Lw"),
                                "Value" = c(n_cars, n_trucks, stopping_dist, Lw))
  AHI_totals <- data.frame(c("Moving AHI", "Waiting AHI", "Total AHI", "Residual AHI"),
                           c(sum(SumTab$Moving_AHI), sum(SumTab$Waiting_AHI), sum(SumTab$Total_AHI), sum(SumTab$Residual_AHI)))
  names(AHI_totals) <- c("AHI Summary", "Value")

  #Generate Plots
  OH <- data.frame(AHI_M = SumTab$Moving_AHI,
                   AHI_W = SumTab$Waiting_AHI,
                   PathName = SumTab$PathName)
  OH_long <- reshape2::melt(OH,id.vars = "PathName")
  OH_long$PathName <- factor(OH_long$PathName, levels = MajorPaths)

  OH2 <- data.frame(AHI_M = SumTab$Moving_AHI,
                   AHI_W2 = SumTab$Waiting_AHI_2,
                   PathName = SumTab$PathName)
  OH_long_2 <- reshape2::melt(OH2,id.vars = "PathName")
  OH_long_2$PathName <- factor(OH_long_2$PathName, levels = MajorPaths)

  # Plot the barplots
  AHI_Barplot <- ggplot(OH_long, aes(x = PathName, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(x = "Path Name", y = "AHI", fill = "Type", title = "Moving and Waiting AHI by Path") +
    theme_minimal()+
    scale_fill_manual(values = c("AHI_M" = "palegreen3", "AHI_W" = "tan1"),
                      labels = c("AHI_M" = "Moving AHI",
                                 "AHI_W" = "Waiting AHI")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "gray87", color = NA),
          plot.background = element_rect(fill = "gray97", color = NA))

  # Plot the barplots
  AHI_Barplot_2 <- ggplot(OH_long_2, aes(x = PathName, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(x = "Path Name", y = "AHI", fill = "Type", title = "Moving and Waiting AHI by Path (Sum 2)") +
    theme_minimal()+
    scale_fill_manual(values = c("AHI_M" = "palegreen3", "AHI_W2" = "tan1"),
                      labels = c("AHI_M" = "Moving AHI",
                                 "AHI_W2" = "Waiting AHI (Sum 2)")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "gray87", color = NA),
          plot.background = element_rect(fill = "gray97", color = NA))

  attributes(AHI_template) <- list(
    MajorPaths = MajorPaths,
    Summary = storage,
    PathTables = station_list,
    AHI_moving_powder = powder,
    AHI_moving_light = light,
    AHI_moving_deep = deep,
    AHI_moving_plunging = plunging,
    input_params = input_parameters,
    calc_params = calc_parameters,
    AHI_waiting = waiting,
    hits = combined_road_hits,
    Probability_event = Ps_matrix,
    Probability_waiting = Pw_matrix,
    Overall_Hazard = SumTab,
    Overall_AHI = AHI_totals,
    plot1 = AHI_Barplot,
    plot2 = AHI_Barplot_2
  )
}
