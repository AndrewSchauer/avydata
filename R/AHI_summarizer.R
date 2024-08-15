#' Create the AHI input data template based on the excel workbook used by Hamre et al.
#'
#' @param input_data The input avalanche record. This format must comply to the DHA avalanche database PathNames in order for the function to work.
#' @param n_frequency The minimum number of events required to include the avalanche path as a 'Major' avalanche path. Default is 4. Value will be ignored if 'major_paths' argument is specified.
#' @param major_paths Optional vector of path names, if you want to look at a specific list of paths.
#' @return A list containing the major paths, summary data frame, and path tables.
#'
#' The list has the following components:
#'
#' \item{MajorPaths}{A character vector of the major paths.}
#' \item{Summary}{A data frame summarizing the data.}
#' \item{PathTables}{A list of data frames, each representing a path.}
#'
#' @name AHI_summarizer
#' @export

AHI_summarizer <- function(input_data, n_frequency = 4, major_paths = NULL) {
  library(dplyr)
  library(tidyr)

  if (is.null(major_paths)) {
    PathCounts <- data.frame(table(input_data$PathName))
    RO <- which(PathCounts$Freq >= n_frequency)
    MajorPaths <- PathCounts[RO, "Var1"]
    MajorPaths <- input_data %>%
      filter(PathName %in% MajorPaths, !is.na(PathName), PathName != "") %>%
      group_by(PathName) %>%
      summarise(count = n()) %>%
      filter(count >= n_frequency) %>%
      pull(PathName)
  } else {
    MajorPaths <- input_data %>%
      filter((RoadLength >= 1 | RoadDepth >= 1) | (RoadHit == TRUE | RoadHit == 1)) %>%
      filter(PathName %in% major_paths, !is.na(PathName), PathName != "") %>%
      group_by(PathName) %>%
      summarise(count = n()) %>%
      filter(count >= 1) %>%
      pull(PathName)
  }

  if (length(MajorPaths) == 0) {
    stop("No major paths found.")
  }

  WorkingPaths <- which(input_data$PathName %in% MajorPaths)
  WorkingData <- input_data[WorkingPaths, ]
  WorkingData[, "xSection_area"] <- WorkingData[, "RoadDepth"] * WorkingData[, "RoadLength"]
  WorkingData$PathName[WorkingData$PathName == ""] <- "Not Recorded"
  MajorPaths[MajorPaths == ""] <- "Not Recorded"

  date_separated <- separate(WorkingData, col = Date, into = c("Month", "Day", "Year"), sep = "/")
  WorkingData[, "Month"] <- as.numeric(date_separated[, "Month"])
  WorkingData[, "Day"] <- as.numeric(date_separated[, "Day"])
  WorkingData[, "Year"] <- as.numeric(date_separated[, "Year"])

  storage <- data.frame(PathName = character(), n_events = integer(), n_naturals = integer(),
                        n_artificial = integer(), firstdate = integer(), lastdate = integer(),
                        recordlength = integer(), n_hits = integer(), n_roadopen = integer(),
                        mitigation_effectiveness = numeric(), residual_risk = numeric(),
                        open_naturals = integer(), closed_naturals = integer(),
                        n_calc_events = integer(), n_light = integer(), n_deep = integer(),
                        n_plunging = integer(), frequency = numeric(), light_frequency = numeric(),
                        deep_frequency = numeric(), plunging_frequency = numeric(),
                        light_percentage = numeric(), deep_percentage = numeric(),
                        plunging_percentage = numeric(), ave_length_light_ft = numeric(),
                        ave_length_deep_ft = numeric(), ave_length_plunging_ft = numeric(),
                        ave_length_light_m = numeric(), ave_length_deep_m = numeric(),
                        ave_length_plunging_m = numeric(), stringsAsFactors = FALSE)

  station_list <- list()

  for (ob in 1:length(MajorPaths)) {
    events <- which(WorkingData$PathName == MajorPaths[ob] &
                      (WorkingData[, "RoadDepth"] > 0 | WorkingData[, "RoadHit"] %in% c("TRUE", 1) |
                         WorkingData[, "RoadLength"] > 0 | WorkingData[, "Size"] >= 4))
    full_record <- which(WorkingData$PathName == MajorPaths[ob])

    if (length(events) == 0 || length(full_record) == 0) next

    storage[ob, "PathName"] <- MajorPaths[ob]
    storage[ob, "n_events"] <- length(events)
    storage[ob, "n_naturals"] <- length(which(WorkingData[events, "Trigger"] == "N"))
    storage[ob, "n_artificial"] <- length(which(!(WorkingData[events, "Trigger"] %in% c("N", ""))))
    storage[ob, "firstdate"] <- min(WorkingData[full_record, "Year"], na.rm = TRUE)
    storage[ob, "lastdate"] <- max(WorkingData[full_record, "Year"], na.rm = TRUE)
    storage[ob, "recordlength"] <- ifelse(storage[ob, "lastdate"] == storage[ob, "firstdate"],
                                          1, storage[ob, "lastdate"] - storage[ob, "firstdate"] + 1)
    storage[ob, "n_hits"] <- length(which(WorkingData[events, "RoadDepth"] > 0 |
                                            WorkingData[events, "RoadHit"] %in% c(TRUE) |
                                            WorkingData[events, "RoadLength"] > 0))
    storage[ob, "n_roadopen"] <- length(which(WorkingData[events, "RoadOpen"] %in% c("1", TRUE) &
                                                (WorkingData[events, "RoadHit"] %in% c("1", TRUE) |
                                                   WorkingData[events, "RoadDepth"] > 0 |
                                                   WorkingData[events, "RoadLength"] > 0)))
    storage[ob, "mitigation_effectiveness"] <- 1 - (storage[ob, "n_roadopen"]/storage[ob, "n_hits"])
    storage[ob, "residual_risk"] <- (storage[ob, "n_roadopen"]/storage[ob, "n_hits"])
    storage[ob, "open_naturals"] <- length(which(WorkingData[events, "RoadOpen"] %in% c("1", TRUE) &
                                                   WorkingData[events, "Trigger"] == "N"))
    storage[ob, "closed_naturals"] <- length(which(WorkingData[events, "RoadOpen"] %in% c("0", FALSE) &
                                                     WorkingData[events, "Trigger"] == "N"))
    storage[ob, "n_light"] <- length(which(WorkingData[events, "RoadDepth"] <= 3 &
                                             WorkingData[events, "RoadDepth"] > 0))
    storage[ob, "n_deep"] <- length(which(WorkingData[events, "RoadDepth"] > 3 &
                                            WorkingData[events, "RoadDepth"] < 20))
    storage[ob, "n_plunging"] <- length(which(WorkingData[events, "RoadDepth"] >= 20))
    storage[ob, "frequency"] <- storage[ob, "n_hits"]/storage[ob, "recordlength"]
    storage[ob, "light_frequency"] <- storage[ob, "n_light"]/storage[ob, "recordlength"]
    storage[ob, "deep_frequency"] <- storage[ob, "n_deep"]/storage[ob, "recordlength"]
    storage[ob, "plunging_frequency"] <- storage[ob, "n_plunging"]/storage[ob, "recordlength"]
    storage[ob, "n_calc_events"] <- length(which(WorkingData[events, "RoadDepth"] > 0 &
                                                   WorkingData[events, "RoadLength"] > 0))

    light_forPercent <- length(which(WorkingData[events, "RoadDepth"] <= 3 &
                                       WorkingData[events, "RoadDepth"] > 0 &
                                       WorkingData[events, "RoadLength"] > 0))
    deep_forPercent <- length(which(WorkingData[events, "RoadDepth"] > 3 &
                                      WorkingData[events, "RoadDepth"] < 20 &
                                      WorkingData[events, "RoadLength"] > 0))
    plunging_forPercent <- length(which(WorkingData[events, "RoadDepth"] >= 20 &
                                          WorkingData[events, "RoadLength"] > 0))

    total_events <- light_forPercent + deep_forPercent + plunging_forPercent
    if (total_events > 0) {
      storage[ob, "light_percentage"] <- light_forPercent / total_events
      storage[ob, "deep_percentage"] <- deep_forPercent / total_events
      storage[ob, "plunging_percentage"] <- plunging_forPercent / total_events
    } else {
      storage[ob, "light_percentage"] <- 0
      storage[ob, "deep_percentage"] <- 0
      storage[ob, "plunging_percentage"] <- 0
    }

    storage[ob, "ave_length_light_ft"] <- mean(WorkingData[events, "RoadLength"][WorkingData[events, "RoadDepth"] <= 3], na.rm = TRUE)
    storage[ob, "ave_length_deep_ft"] <- mean(WorkingData[events, "RoadLength"][WorkingData[events, "RoadDepth"] > 3 & WorkingData[events, "RoadDepth"] < 20], na.rm = TRUE)
    storage[ob, "ave_length_plunging_ft"] <- mean(WorkingData[events, "RoadLength"][WorkingData[events, "RoadDepth"] >= 20], na.rm = TRUE)
    storage[ob, "ave_length_light_m"] <- storage[ob, "ave_length_light_ft"] * 0.3048
    storage[ob, "ave_length_deep_m"] <- storage[ob, "ave_length_deep_ft"] * 0.3048
    storage[ob, "ave_length_plunging_m"] <- storage[ob, "ave_length_plunging_ft"] * 0.3048

    PathInfo <- WorkingData[events, ] %>% select_if(~any(!is.na(.) & . != ""))
    station_list[[ob]] <- list(Name = MajorPaths[ob], Table = PathInfo)
  }

  attributes(AHI_summarizer) <- list("MajorPaths" = MajorPaths,
                                     "Summary" = storage,
                                     "PathTables" = station_list)

  return(list(MajorPaths = MajorPaths, Summary = storage, PathTables = station_list))
}
