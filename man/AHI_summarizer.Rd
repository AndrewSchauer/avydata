% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AHI_summarizer.R
\name{AHI_summarizer}
\alias{AHI_summarizer}
\title{Create the AHI input data template based on the excel workbook used by Hamre et al.}
\usage{
AHI_summarizer(input_data, n_frequency = 4, major_paths = NULL)
}
\arguments{
\item{input_data}{The input avalanche record. This format must comply to the DHA avalanche database PathNames in order for the function to work.}

\item{n_frequency}{The minimum number of events required to include the avalanche path as a 'Major' avalanche path. Default is 4. Value will be ignored if 'major_paths' argument is specified.}

\item{major_paths}{Optional vector of path names, if you want to look at a specific list of paths.}
}
\value{
A list containing the major paths, summary data frame, and path tables.

The list has the following components:

\item{MajorPaths}{A character vector of the major paths.}
\item{Summary}{A data frame summarizing the data.}
\item{PathTables}{A list of data frames, each representing a path.}
}
\description{
Create the AHI input data template based on the excel workbook used by Hamre et al.
}
