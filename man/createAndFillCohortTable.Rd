% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createAndFillCohortTable.R
\name{createAndFillCohortTable}
\alias{createAndFillCohortTable}
\title{Creates and fills cohort table that is going to be used by the Trajectories package}
\usage{
createAndFillCohortTable(
  connection,
  trajectoryAnalysisArgs,
  trajectoryLocalArgs
)
}
\arguments{
\item{connection}{DatabaseConnectorConnection object that is used to connect with database}

\item{trajectoryAnalysisArgs}{Object created by Trajectories::createTrajectoryAnalysisArgs() method}

\item{trajectoryLocalArgs}{Object created by Trajectories::createTrajectoryLocalArgs() method}
}
\value{

}
\description{
First, it creates the cohort table. Note that this not the standard cohort table that is used in OMOP CDM schema, it is specifically created for this package amd later deleted.
Second, it loads cohort definition SQL file from the input path (given in trajectoryAnalysisArgs) and executes it in the database so the cohort table will  be filled in with event-periods that satisfy the cohort requirements.
The cohort_id of the built cohort is 1.
}
