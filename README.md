# Shiny application
The goal of the thesis is to produce an interactive tool to visualize medical data analysis results using graphs. The interactive tool is a part of a larger R package called Trajectories. Trajectories package has an algorithm that identifies common sequences of events in medical data. Such trajectories of diagnoses, drug prescriptions and procedures can get rather complicated and good visualisation methods are needed for finding relevant patterns in the data. In this thesis, a Shiny application was made, which can be used to browse the results of patterns found in medical data and do further characterization of trajectories of interest.

## App code location
The code that author is responsible for can be found under [inst/shiny](inst/shiny) folder. Also to integrate the app with Trajectories [R/VisualisingTrajectories.R](R/VisualisingTrajectories.R) file was made. The rest of the package is developed by Trajectories team.

## Code usage order:
1. To start the application first follow Trajectories Installation guide.
2. Make sure you have Trajectories algorithm analysis result file to use the visualisation with. Test files can be found under [inst/shiny/VisualisingTrajectories/Data](inst/shiny/VisualisingTrajectories/Data). For example to get data you can use command: `data <- as.data.frame(read.xlsx("inst/shiny/VisualisingTrajectories/Data/event_pairs_tested_2.xlsx"))`. 
3. Then you can start the app using `Trajectories::visualize_data_pairs(data)`, where `data` is the file described above.
 
 

# Trajectories R-package

To detect an visualize statistically significant event sequencies in OMOP data.

## Prerequisites

In order to run the package, you need:

1. A database that has data in OMOP CDM v5 format. The database should also contain OMOP vocabulary, but this can be in a separate schema.
2. A database user + passwords that has:
 a. Read (SELECT) permission from OMOP CDM tables and vocabulary
 b. CREATE, DROP, SELECT, INSERT, UPDATE, DELETE permission in some schema in the same database. This is used for creating and population temporary data tables. For this, you can create a separate schema in the same database.
3. Rstudio

## Installation

1. Clone this repository
2. Open the project (file ***Trajectories.Rproj***) in RStudio
3. Install the package via top-right menu: ***Build*** -> ***Install and Restart***

## Setup & running

1. Rename ***Renviron.template*** to ***.Renviron***
2. Add your database username and password to ***.Renviron*** (these are the database credentials for accessing OMOP CDM and writing temporary analysis tables/data). After you restart your RStudio, it automatically reads the database credentials from that file so that you do not have to enter them each time. Also, .Renviron is not under version control, therefore it is kept unchanged even if you pull the updates of the R-package.
3. Restart RStudio to automatically read in database credentials.
4. Open ***extras/CodeToRun.R***
5. Check that parameters up to *"# The following parameters are used in the calculations."* are correct and run the code.
6. It should produce quite a lot of output to the R console and ultimately create bunch of graphs to the folder that is set by variable "mainOutputFolder"

## Contributors

This package is developed by Kadri Künnapuu, Sulev Reisberg, Raivo Kolde (University of Tartu and STACC) with the help of other people. Its development started in IMI EHDEN project. the contributors are not limited to EHDEN consortium members (University of Tartu, STACC).
