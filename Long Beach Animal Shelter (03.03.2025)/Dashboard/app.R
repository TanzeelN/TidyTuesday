library(shiny)
library(here)
library(bslib)
library(DT)
library(tidytuesdayR)

#Working Directory needs to be set to app.r

tuesdata <- tidytuesdayR::tt_load('2025-03-04')
Data <- tuesdata[[1]]


source("Data Processing.R",local = TRUE)
source("Plots.R", local = TRUE)

# Define the UI
ui <- fluidPage(
    strong("Long Beach Animal Shelter Analysis"),
    #Data Introduction & Data View----
    navset_tab(
            nav_menu(
                "Introduction",
                nav_panel("Dataset Introduction", htmlOutput("Summary")),
                nav_panel("Data View", 
                          fluidRow(div(DTOutput("DataView"),style = "font-size: 85%; width: 100%")),
                          fluidRow(htmlOutput("DataViewComments")))
        ),
        #Activity Plots----
        nav_panel("Animal Shelter Activity",
                  fluidRow(
                      column(6, 
                             plotOutput("ActivityByMonth",
                                        height = "400px")),
                      column(6, 
                             plotOutput("ActivityByYear",
                                        height = "400px"))),
                  fluidRow(
                      htmlOutput("ActivityFindings")
                  )),
        #Intake Plots----
        nav_panel("Animal Intakes",
                  fluidRow(
                      column(6, plotOutput("TopIntakes", height = "400px")),
                      column(6, plotOutput("AgeAtIntake", height = "400px"))
                  ),
                  fluidRow(
                      column(6, htmlOutput("IntakeFindings")),
                      column(6, tableOutput("IntakeTable"))
                  )),
       
        #Outcome Plots----
        nav_panel("Animal Outcomes",
                  fluidRow(
                      column(6, plotOutput("TopOutcomes", height = "400px")),
                      column(6, plotOutput("OutcomesByAnimal", height = "400px"))
                  ),
                  fluidRow(htmlOutput("OutcomeFindings"))
                  ),
        
        
        id = "Current Panel"
    )
)



#Server Function ----
server <- function(input, output) {
    #Activity Plots & Findings----
    output$ActivityByMonth <- renderPlot(PlotOccurencesByMonth)
    output$ActivityByYear <- renderPlot(PlotOccurencesByYear)
    output$ActivityFindings <- renderUI({
        HTML(paste("<b>Highlights:</b>
           <br>- Every year appears to develop differently.
           <br>- The largest peaks and troughs for each year are generally between month 6 & 7.
           <br>- The amount of animal intakes averages 4000 per year excluding 2020 & 2021.
           <br><span style='margin-left: 10px;'>Coincidently, COVID did strike in these years, so it could have been an impact to the data.</span>"
                   ))
    })
    
    
    #Intake Plots & Findings----
    
    output$TopIntakes <- renderPlot(PlotAnimalIntakes)
    output$AgeAtIntake <- renderPlot(PlotAgeAtIntake)
    output$IntakeTable <- renderTable(AnimalIntakesTable, width = "100%")
    
    output$IntakeFindings <- renderUI({
        HTML(paste("<b> Highlights </b>
                   <br>- The animal shelters greatest intakes of animals are cats & dogs.
                   <br>- Birds are the third largest intake generally but 'wild' & 'other' closely follow.
                   <br>- The age of the animals on average are less than 3. However, there are 4300 unknown entries which could be skewing the data.
                   <br>- The table on the bottom right shows all the different animal categories with how many were taken in by the shelter.
                   "))
    })
    
    
    #Outcomes Plots & Findings----
    
    output$TopOutcomes <- renderPlot(PlotOutcome)
    output$OutcomesByAnimal <- renderPlot(PlotOutcomeByAnimal)
    
    output$OutcomeFindings <- renderUI({
        HTML(paste("<b> Highlights </b>
                   <br>- Rescue & adoption make up majority of the outcomes.
                   <br>- Dogs seem to be the only animal which require to be returned to the owner."))
    })
    
    
    
    
    #Introduction & Data View:
    
    output$Summary <- renderUI({
        HTML(paste(". "))
    })
    
    output$DataView <- renderTable(RescueData)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #Introduction Tab----
    output$Summary <- renderUI({
        HTML(paste("<b>Project Basis</b>
                   <br>- This data has come from the Tidy Tuesday social data project: https://github.com/rfordatascience/tidytuesday
                   <br>- The purpose in doing this was to try different tools & become more profecient in R.
                   <br>- From this project a basic understanding of HTML/Shiny was developed alongside a more thorough understanding of ggplot, Data.Table & git.
                   
                   
                   <br>
                   <br>
                   <b>The Data </b>
                   <br>- This data comes from Tidy Tuesday Github Repo, dated 04/03/2025 & https://github.com/rfordatascience/tidytuesday/blob/main/data/2025/2025-03-04/readme.md.
                   <br>- Further info & where the data was collected from can be found from here: https://www.longbeach.gov/acs/.
                   <br>- This project looks to utilise some of the fields & create some visualisations.
                   <br>- The fields specification can be found in the Data View tab.
                   
                   
                   <br>
                   <br>
                   <b>Github Repo: https://github.com/TanzeelN/TidyTuesday/tree/main/Long%20Beach%20Animal%20Shelter%20(03.03.2025)
                   
                   
                   "))
    })
    output$DataView <- renderDT(setnames(RescueData[!is.na(dob),.(animal_id,
                                              animal_type,
                                              sex,
                                              dob,
                                              intake_date,
                                              intake_condition,
                                              intake_type,
                                              outcome_date,
                                              outcome_type,
                                              round(age_at_intake,2)
                                            )],"V10","intake_age"))

    output$DataViewComments <- renderUI({
        HTML(paste("<b> Field Specification </b>
                   <br>- animal_type: Category the animal falls into. some generalised categories include 'wild' & 'Livestock'
                   <br>- sex: Gender of Animal,
                   <br>- dob: Age of animal. NA entries were frequent.
                   <br>- intake fields: Fields relate to why/why the animal was taken in.
                   <br>- outcome fields: Fields relate to the information relating to the animals outcome.
                   <br>- age_at_intake: Manually calculated variable based on intake_date & dob."))
})

}
# Run the Shiny app
shinyApp(ui = ui, server = server)
