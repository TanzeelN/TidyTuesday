library(shiny)
library(here)
library(bslib)
library(DT)
Data <- read.csv(here("Long Beach Animal Shelter (03.03.2025)","Data.csv"))



source(here("Long Beach Animal Shelter (03.03.2025)","Data Processing.R"),local = TRUE)
source(here("Long Beach Animal Shelter (03.03.2025)","Plots.R"), local = TRUE)

# Define the UI
ui <- fluidPage(
    "Long Beach Animal Shelter Analysis",
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
                      htmlOutput("IntakeFindings")
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
        HTML(paste("<b>Key Findings:</b>
           <br>- Every year appears to develop differently.
           <br>- The largest peaks and troughs for each year seem to be between month 6 & 7.
           <br>- The amount of animal intakes averages (4000) per year excluding 2020 & 2021.
           <br><span style='margin-left: 10px;'>These years seem to be COVID years, it could be less workforce..</span>"
                   ))
    })
    
    
    #Intake Plots & Findings----
    
    output$TopIntakes <- renderPlot(PlotAnimalIntakes)
    output$AgeAtIntake <- renderPlot(PlotAgeAtIntake)
    output$IntakeTable <- renderTable(AnimalIntakesTable)
    
    output$IntakeFindings <- renderUI({
        HTML(paste(". "))
    })
    
    
    #Outcomes Plots & Findings----
    
    output$TopOutcomes <- renderPlot(PlotOutcome)
    output$OutcomesByAnimal <- renderPlot(PlotOutcomeByAnimal)
    
    output$OutcomeFindings <- renderUI({
        HTML(paste(". "))
    })
    
    
    
    
    #Introduction & Data View:
    
    output$Summary <- renderUI({
        HTML(paste(". "))
    })
    
    output$DataView <- renderTable(RescueData)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #Introduction Tab----
    output$Summary <- renderUI({
        HTML(paste("This for the Data Introduction,Column Specs & Reference"))
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
        HTML(paste("This is the column Specs"))
})

}
# Run the Shiny app
shinyApp(ui = ui, server = server)
