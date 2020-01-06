
library(shiny)
library(leaflet)

shinyServer(fluidPage(
    titlePanel("San Francisco Criminal Offenses by District and Month"),
    sidebarLayout(
        sidebarPanel(
            selectInput("districtInput","Select your district:", c("Bayview", "Central",
                                                                   "Ingleside", "Mission",
                                                                   "Northern", "Out of SF",
                                                                   "Park", "Richmond",
                                                                   "Southern", "Taraval",
                                                                   "Tenderloin")),
            selectInput("monthInput", "Select the time of year:", c("January", "February",
                                                                    "March", "April",
                                                                    "May", "June",
                                                                    "July", "August",
                                                                    "September", "October",
                                                                    "November", "December")),
            submitButton("Submit"),
            h3("Is data available:"),
            textOutput("check1")
        ),
        mainPanel(
            leafletOutput("plot1"),
            h3("Zoltar predicts the next crime in your district will be:"),
            textOutput("predCrime")
        )
    )
))