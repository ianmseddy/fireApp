library(shiny)
require(raster)
require(sp)
require(reproducible)

filepath <- file.path(tempdir(), "scratch")
BC_eco <- prepInputs(targetFile = file.path(filepath, "ERC_ECOPRO_polygon.shp"),
                     url = "https://drive.google.com/open?id=1TrAMT5WBs4yyQXH3EL9LuRmuTLS7BPEc",
                     destinationPath = filepath,
                     overwrite = TRUE)

#Fires may download slowly...
fires <- prepInputs(targetFile = file.path(filepath, "NFDB_FirePoints.shp"),
                    destinationPath = filepath,
                    url = "https://drive.google.com/open?id=1KqN3t8Xma7oIOzNi4UsCamnGI_vbY7Qt",
                    overwrite = TRUE,
                    useCache = TRUE,
                    useSAcrs = TRUE,
                    alsoExtract = 'similar')

fires <- spTransform(fires, CRSobj = crs(BC_eco))
fires <- fires[fires$SIZE_HA > 1,]

#Define UI
ui <- fluidPage(

   # Application title
   titlePanel("Fire Size Distribution by Ecoregion"),

   sidebarLayout(
      sidebarPanel(
        selectInput("Ecoprovince",label = c("Select an Ecoprovince"),
                    choices = c("SOUTHERN ALASKA MOUNTAINS","NORTHERN BOREAL MOUNTAINS","TAIGA PLAINS",
                                "BOREAL PLAINS","SUB-BOREAL INTERIOR","SOUTHERN INTERIOR MOUNTAINS",
                                "SOUTHERN INTERIOR","COAST AND MOUNTAINS","GEORGIA DEPRESSION",
                                "NORTHEAST PACIFIC","CENTRAL INTERIOR"),selected = "GEORGIA DEPRESSION")
      ),

      # Show a plot of the Ecoprovince with fire locations, and fire size histogram
    mainPanel(

      plotOutput(outputId = "GISPlot"),
      plotOutput(outputId = "distPlot")

      )
   )
)


server <- function(input, output) {
    output$GISPlot <- renderPlot({
    # Plot(BC_select, title = paste(input$EcoProvince))

    BC_select <- BC_eco[BC_eco$CPRVNCNM == input$Ecoprovince,]
    fireSelect <- intersect(x = fires, y = BC_select)
    fireSelect <- fireSelect[!is.na(fireSelect$SIZE_HA),] #remove NA
    plot(BC_select)
    plot(fireSelect, add = TRUE, col = "red", pch = 20, cex = 1)

  })

  output$distPlot <- renderPlot({
    BC_select <- BC_eco[BC_eco$CPRVNCNM == input$Ecoprovince,]
    fireSelect <- intersect(x = fires, y = BC_select)
    fireSelect <- fireSelect[!is.na(fireSelect$SIZE_HA),] #remove NA
    if (length(fireSelect) > 1) {
    hist(log(fireSelect$SIZE_HA), main = "1950-2017 fire distribution (1 ha min.)", xlab = "log of fire size (ha)")
    }
    else{
      hist(0, main = "no fires")
    }
  })

}

shinyApp(ui = ui, server = server)

