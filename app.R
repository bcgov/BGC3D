# # BGC3D
# A shiny app for visualizing biogeoclimatic units in 3D climate space
# Colin Mahony colin.mahony@gov.bc.ca

# Copyright 2021 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

library(shiny)
library(rgl)
library(sf)
library(leaflet)
library(leafem)
library(htmlwidgets)
library(leaflet.opacity)
library(raster)

# ------------------------------------------
# Load the input data
# ------------------------------------------

#BGC zone color scheme
BGCcolors.BC <- read.csv("data/BGCzone_Colorscheme.csv")
BGCcolors <- read.csv("data/WNAv11_Zone_Colours.csv")
BGCcolors$colour <- as.character(BGCcolors$colour)
BGCcolors$colour[match(BGCcolors.BC$zone, BGCcolors$classification)] <- as.character(BGCcolors.BC$HEX)
ColScheme.zone <- factor(BGCcolors$colour, levels=BGCcolors$colour)
zones <- factor(BGCcolors$classification, levels=BGCcolors$classification)

# BGC labels
BGCs.BC <- read.csv("data/Clim.BGCs.BC.csv")[,1]
BGCs.WNA <- read.csv("data/Clim.BGCs.WNA.csv")[,1]
BGCs.sample.BC <- read.csv("data/Clim.sample.BC.csv")[,1]
BGCs.sample.WNA <- read.csv("data/Clim.sample.WNA.csv")[,1]
BGCs.future <- read.csv("data/Clim.BGCs.BC.future.csv")[,1]

scenario <- read.csv("data/Clim.BGCs.BC.future.csv")[,2:4]
rcps <- sort(unique(scenario[,2]))
proj.years <- unique(scenario[,3])

# Geographic Data
Geog.BGCs.BC <- read.csv("data/Clim.BGCs.BC.csv")[,c(3,2,4)]
Geog.BGCs.WNA <- read.csv("data/Clim.BGCs.WNA.csv")[,c(3,2,4)]
Geog.BGCs.BC.2004 <- read.csv("data/Clim.BGCs.BC.2004.csv")[,c(3,2,4)]
Geog.sample.BC <- read.csv("data/Clim.sample.BC.csv")[,c(3,2,4)]
Geog.sample.WNA <- read.csv("data/Clim.sample.WNA.csv")[,c(3,2,4)]
Geog.BGCs.BC.future <- read.csv("data/Clim.BGCs.BC.future.csv")[,c(6,5,7)]

# Climate Data
Clim.BGCs.BC.all <- read.csv("data/Clim.BGCs.BC.csv")[,-c(1:4)]
Clim.BGCs.WNA.all <- read.csv("data/Clim.BGCs.WNA.csv")[,-c(1:4)]
Clim.BGCs.BC.2004.all <- read.csv("data/Clim.BGCs.BC.2004.csv")[,-c(1:4)]
Clim.sample.BC.all <- read.csv("data/Clim.sample.BC.csv")[,-c(1:4)]
Clim.sample.WNA.all <- read.csv("data/Clim.sample.WNA.csv")[,-c(1:4)]
Clim.BGCs.BC.future.all <- read.csv("data/Clim.BGCs.BC.future.csv")[,-c(1:6)]
Clim.BGCs.BC.future.all <- Clim.BGCs.BC.future.all[which(names(Clim.BGCs.BC.future.all)%in%names(Clim.BGCs.BC.all))]

# # reorder variables
# Clim.BGCs.BC.all  <- Clim.BGCs.BC.all [,order(names(Clim.BGCs.BC.all ))]
# Clim.BGCs.WNA.all  <- Clim.BGCs.WNA.all [,order(names(Clim.BGCs.WNA.all ))]
# Clim.BGCs.BC.2004.all  <- Clim.BGCs.BC.2004.all [,order(names(Clim.BGCs.BC.2004.all ))]
# Clim.sample.BC.all  <- Clim.sample.BC.all [,order(names(Clim.sample.BC.all ))]
# Clim.sample.WNA.all  <- Clim.sample.WNA.all [,order(names(Clim.sample.WNA.all ))]
# Clim.BGCs.BC.future.all  <- Clim.BGCs.BC.future.all [,order(names(Clim.BGCs.BC.future.all ))]

# # remove CMD (too skewed for PCA)
# Clim.BGCs.BC <- Clim.BGCs.BC[-grep("CMD", names(Clim.BGCs.BC))]
# Clim.BGCs.WNA <- Clim.BGCs.WNA[-grep("CMD", names(Clim.BGCs.WNA))]
# Clim.BGCs.BC.2004 <- Clim.BGCs.BC.2004[-grep("CMD", names(Clim.BGCs.BC.2004))]
# Clim.sample.BC <- Clim.sample.BC[-grep("CMD", names(Clim.sample.BC))]
# Clim.sample.WNA <- Clim.sample.WNA[-grep("CMD", names(Clim.sample.WNA))]
# Clim.BGCs.BC.future <- Clim.BGCs.BC.future[-grep("CMD", names(Clim.BGCs.BC.future))]
# 
# # PC Scores without log transformation
# pca.BGCs <- prcomp(Clim.BGCs.BC, scale=T)
# # Cor <- cor(Clim.BGCs.BC,predict(pca.BGCs, logT.clim(Clim.BGCs.BC)))
# Clim.BGCs.BC <- cbind(predict(pca.BGCs, Clim.BGCs.BC)[,1:3], Clim.BGCs.BC)
# Clim.BGCs.WNA <- cbind(predict(pca.BGCs, Clim.BGCs.WNA)[,1:3],Clim.BGCs.WNA)
# Clim.BGCs.BC.2004 <- cbind(predict(pca.BGCs, Clim.BGCs.BC.2004)[,1:3],Clim.BGCs.BC.2004)
# Clim.sample.BC <- cbind(predict(pca.BGCs, Clim.sample.BC)[,1:3],Clim.sample.BC)
# Clim.sample.WNA <- cbind(predict(pca.BGCs, Clim.sample.WNA)[,1:3],Clim.sample.WNA)
# Clim.BGCs.BC.future <- cbind(predict(pca.BGCs, Clim.BGCs.BC.future)[,1:3],Clim.BGCs.BC.future)

zones.BC <- rep(NA, length(BGCs.BC))
for(i in BGCcolors$classification){ zones.BC[grep(i,BGCs.BC)] <- i }
zones.BC <- factor(zones.BC, BGCcolors$classification)

zones.WNA <- rep(NA, length(BGCs.WNA))
for(i in BGCcolors$classification){ zones.WNA[grep(i,BGCs.WNA)] <- i }
zones.WNA <- factor(zones.WNA, BGCcolors$classification)

vars.cbst <- c("Latitude",  "MAT", "MCMT", "TD", "MAP", "MSP", "DD5", "PAS")
vars.basic <- paste(rep(c("Tmin", "Tmax", "PPT"), each=4), rep(c("wt", "sp", "sm", "at"), times=4), sep="_")
vars.cciss <- c("CMD.total", "DD5_sp", "PPT_JAS", "bFFP", "MCMT", "AHM", "CMD_sp", "CMDMax", "DD5", "DD5_sm", "Eref_sm", "Eref_sp", "MWMT", "NFFD", "PAS_sp", "PAS_wt", "PPT_MJ", "SHM", "Tmax_sm")
varsets <- c("basic", "cbst", "cciss")

variable.names <- read.csv("data/Variables_ClimateBC.csv")

## SPATIAL DATA
bgc.simple <- st_read("data/bgc.simple.shp")
bgc.maprecord <- as.character(bgc.simple$BGC)
zone.maprecord <- bgc.maprecord
for(i in BGCcolors$classification){ zone.maprecord[grep(i,bgc.maprecord)] <- i }
bgc.list <- sort(unique(bgc.maprecord))
zone.list <- sort(unique(zone.maprecord))

# ------------------------------------------
# Define UI 
# ------------------------------------------

ui <- fluidPage(
  navbarPage(title = paste("BGC projections in climate space"), theme = "bcgov.css", 
             tabPanel("3D", 
                      fluidRow(
                        column(2,
                               helpText("Use mouse to spin and zoom"),
                               
                               tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
                               
                               checkboxInput("map", label = "Map mode", value = FALSE),
                               
                               checkboxInput("labels", label = "Show BGC labels", value = TRUE),
                               
                               selectInput("focal1", 
                                           label = "show spatial range of a BGC unit",
                                           choices = as.list(c("none", as.character(BGCs.BC))),
                                           selected = "none"),
                               
                               selectInput("focal2", 
                                           label = "show spatial range of another BGC unit",
                                           choices = as.list(c("none", as.character(BGCs.BC))),
                                           selected = "none"),
                               
                               checkboxInput("exotic", label = "Include BGC units outside BC", value = FALSE),
                               
                               conditionalPanel(
                                 condition = "input.exotic == true",
                                 
                                 selectInput("focalWNA", 
                                             label = "show spatial range of a non-BC BGC unit",
                                             choices = as.list(c("none", as.character(BGCs.WNA))),
                                             selected = "none"),
                                 
                               ),
                               
                               conditionalPanel(
                                 condition = "input.map == false",
                                 
                                 radioButtons("vars", inline = T, 
                                            label = "Choose a variable set",
                                            choices = list("Basic" = 1, "CBST" = 2, "CCISS"=3),
                                            selected = 1),
                               
                               checkboxInput("iso", label = "Equal-scale axes (recommended for PCs)", value = FALSE),
                               
                               checkboxInput("biplot", label = "Show variable correlations", value = FALSE),
                               
                               conditionalPanel(
                                 condition = "input.vars == 1",
                                 
                                 selectInput("var1.1", 
                                             label = "Choose the primary variable",
                                             choices = as.list(c("PC1", "PC2", "PC3", vars.basic)),
                                             selected = "PC1"),
                                 
                                 selectInput("var2.1", 
                                             label = "Choose the secondary variable",
                                             choices = as.list(c("PC1", "PC2", "PC3", vars.basic)),
                                             selected = "PC2"),
                                 
                                 selectInput("var3.1", 
                                             label = "Choose the tertiary variable",
                                             choices = as.list(c("PC1", "PC2", "PC3", vars.basic)),
                                             selected = "PC3"),
                                 
                               ),
                               
                               conditionalPanel(
                                 condition = "input.vars == 2",
                                 
                                 selectInput("var1.2", 
                                             label = "Choose the primary variable",
                                             choices = as.list(c("PC1", "PC2", "PC3", vars.cbst)),
                                             selected = "PC1"),
                                 
                                 selectInput("var2.2", 
                                             label = "Choose the secondary variable",
                                             choices = as.list(c("PC1", "PC2", "PC3", vars.cbst)),
                                             selected = "PC2"),
                                 
                                 selectInput("var3.2", 
                                             label = "Choose the tertiary variable",
                                             choices = as.list(c("PC1", "PC2", "PC3", vars.cbst)),
                                             selected = "PC3"),
                                 
                               ),
                               
                               
                               checkboxInput("recent", label = "Show shift to 2001-2019 climate", value = FALSE),
                               
                               checkboxInput("future", label = "Show shifts to future climates", value = FALSE),
                               
                               conditionalPanel(
                                 condition = "input.future == true",
                                 
                                 selectInput("focal", 
                                             label = "Choose a focal BGC unit",
                                             choices = as.list(c(as.character(BGCs.BC))),
                                             selected = "BGxh1"),
                                 
                                 radioButtons("proj.year", inline = T,
                                              label = "Choose a future period",
                                              choices = list("2011-2040" = 1, "2041-2070" = 2, "2071-2100" = 3),
                                              selected = 1),
                                 
                                 radioButtons("rcp", inline = T,
                                              label = "Choose an emissions scenario",
                                              choices = list("RCP4.5" = 1, "RCP8.5" = 2),
                                              selected = 1)

                               ),
                               ),
                        ),    
                        
                        column(10, 
                               
                               rglwidgetOutput(outputId = "plot", width="86vw", height="86vh")
                               # plotOutput(outputId = "plot", height="86vh")
                               
                        ),                
                        column(width = 12,
                               style = "background-color:#003366; border-top:2px solid #fcba19;",
                               
                               tags$footer(class="footer",
                                           tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                    tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                            tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                            tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                            tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                            tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                            tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                            tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                    )
                                           )
                               )
                        )
                      )
             ),
             
             tabPanel("Find-a-BEC",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Choose a BGC zone or subzone-variant to show it on the map"),
                          
                          tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
                          
                          selectInput("showbgc", 
                                      label = "Choose a BGC subzone-variant",
                                      choices = as.list(c("none", bgc.list)),
                                      selected = "none"),
                          
                          selectInput("showzone", 
                                      label = "Choose a BGC zone",
                                      choices = as.list(c("none", zone.list)),
                                      selected = "none"),
                          
                        ),    
                        
                        mainPanel(
                          
                          leafletOutput(outputId = "becmap", height="86vh")
                          
                        )
                      ),
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             
                             tags$footer(class="footer",
                                         tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                  )
                                         )
                             )
                      )
             ),
             
             tabPanel("About",
                      
                      includeMarkdown("about.Rmd"),
                      
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             
                             tags$footer(class="footer",
                                         tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                  )
                                         )
                             )
                      )
             )
             
  )
)

# ------------------------------------------
# Define server logic 
# ------------------------------------------
server <- function(input, output) {
  
  
  output$plot <- renderRglwidget({
    
    variables <- c("PC1", "PC2", "PC3", get(paste("vars", varsets[as.numeric(input$vars)], sep=".")))
    
    variable.types <- rep(NA, length(variables))
    variable.types[grep("PPT|DD|PAS|NFFD|Eref|FFP|CMD|MAP|MSP|AHM|SHM|Rad|MAR", variables)] <- "ratio"
    variable.types[grep("Tmax|Tmin|Tave|MAT|MWMT|MCMT|TD|EMT|EXT|bFFP|eFFP|PC", variables)] <- "interval"
    variable.types[grep("RH", variables)] <- "pct"
    
    # reorder variables
    Clim.BGCs.BC <- Clim.BGCs.BC.all[,which(names(Clim.BGCs.BC.all)%in%variables)]
    Clim.BGCs.WNA <- Clim.BGCs.WNA.all[,which(names(Clim.BGCs.WNA.all)%in%variables)]
    Clim.BGCs.BC.2004 <- Clim.BGCs.BC.2004.all[,which(names(Clim.BGCs.BC.2004.all)%in%variables)]
    Clim.sample.BC <- Clim.sample.BC.all[,which(names(Clim.sample.BC.all)%in%variables)]
    Clim.sample.WNA <- Clim.sample.WNA.all[,which(names(Clim.sample.WNA.all)%in%variables)]
    Clim.BGCs.BC.future <- Clim.BGCs.BC.future.all[,which(names(Clim.BGCs.BC.future.all)%in%variables)]
    
    # PC Scores with log transformation
    logT.clim <- function(x){
      zerolim <- grep("MAP|PPT|PAS|DD|CMD|NFFD|FFP|MSP|Eref",names(x))
      for(i in zerolim){x[which(x[,i]<1),i] <- 1}  #set zero values to one, to facilitate log-transformation
      x[,zerolim] <- log(x[,zerolim]) #log-transform the zero-limited variables
      return(x)
    }
    pca.BGCs <- prcomp(logT.clim(Clim.BGCs.BC), scale=T)
    # Cor <- cor(Clim.BGCs.BC,predict(pca.BGCs, logT.clim(Clim.BGCs.BC)))
    Clim.BGCs.BC <- cbind(predict(pca.BGCs, logT.clim(Clim.BGCs.BC))[,1:3], Clim.BGCs.BC)
    Clim.BGCs.WNA <- cbind(predict(pca.BGCs, logT.clim(Clim.BGCs.WNA))[,1:3],Clim.BGCs.WNA)
    Clim.BGCs.BC.2004 <- cbind(predict(pca.BGCs, logT.clim(Clim.BGCs.BC.2004))[,1:3],Clim.BGCs.BC.2004)
    Clim.sample.BC <- cbind(predict(pca.BGCs, logT.clim(Clim.sample.BC))[,1:3],Clim.sample.BC)
    Clim.sample.WNA <- cbind(predict(pca.BGCs, logT.clim(Clim.sample.WNA))[,1:3],Clim.sample.WNA)
    Clim.BGCs.BC.future <- cbind(predict(pca.BGCs, logT.clim(Clim.BGCs.BC.future))[,1:3],Clim.BGCs.BC.future)
    
    if(input$map == T){
      
      cols <- as.character(ColScheme.zone[match(zones.BC,zones)]) 
      cols.WNA <- as.character(ColScheme.zone[match(zones.WNA,zones)]) 
      
      x <- Geog.BGCs.BC[, 1] 
      y <- Geog.BGCs.BC[, 2] 
      z <- Geog.BGCs.BC[, 3] 
      
      if(input$focal1 != "none"){
        x1 <- Geog.sample.BC[which(BGCs.sample.BC==input$focal1), 1] 
        y1 <- Geog.sample.BC[which(BGCs.sample.BC==input$focal1), 2] 
        z1 <- Geog.sample.BC[which(BGCs.sample.BC==input$focal1), 3] 
      }
      
      if(input$focal2 != "none"){
        x2 <- Geog.sample.BC[which(BGCs.sample.BC==input$focal2), 1] 
        y2 <- Geog.sample.BC[which(BGCs.sample.BC==input$focal2), 2] 
        z2 <- Geog.sample.BC[which(BGCs.sample.BC==input$focal2), 3] 
      }
      
      if(input$focalWNA != "none"){
        x3 <- Geog.sample.WNA[which(BGCs.sample.WNA==input$focalWNA), 1] 
        y3 <- Geog.sample.WNA[which(BGCs.sample.WNA==input$focalWNA), 2] 
        z3 <- Geog.sample.WNA[which(BGCs.sample.WNA==input$focalWNA), 3] 
      }
      
      #core plot with BGC centroids
      plot3d(x,y,z, xlab="Longitude", ylab="Latitude", zlab="Elevation",
             type="s",
             col =  cols,
             alpha = 0.5,
             size=.5)
      
      #BGC labels (bug in package means you need to overwrite a few times to get some to show)
      if(input$labels == T){
        text3d(x,y,z, BGCs.BC, adj=-0.2, font=2, cex=0.6)
        text3d(x[1:30],y[1:30],z[1:30], BGCs.BC[1:30], adj=-0.2, font=2, cex=0.6)
        text3d(x[31:60],y[31:60],z[31:60], BGCs.BC[31:60], adj=-0.2, font=2, cex=0.6)
        text3d(x[61:90],y[61:90],z[61:90], BGCs.BC[61:90], adj=-0.2, font=2, cex=0.6)
        # for(i in 0:5)  text3d(x[i*100+1:99],y[i*100+1:99],z[i*100+1:99], BGCs.BC[i*100+1:99], adj=-0.2, font=2, cex=0.6)
        # for(i in 1:length(BGCs.BC))  text3d(x[i],y[i],z[i], BGCs.BC[i], adj=-0.2, font=2, cex=0.6)
      }
      
      if(input$exotic == T){
        x.WNA <- Geog.BGCs.WNA[, 1] 
        y.WNA <- Geog.BGCs.WNA[, 2] 
        z.WNA <- Geog.BGCs.WNA[, 3] 
        plot3d(x.WNA,y.WNA,z.WNA, type="s",col =  cols.WNA,alpha = 0.5,size=0.25, add=T)
        if(input$labels == T){
          text3d(x.WNA,y.WNA,z.WNA, BGCs.WNA, adj=-0.2, font=2, cex=0.6, color="blue")
          text3d(x.WNA[1:30],y.WNA[1:30],z.WNA[1:30], BGCs.WNA[1:30], adj=-0.2, font=2, cex=0.6, color="blue")
          text3d(x.WNA[31:60],y.WNA[31:60],z.WNA[31:60], BGCs.WNA[31:60], adj=-0.2, font=2, cex=0.6, color="blue")
        }
      }
      
      #
      if(input$focal1 != "none"){
        plot3d(x1,y1,z1, col=cols[which(BGCs.BC==input$focal1)], type="s",size=0.2, alpha=0.4, add=T)
      }
      
      if(input$focal2 != "none"){
        plot3d(x2,y2,z2, col=cols[which(BGCs.BC==input$focal2)], type="s",size=0.2, alpha=0.4, add=T)
      }
      
      if(input$focalWNA != "none"){
        plot3d(x3,y3,z3, col=cols.WNA[which(BGCs.WNA==input$focalWNA)], type="s",size=0.2, alpha=0.4, add=T)
      }
      
      rgl.viewpoint( theta = 0, phi = 0, fov = 60, zoom = 0.65 )
      
      axes3d(tick=F, xlab="Longitude", ylab="Latitude", zlab="Elevation", col="black")
    } else {
    
    proj.year <-  proj.years[as.numeric(input$proj.year)]
    rcp <- rcps[as.numeric(input$rcp)]
    
    var1 <- if(input$vars==1) input$var1.1 else input$var1.2
    var2 <- if(input$vars==1) input$var2.1 else input$var2.2
    var3 <- if(input$vars==1) input$var3.1 else input$var3.2
    variable.type1 <- variable.types[which(variables==var1)]
    variable.type2 <- variable.types[which(variables==var2)]
    variable.type3 <- variable.types[which(variables==var3)]
    
    x <- Clim.BGCs.BC[, which(variables==var1)] 
    y <- Clim.BGCs.BC[, which(variables==var2)] 
    z <- Clim.BGCs.BC[, which(variables==var3)] 
    
    Cor <- cor(Clim.BGCs.BC,data.frame(x,y,z))
    
    x.recent <- Clim.BGCs.BC.2004[, which(variables==var1)]
    y.recent <- Clim.BGCs.BC.2004[, which(variables==var2)]
    z.recent <- Clim.BGCs.BC.2004[, which(variables==var3)]
    
    bgc.focal <- input$focal
    x.future <- Clim.BGCs.BC.future[which(BGCs.future==bgc.focal & scenario$rcp==rcp & scenario$proj.year==proj.year), which(variables==var1)]
    y.future <- Clim.BGCs.BC.future[which(BGCs.future==bgc.focal & scenario$rcp==rcp & scenario$proj.year==proj.year), which(variables==var2)]
    z.future <- Clim.BGCs.BC.future[which(BGCs.future==bgc.focal & scenario$rcp==rcp & scenario$proj.year==proj.year), which(variables==var3)]
    
    if(input$focal1 != "none"){
      x1 <- Clim.sample.BC[which(BGCs.sample.BC==input$focal1), which(variables==var1)] 
      y1 <- Clim.sample.BC[which(BGCs.sample.BC==input$focal1), which(variables==var2)] 
      z1 <- Clim.sample.BC[which(BGCs.sample.BC==input$focal1), which(variables==var3)] 
    }
    
    if(input$focal2 != "none"){
      x2 <- Clim.sample.BC[which(BGCs.sample.BC==input$focal2), which(variables==var1)] 
      y2 <- Clim.sample.BC[which(BGCs.sample.BC==input$focal2), which(variables==var2)] 
      z2 <- Clim.sample.BC[which(BGCs.sample.BC==input$focal2), which(variables==var3)] 
    }
    
    if(input$focalWNA != "none"){
      x3 <- Clim.sample.WNA[which(BGCs.sample.WNA==input$focalWNA), which(variables==var1)] 
      y3 <- Clim.sample.WNA[which(BGCs.sample.WNA==input$focalWNA), which(variables==var2)] 
      z3 <- Clim.sample.WNA[which(BGCs.sample.WNA==input$focalWNA), which(variables==var3)] 
    }
    
    cols <- as.character(ColScheme.zone[match(zones.BC,zones)]) 
    cols.WNA <- as.character(ColScheme.zone[match(zones.WNA,zones)]) 
    
    #core plot with BGC centroids
    plot3d(x,y,z, xlab=var1, ylab=var2, zlab=var3,
           type="s",
           col =  cols,
           alpha = 0.5,
           size=.5)
    
    
    #BGC labels (bug in package means you need to overwrite a few times to get some to show)
    if(input$labels == T){
      text3d(x,y,z, BGCs.BC, adj=-0.2, font=2, cex=0.6)
      text3d(x[1:30],y[1:30],z[1:30], BGCs.BC[1:30], adj=-0.2, font=2, cex=0.6)
      text3d(x[31:60],y[31:60],z[31:60], BGCs.BC[31:60], adj=-0.2, font=2, cex=0.6)
      text3d(x[61:90],y[61:90],z[61:90], BGCs.BC[61:90], adj=-0.2, font=2, cex=0.6)
      # for(i in 0:5)  text3d(x[i*100+1:99],y[i*100+1:99],z[i*100+1:99], BGCs.BC[i*100+1:99], adj=-0.2, font=2, cex=0.6)
      # for(i in 1:length(BGCs.BC))  text3d(x[i],y[i],z[i], BGCs.BC[i], adj=-0.2, font=2, cex=0.6)
    }
    
    if(input$exotic == T){
      x.WNA <- Clim.BGCs.WNA[, which(variables==var1)] 
      y.WNA <- Clim.BGCs.WNA[, which(variables==var2)] 
      z.WNA <- Clim.BGCs.WNA[, which(variables==var3)] 
      plot3d(x.WNA,y.WNA,z.WNA, type="s",col =  cols.WNA,alpha = 0.5,size=0.25, add=T)
      if(input$labels == T){
        text3d(x.WNA,y.WNA,z.WNA, BGCs.WNA, adj=-0.2, font=2, cex=0.6, color="blue")
        text3d(x.WNA[1:30],y.WNA[1:30],z.WNA[1:30], BGCs.WNA[1:30], adj=-0.2, font=2, cex=0.6, color="blue")
        text3d(x.WNA[31:60],y.WNA[31:60],z.WNA[31:60], BGCs.WNA[31:60], adj=-0.2, font=2, cex=0.6, color="blue")
      }
    }
    
    #add in Biplot arrows
    if(input$biplot == T){
      EF <- 3    #arrow length modifier
      
      rgl.lines(t(cbind(rep(mean(x), dim(Cor)[1]),mean(x)+Cor[,1]*EF*sd(x))), 
                t(cbind(rep(mean(y), dim(Cor)[1]),mean(y)+Cor[,2]*EF*sd(y))), 
                t(cbind(rep(mean(z), dim(Cor)[1]),mean(z)+Cor[,3]*EF*sd(z))), 
                color="red")
      text3d(mean(x)+1.1*(Cor[,1]*EF*sd(x)),
             mean(y)+1.1*(Cor[,2]*EF*sd(y)),
             mean(z)+1.1*(Cor[,3]*EF*sd(z)), 
             rownames(Cor), col="red", xpd=T, cex=0.9, font=2)
    }
    
    #
    if(input$focal1 != "none"){
      plot3d(x1,y1,z1, col=cols[which(BGCs.BC==input$focal1)], type="s",size=0.2, alpha=0.4, add=T)
    }
    
    if(input$focal2 != "none"){
      plot3d(x2,y2,z2, col=cols[which(BGCs.BC==input$focal2)], type="s",size=0.2, alpha=0.4, add=T)
    }
    
    if(input$focalWNA != "none"){
      plot3d(x3,y3,z3, col=cols.WNA[which(BGCs.WNA==input$focalWNA)], type="s",size=0.2, alpha=0.4, add=T)
    }
    
    if(input$iso==T) aspect3d("iso")
    
    if(input$recent==T) rgl.lines(t(cbind(x,x.recent)), t(cbind(y,y.recent)), t(cbind(z,z.recent)), color="grey")
    
    if(input$future==T){
      rgl.lines(t(cbind(rep(x[which(BGCs.BC==input$focal)], length(x.future)),x.future)), t(cbind(rep(y[which(BGCs.BC==input$focal)], length(y.future)),y.future)), t(cbind(rep(z[which(BGCs.BC==input$focal)], length(z.future)),z.future)), color="black")
    }
    
    # rgl.viewpoint( theta = 0, phi = 0, fov = 60, zoom = 0.65 )
    
    axes3d(tick=F, xlab=var1, ylab=var2, zlab=var3, col="black")
    
    }
    
    rglwidget(reuse=T)
    
    
    
  },
  # height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*0.4,0))
  )
  
  #-------------------------
  # Find-a-BEC
  #-------------------------
  
  # showbgc <- "BGxh1"
  # showzone <- "BG"
  
  output$becmap <- renderLeaflet({
    
    showbgc <- input$showbgc
    showzone <- input$showzone
    
    leaflet() %>% 
      addTiles() %>% 
      addProviderTiles("Esri.WorldImagery", group = "Satellite view") %>%
      addProviderTiles("Esri.WorldTerrain", group = "Terrain only") %>%
      addProviderTiles("Esri.WorldTopoMap", group = "Base map") %>%
      fitBounds(lng1 = extent(bgc.simple)[1], lat1 = extent(bgc.simple)[3], lng2 = extent(bgc.simple)[2], lat2 = extent(bgc.simple)[4]) %>%
      addLayersControl(
        baseGroups = c("Base map", "Terrain only", "Satellite view"),
        options = layersControlOptions(collapsed = FALSE),
      ) %>%
      addPolygons(data=bgc.simple[zone.maprecord == showzone,], fillColor = "red", color="red", smoothFactor = 0.2, fillOpacity = 0.4, weight=2, opacity=1)%>%
      addPolygons(data=bgc.simple[bgc.maprecord == showbgc,], fillColor = "black", color="black", smoothFactor = 0.2, fillOpacity = 0.4, weight=2, opacity=1) 
    
  },
  )
  
  
  
}




# Run the app ----
shinyApp(ui = ui, server = server)

# vars <- get(paste("vars", varsets[2], sep="."))
# variables <- c("PC1", "PC2", "PC3", vars)
# proj.year <- 2055
# rcp <- "rcp45"
# gcm <- "CESM1-CAM5"
# var1 <- variables[1]
# var2 <- variables[2]
# var3 <- variables[3]
# x <- Clim.BGCs.BC[, which(variables==var1)]
# y <- Clim.BGCs.BC[, which(variables==var2)]
# z <- Clim.BGCs.BC[, which(variables==var3)]
# BGCs <- as.character(BGCs.BC)
# cols <- as.character(ColScheme.zone[match(zones.BC,zones)])
# bgc.focal <- "BGxh1"
