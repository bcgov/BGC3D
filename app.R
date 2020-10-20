library(shiny)
# library(shinyRGL)
library(rgl)
# library(RColorBrewer)
# library(DT)
# library(rgdal)
# library(leaflet)
# library(htmlwidgets)
# library(leaflet.opacity)
# library(sf)
# install.packages("devtools")
# library(devtools)
# install_github("rgl", "trestletech", "js-class")

# setwd("C:\\Users\\CMAHONY\\OneDrive - Government of BC\\Projects\\2020_BGCProjections\\app_3D\\BGC3D")

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
Clim.BGCs.BC <- read.csv("data/Clim.BGCs.BC.csv")[,-c(1:4)]
Clim.BGCs.WNA <- read.csv("data/Clim.BGCs.WNA.csv")[,-c(1:4)]
Clim.BGCs.BC.2004 <- read.csv("data/Clim.BGCs.BC.2004.csv")[,-c(1:4)]
Clim.sample.BC <- read.csv("data/Clim.sample.BC.csv")[,-c(1:4)]
Clim.sample.WNA <- read.csv("data/Clim.sample.WNA.csv")[,-c(1:4)]
Clim.BGCs.BC.future <- read.csv("data/Clim.BGCs.BC.future.csv")[,-c(1:7)]
Clim.BGCs.BC.future <- Clim.BGCs.BC.future[which(names(Clim.BGCs.BC.future)%in%names(Clim.BGCs.BC))]

# reorder variables
Clim.BGCs.BC <- Clim.BGCs.BC[,order(names(Clim.BGCs.BC))]
Clim.BGCs.WNA <- Clim.BGCs.WNA[,order(names(Clim.BGCs.WNA))]
Clim.BGCs.BC.2004 <- Clim.BGCs.BC.2004[,order(names(Clim.BGCs.BC.2004))]
Clim.sample.BC <- Clim.sample.BC[,order(names(Clim.sample.BC))]
Clim.sample.WNA <- Clim.sample.WNA[,order(names(Clim.sample.WNA))]
Clim.BGCs.BC.future <- Clim.BGCs.BC.future[,order(names(Clim.BGCs.BC.future))]

# # remove a variable
# Clim.BGCs.BC <- Clim.BGCs.BC[,-which(names(Clim.BGCs.BC)=="MCMT")]
# Clim.BGCs.WNA <- Clim.BGCs.WNA[,-which(names(Clim.BGCs.WNA)=="MCMT")]
# Clim.BGCs.BC.2004 <- Clim.BGCs.BC.2004[,-which(names(Clim.BGCs.BC.2004)=="MCMT")]
# Clim.sample.BC <- Clim.sample.BC[,-which(names(Clim.sample.BC)=="MCMT")]
# Clim.sample.WNA <- Clim.sample.WNA[,-which(names(Clim.sample.WNA)=="MCMT")]
# Clim.BGCs.BC.future <- Clim.BGCs.BC.future[,-which(names(Clim.BGCs.BC.future)=="MCMT")]

names(Clim.BGCs.BC)
names(Clim.BGCs.BC.2004)

# PC Scores with log transformation
logT.clim <- function(x){
  zerolim <- grep("MAP|PPT|PAS|DD|CMD|NFFD|FFP|MSP|Eref",names(x))
  for(i in zerolim){x[which(x[,i]==0),i] <- 1}  #set zero values to one, to facilitate log-transformation
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

variables <- names(Clim.BGCs.BC)
variable.names <- read.csv("data/Variables_ClimateBC.csv")

variable.types <- rep(NA, length(variables))
variable.types[grep("PPT|DD|PAS|NFFD|Eref|FFP|CMD|MAP|MSP|AHM|SHM|Rad|MAR", variables)] <- "ratio"
variable.types[grep("Tmax|Tmin|Tave|MAT|MWMT|MCMT|TD|EMT|EXT|bFFP|eFFP|PC", variables)] <- "interval"
variable.types[grep("RH", variables)] <- "pct"

# 
# test <- Clim.sample.BC[which(BGCs.sample.BC=="CWHvm1"),]
# x <- test[,1]
# y <- test[,2]
# plot(x,y)
# x <- test[,2]
# y <- test[,3]
# plot(x,y)
# 
# for(i in 4:length(test)){
#   par(mar=c(2,2,0,0), mfrow=c(5,7), mgp=c(0.25, 0,0))
#   for(j in 4:length(test)){
#     x <- test[,i]
#     y <- test[,j]
#     plot(x,y, xaxt="n", yaxt="n", xlab=names(test)[i], ylab=names(test)[j])
#   }
# }




# ------------------------------------------
# Define UI 
# ------------------------------------------

ui <- fluidPage(
  navbarPage(title = paste("BGC projections in climate space"), theme = "bcgov.css", 
             tabPanel("3D", 
                      fluidRow(
                        column(2,
                               helpText("Biogeoclimatic units in climate space"),
                               
                               tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
                               
                               checkboxInput("map", label = "Map mode", value = FALSE),
                               
                               checkboxInput("labels", label = "Show BGC labels", value = TRUE),
                               
                               checkboxInput("exotic", label = "Include BGC units outside BC", value = FALSE),
                               
                               selectInput("focal1", 
                                           label = "show spatial range of a BGC unit",
                                           choices = as.list(c("none", as.character(BGCs.BC))),
                                           selected = "none"),
                               
                               selectInput("focal2", 
                                           label = "show spatial range of another BGC unit",
                                           choices = as.list(c("none", as.character(BGCs.BC))),
                                           selected = "none"),
                               
                               selectInput("focalWNA", 
                                           label = "show spatial range of a non-BC BGC unit",
                                           choices = as.list(c("none", as.character(BGCs.WNA))),
                                           selected = "none"),
                               
                               checkboxInput("iso", label = "Equal-scale axes (recommended for PCs)", value = FALSE),
                               
                               checkboxInput("biplot", label = "Show variable correlations", value = FALSE),
                               
                               selectInput("var1", 
                                           label = "Choose the primary variable",
                                           choices = as.list(variables),
                                           selected = "PC1"),
                               
                               selectInput("var2", 
                                           label = "Choose the secondary variable",
                                           choices = as.list(variables),
                                           selected = "PC2"),
                               
                               selectInput("var3", 
                                           label = "Choose the tertiary variable",
                                           choices = as.list(variables),
                                           selected = "PC3"),
                               
                               checkboxInput("recent", label = "Show shift to 1991-2019 climate", value = FALSE),
                               
                               checkboxInput("future", label = "Show shifts to future climates", value = FALSE),
                               
                               selectInput("focal", 
                                           label = "Choose a focal BGC unit",
                                           choices = as.list(c(as.character(BGCs.BC))),
                                           selected = "IDFdk3"),
                               
                               radioButtons("proj.year",
                                            label = "Choose a future period",
                                            choices = list("2011-2040" = 1, "2041-2070" = 2, "2071-2100" = 3),
                                            selected = 1),
                               
                               radioButtons("rcp",
                                            label = "Choose an emissions scenario",
                                            choices = list("RCP4.5" = 1, "RCP8.5" = 2),
                                            selected = 1)
                               
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
             )
  )
)

# ------------------------------------------
# Define server logic 
# ------------------------------------------
server <- function(input, output) {
  
  
  output$plot <- renderRglwidget({
    
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
    
    var1 <- input$var1
    var2 <- input$var2
    var3 <- input$var3
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
    
    x.future <- Clim.BGCs.BC.future[which(BGCs.future==input$focal & scenario$rcp==rcp & scenario$proj.year==proj.year), which(variables==var1)]
    y.future <- Clim.BGCs.BC.future[which(BGCs.future==input$focal & scenario$rcp==rcp & scenario$proj.year==proj.year), which(variables==var2)]
    z.future <- Clim.BGCs.BC.future[which(BGCs.future==input$focal & scenario$rcp==rcp & scenario$proj.year==proj.year), which(variables==var3)]
    
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
    
    rgl.viewpoint( theta = 0, phi = 0, fov = 60, zoom = 0.65 )
    
    axes3d(tick=F, xlab=var1, ylab=var2, zlab=var3, col="black")
    
    }
    
    rglwidget()
    
    
    
  },
  # height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*0.4,0))
  )
  
}




# Run the app ----
shinyApp(ui = ui, server = server)

# proj.year <- 2055
# rcp <- "rcp45"
# gcm <- "CESM1-CAM5"
# var1 <- "MCMT"
# var2 <- "MWMT"
# var3 <- "PPT_JAS"
# x <- Clim.BGCs.BC[, which(variables==var1)]
# y <- Clim.BGCs.BC[, which(variables==var2)]
# z <- Clim.BGCs.BC[, which(variables==var3)]
# BGCs <- as.character(BGCs.BC)
# cols <- as.character(ColScheme.zone[match(zones.BC,zones)])
