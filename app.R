library(knitr)
library(rgl)
library(ggplot2)
library(BIGL)
library(Hmisc)
library(stringr)
library(dplyr)
library(grid)
library(gridExtra)
library(drc)
library(shiny)
library(ggpubr)
library(ggsci)
library(DT)
library(synergyfinder)
library(reshape)
library(sjPlot)
library(plotly)
library(bslib)
library(shinythemes)
library(colourpicker)
library(shinycssloaders)
library(ComplexHeatmap)
library(circlize)
library(shinyWidgets)
library(bs4Dash)
library(leaflet)

# list of test to compute by shiny app
test.vector = c("ZIP_synergy", "Bliss_synergy", "HSA_synergy", "Loewe_synergy")
#names.drugs = read.csv("George_Selleck_Library_Reformatted_Plate_Lists_V2.csv",header = T)
#names.drugs = read.csv("~/OneDrive - Australian National University/amee/mds_screening_v5/info/George_Selleck_Library_Reformatted_Plate_Lists_V2.csv",header = T)

shinyApp(
  
  ui = dashboardPage(scrollToTop = TRUE, fullscreen = TRUE,
    dashboardHeader(title = dashboardBrand(title = "DRUG SCREENING",image="drug_logo.jpg"),fixed = TRUE, border = TRUE),
    dashboardSidebar(width = 500,
      sidebarMenu(id = "sidebar",
        menuItem("Analysis",tabName = "analysis",icon=icon("wrench")),
        menuItem("User Information",tabName = "userInfo",icon = icon("list")),
        menuItem("Contact",tabName = "contact",icon = icon("user"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName="analysis",
                fluidRow(
              box(title = tagList(shiny::icon("gear"),"Parameters"), width = 4,
                   h2("Drug screening file:"),
                   
                   fileInput("names.drugs","Choose a plate reference file to upload (csv)",buttonLabel = "Upload...",multiple = FALSE),
                   fileInput("upload","Choose a drug file to upload (csv)",buttonLabel = "Upload...",multiple = FALSE),
                   fileInput("upload2","Choose a vehicle file to upload (csv)",buttonLabel = "Upload...",multiple = FALSE),
                   
                   splitLayout(cellWidths = c("60%", "30%"),
                               textInput("drug.name",label="Drug Name",value=""),
                               #helpText("Write drug name tested"),
                               numericInput("drug.con",label="Conc. (uM)",value = ""),
                               #helpText("Write concentration from drug tested (uM)")
                   ),
                   
                   #actionButton("submit","Submit"),
                   h2("Estimate IC50:"),
                   #selectInput("ic50", label = "",""),
                   selectInput("drug", label = "Select Drug",""),
                   #pickerInput("ic50",label = "",choices = "",options = list(`actions-box` = TRUE),multiple=F),
                   #helpText("Select plate"),
                   h2("Synergy analysis:"),
                   h3(em("Drug selection")),
                   radioButtons("response","Response",choices = c("viability","inhibition")),
                   selectInput("synergy", label = "",""),
                   helpText("Select drug"),
                   h3(em("Synergy model")),
                   selectInput("method", label = "",""),
                   helpText("Select synergy score method")
                ),
              tabBox(
                title = tagList(shiny::icon("tasks"),"Outputs"), side = "right", width = 8,
                tabPanel("IC50",
                         #div(style="display: inline-block;vertical-align:top;",selectInput("drug", label = "Select Drug","")),
                         #div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
                         br(),
                         shinycssloaders::withSpinner(plotOutput("ic50.plot",inline = TRUE),type = 5),
                         div(style="display: inline-block;vertical-align:top; width: 150px; margin-top: 10px;",downloadButton(outputId = "download_plot1",label="Download .SVG")),
                         colourInput("col1","Select colour Drug","orange"),
                         colourInput("col2","Select colour Vehicle","grey"),
                         br(),
                         DT::dataTableOutput("ic50.table")),
                tabPanel("Synergy",
                         br(),
                         downloadButton(outputId = "download_plot2",label="Download .SVG"),
                         br(),
                         shinycssloaders::withSpinner(plotOutput("synergy.plot",inline = TRUE),type = 5),
                         colourInput("col3","High response value color","#FF0000"),
                         colourInput("col4","Low response value color","#0037ff"),
                         br(),
                         DT::dataTableOutput("synergy.table")),
                tabPanel("Synergy table",
                         br(),
                         #pickerInput("ht.drugs",label = "",choices ="",options = list(`actions-box` = TRUE),multiple=T),
                         uiOutput("picker"),
                         br(),
                         sliderInput("threshold",label = "Synergy threshold",min = -100,max=100,value = -100),
                         br(),
                         downloadButton(outputId = "download_plot3",label="Download .SVG"),
                         br(),
                         shinycssloaders::withSpinner(plotOutput("synergy.heatmap",inline = TRUE),type = 5),
                         DT::dataTableOutput("synergy.table.all"))
              ),
                )
              ),
        tabItem(tabName = "userInfo",
                box(title = h2("Analysis description"), collapsible = FALSE, width = 12,
                p("The experimental design of this drug screening analysis is base on the idea of comparing whats the synergy of one experimental drug againts a battery of multiple drugs."),
                p("You will need 3 different files one for the reference of the drugs used and two with the tested drugs one as a drug plate and another as a vehicle plate."),
                p("The experiments has to be perform on a 384 standar plates.")
                ),
                box( title = h4("Data format:"), collapsible = TRUE, width = 12, collapsed = TRUE,
                p("Upload a CSV file with the following columns for the plate reference file."),
                tags$ul(
                  tags$li("Plate"),
                  tags$li("Well ID"),
                  tags$li("Well Row"),
                  tags$li("Well Column"),
                  tags$li("Well Type"),
                  tags$li("Compound"),
                  tags$li("Concentration (uM)")
                ),
                p("Upload a CSV file with the following columns for the drug and vehicle plate."),
                tags$ul(
                  tags$li("Plate ID (add _Drug or _Veh at the end of your Plate ID)"),
                  tags$li("Well ID"),
                  tags$li("Well Type"),
                  tags$li("Compound"),
                  tags$li("Concentration (uM)"),
                  tags$li("Count of Counting beads"),
                  tags$li("Count of Live cells"),
                  tags$li("Count of Dead cells"),
                  tags$li("% Live cells"),
                  tags$li("% Dead cells"),
                  tags$li("Number of live cells per well"),
                  tags$li("Number of dead cells per well"),
                  tags$li("Median CellTrace FR-A (RL1-A) of Live cells")
                ),
                p("Provide the drug name of the tested drug to compare with the screening series drugs and concentration"),
                tags$ul(
                  tags$li("Drug tested name"),
                  tags$li("Concentration of the tested drug (uM)")
                )),
                hr(),
                box(title = h4("IC50 calculation:"), collapsible = TRUE, width = 12, collapsed = TRUE,
                p("Estimate IC50 for every drug."),
                tags$ul(
                  tags$li("% Live Cells"),
                  tags$li("% Increment Median Cell Trace [((Median cell trace / mean(Median cell trace control))-1)*100]")),
                p("Summary table with the IC50 calculated by drug on uM units."),
                p("Drugs evaluated are on the rows, plates are on the columns.")
                ),
                hr(),
                box(title = h4("Synergy Analysis:"), collapsible = TRUE, width = 12,  collapsed = TRUE,
                p(style="text-align: justify;","Synergy analysis based on the SynergyFinder Plus package in R from Shuyu Zheng et al. from Reserach Program in system Oncology, Faculty of Medicine, University of Helsinki."),
                p(a("SynergyFinder",href="https://www.bioconductor.org/packages/release/bioc/html/synergyfinder.html")),
                h4("Response."),
                p("User can choose between two type of observations on the synergy response:"),
                tags$ul(
                  tags$li(style="text-align: justify;",strong("viability:"), " use % Live cells as a response."),
                  tags$li(style="text-align: justify;",strong("inhibition:"), " use % Increment Median Cell Trace as a response."),
                ),
                p("4 synergy scoring are computed."),
                tags$ul(
                  tags$li(style="text-align: justify;",strong("Highest Single Agent (HSA):"), " states that the expected combination effect equals to the higher effect of individual drugs."),
                  tags$li(style="text-align: justify;",strong("Bliss model (Bliss):"), " assumes a stochastic process in which two drugs exert their effects independently, and the expected combination effect can be calculated based on the probability of independent events."),
                  tags$li(style="text-align: justify;",strong("Loewe additivity model (Loewe):"), " is based on the assumption that no compound interacts with itself and that two doses from different compounds having the same effect are equivalent."),
                  tags$li(style="text-align: justify;",strong("Zero Interaction Potency (ZIP):"), " calculates the expected effect of two drugs under the assumption that they do not potentiate each other, i.e. both the assumptions of the Loewe model and the Bliss model are met.")
                ),
                h4("Synergy plots."),
                p(style="text-align: justify;",strong("a:")," Heatmap form the dose response matrix, columns are the concentration of the drug 1, rows the concentration of the drug 2. Numbers inside indicate the % of inhibition. Mean/Median indicate the mean/median percentage inhibition of all the possible combinations for the two drugs."),
                p(style="text-align: justify;",strong("b:")," Heatmap form the Synergy Score (chose by user), columns are the concentration of the drug 1, rows the concentration of the drug 2. Numbers inside indicate the Synergy score."),
                p(style="text-align: justify;",strong("c:")," Summary barplots:"),
                tags$ul(
                  tags$li("concentration drug 1."),
                  tags$li("concentration drug 2."),
                  tags$li("% of inhibition."),
                  tags$li("ZIP score for every concentration drug combination."),
                  tags$li("Loewe score for every concentration drug combination."),
                  tags$li("HSA score for every concentration drug combination."),
                  tags$li("Bliss score for every concentration drug combination.")
                ),
                p(style="text-align: justify;",strong("d:")," Barometer plot, barometer for given concentration 
                                              combination (max ZIP synergy score by concentration 1 and 2) in a matrix. The needle of the barometer 
                                              points to the observed response value. The expected responses from different models are marked as the 
                                              ticks on the color bar. The observed response and the concentration of the combined drugs are tested 
                                              at the center of the barometer.")
                ),
                box(title = h4("Synergy summary:"), collapsible = TRUE, width = 12, collapsed = TRUE,
                p("Summary table with the Synergy scores by every drug concentration."),
                p("Summary heatmap with the Synergy scores by every drug concentration (you can chose how many drugs 
                                              do you want to plot on the heatmap)."),
                p("The concentration and synergy scores ploted by drug is the one that have the highest mean score from all synergy scores by concentration."),
                p("A threshold of the synergy score can be applied using the slider bar.")
                ),
                hr()
                ),
        tabItem(tabName = "contact",
                box(title = h4(tagList(shiny::icon("globe"),strong("Genome Sciences and Cancer Division"))),collapsible = FALSE,width = 12,
                p(
                  h6("The John Curtin School of Medical Research"),
                  h6("131 Garran Road"),
                  h6("The Australian National University"),
                  h6("Acton ACT 2601")),
                  leafletOutput("jcsmrMap")),
                hr(),
                box(title = h4(tagList(shiny::icon("bell"),strong("Contact Us"))), collapsible = FALSE, width = 12,
                p(em(tagList(shiny::icon("user"),"Contact : adria.closamosquera@anu.edu.au"))),
                p(a(tagList(shiny::icon("github"),"GitHub Drug Screening"),href="https://github.com/comprna/drug_screening"))
                )
                )
            )
      ),
  footer = dashboardFooter(left=h5("The Hannan Group"), right = tags$img(src="anu_logo.png",height='40'),fixed = TRUE)),
  
  server = function(input, output, session) {
    
    output$jcsmrMap = renderLeaflet({
      m = leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=149.1149376416087, lat=-35.28187528455249, popup="JCSMR: The John Curtin School of Medical Research")
      m
    })
    
    ref.plate = reactive({
      req(input$names.drugs)
      inFile = input$names.drugs
      if (is.null(inFile)){
        ref.d = ""
      } else {
        ref.d = read.csv(inFile$datapath,header = T)
      }
      ref.d
    })
    
    plate = reactive({
      req(input$upload)
      inFile = input$upload
      if (is.null(inFile)){
        d = ""
      } else {
        d = read.csv(inFile$datapath,header = T)
        #call.drugs = names.drugs[names.drugs$Plate == unique(d$Plate),]
        call.drugs = ref.plate()[ref.plate()$Plate %in% unique(gsub("_Drug","",d$Plate.ID)),]
        call.drugs$New.ID = paste(call.drugs$Plate,call.drugs$Well.ID, sep = "_Drug-")
        d$New.ID = paste(d$Plate.ID,d$Well.ID, sep = "-")
        d = merge(d,call.drugs,by.x="New.ID",by.y = "New.ID", all.x=TRUE)
        d$Plate.ID2 = str_sub(d$Plate.ID,end=-6)
      }
      d
    })
    plate2 = reactive({
      req(input$upload2)
      inFile2 = input$upload2
      if (is.null(inFile2)){
        d2 = ""
      } else {
        d2 = read.csv(inFile2$datapath,header = T)
        #call.drugs = names.drugs[names.drugs$Plate == unique(d$Plate),]
        call.drugs2 = ref.plate()[ref.plate()$Plate %in% unique(gsub("_Veh","",d2$Plate.ID)),]
        call.drugs2$New.ID = paste(call.drugs2$Plate,call.drugs2$Well.ID, sep = "_Veh-")
        d2$New.ID = paste(d2$Plate.ID,d2$Well.ID, sep = "-")
        d2 = merge(d2,call.drugs2,by.x="New.ID",by.y = "New.ID", all.x=TRUE)
        d2$Plate.ID2 = str_sub(d2$Plate.ID,end=-5)
      }
      d2
    })
    
    plate3 = reactive({
      rbind(plate(),plate2())
      
    })
    
    #output$contents <- renderTable({
    #    plate()[,1:5] %>% filter(stringr::str_detect(Plate,as.character(input$ic50)))
    #})
    
    # IC50 plots   
    
    plotInput <- reactive({
      
      #exp = plate3() %>% filter(stringr::str_detect(Plate.ID2,as.character(input$ic50)))
      
      ## by drug
      #exp = plate3() %>% filter(stringr::str_detect(Compound.y,as.character(input$drug)))
      exp = plate3()[plate3()$Compound.y == input$drug,]
      
      #comp = unique(exp$Compound.y)
      comp = unique(plate3()$Compound.y)
      comp[1] = "None"
      
      #exp[exp$Compound.y == "",]$Compound.y = "None"
      
      list.p = list()
      ic50.res = list()
      list.p2 = list()
      ic50.res2 = list()
      #for (i in 2:length(comp)){
      #sub.exp = exp[comp[i] == exp$Compound.y,]
      # estimate mean CTFR for drug and veh
      exp.main = plate3()
      exp.main = exp.main[exp.main$Plate.ID %in% unique(exp$Plate.ID) & exp.main$Well.Type.x == "Negative",]
      
      
      #comp.base.drug = exp[grep("Drug",exp$Plate.ID),]
      comp.base.drug = exp.main[grep("Drug",exp.main$Plate.ID),]
      comp.base.drug = comp.base.drug[comp.base.drug$Well.Type.x == "Negative",]
      
      mean.ctfr = mean(comp.base.drug$Median.CellTrace.FR.A..RL1.A..of.Live.cells)
      
      #comp.base.veh = exp[grep("Veh",exp$Plate.ID),]
      comp.base.veh = exp.main[grep("Veh",exp.main$Plate.ID),]
      comp.base.veh = comp.base.veh[comp.base.veh$Well.Type.x == "Negative",]
      
      mean.ctfr.veh = mean(comp.base.veh$Median.CellTrace.FR.A..RL1.A..of.Live.cells)
      
      #exp = exp[exp$Compound.y %nin% "None",]
      
      #sub.exp = exp[exp$Compound.y == input$drug,]
      sub.exp = exp
      
      sub.exp = sub.exp[grep("_Drug",sub.exp$Plate.ID),]
      sub.comp = unique(sub.exp$Compound.y)
      if (min(sub.exp$X..Live.cells) < 50) {
        ic50.list = list()
        for (j in 1:length(sub.comp)){
          sub.exp1 = sub.exp[sub.exp$Compound.y %in% sub.comp[j],]
          curved_fit <- drm(
            formula = X..Live.cells ~ Concentration.x,
            data = sub.exp1,
            fct = LL.4(names = c("hill", "min_value", "max_value", "ec_50"))
          )
          summary(curved_fit)
          
          coefs <- setNames(
            curved_fit$coefficients,
            c("hill", "min_value", "max_value", "ec_50")
          )
          
          ic_50 <- with(
            as.list(coefs),
            exp(
              log(ec_50) + (1 / hill) * log(max_value / (max_value - 2 * min_value))
            )
          )
          ic50.list[[j]] = log10(ic_50)
        }
      } else { 
        ic50.list = list()
      }
      
      #sub.exp2 = exp[exp$Compound.y == input$drug,]
      sub.exp2 = exp
      
      sub.exp2 = sub.exp2[grep("_Veh",sub.exp2$Plate.ID),]
      sub.comp2 = unique(sub.exp2$Compound.y)
      if (min(sub.exp2$X..Live.cells) < 50) {
        ic50.list2 = list()
        for (j in 1:length(sub.comp2)){
          sub.exp1 = sub.exp2[sub.exp2$Compound.y %in% sub.comp[j],]
          curved_fit <- drm(
            formula = X..Live.cells ~ Concentration.x,
            data = sub.exp1,
            fct = LL.4(names = c("hill", "min_value", "max_value", "ec_50"))
          )
          summary(curved_fit)
          
          coefs <- setNames(
            curved_fit$coefficients,
            c("hill", "min_value", "max_value", "ec_50")
          )
          
          ic_50 <- with(
            as.list(coefs),
            exp(
              log(ec_50) + (1 / hill) * log(max_value / (max_value - 2 * min_value))
            )
          )
          ic50.list2[[j]] = log10(ic_50)
        }
      } else { 
        ic50.list2 = list()
      }
      
      p <- ggplot(
        data = sub.exp, aes(x = log10(Concentration.x),color=Compound.y)
      ) +
        geom_point(aes(y = X..Live.cells),size=3, alpha=0.7) + geom_point(aes(y = (Median.CellTrace.FR.A..RL1.A..of.Live.cells/mean.ctfr-1)*100),shape=10,size=3,alpha=0.7) +
        geom_line( aes(y = X..Live.cells,linetype = "solid")) + geom_line(aes(y= (Median.CellTrace.FR.A..RL1.A..of.Live.cells/mean.ctfr-1)*100,linetype = "longdash")) + 
        scale_y_continuous(name = "% Live Cells",sec.axis = sec_axis(~., name="% Increment Median Cell Trace")) +
        labs(x = paste("Drug Concentration log10(uM)",paste("IC50 ",round(10^mean(unlist(ic50.list)),digits = 4)," uM",sep=""),sep="\n")) +
        geom_vline(xintercept = mean(unlist(ic50.list)),color="grey", linetype="solid") + 
        annotate("text", x=min(log10(sub.exp$Concentration.x))+0.5, y=80, label= paste("IC50 = ",round(mean(unlist(ic50.list)),digits = 2),sep="")) + ggtitle(paste("Drug ",unique(sub.exp$Compound.y),sep="")) + 
        scale_color_manual(name = "Compound",values=input$col1) + 
        scale_linetype_manual(name="Cell data",labels=c("% Increment Median Cell Trace","% Live Cells"),values = rev(c("solid","longdash"))) + theme_minimal() + 
        theme(legend.box = "vertical",rect = element_rect(fill = "white"))
      
      p2 <- ggplot(
        data = sub.exp2, aes(x = log10(Concentration.x),color=Compound.y)
      ) +
        geom_point(aes(y = X..Live.cells),size=3, alpha=0.7) + geom_point(aes(y = (Median.CellTrace.FR.A..RL1.A..of.Live.cells/mean.ctfr.veh-1)*100),shape=10,size=3,alpha=0.7) +
        geom_line( aes(y = X..Live.cells,linetype = "solid")) + geom_line(aes(y= (Median.CellTrace.FR.A..RL1.A..of.Live.cells/mean.ctfr.veh-1)*100,linetype = "longdash")) + 
        scale_y_continuous(name = "% Live Cells",sec.axis = sec_axis(~., name="% Increment Median Cell Trace")) +
        labs(x = paste("Drug Concentration log10(uM)",paste("IC50 ",round(10^mean(unlist(ic50.list2)),digits = 4)," uM",sep=""),sep="\n")) +
        geom_vline(xintercept = mean(unlist(ic50.list2)),color="grey", linetype="solid") + 
        annotate("text", x=min(log10(sub.exp$Concentration.x))+0.5, y=80, label= paste("IC50 = ",round(mean(unlist(ic50.list2)),digits = 2),sep="")) + ggtitle(paste("Veh ",unique(sub.exp2$Compound.y),sep="")) + 
        scale_color_manual(name = "Compound",values=input$col2) + 
        scale_linetype_manual(name="Cell data",labels=c("% Increment Median Cell Trace","% Live Cells"),values = rev(c("solid","longdash"))) + theme_minimal() + 
        theme(legend.box = "vertical",rect = element_rect(fill = "white"))
      p3 = ggarrange(p,p2,legend="bottom", common.legend = TRUE,ncol=2)
      #list.p[[i]] = p
      #ic50.res[[i]] = mean(unlist(ic50.list))
      #}
      #list.plates = ggarrange(plotlist=list.p[2:length(comp)],legend = "bottom",common.legend = TRUE,ncol = 3,nrow=round(length(comp)/3))
      #ic50.plates = unlist(ic50.res)
      #p = list.plates
      
      
    })
    
    output$ic50.plot <- renderPlot({
      #input$submit
      #isolate(grid.draw(plotInput()))
      grid.draw(plotInput())
    }, height = 400,600)
    
    output$download_plot1 <- downloadHandler(
      
      filename = function() { paste0("plot_", Sys.Date(), ".svg") },
      
      content = function(file) {
        
        device <- function(..., width, height) {
          #grDevices::png(..., width = 2200, height = 1200, res = 300, units = "px")
          grDevices::svg(..., width = 10, height = 5, pointsize = 15)}
        
        ggsave(file, plot = plotInput(), device = device,bg="white")
        
        
      })
    
    # IC50 data table
    
    output$ic50.table = DT::renderDataTable({
      #exp = plate3() %>% filter(stringr::str_detect(Plate.ID2,as.character(input$ic50)))
      mother.IC50 = list()
      mother.IC50.veh = list()
      com.list = list()
      for (k in 1:length(unique(plate3()$Plate.ID2))){
        exp = plate3()[plate3()$Plate.ID2 == unique(plate3()$Plate.ID2)[k],]
        #exp = plate()
        #comp = unique(str_split_fixed(exp$Compound,"_",2)[,1])
        comp = unique(exp$Compound.y)
        comp[1] = "None"
        com.list[[k]] = comp[2:length(comp)]
        
        exp[exp$Compound.y == "",]$Compound.y = "None"
        
        # estimate mean CTFR for drug and veh
        comp.base.drug = exp[grep("Drug",exp$Plate.ID),]
        comp.base.drug = comp.base.drug[comp.base.drug$Well.Type.x == "Negative",]
        
        mean.ctfr = mean(comp.base.drug$Median.CellTrace.FR.A..RL1.A..of.Live.cells)
        
        comp.base.veh = exp[grep("Veh",exp$Plate.ID),]
        comp.base.veh = comp.base.veh[comp.base.veh$Well.Type.x == "Negative",]
        
        mean.ctfr.veh = mean(comp.base.veh$Median.CellTrace.FR.A..RL1.A..of.Live.cells)
        
        exp = exp[exp$Compound.y %nin% "None",]
        
        list.p = list()
        ic50.res = list()
        list.p2 = list()
        ic50.res2 = list()
        for (i in 2:length(comp)){
          sub.exp = exp[comp[i] == exp$Compound.y,]
          sub.exp = sub.exp[grep("_Drug",sub.exp$Plate.ID),]
          sub.comp = unique(sub.exp$Compound.y)
          if (min(sub.exp$X..Live.cells) < 50) {
            ic50.list = list()
            for (j in 1:length(sub.comp)){
              sub.exp1 = sub.exp[sub.exp$Compound.y %in% sub.comp[j],]
              curved_fit <- drm(
                formula = X..Live.cells ~ Concentration.x,
                data = sub.exp1,
                fct = LL.4(names = c("hill", "min_value", "max_value", "ec_50"))
              )
              summary(curved_fit)
              
              coefs <- setNames(
                curved_fit$coefficients,
                c("hill", "min_value", "max_value", "ec_50")
              )
              
              ic_50 <- with(
                as.list(coefs),
                exp(
                  log(ec_50) + (1 / hill) * log(max_value / (max_value - 2 * min_value))
                )
              )
              ic50.list[[j]] = log10(ic_50)
            }
          } else { 
            ic50.list = list()
          }
          
          p <- ggplot(
            data = sub.exp, aes(x = log10(Concentration.x),color=Compound.y)
          ) +
            geom_point(aes(y = X..Live.cells),size=3, alpha=0.7) + geom_point(aes(y = Median.CellTrace.FR.A..RL1.A..of.Live.cells/mean.ctfr),shape=10,size=3,alpha=0.7) +
            geom_line( aes(y = X..Live.cells,linetype = "solid")) + geom_line(aes(y= Median.CellTrace.FR.A..RL1.A..of.Live.cells/mean.ctfr,linetype = "longdash")) + 
            scale_y_continuous(name = "% Live Cells",sec.axis = sec_axis(~., name="Median Cell Trace / 10e3")) +
            labs(x = paste("Drug Concentration log10(uM)",paste("IC50 ",round(10^mean(unlist(ic50.list)),digits = 4)," uM",sep=""),sep="\n")) +
            geom_vline(xintercept = mean(unlist(ic50.list)),color="grey", 
                       linetype="solid") + annotate("text", x=-4, y=80, label= paste("IC50 = ",round(mean(unlist(ic50.list)),digits = 2),sep="")) + ggtitle(comp[i]) + scale_color_futurama(name = "Replicates",labels = c("Replicate 1", "Replicate 2", "Replicate 3","Replicate 4")) + scale_linetype_manual(name="Cell data",labels=c("Median Cell Trace","% Live Cells"),values = rev(c("solid","longdash"))) + theme_minimal() + theme(legend.box = "vertical")
          
          list.p[[i]] = p
          ic50.res[[i]] = mean(unlist(ic50.list))
        }
        
        for (i in 2:length(comp)){
          sub.exp = exp[comp[i] == exp$Compound.y,]
          sub.exp = sub.exp[grep("_Veh",sub.exp$Plate.ID),]
          sub.comp = unique(sub.exp$Compound.y)
          if (min(sub.exp$X..Live.cells) < 50) {
            ic50.list = list()
            for (j in 1:length(sub.comp)){
              sub.exp1 = sub.exp[sub.exp$Compound.y %in% sub.comp[j],]
              curved_fit <- drm(
                formula = X..Live.cells ~ Concentration.x,
                data = sub.exp1,
                fct = LL.4(names = c("hill", "min_value", "max_value", "ec_50"))
              )
              summary(curved_fit)
              
              coefs <- setNames(
                curved_fit$coefficients,
                c("hill", "min_value", "max_value", "ec_50")
              )
              
              ic_50 <- with(
                as.list(coefs),
                exp(
                  log(ec_50) + (1 / hill) * log(max_value / (max_value - 2 * min_value))
                )
              )
              ic50.list[[j]] = log10(ic_50)
            }
          } else { 
            ic50.list = list()
          }
          
          p <- ggplot(
            data = sub.exp, aes(x = log10(Concentration.x),color=Compound.y)
          ) +
            geom_point(aes(y = X..Live.cells),size=3, alpha=0.7) + geom_point(aes(y = Median.CellTrace.FR.A..RL1.A..of.Live.cells/mean.ctfr),shape=10,size=3,alpha=0.7) +
            geom_line( aes(y = X..Live.cells,linetype = "solid")) + geom_line(aes(y= Median.CellTrace.FR.A..RL1.A..of.Live.cells/mean.ctfr,linetype = "longdash")) + 
            scale_y_continuous(name = "% Live Cells",sec.axis = sec_axis(~., name="Median Cell Trace / 10e3")) +
            labs(x = paste("Drug Concentration log10(uM)",paste("IC50 ",round(10^mean(unlist(ic50.list)),digits = 4)," uM",sep=""),sep="\n")) +
            geom_vline(xintercept = mean(unlist(ic50.list)),color="grey", 
                       linetype="solid") + annotate("text", x=-4, y=80, label= paste("IC50 = ",round(mean(unlist(ic50.list)),digits = 2),sep="")) + ggtitle(comp[i]) + scale_color_futurama(name = "Replicates",labels = c("Replicate 1", "Replicate 2", "Replicate 3","Replicate 4")) + scale_linetype_manual(name="Cell data",labels=c("Median Cell Trace","% Live Cells"),values = rev(c("solid","longdash"))) + theme_minimal() + theme(legend.box = "vertical")
          
          list.p2[[i]] = p
          ic50.res2[[i]] = mean(unlist(ic50.list))
        }
        
        ic50.plates = as.data.frame(unlist(ic50.res))
        ic50.plates2 = as.data.frame(unlist(ic50.res2))
        mother.IC50[[k]] = ic50.plates
        mother.IC50.veh[[k]] = ic50.plates2
      }
      ic50.plates = do.call(rbind,mother.IC50)
      ic50.plates2 = do.call(rbind,(mother.IC50.veh))
      comp = unlist(com.list)
      #colnames(ic50.plates) = paste(unique(exp$Plate.ID2),"Drug (uM)",sep=" ")
      colnames(ic50.plates) = "Drug (uM)"
      #rownames(ic50.plates) = comp[2:length(comp)]
      #ic50.plates2 = as.data.frame(unlist(ic50.res2))
      #colnames(ic50.plates2) = paste(unique(exp$Plate.ID2),"Veh (uM)",sep=" ")
      colnames(ic50.plates2) ="Veh (uM)"
      #rownames(ic50.plates2) = comp[2:length(comp)]
      ic50.plates = cbind(ic50.plates,ic50.plates2)
      rownames(ic50.plates) = comp
      round(10^(ic50.plates),digits = 4) %>%
        datatable(extensions = 'Buttons',
                  options = list(dom = 'Blfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'print'),
                                 lengthMenu = list(c(10,25,50,-1),
                                                   c(10,25,50,"All"))))
    })
    
    # Synergy plot
    
    plotInput2 = reactive({
      #con.pmr = c(0,3,6,9)
      #syn.df = list()
      #tabledata = plate()
      #tabledata = as.data.frame(tabledata)
      data <- plate3()
      n.plates = unique(plate3()$Plate.ID)
      #for (z in 1:length(n.plates)){
      
      #exp = data[data$Plate %in% n.plates[z],]
      exp = data
      
      comp = unique(str_split_fixed(exp$Compound.y,"_",2)[,1])
      comp[1] = "None"
      
      s.exp = subset(exp)[, c("X..Live.cells", "Plate.ID","Well.Type.x","Compound.y", "Concentration.y","Median.CellTrace.FR.A..RL1.A..of.Live.cells")]
      
      if (input$response == "viability"){
        list.comp.a = list()
        for(i in 2:length(comp)){
          #comp.a = s.exp[grep(paste("^",comp[i],"$",sep=""),s.exp$Compound.y,fixed = FALSE),]
          comp.a = s.exp[s.exp$Compound.y %in% comp[i],]
          comp.base.drug = s.exp[grep("Drug",s.exp$Plate.ID),]
          comp.base.drug = comp.base.drug[comp.base.drug$Well.Type.x == "Negative",]
          mean.drug = mean(comp.base.drug$X..Live.cells)
          comp.base.drug = comp.base.drug[1,]
          comp.base.drug$X..Live.cells = mean.drug
          
          comp.base.veh = s.exp[grep("Veh",s.exp$Plate.ID),]
          comp.base.veh = comp.base.veh[comp.base.veh$Well.Type.x == "Negative",]
          mean.veh = mean(comp.base.veh$X..Live.cells)
          comp.base.veh = comp.base.veh[1,]
          comp.base.veh$X..Live.cells = mean.veh
          comp.a = rbind(comp.a,comp.base.drug,comp.base.veh)
          #comp.null = s.exp[grep("PMR116 only",s.exp$Well.Type),]
          #comp.null$Compound = "PMR116 only"
          #comp.null$Concentration = con.pmr[z]
          #comp.a = rbind(comp.a,comp.null)
          #mean.comp.a = comp.a %>% group_by(Concentration) %>% summarize(var3.mean = mean(X..live.cells))
          #list.comp.a[[i]] = as.data.frame(mean.comp.a)[,2]
          comp.a$Compound.z = input$drug.name
          comp.a$Concentration.z = c(rep(input$drug.con,c(dim(comp.a)[1]-2)/2),rep(0,c(dim(comp.a)[1]-2)/2),input$drug.con,0)
          gg = data.frame(block_id = 1, drug_col = comp.a$Compound.z, drug_row = comp.a$Compound.y,conc_c = comp.a$Concentration.z, conc_r = comp.a$Concentration.y, response = comp.a$X..Live.cells, conc_r_unit = "uM",conc_c_unit = "uM")
          gg[is.na(gg)] = 0
          gg$drug_row <- sub("^$", unique(gg$drug_row)[1], gg$drug_row)
          list.comp.a[[i]] = gg
        }
      } else {
        
        list.comp.a = list()
        for(i in 2:length(comp)){
          #comp.a = s.exp[grep(comp[i],s.exp$Compound.y,fixed = TRUE),]
          comp.a = s.exp[s.exp$Compound.y %in% comp[i],]
          comp.base.drug = s.exp[grep("Drug",s.exp$Plate.ID),]
          comp.base.drug = comp.base.drug[comp.base.drug$Well.Type.x == "Negative",]
          mean.drug = mean(comp.base.drug$X..Live.cells)
          mean.ctfr = mean(comp.base.drug$Median.CellTrace.FR.A..RL1.A..of.Live.cells)
          comp.base.drug = comp.base.drug[1,]
          comp.base.drug$X..Live.cells = mean.drug
          comp.base.drug$Median.CellTrace.FR.A..RL1.A..of.Live.cells = mean.ctfr
          
          comp.base.veh = s.exp[grep("Veh",s.exp$Plate.ID),]
          comp.base.veh = comp.base.veh[comp.base.veh$Well.Type.x == "Negative",]
          mean.veh = mean(comp.base.veh$X..Live.cells)
          mean.ctfr.veh = mean(comp.base.veh$Median.CellTrace.FR.A..RL1.A..of.Live.cells)
          comp.base.veh = comp.base.veh[1,]
          comp.base.veh$X..Live.cells = mean.veh
          comp.base.veh$Median.CellTrace.FR.A..RL1.A..of.Live.cells = mean.ctfr.veh
          comp.a = rbind(comp.a,comp.base.drug,comp.base.veh)
          #comp.null = s.exp[grep("PMR116 only",s.exp$Well.Type),]
          #comp.null$Compound = "PMR116 only"
          #comp.null$Concentration = con.pmr[z]
          #comp.a = rbind(comp.a,comp.null)
          #mean.comp.a = comp.a %>% group_by(Concentration) %>% summarize(var3.mean = mean(X..live.cells))
          #list.comp.a[[i]] = as.data.frame(mean.comp.a)[,2]
          #comp.a$Compound.z = "PMR-116"
          #comp.a$Concentration.z = c(rep(0.4,7),rep(0,7),0.4,0)
          comp.a$Compound.z = input$drug.name
          comp.a$Concentration.z = c(rep(input$drug.con,c(dim(comp.a)[1]-2)/2),rep(0,c(dim(comp.a)[1]-2)/2),input$drug.con,0)
          comp.a$X..CTFR = (c(c(comp.a[grep("Drug",comp.a$Plate.ID),]$Median.CellTrace.FR.A..RL1.A..of.Live.cells / comp.a[comp.a$Well.Type.x == "Negative",]$Median.CellTrace.FR.A..RL1.A..of.Live.cells[1])[-length(c(comp.a[grep("Drug",comp.a$Plate.ID),]$Median.CellTrace.FR.A..RL1.A..of.Live.cells / comp.a[comp.a$Well.Type.x == "Negative",]$Median.CellTrace.FR.A..RL1.A..of.Live.cells[1]))],
                              c(comp.a[grep("Veh",comp.a$Plate.ID),]$Median.CellTrace.FR.A..RL1.A..of.Live.cells / comp.a[comp.a$Well.Type.x == "Negative",]$Median.CellTrace.FR.A..RL1.A..of.Live.cells[1])[-length(c(comp.a[grep("Veh",comp.a$Plate.ID),]$Median.CellTrace.FR.A..RL1.A..of.Live.cells / comp.a[comp.a$Well.Type.x == "Negative",]$Median.CellTrace.FR.A..RL1.A..of.Live.cells[2]))],1,1)-1)*100
          gg = data.frame(block_id = 1, drug_col = comp.a$Compound.z, drug_row = comp.a$Compound.y,conc_c = comp.a$Concentration.z, conc_r = comp.a$Concentration.y, response = comp.a$X..CTFR, conc_r_unit = "uM",conc_c_unit = "uM")
          gg[is.na(gg)] = 0
          gg$drug_row <- sub("^$", unique(gg$drug_row)[1], gg$drug_row)
          list.comp.a[[i]] = gg
        }
        
      }
      #mean.comp = as.data.frame(do.call(cbind,list.comp.a))
      #colnames(mean.comp) = comp[c(2:length(comp))]
      #mean.comp$Concentration = as.data.frame(mean.comp.a)[,1]
      #mean.comp = melt(mean.comp,id.vars = "Concentration")
      #mean.comp$plate = rep(unique(exp$Plate),dim(mean.comp)[1])
      #syn.df[[z]] = mean.comp
      #}
      
      #syn.df.mother = do.call(rbind,syn.df)
      syn.df.mother = do.call(rbind,list.comp.a)
      #syn.df.mother.1 = syn.df.mother[syn.df.mother$Concentration %nin% con.pmr,]
      #syn.df.null = syn.df.mother[syn.df.mother$Concentration %in% con.pmr,]
      #syn.df.null$variable = "PMR116 only"
      #syn.df.null = unique(syn.df.null)
      
      test.vector = c("ZIP_synergy", "Bliss_synergy", "HSA_synergy", "Loewe_synergy")
      m.syn.zip = list()
      df.syn = list()
      lst.syn.df = list()
      #            for (i in 2:length(comp)){
      
      #syn.df.null$Concentration = 0
      #syn.df.null$variable = input$synergy
      #syn.df.null$plate = con.pmr
      
      syn.df.exp = syn.df.mother[syn.df.mother$drug_row == input$synergy,]
      #syn.df.exp = rbind(syn.df.exp,syn.df.null)
      #syn.df.exp$Concentration1 = c(rep(0.0,7),rep(3,7),rep(6,7),rep(9,7),c(0,3,6,9))
      
      # synergy analysis by exp
      #library(synergyfinder)
      
      data = syn.df.exp
      #data$plate = 1
      #data$drug_col = "PMR116"
      #data$conc_c_unit = "uM"
      #data$conc_r_unit = "uM"
      #colnames(data)[1] = "conc_r"
      #colnames(data)[2] = "drug_row"
      #colnames(data)[3] = "response"
      #colnames(data)[4] = "block_id"
      #colnames(data)[5] = "conc_c"
      #data$drug_row = as.character(data$drug_row)
      
      res <- ReshapeData(
        data = data,
        data_type = input$response,
        impute = TRUE,
        impute_method = NULL,
        noise = TRUE,
        seed = 1)
      
      res <- CalculateSynergy(
        data = res,
        method = c("ZIP", "HSA", "Bliss", "Loewe"),
        Emin = NA,
        Emax = NA,
        adjusted = TRUE,
        correct_baseline = "non")
      
      res <- CalculateSensitivity(
        data = res,
        correct_baseline = "non"
      )
      
      df.syn[[i]] = res
      
      # Heatmap
      ht.1 = Plot2DrugHeatmap(
        data = res,
        plot_block = 1,
        drugs = c(1, 2),
        plot_value = "response",
        dynamic = FALSE,
        summary_statistic = c("mean",  "median"),
        text_label_size_scale = 2,
        high_value_color = input$col3,
        low_value_color = input$col4
      )
      
      ht.2 = Plot2DrugHeatmap(
        data = res,
        plot_block = 1,
        drugs = c(1, 2),
        plot_value = input$method,
        dynamic = FALSE,
        summary_statistic = c( "quantile_25", "quantile_75"),
        text_label_size_scale = 2,
        high_value_color = input$col3,
        low_value_color = input$col4
      )
      
      # 2D Contour plot
      cp.1 = Plot2DrugContour(
        data = res,
        plot_block = 1,
        drugs = c(1, 2),
        plot_value = "response",
        dynamic = FALSE,
        summary_statistic = c("mean", "median"),
        high_value_color = input$col3,
        low_value_color = input$col4
      )
      cp.2 = Plot2DrugContour(
        data = res,
        plot_block = 1,
        drugs = c(1, 2),
        plot_value = input$method,
        dynamic = FALSE,
        summary_statistic = c("quantile_25", "quantile_75"),
        high_value_color = input$col3,
        low_value_color = input$col4
      )
      
      # 3D surface plot
      sp.1 = Plot2DrugSurface(
        data = res,
        plot_block = 1,
        drugs = c(1, 2),
        plot_value = "response",
        dynamic = TRUE,
        summary_statistic = c("mean", "quantile_25", "median", "quantile_75"),
        high_value_color = input$col3,
        low_value_color = input$col4
      )
      sp.2 = Plot2DrugSurface(
        data = res,
        plot_block = 1,
        drugs = c(1, 2),
        plot_value = "ZIP_synergy",
        dynamic = TRUE,
        summary_statistic = c("mean", "quantile_25", "median", "quantile_75"),
        high_value_color = input$col3,
        low_value_color = input$col4
      )
      
      con1 = filter(res$synergy_scores,(res$synergy_scores$ZIP_synergy == max(res$synergy_scores$ZIP_synergy)))$conc1[1]
      con2 = filter(res$synergy_scores,(res$synergy_scores$ZIP_synergy == max(res$synergy_scores$ZIP_synergy)))$conc2[1]
      # barplot 
      dr.bar = PlotMultiDrugBar(
        data = res,
        plot_block = 1,
        plot_value = c("response", "ZIP_synergy", "Loewe_synergy", "HSA_synergy", "Bliss_synergy"),
        sort_by = "response",
        highlight_row = c(con1, con2),
        highlight_label_size = 8,
        panel_title_size = 8,
        data_table = T
      )
      
      # barometer plot
      bar.1 = PlotBarometer(
        data = res,
        plot_block = 1,
        plot_concs = c(con1, con2), 
        needle_text_offset = 2.5 # Move the texts below the needle
      )
      
      lst.syn = list(ht.1,ht.2,cp.1,cp.2,sp.1,sp.2,bar.1,dr.bar$plot)
      lst.syn.df = dr.bar$data_table
      names(lst.syn) = c("ht.1","ht.2","cp.1","cp.2","sp.1","sp.2","bar.1","dr.bar")
      m.syn.zip = lst.syn
      
      gga = m.syn.zip[c(1, 2, 8, 7)]
    })
    
    output$synergy.plot <- renderPlot({
      ggarrange(plotlist = plotInput2(),ncol=1, labels="auto")
    }, height = 1000,700)
    
    output$download_plot2 <- downloadHandler(
      
      filename = function() { paste0("plot_", Sys.Date(), ".svg") },
      
      content = function(file) {
        
        device <- function(..., width, height) {
          #grDevices::png(..., width = 2500, height = 4000, res = 300, units = "px")
          grDevices::svg(..., width = 10, height = 12, pointsize = 15)}
        
        ggsave(file, plot = ggarrange(plotlist = plotInput2(),ncol=1, labels="auto"), device = device,bg="white")
        
        
      })
    
    # synergy data table
    
    output$synergy.table = DT::renderDataTable({
      #con.pmr = c(0,3,6,9)
      #syn.df = list()
      #tabledata = plate()
      #tabledata = as.data.frame(tabledata)
      data <- plate3()
      n.plates = unique(plate3()$Plate.ID)
      #for (z in 1:length(n.plates)){
      
      #exp = data[data$Plate %in% n.plates[z],]
      exp = data
      
      comp = unique(str_split_fixed(exp$Compound.y,"_",2)[,1])
      comp[1] = "None"
      
      s.exp = subset(exp)[, c("X..Live.cells", "Plate.ID","Well.Type.x","Compound.y", "Concentration.y","Median.CellTrace.FR.A..RL1.A..of.Live.cells")]
      
      if (input$response == "viability"){
        list.comp.a = list()
        for(i in 2:length(comp)){
          #comp.a = s.exp[grep(comp[i],s.exp$Compound.y,fixed = TRUE),]
          comp.a = s.exp[s.exp$Compound.y %in% comp[i],]
          comp.base.drug = s.exp[grep("Drug",s.exp$Plate.ID),]
          comp.base.drug = comp.base.drug[comp.base.drug$Well.Type.x == "Negative",]
          mean.drug = mean(comp.base.drug$X..Live.cells)
          comp.base.drug = comp.base.drug[1,]
          comp.base.drug$X..Live.cells = mean.drug
          
          comp.base.veh = s.exp[grep("Veh",s.exp$Plate.ID),]
          comp.base.veh = comp.base.veh[comp.base.veh$Well.Type.x == "Negative",]
          mean.veh = mean(comp.base.veh$X..Live.cells)
          comp.base.veh = comp.base.veh[1,]
          comp.base.veh$X..Live.cells = mean.veh
          comp.a = rbind(comp.a,comp.base.drug,comp.base.veh)
          #comp.null = s.exp[grep("PMR116 only",s.exp$Well.Type),]
          #comp.null$Compound = "PMR116 only"
          #comp.null$Concentration = con.pmr[z]
          #comp.a = rbind(comp.a,comp.null)
          #mean.comp.a = comp.a %>% group_by(Concentration) %>% summarize(var3.mean = mean(X..live.cells))
          #list.comp.a[[i]] = as.data.frame(mean.comp.a)[,2]
          comp.a$Compound.z = input$drug.name
          comp.a$Concentration.z = c(rep(input$drug.con,c(dim(comp.a)[1]-2)/2),rep(0,c(dim(comp.a)[1]-2)/2),input$drug.con,0)
          gg = data.frame(block_id = 1, drug_col = comp.a$Compound.z, drug_row = comp.a$Compound.y,conc_c = comp.a$Concentration.z, conc_r = comp.a$Concentration.y, response = comp.a$X..Live.cells, conc_r_unit = "uM",conc_c_unit = "uM")
          gg[is.na(gg)] = 0
          gg$drug_row <- sub("^$", unique(gg$drug_row)[1], gg$drug_row)
          list.comp.a[[i]] = gg
        }
      } else {
        
        list.comp.a = list()
        for(i in 2:length(comp)){
          #comp.a = s.exp[grep(comp[i],s.exp$Compound.y,fixed = TRUE),]
          comp.a = s.exp[s.exp$Compound.y %in% comp[i],]
          comp.base.drug = s.exp[grep("Drug",s.exp$Plate.ID),]
          comp.base.drug = comp.base.drug[comp.base.drug$Well.Type.x == "Negative",]
          mean.drug = mean(comp.base.drug$X..Live.cells)
          mean.ctfr = mean(comp.base.drug$Median.CellTrace.FR.A..RL1.A..of.Live.cells)
          comp.base.drug = comp.base.drug[1,]
          comp.base.drug$X..Live.cells = mean.drug
          comp.base.drug$Median.CellTrace.FR.A..RL1.A..of.Live.cells = mean.ctfr
          
          comp.base.veh = s.exp[grep("Veh",s.exp$Plate.ID),]
          comp.base.veh = comp.base.veh[comp.base.veh$Well.Type.x == "Negative",]
          mean.veh = mean(comp.base.veh$X..Live.cells)
          mean.ctfr.veh = mean(comp.base.veh$Median.CellTrace.FR.A..RL1.A..of.Live.cells)
          comp.base.veh = comp.base.veh[1,]
          comp.base.veh$X..Live.cells = mean.veh
          comp.base.veh$Median.CellTrace.FR.A..RL1.A..of.Live.cells = mean.ctfr.veh
          comp.a = rbind(comp.a,comp.base.drug,comp.base.veh)
          #comp.null = s.exp[grep("PMR116 only",s.exp$Well.Type),]
          #comp.null$Compound = "PMR116 only"
          #comp.null$Concentration = con.pmr[z]
          #comp.a = rbind(comp.a,comp.null)
          #mean.comp.a = comp.a %>% group_by(Concentration) %>% summarize(var3.mean = mean(X..live.cells))
          #list.comp.a[[i]] = as.data.frame(mean.comp.a)[,2]
          comp.a$Compound.z = input$drug.name
          comp.a$Concentration.z = c(rep(input$drug.con,c(dim(comp.a)[1]-2)/2),rep(0,c(dim(comp.a)[1]-2)/2),input$drug.con,0)
          comp.a$X..CTFR = (c(c(comp.a[grep("Drug",comp.a$Plate.ID),]$Median.CellTrace.FR.A..RL1.A..of.Live.cells / comp.a[comp.a$Well.Type.x == "Negative",]$Median.CellTrace.FR.A..RL1.A..of.Live.cells[1])[-length(c(comp.a[grep("Drug",comp.a$Plate.ID),]$Median.CellTrace.FR.A..RL1.A..of.Live.cells / comp.a[comp.a$Well.Type.x == "Negative",]$Median.CellTrace.FR.A..RL1.A..of.Live.cells[1]))],
                              c(comp.a[grep("Veh",comp.a$Plate.ID),]$Median.CellTrace.FR.A..RL1.A..of.Live.cells / comp.a[comp.a$Well.Type.x == "Negative",]$Median.CellTrace.FR.A..RL1.A..of.Live.cells[1])[-length(c(comp.a[grep("Veh",comp.a$Plate.ID),]$Median.CellTrace.FR.A..RL1.A..of.Live.cells / comp.a[comp.a$Well.Type.x == "Negative",]$Median.CellTrace.FR.A..RL1.A..of.Live.cells[2]))],1,1)-1)*100
          gg = data.frame(block_id = 1, drug_col = comp.a$Compound.z, drug_row = comp.a$Compound.y,conc_c = comp.a$Concentration.z, conc_r = comp.a$Concentration.y, response = comp.a$X..CTFR, conc_r_unit = "uM",conc_c_unit = "uM")
          gg[is.na(gg)] = 0
          gg$drug_row <- sub("^$", unique(gg$drug_row)[1], gg$drug_row)
          list.comp.a[[i]] = gg
        }
        
      }
      #mean.comp = as.data.frame(do.call(cbind,list.comp.a))
      #colnames(mean.comp) = comp[c(2:length(comp))]
      #mean.comp$Concentration = as.data.frame(mean.comp.a)[,1]
      #mean.comp = melt(mean.comp,id.vars = "Concentration")
      #mean.comp$plate = rep(unique(exp$Plate),dim(mean.comp)[1])
      #syn.df[[z]] = mean.comp
      #}
      
      #syn.df.mother = do.call(rbind,syn.df)
      syn.df.mother = do.call(rbind,list.comp.a)
      #syn.df.mother.1 = syn.df.mother[syn.df.mother$Concentration %nin% con.pmr,]
      #syn.df.null = syn.df.mother[syn.df.mother$Concentration %in% con.pmr,]
      #syn.df.null$variable = "PMR116 only"
      #syn.df.null = unique(syn.df.null)
      
      test.vector = c("ZIP_synergy", "Bliss_synergy", "HSA_synergy", "Loewe_synergy")
      m.syn.zip = list()
      df.syn = list()
      lst.syn.df = list()
      #            for (i in 2:length(comp)){
      
      #syn.df.null$Concentration = 0
      #syn.df.null$variable = input$synergy
      #syn.df.null$plate = con.pmr
      
      syn.df.exp = syn.df.mother[syn.df.mother$drug_row == input$synergy,]
      #syn.df.exp = rbind(syn.df.exp,syn.df.null)
      #syn.df.exp$Concentration1 = c(rep(0.0,7),rep(3,7),rep(6,7),rep(9,7),c(0,3,6,9))
      
      # synergy analysis by exp
      #library(synergyfinder)
      
      data = syn.df.exp
      #data$plate = 1
      #data$drug_col = "PMR116"
      #data$conc_c_unit = "uM"
      #data$conc_r_unit = "uM"
      #colnames(data)[1] = "conc_r"
      #colnames(data)[2] = "drug_row"
      #colnames(data)[3] = "response"
      #colnames(data)[4] = "block_id"
      #colnames(data)[5] = "conc_c"
      #data$drug_row = as.character(data$drug_row)
      
      res <- ReshapeData(
        data = data,
        data_type = input$response,
        impute = TRUE,
        impute_method = NULL,
        noise = TRUE,
        seed = 1)
      
      res <- CalculateSynergy(
        data = res,
        method = c("ZIP", "HSA", "Bliss", "Loewe"),
        Emin = NA,
        Emax = NA,
        adjusted = TRUE,
        correct_baseline = "non")
      
      res <- CalculateSensitivity(
        data = res,
        correct_baseline = "non"
      )
      
      df.syn[[i]] = res
      
      con1 = filter(res$synergy_scores,(res$synergy_scores$ZIP_synergy == max(res$synergy_scores$ZIP_synergy)))$conc1[1]
      con2 = filter(res$synergy_scores,(res$synergy_scores$ZIP_synergy == max(res$synergy_scores$ZIP_synergy)))$conc2[1]
      # barplot 
      dr.bar = PlotMultiDrugBar(
        data = res,
        plot_block = 1,
        plot_value = c("response", "ZIP_synergy", "Loewe_synergy", "HSA_synergy", "Bliss_synergy"),
        sort_by = "response",
        highlight_row = c(con1, con2),
        highlight_label_size = 8,
        panel_title_size = 8,
        data_table = T
      )
      
      #lst.syn = list(ht.1,ht.2,cp.1,cp.2,sp.1,sp.2,bar.1,dr.bar$plot)
      lst.syn.df = dr.bar$data_table
      
      round(lst.syn.df,digits = 4) %>%
        datatable(extensions = 'Buttons',
                  options = list(dom = 'Blfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'print'),
                                 lengthMenu = list(c(10,25,50,-1),
                                                   c(10,25,50,"All"))))
    })
    
    
    # synergy all drugs table for render plot and table         
    data.syn = reactive({
      
      data <- plate3()
      n.plates = unique(plate3()$Plate.ID)
      
      exp = data
      
      comp = unique(str_split_fixed(exp$Compound.y,"_",2)[,1])
      comp[1] = "None"
      
      s.exp = subset(exp)[, c("X..Live.cells", "Plate.ID","Well.Type.x","Compound.y", "Concentration.y","Median.CellTrace.FR.A..RL1.A..of.Live.cells")]
      
      if (input$response == "viability"){
        list.comp.a = list()
        for(i in 2:length(comp)){
          #comp.a = s.exp[grep(comp[i],s.exp$Compound.y,fixed = TRUE),]
          comp.a = s.exp[s.exp$Compound.y %in% comp[i],]
          comp.base.drug = s.exp[grep("Drug",s.exp$Plate.ID),]
          comp.base.drug = comp.base.drug[comp.base.drug$Well.Type.x == "Negative",]
          mean.drug = mean(comp.base.drug$X..Live.cells)
          comp.base.drug = comp.base.drug[1,]
          comp.base.drug$X..Live.cells = mean.drug
          
          comp.base.veh = s.exp[grep("Veh",s.exp$Plate.ID),]
          comp.base.veh = comp.base.veh[comp.base.veh$Well.Type.x == "Negative",]
          mean.veh = mean(comp.base.veh$X..Live.cells)
          comp.base.veh = comp.base.veh[1,]
          comp.base.veh$X..Live.cells = mean.veh
          comp.a = rbind(comp.a,comp.base.drug,comp.base.veh)
          #comp.null = s.exp[grep("PMR116 only",s.exp$Well.Type),]
          #comp.null$Compound = "PMR116 only"
          #comp.null$Concentration = con.pmr[z]
          #comp.a = rbind(comp.a,comp.null)
          #mean.comp.a = comp.a %>% group_by(Concentration) %>% summarize(var3.mean = mean(X..live.cells))
          #list.comp.a[[i]] = as.data.frame(mean.comp.a)[,2]
          comp.a$Compound.z = input$drug.name
          comp.a$Concentration.z = c(rep(input$drug.con,c(dim(comp.a)[1]-2)/2),rep(0,c(dim(comp.a)[1]-2)/2),input$drug.con,0)
          gg = data.frame(block_id = 1, drug_col = comp.a$Compound.z, drug_row = comp.a$Compound.y,conc_c = comp.a$Concentration.z, conc_r = comp.a$Concentration.y, response = comp.a$X..Live.cells, conc_r_unit = "uM",conc_c_unit = "uM")
          gg[is.na(gg)] = 0
          gg$drug_row <- sub("^$", unique(gg$drug_row)[1], gg$drug_row)
          list.comp.a[[i]] = gg
        }
      } else {
        
        list.comp.a = list()
        for(i in 2:length(comp)){
          #comp.a = s.exp[grep(comp[i],s.exp$Compound.y,fixed = TRUE),]
          comp.a = s.exp[s.exp$Compound.y %in% comp[i],]
          comp.base.drug = s.exp[grep("Drug",s.exp$Plate.ID),]
          comp.base.drug = comp.base.drug[comp.base.drug$Well.Type.x == "Negative",]
          mean.drug = mean(comp.base.drug$X..Live.cells)
          mean.ctfr = mean(comp.base.drug$Median.CellTrace.FR.A..RL1.A..of.Live.cells)
          comp.base.drug = comp.base.drug[1,]
          comp.base.drug$X..Live.cells = mean.drug
          comp.base.drug$Median.CellTrace.FR.A..RL1.A..of.Live.cells = mean.ctfr
          
          comp.base.veh = s.exp[grep("Veh",s.exp$Plate.ID),]
          comp.base.veh = comp.base.veh[comp.base.veh$Well.Type.x == "Negative",]
          mean.veh = mean(comp.base.veh$X..Live.cells)
          mean.ctfr.veh = mean(comp.base.veh$Median.CellTrace.FR.A..RL1.A..of.Live.cells)
          comp.base.veh = comp.base.veh[1,]
          comp.base.veh$X..Live.cells = mean.veh
          comp.base.veh$Median.CellTrace.FR.A..RL1.A..of.Live.cells = mean.ctfr.veh
          comp.a = rbind(comp.a,comp.base.drug,comp.base.veh)
          #comp.null = s.exp[grep("PMR116 only",s.exp$Well.Type),]
          #comp.null$Compound = "PMR116 only"
          #comp.null$Concentration = con.pmr[z]
          #comp.a = rbind(comp.a,comp.null)
          #mean.comp.a = comp.a %>% group_by(Concentration) %>% summarize(var3.mean = mean(X..live.cells))
          #list.comp.a[[i]] = as.data.frame(mean.comp.a)[,2]
          comp.a$Compound.z = input$drug.name
          comp.a$Concentration.z = c(rep(input$drug.con,c(dim(comp.a)[1]-2)/2),rep(0,c(dim(comp.a)[1]-2)/2),input$drug.con,0)
          comp.a$X..CTFR = (c(c(comp.a[grep("Drug",comp.a$Plate.ID),]$Median.CellTrace.FR.A..RL1.A..of.Live.cells / comp.a[comp.a$Well.Type.x == "Negative",]$Median.CellTrace.FR.A..RL1.A..of.Live.cells[1])[-length(c(comp.a[grep("Drug",comp.a$Plate.ID),]$Median.CellTrace.FR.A..RL1.A..of.Live.cells / comp.a[comp.a$Well.Type.x == "Negative",]$Median.CellTrace.FR.A..RL1.A..of.Live.cells[1]))],
                              c(comp.a[grep("Veh",comp.a$Plate.ID),]$Median.CellTrace.FR.A..RL1.A..of.Live.cells / comp.a[comp.a$Well.Type.x == "Negative",]$Median.CellTrace.FR.A..RL1.A..of.Live.cells[1])[-length(c(comp.a[grep("Veh",comp.a$Plate.ID),]$Median.CellTrace.FR.A..RL1.A..of.Live.cells / comp.a[comp.a$Well.Type.x == "Negative",]$Median.CellTrace.FR.A..RL1.A..of.Live.cells[2]))],1,1)-1)*100
          gg = data.frame(block_id = 1, drug_col = comp.a$Compound.z, drug_row = comp.a$Compound.y,conc_c = comp.a$Concentration.z, conc_r = comp.a$Concentration.y, response = comp.a$X..CTFR, conc_r_unit = "uM",conc_c_unit = "uM")
          gg[is.na(gg)] = 0
          gg$drug_row <- sub("^$", unique(gg$drug_row)[1], gg$drug_row)
          list.comp.a[[i]] = gg
        }
        
      }
      
      syn.df.mother = do.call(rbind,list.comp.a)
      
      test.vector = c("ZIP_synergy", "Bliss_synergy", "HSA_synergy", "Loewe_synergy")
      m.syn.zip = list()
      df.syn = list()
      lst.syn.df = list()
      
      all.synergy = list()
      withProgress(message = "Calculating Synergy...", value=0, { 
        
        comp.short = comp[2:length(comp)]
        
        for(i in 1:length(comp.short)){
          n = length(comp.short)
          syn.df.exp = syn.df.mother[syn.df.mother$drug_row == comp.short[i],]
          
          data = syn.df.exp
          
          res <- ReshapeData(
            data = data,
            data_type = input$response,
            impute = TRUE,
            impute_method = NULL,
            noise = TRUE,
            seed = 1)
          
          res <- CalculateSynergy(
            data = res,
            method = c("ZIP", "HSA", "Bliss", "Loewe"),
            Emin = NA,
            Emax = NA,
            adjusted = TRUE,
            correct_baseline = "non")
          
          res <- CalculateSensitivity(
            data = res,
            correct_baseline = "non"
          )
          
          df.syn[[i]] = res
          
          con1 = filter(res$synergy_scores,(res$synergy_scores$ZIP_synergy == max(res$synergy_scores$ZIP_synergy)))$conc1[1]
          con2 = filter(res$synergy_scores,(res$synergy_scores$ZIP_synergy == max(res$synergy_scores$ZIP_synergy)))$conc2[1]
          # barplot 
          dr.bar = PlotMultiDrugBar(
            data = res,
            plot_block = 1,
            plot_value = c("response", "ZIP_synergy", "Loewe_synergy", "HSA_synergy", "Bliss_synergy"),
            sort_by = "response",
            highlight_row = c(con1, con2),
            highlight_label_size = 8,
            panel_title_size = 8,
            data_table = T
          )
          
          #lst.syn = list(ht.1,ht.2,cp.1,cp.2,sp.1,sp.2,bar.1,dr.bar$plot)
          dr.bar$data_table$Drug = rep(comp.short[i],dim(dr.bar$data_table)[1])
          all.synergy[[i]] = dr.bar$data_table
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = paste("Drug synergy", i))
        }
      })
      lst.syn.df = as.data.frame(do.call(rbind,all.synergy))
      #lst.syn.df = lst.syn.df[lst.syn.df$Drug %in% unique(lst.syn.df[rowMeans(lst.syn.df[,c(4:7)]) >= input$threshold,]$Drug),]
    })
    
    # synergy all heatmap
    
    output$picker = renderUI({
      pickerInput("ht.drugs",label = "",choices = unique(data.syn()$Drug), selected = unique(data.syn()$Drug),options = list(`actions-box` = TRUE),multiple=T)
    })
    
    data.ht = reactive({
      lst.syn.df = data.syn()
      max.sum = list()
      for (i in 1:length(unique(lst.syn.df$Drug))){
        sum.drug = lst.syn.df[lst.syn.df$Drug == unique(lst.syn.df$Drug)[i],]
        sum.drug$meanSyn = rowMeans(sum.drug[,c(4:7)])
        max.res = sum.drug[sum.drug$meanSyn == max(sum.drug$meanSyn),]
        max.sum[[i]] = max.res
      }
      
      max.sum = do.call(rbind,max.sum)
      
      max.sum = max.sum[order(max.sum$ZIP_synergy,decreasing = T),]
      max.sum = max.sum[!duplicated(max.sum$Drug),]
      
      ht.table = max.sum[,c(4:7)]
      rownames(ht.table) = max.sum$Drug
      
      # add multiple selector drugs for heatmap drugs.
      ht.table = ht.table[rownames(ht.table) %in% input$ht.drugs,]
      ht.table
      ####
    })
    
    data.max = reactive({
      lst.syn.df = data.syn()
      max.sum = list()
      for (i in 1:length(unique(lst.syn.df$Drug))){
        sum.drug = lst.syn.df[lst.syn.df$Drug == unique(lst.syn.df$Drug)[i],]
        sum.drug$meanSyn = rowMeans(sum.drug[,c(4:7)])
        max.res = sum.drug[sum.drug$meanSyn == max(sum.drug$meanSyn),]
        max.sum[[i]] = max.res
      }
      
      max.sum = do.call(rbind,max.sum)
      
      max.sum = max.sum[order(max.sum$ZIP_synergy,decreasing = T),]
      max.sum = max.sum[!duplicated(max.sum$Drug),]
      
      ht.table = max.sum[,c(4:7)]
      rownames(ht.table) = max.sum$Drug
      
      # add multiple selector drugs for heatmap drugs.
      #ht.table = ht.table[rownames(ht.table) %in% input$ht.drugs,]
      max.sum = max.sum[max.sum$Drug %in% input$ht.drugs,]
      max.sum
      ####
    })
    plotInput3 = reactive({
      #lst.syn.df = data.syn()
      #max.sum = list()
      #for (i in 1:length(unique(lst.syn.df$Drug))){
      #  sum.drug = lst.syn.df[lst.syn.df$Drug == unique(lst.syn.df$Drug)[i],]
      #  max.res = sum.drug[sum.drug$response == max(sum.drug$response),]
      #  max.sum[[i]] = max.res
      #}
      
      #max.sum = do.call(rbind,max.sum)
      
      #max.sum = max.sum[order(max.sum$ZIP_synergy,decreasing = T),]
      
      #ht.table = max.sum[,c(4:7)]
      #rownames(ht.table) = max.sum$Drug
      
      # add multiple selector drugs for heatmap drugs.
      ht.table = data.ht()[rownames(data.ht()) %in% input$ht.drugs & 
                             rownames(data.ht()) %in% rownames(data.ht()[rowMeans(data.ht()) >= input$threshold,]),]
      
      max.sum = data.max()[data.max()$Drug %in% input$ht.drugs & 
                             data.max()$Drug %in% data.max()[rowMeans(data.max()[,c(4:7)]) >= input$threshold,]$Drug,]
      
      
      #ht.table = ht.table[rownames(ht.table) %in% input$ht.drugs,]
      #max.sum = max.sum[max.sum$Drug %in% input$ht.drugs,]
      ####
      
      col_fun = colorRamp2(c(0, 10), c("white", "orange"))
      right_ha = rowAnnotation(concentration = cbind(max.sum$conc1,max.sum$conc2), col = list(concentration=col_fun),
                               annotation_legend_param = list(direction="horizontal"))
      left_ha = rowAnnotation(response = anno_barplot(max.sum$response))
      
      ht1 = Heatmap(ht.table, name = "synergy", column_title = "Scores", row_title = "Drugs", cluster_columns = FALSE, cluster_rows = TRUE, 
                    right_annotation = left_ha, left_annotation = right_ha, heatmap_legend_param = list(direction = "horizontal"))
    })
    
    output$synergy.heatmap = renderPlot({
      
      grid.draw(grid.grabExpr(draw(plotInput3(),heatmap_legend_side = "top")))
      
    }, height = 800,700)
    
    output$download_plot3 <- downloadHandler(
      
      filename = function() { paste0("plot_", Sys.Date(), ".svg") },
      
      content = function(file) {
        
        device <- function(..., width, height) {
          #grDevices::png(..., width = 2500, height = 4000, res = 300, units = "px")
          grDevices::svg(..., width = 9, height = 10, pointsize = 12)}
        
        ggsave(file, plot = grid.draw(grid.grabExpr(draw(plotInput3(),heatmap_legend_side = "top"))), device = device,bg="white")
        
        
      })
    
    # synergy all table
    output$synergy.table.all = DT::renderDataTable({
      
      lst.syn.df = data.syn()
      #lst.syn.df = as.data.frame(do.call(rbind,all.synergy))
      lst.syn.df %>% mutate_if(is.numeric,round,digits = 4) %>%
        datatable(extensions = 'Buttons',
                  options = list(dom = 'Blfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'print'),
                                 lengthMenu = list(c(10,25,50,-1),
                                                   c(10,25,50,"All"))))
    })
    
    
    observe({
      #updateSelectInput(session,"ic50",
      #                       label = "ic50",
      #                       choices= unique(plate3()$Plate.ID2),
      #                       selected = unique(plate3()$Plate.ID2)[1])
      #updatePickerInput(session,"ic50",
      #                  label = "ic50",
      #                  choices= unique(plate3()$Plate.ID2),
      #                  selected = unique(plate3()$Plate.ID2)[1])
      updateSelectInput(session,"drug",
                        label = "Select Drug",
                        choices= unique(plate3()$Compound.y),
                        #choices = unique(plate3()[plate3()$Plate.ID2 %in% input$ic50,]$Compound.y),
                        selected = unique(plate3()$Compound.y)[2])
      #selected = unique(plate3()[plate3()$Plate.ID2 %in% input$ic50,]$Compound.y)[2])
      updateSelectInput(session,"synergy",
                        label = "synergy",
                        choices= unique(str_split_fixed(plate3()$Compound.y,"_",2)[,1])[2:length(unique(str_split_fixed(plate3()$Compound.y,"_",2)[,1]))],
                        selected = unique(str_split_fixed(plate3()$Compound.y,"_",2)[,1])[2:length(unique(str_split_fixed(plate3()$Compound.y,"_",2)[,1]))][1])
      updateSelectInput(session,"method",
                        label = "method",
                        choices= test.vector,
                        selected = test.vector[1])})
    #updatePickerInput(session,"ht.drugs",
    #               label = "Select Drugs",
    #              choices= unique(data.syn()$Drug),
    #             selected = unique(data.syn()$Drug))
    
    
    },
  
)