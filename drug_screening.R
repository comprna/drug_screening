#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

# list of test to compute by shiny app
test.vector = c("ZIP_synergy", "Bliss_synergy", "HSA_synergy", "Loewe_synergy")

shinyApp(
    
    ui = fluidPage(theme = shinytheme("simplex"), titlePanel(strong("Drug Screening")),
        sidebarLayout(
            sidebarPanel(
                h2("Drug screening file:"),
                fileInput("upload","Choose a file to upload (csv)",buttonLabel = "Upload...",multiple = FALSE),
                h2("Estimate IC50:"),
                selectInput("ic50", label = "",""),
                helpText("Select plate"),
                h2("Synergy analysis:"),
                h3(em("Drug selection")),
                selectInput("synergy", label = "",""),
                helpText("Select drug"),
                h3(em("Synergy model")),
                selectInput("method", label = "",""),
                helpText("Select synergy score method")
                ),
            mainPanel(tabsetPanel(
                #tableOutput("contents"),
                tabPanel("IC50",br(),downloadButton(outputId = "download_plot1",label="Download plot"),br(),plotOutput("ic50.plot",inline = TRUE),
                        br(),
                        DT::dataTableOutput("ic50.table")),
                tabPanel("Synergy",br(),downloadButton(outputId = "download_plot2",label="Download plot"),br(),plotOutput("synergy.plot",inline = TRUE),
                        br(),
                        DT::dataTableOutput("synergy.table")),
                tabPanel("Info",h2("Analysis description"),
                         h3("Data format:"),
                         p("Upload a CSV file with the following columns."),
                         tags$ul(
                             tags$li("Plate"),
                             tags$li("Well ID"),
                             tags$li("Well Row"),
                             tags$li("Well Column"),
                             tags$li("Compound"),
                             tags$li("Concentration"),
                             tags$li("Count of beads"),
                             tags$li("% live cells"),
                             tags$li("% dead cells"),
                             tags$li("absolute count of live cells per well"),
                             tags$li("absolute count of dead cells per well"),
                             tags$li("Median CellTrace FR (RL1-A) of live cells")
                         ),
                         h3("IC50 calculation:"),
                         p("Estimate IC50 for every drug by plate."),
                         tags$ul(
                             tags$li("% Live Cells"),
                             tags$li("Median Cell trace/10e5")),
                        p("Summary table with the IC50 calculated by drug on uM units."),
                        p("Drugs evaluated are on the rows, plates are on the columns."),
                        h3("Synergy Analysis:"),
                        p("Synergy analysis based on the SynergyFinder Plus package in R from Shuyu Zheng et al. from Reserach Program in system Oncology, Faculty of Medicine, University of Helsinki."),
                        p(a("SynergyFinder",href="https://www.bioconductor.org/packages/release/bioc/html/synergyfinder.html")),
                        p("4 synergy scoring are computed."),
                        tags$ul(
                            tags$li(strong("Highest Single Agent (HSA):"), " states that the expected combination effect equals to the higher effect of individual drugs."),
                            tags$li(strong("Bliss model (Bliss):"), " assumes a stochastic process in which two drugs exert their effects independently, and the expected combination effect can be calculated based on the probability of independent events."),
                            tags$li(strong("Loewe additivity model (Loewe):"), " is based on the assumption that no compound interacts with itself and that two doses from different compounds having the same effect are equivalent."),
                            tags$li(strong("Zero Interaction Potency (ZIP):"), " calculates the expected effect of two drugs under the assumption that they do not potentiate each other, i.e. both the assumptions of the Loewe model and the Bliss model are met.")
                        ),
                        h4("Synergy plots:"),
                        p(strong("a:")," Heatmap form the dose response matrix, columns are the concentration of the drug 1, rows the concentration of the drug 2. Numbers inside indicate the % of inhibition. Mean/Median indicate the mean/median percentage inhibition of all the possible combinations for the two drugs."),
                        p(strong("b:")," Heatmap form the Synergy Score (chose by user), columns are the concentration of the drug 1, rows the concentration of the drug 2. Numbers inside indicate the Synergy score."),
                        p(strong("c:")," Summary barplots:"),
                        tags$ul(
                            tags$li("concentration drug 1."),
                            tags$li("concentration drug 2."),
                            tags$li("% of inhibition."),
                            tags$li("ZIP score for every concentration drug combination."),
                            tags$li("Loewe score for every concentration drug combination."),
                            tags$li("HSA score for every concentration drug combination."),
                            tags$li("Bliss score for every concentration drug combination.")
                        ),
                        p(strong("d:")," Barometer plot, barometer for given concentration combination (max ZIP synergy score by concentration 1 and 2) in a matrix. The needle of the barometer points to the observed response value. The expected responses from different models are marked as the ticks on the color bar. The observed response and the concentration of the combined drugs are tested at the center of the barometer.")
                         )
    )),fluid = TRUE)),
    
    server = function(input, output,session) {
# reactive data table 
        plate = reactive({
            req(input$upload)
            inFile = input$upload
            if (is.null(inFile)){
            d = ""
        } else {
            d = read.csv(inFile$datapath,header = T)
        }
        d
    })

        #output$contents <- renderTable({
        #    plate()[,1:5] %>% filter(stringr::str_detect(Plate,as.character(input$ic50)))
        #})
        
# IC50 plots   
        
        plotInput = reactive({
            
                exp = plate() %>% filter(stringr::str_detect(Plate,as.character(input$ic50)))
                comp = unique(str_split_fixed(exp$Compound,"_",2)[,1])
                comp[1] = "None"
                
                exp[exp$Compound == "",]$Compound = "None"
                exp = exp[exp$Compound %nin% "None",]
                
                list.p = list()
                ic50.res = list()
                for (i in 2:length(comp)){
                    sub.exp = exp[grep(comp[i],exp$Compound),]
                    sub.comp = unique(sub.exp$Compound)
                    if (min(sub.exp$X..live.cells) < 50) {
                        ic50.list = list()
                        for (j in 1:length(sub.comp)){
                            sub.exp1 = sub.exp[sub.exp$Compound %in% sub.comp[j],]
                            curved_fit <- drm(
                                formula = X..live.cells ~ Concentration,
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
                        data = sub.exp, aes(x = log10(Concentration),color=Compound)
                    ) +
                        geom_point(aes(y = X..live.cells),size=3, alpha=0.7) + geom_point(aes(y = Median.CellTrace.FR..RL1.A..of.live.cells/100000),shape=10,size=3,alpha=0.7) +
                        geom_line( aes(y = X..live.cells,linetype = "solid")) + geom_line(aes(y=Median.CellTrace.FR..RL1.A..of.live.cells/100000,linetype = "longdash")) + 
                        scale_y_continuous(name = "% Live Cells",sec.axis = sec_axis(~., name="Median Cell Trace / 10e5")) +
                        labs(x = paste("Drug Concentration log10(uM)",paste("IC50 ",round(10^mean(unlist(ic50.list)),digits = 4)," uM",sep=""),sep="\n")) +
                        geom_vline(xintercept = mean(unlist(ic50.list)),color="grey", 
                                   linetype="solid") + annotate("text", x=0, y=80, label= paste("IC50 = ",round(mean(unlist(ic50.list)),digits = 2),sep="")) + ggtitle(comp[i]) + scale_color_futurama(name = "Replicates",labels = c("Replicate 1", "Replicate 2", "Replicate 3","Replicate 4")) + scale_linetype_manual(name="Cell data",labels=c("Median Cell Trace","% Live Cells"),values = rev(c("solid","longdash"))) + theme_minimal() + theme(legend.box = "vertical",rect = element_rect(fill = "white"))
                    
                    list.p[[i]] = p
                    ic50.res[[i]] = mean(unlist(ic50.list))
                }
                list.plates = ggarrange(plotlist=list.p[2:length(comp)],legend = "bottom",common.legend = TRUE,ncol = 3,nrow=round(length(comp)/3))
                ic50.plates = unlist(ic50.res)
                p = list.plates
            
        })
        
        output$ic50.plot <- renderPlot({
            grid.draw(plotInput())
        }, height = 800,800)
        
        output$download_plot1 <- downloadHandler(
            
            filename = function() { paste0("plot_", Sys.Date(), ".png") },
            
            content = function(file) {
                
                device <- function(..., width, height) {
                    grDevices::png(..., width = 4000, height = 4000, res = 300, units = "px")}
                
                ggsave(file, plot = plotInput(), device = device,bg="white")
                
                
            })
        
# IC50 data table
        
        output$ic50.table = DT::renderDataTable({
            exp = plate() %>% filter(stringr::str_detect(Plate,as.character(input$ic50)))
            comp = unique(str_split_fixed(exp$Compound,"_",2)[,1])
            comp[1] = "None"
            
            exp[exp$Compound == "",]$Compound = "None"
            exp = exp[exp$Compound %nin% "None",]
            
            list.p = list()
            ic50.res = list()
            for (i in 2:length(comp)){
                sub.exp = exp[grep(comp[i],exp$Compound),]
                sub.comp = unique(sub.exp$Compound)
                if (min(sub.exp$X..live.cells) < 50) {
                    ic50.list = list()
                    for (j in 1:length(sub.comp)){
                        sub.exp1 = sub.exp[sub.exp$Compound %in% sub.comp[j],]
                        curved_fit <- drm(
                            formula = X..live.cells ~ Concentration,
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
                    data = sub.exp, aes(x = log10(Concentration),color=Compound)
                ) +
                    geom_point(aes(y = X..live.cells),size=3, alpha=0.7) + geom_point(aes(y = Median.CellTrace.FR..RL1.A..of.live.cells/100000),shape=10,size=3,alpha=0.7) +
                    geom_line( aes(y = X..live.cells,linetype = "solid")) + geom_line(aes(y=Median.CellTrace.FR..RL1.A..of.live.cells/100000,linetype = "longdash")) + 
                    scale_y_continuous(name = "% Live Cells",sec.axis = sec_axis(~., name="Median Cell Trace / 10e5")) +
                    labs(x = paste("Drug Concentration log10(uM)",paste("IC50 ",round(10^mean(unlist(ic50.list)),digits = 4)," uM",sep=""),sep="\n")) +
                    geom_vline(xintercept = mean(unlist(ic50.list)),color="grey", 
                               linetype="solid") + annotate("text", x=0, y=80, label= paste("IC50 = ",round(mean(unlist(ic50.list)),digits = 2),sep="")) + ggtitle(comp[i]) + scale_color_futurama(name = "Replicates",labels = c("Replicate 1", "Replicate 2", "Replicate 3","Replicate 4")) + scale_linetype_manual(name="Cell data",labels=c("Median Cell Trace","% Live Cells"),values = rev(c("solid","longdash"))) + theme_minimal() + theme(legend.box = "vertical")
                
                list.p[[i]] = p
                ic50.res[[i]] = mean(unlist(ic50.list))
            }
            ic50.plates = as.data.frame(unlist(ic50.res))
            colnames(ic50.plates) = paste(unique(exp[,1]),"(uM)",sep=" ")
            rownames(ic50.plates) = comp[2:length(comp)]
            round(10^(ic50.plates),digits = 4) %>%
                datatable(extensions = 'Buttons',
                          options = list(dom = 'Blfrtip',
                                         buttons = c('copy', 'csv', 'excel', 'print'),
                                         lengthMenu = list(c(10,25,50,-1),
                                                           c(10,25,50,"All"))))
        })

# Synergy plot

        plotInput2 = reactive({
            con.pmr = c(0,3,6,9)
            syn.df = list()
            #tabledata = plate()
            #tabledata = as.data.frame(tabledata)
            data <- plate()
            n.plates = unique(plate()$Plate)
            for (z in 1:length(n.plates)){
                
                exp = data[data$Plate %in% n.plates[z],]

                comp = unique(str_split_fixed(exp$Compound,"_",2)[,1])
                comp[1] = "None"
                
                s.exp = subset(exp)[, c("X..live.cells", "Well.Type","Compound", "Concentration")]
                
                list.comp.a = list()
                for(i in 2:length(comp)){
                    comp.a = s.exp[grep(comp[i],s.exp$Compound),]
                    comp.null = s.exp[grep("PMR116 only",s.exp$Well.Type),]
                    comp.null$Compound = "PMR116 only"
                    comp.null$Concentration = con.pmr[z]
                    comp.a = rbind(comp.a,comp.null)
                    mean.comp.a = comp.a %>% group_by(Concentration) %>% summarize(var3.mean = mean(X..live.cells))
                    list.comp.a[[i]] = as.data.frame(mean.comp.a)[,2]
                }
                mean.comp = as.data.frame(do.call(cbind,list.comp.a))
                colnames(mean.comp) = comp[c(2:length(comp))]
                mean.comp$Concentration = as.data.frame(mean.comp.a)[,1]
                mean.comp = melt(mean.comp,id.vars = "Concentration")
                mean.comp$plate = rep(unique(exp$Plate),dim(mean.comp)[1])
                syn.df[[z]] = mean.comp
                }
            
            syn.df.mother = do.call(rbind,syn.df)
            syn.df.mother.1 = syn.df.mother[syn.df.mother$Concentration %nin% con.pmr,]
            syn.df.null = syn.df.mother[syn.df.mother$Concentration %in% con.pmr,]
            syn.df.null$variable = "PMR116 only"
            syn.df.null = unique(syn.df.null)
            
            test.vector = c("ZIP_synergy", "Bliss_synergy", "HSA_synergy", "Loewe_synergy")
            m.syn.zip = list()
            df.syn = list()
            lst.syn.df = list()
#            for (i in 2:length(comp)){

                syn.df.null$Concentration = 0
                syn.df.null$variable = input$synergy
                syn.df.null$plate = con.pmr
                
                syn.df.exp = syn.df.mother.1[syn.df.mother.1$variable == input$synergy,]
                syn.df.exp = rbind(syn.df.exp,syn.df.null)
                syn.df.exp$Concentration1 = c(rep(0.0,7),rep(3,7),rep(6,7),rep(9,7),c(0,3,6,9))
                
                # synergy analysis by exp
                #library(synergyfinder)
                
                data = syn.df.exp
                data$plate = 1
                data$drug_col = "PMR116"
                data$conc_c_unit = "uM"
                data$conc_r_unit = "uM"
                colnames(data)[1] = "conc_r"
                colnames(data)[2] = "drug_row"
                colnames(data)[3] = "response"
                colnames(data)[4] = "block_id"
                colnames(data)[5] = "conc_c"
                data$drug_row = as.character(data$drug_row)
                
                res <- ReshapeData(
                    data = data,
                    data_type = "viability",
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
                    high_value_color = "#FF0000",
                    low_value_color = "#0037ff"
                )
                
                ht.2 = Plot2DrugHeatmap(
                    data = res,
                    plot_block = 1,
                    drugs = c(1, 2),
                    plot_value = input$method,
                    dynamic = FALSE,
                    summary_statistic = c( "quantile_25", "quantile_75"),
                    text_label_size_scale = 2,
                    high_value_color = "#FF0000",
                    low_value_color = "#0037ff"
                )
                
                # 2D Contour plot
                cp.1 = Plot2DrugContour(
                    data = res,
                    plot_block = 1,
                    drugs = c(1, 2),
                    plot_value = "response",
                    dynamic = FALSE,
                    summary_statistic = c("mean", "median"),
                    high_value_color = "#FF0000",
                    low_value_color = "#0037ff"
                )
                cp.2 = Plot2DrugContour(
                    data = res,
                    plot_block = 1,
                    drugs = c(1, 2),
                    plot_value = input$method,
                    dynamic = FALSE,
                    summary_statistic = c("quantile_25", "quantile_75"),
                    high_value_color = "#FF0000",
                    low_value_color = "#0037ff"
                )
                
                # 3D surface plot
                sp.1 = Plot2DrugSurface(
                    data = res,
                    plot_block = 1,
                    drugs = c(1, 2),
                    plot_value = "response",
                    dynamic = TRUE,
                    summary_statistic = c("mean", "quantile_25", "median", "quantile_75"),
                    high_value_color = "#FF0000",
                    low_value_color = "#0037ff"
                )
                sp.2 = Plot2DrugSurface(
                    data = res,
                    plot_block = 1,
                    drugs = c(1, 2),
                    plot_value = "ZIP_synergy",
                    dynamic = TRUE,
                    summary_statistic = c("mean", "quantile_25", "median", "quantile_75"),
                    high_value_color = "#FF0000",
                    low_value_color = "#0037ff"
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
            
            filename = function() { paste0("plot_", Sys.Date(), ".png") },
            
            content = function(file) {
                
                device <- function(..., width, height) {
                    grDevices::png(..., width = 2500, height = 4000, res = 300, units = "px")}
                
                ggsave(file, plot = ggarrange(plotlist = plotInput2(),ncol=1, labels="auto"), device = device,bg="white")
                
                
            })
        
# synergy data table
        
        output$synergy.table = DT::renderDataTable({
            con.pmr = c(0,3,6,9)
            syn.df = list()
            #tabledata = plate()
            #tabledata = as.data.frame(tabledata)
            data <- plate()
            n.plates = unique(plate()$Plate)
            for (z in 1:length(n.plates)){
                
                exp = data[data$Plate %in% n.plates[z],]
                
                comp = unique(str_split_fixed(exp$Compound,"_",2)[,1])
                comp[1] = "None"
                
                s.exp = subset(exp)[, c("X..live.cells", "Well.Type","Compound", "Concentration")]
                
                list.comp.a = list()
                for(i in 2:length(comp)){
                    comp.a = s.exp[grep(comp[i],s.exp$Compound),]
                    comp.null = s.exp[grep("PMR116 only",s.exp$Well.Type),]
                    comp.null$Compound = "PMR116 only"
                    comp.null$Concentration = con.pmr[z]
                    comp.a = rbind(comp.a,comp.null)
                    mean.comp.a = comp.a %>% group_by(Concentration) %>% summarize(var3.mean = mean(X..live.cells))
                    list.comp.a[[i]] = as.data.frame(mean.comp.a)[,2]
                }
                mean.comp = as.data.frame(do.call(cbind,list.comp.a))
                colnames(mean.comp) = comp[c(2:length(comp))]
                mean.comp$Concentration = as.data.frame(mean.comp.a)[,1]
                mean.comp = melt(mean.comp,id.vars = "Concentration")
                mean.comp$plate = rep(unique(exp$Plate),dim(mean.comp)[1])
                syn.df[[z]] = mean.comp
            }
            
            syn.df.mother = do.call(rbind,syn.df)
            syn.df.mother.1 = syn.df.mother[syn.df.mother$Concentration %nin% con.pmr,]
            syn.df.null = syn.df.mother[syn.df.mother$Concentration %in% con.pmr,]
            syn.df.null$variable = "PMR116 only"
            syn.df.null = unique(syn.df.null)
            
            test.vector = c("ZIP_synergy", "Bliss_synergy", "HSA_synergy", "Loewe_synergy")
            m.syn.zip = list()
            df.syn = list()
            lst.syn.df = list()
            #            for (i in 2:length(comp)){
            
            syn.df.null$Concentration = 0
            syn.df.null$variable = input$synergy
            syn.df.null$plate = con.pmr
            
            syn.df.exp = syn.df.mother.1[syn.df.mother.1$variable == input$synergy,]
            syn.df.exp = rbind(syn.df.exp,syn.df.null)
            syn.df.exp$Concentration1 = c(rep(0.0,7),rep(3,7),rep(6,7),rep(9,7),c(0,3,6,9))
            
            # synergy analysis by exp
            #library(synergyfinder)
            
            data = syn.df.exp
            data$plate = 1
            data$drug_col = "PMR116"
            data$conc_c_unit = "uM"
            data$conc_r_unit = "uM"
            colnames(data)[1] = "conc_r"
            colnames(data)[2] = "drug_row"
            colnames(data)[3] = "response"
            colnames(data)[4] = "block_id"
            colnames(data)[5] = "conc_c"
            data$drug_row = as.character(data$drug_row)
            
            res <- ReshapeData(
                data = data,
                data_type = "viability",
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
        
        observe({
            updateSelectInput(session,"ic50",
                                   label = "ic50",
                                   choices= unique(plate()$Plate),
                                   selected = unique(plate()$Plate)[1])
            updateSelectInput(session,"synergy",
                              label = "synergy",
                              choices= unique(str_split_fixed(plate()$Compound,"_",2)[,1])[2:length(unique(str_split_fixed(plate()$Compound,"_",2)[,1]))],
                              selected = unique(str_split_fixed(plate()$Compound,"_",2)[,1])[2:length(unique(str_split_fixed(plate()$Compound,"_",2)[,1]))][1])
            updateSelectInput(session,"method",
                            label = "method",
                            choices= test.vector,
                            selected = test.vector[1])})

    },
    
    options = list(height = 1000, width = 1000)
)

# Run the application 
#shinyApp(ui = ui, server = server)
