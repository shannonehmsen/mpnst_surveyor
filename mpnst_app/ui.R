#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(shinythemes)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinycssloaders)
library(ggplot2)
library(BioCircos)
library(RColorBrewer)





shinyUI(
  navbarPage(fluid =TRUE, position = c("static-top"), 
             
             "MPNST Surveyor", theme = shinytheme("cerulean"),
             
          
             
             
             tabPanel("HOME",
                      
                      setBackgroundColor(
                        color = c("#FFFFFF", "#EFFFFF"),
                        gradient = "linear",
                        direction = "bottom"
                      ),
                      
                    
                      #<link rel="stylesheet" type="text/css" href="footer.css" media="screen"/>
                      
                      #ensures that each header is set as the same style font 
                      
                      tags$head(tags$style('h1 {color: black; font-family: Verdana}')),
                      
                      
                      
                      
                      div(class = "home_page",
                          includeHTML("footer.html")
                      )
                
                   
             ),
             tabPanel(
               
               "SAMPLES",
               
               h2("This data table displays clinical and genomic information about the patients in the GeM cohort:" , style="font-family: Verdana; font-weight: bolder; font-size: 11pt; color: black;"),
               
               br(),
               
               h5("Please use the filter boxes to refine your search.", style="font-family: Verdana; font-size: 11pt; color: black;"),
               
               br(),
               
               div(DT::dataTableOutput("sampletable"), style = 'overflow-x: scroll; padding: 25px; font-size: 84%; width = 62%')
    
             ),
             
             tabPanel("EXPLORE", id="biocircos_panel",
                      
                      
                      fluidRow(sidebarPanel
                      (width=12, id="sidebar_circos_top",
                      
                      h1("Interactive circos plots reporting SNVs, 
											indels, total CNV, minor CN (LOH), and SV calls",
                          style="text-align: center; font-size:26px;"))),
                      
                      
                      ### HTML commands to change the background color of the side panels 
                      
                      
                      tags$head(tags$style(
                            HTML('
                                 #sidebar_circos_top{
                                    background-color: #FFFFFF;
                                 }
                                 
                                 #sidebar_circos {
                                    background-color: #FFFFFF;
                                 }
                                 #wellPanel_circos {
                                    background-color: #FFFFFF
                                 }
                        
                                body, label, input, button, select { 
                                  font-family: "Verdana"; color:black;
                                }')
                                              )),
                      
                      fluidRow(
                        #	 shiny::column(3,
                        sidebarPanel(width=3, id="sidebar_circos",
                                     wellPanel(id="wellPanel_circos",
                                  
                                       uiOutput("donor_choice"),
                                       uiOutput("chr_selection_circos"),
                                  
                                      
                                       
                                       #h3('Interactive circos plots reporting SNVs, indels, total CNV, minor CN (LOH), and SV calls')
                                       #,
                                       #fluidRow(
                                       
                                       #sidebarPanel(width=3,
                                       h5("Circos plot options*",style="font-weight: bold;"),
                                       checkboxInput( inputId='show_patho_indels', label='Show pathogenic indels',value = FALSE),
                                       checkboxInput( inputId='show_nopatho_indels', label='Show non-pathogenic indels',value = FALSE),
                                       checkboxInput( inputId='show_patho', label='Show pathogenic SNVs',value = FALSE),
                                       # "nonsynonymous SNV","stopgain","stoploss","frameshift deletion","frameshift insertion"
                                       checkboxInput( inputId='show_nopatho', label='Show non-pathogenic SNVs',value = FALSE),
                                       # range for distanc
                                       sliderInput(step = 50000,"range_dist", "Range intermutation distance:", 
                                                   min = 0, max = 6000000, value = c(0,1000000)),
                                       
                                       # show genes?
                                       checkboxInput( inputId='show_genes', label='Show gene track',value = TRUE),
                                       
                                       
                                       br(),
                                       br(),
                                    
                                       
                                       h5("Scroll here to view the associated data below", style="color:black; font-family: Verdana;"),
                                       
                                       
                                       br(),
                                       br(),
                                       
                                       h5("*Note: scrolling within the BioCircos chart activates zoom functionality", style="color:black; font-family: Verdana;"),
                                       
                                       br()
                                       # br(),
                                       # 
                                       # actionLink("link_to_reload_biocircos", "Reload BioCircos")
                                    
                                       
                                     )
                        ),
                        mainPanel(width=9,
                                  div(style="display: inline-block;",
                                      dropdownButton(
                                        div(style="display: inline-block;",
                                            tags$h3("Track information (from outside to inside)",style = "display: inline-block;")),
                                        
                                        # cytobands
                                        div( tags$h4(tags$b("Track 1. GRCh38 cytobands"))),
                                        
                                        # intermutation distance
                                        div(tags$h4(tags$b("Track 2. Intermutation distance"),style = "display: inline-block;"),
                                            tags$br(),                                         
                                            tags$h4("This track displays the intermutation distance for:
																																																(i) pathogenic indels (large green dots),
																																																(ii) nonpathogenic indels (small black dots), (iii) pathogenic SNVs (large blue dots), 
																																																and (iv) nonpathogenic SNVs (small dots coloured according to the type of substitution)
																																																and SNVs. 
																																																Pathogenic mutations are defined as 
																																																nonsynonymous, stopgain, stoploss, frameshift deletion,frameshift insertion. 
																																																The y-axis, which can be modified using the slider on the left-hand side, represents the distance to the next mutation in the genome.")),
                                        
                                        # chromothripsis region
                                        tags$br(),  
                                        div(tags$h4(tags$b("Track 2. Chromothripsis regions"),style = "display: inline-block;"),
                                            tags$h4("High-confidence chromothripsis regions are indicated by a yellow bar on the same track as the SNVs and indels.
																																																			Low-confidence regions are indicated with a green bar. Please see the tables below the circos plot for further information.")),
                                        
                                        # CN
                                        tags$br(),  
                                        div(tags$h4(tags$b("Track 3. Total copy number (CN)"),style = "display: inline-block;"),
                                            tags$h4("The total CN values are displayed in black. This track has a light blue background.")),
                                        
                                        # minor CN
                                        tags$br(),  
                                        div(tags$h4(tags$b("Track 4. Minor copy number"),style = "display: inline-block;"),
                                            tags$h4("Loss-of-heterozygosity (LOH) regions 																																		  are shown in red. This track has a grey background.")),
                                        
                                        # genes 
                                        tags$br(),  
                                        div(tags$h4(tags$b("Track 5. Gene annotations"),style = "display: inline-block;"),
                                            tags$h4("Tumor suppressors and oncogenes are displayed in blue and red, respectively.")),
                                        
                                        # SVs 
                                        tags$br(),  
                                        div(tags$h4(tags$b("Track 6. Structural variations. "),style = "display: inline-block;"),
                                            tags$h4("Duplication-like SVs, deletion-like SVs, head-to-head and tail-to-tail inversions 
																																																						are depicted in blue, orange, black, and green, respectively.")),
                                        
                                        # NOTE
                                        tags$br(),  
                                        div(tags$h4(tags$b("NOTE"),style = "display: inline-block;"),
                                            tags$h4("Please note that if you display the SNVs/indels for multiple chromosomes at the same time the plot
																																																									might take a few seconds to load. Please be patient.. Only the chromosomes harboring chromothripsis are shown
																																																									for the selected tumor (other chromosomes can be selected by using the menu on the
												
                                      																																																			left-hand side).")),
                                        
                                        
                                        
                                        circle = TRUE, status = "info", icon = icon("info-circle"), width = "950px",
                                        tooltip = tooltipOptions(title = "Track details",placement="bottom"))),
                                  
                                  #includeHTML("test_igv.html"),
                                  div(style="display: inline-block; padding: 10px;",
                                      dropdownButton(
                                        div( withSpinner(DT::dataTableOutput("table_donor_info")), 
                                             style = "overflow-x: scroll;"),
                                        circle = TRUE, status = "info", icon = icon("user-md"), width = "500px",
                                        tooltip = tooltipOptions(title = "Patient information and clinical data")
                                      )),
                                  
                                  withSpinner(
                                    BioCircosOutput("biocirc", height='900px',width="1300px")
                                    #plotOutput("plot_chromo_gg",height='600px')
                                    #)
                                  ),
                                  #					  div(style="display: inline-block; padding: 0px;",
                                  #		withSpinner(DT::dataTableOutput("table_SNVs"))
                                  #		)
                        ),
                        
                        
                        
                      ),
                      
                     
                      
                      tags$h4("SNVs", style = "font-size:27px; color: black; font-family: Verdana; padding: 20px;"),
                      fluidRow( #sidebarPanel(width=12,
                        shiny::column(12,
                                      
                                      div(
                                        div(
                                          DT::dataTableOutput("table_SNVs"), style = "overflow-x: scroll; padding: 25px; font-size: 83%; width = 70%;")
                                        
                                      ))
                      ),
                      
                      br(),
                      br(),
                      
                      tags$h4("INDELs", style = "font-size:27px; color: black; font-family: Verdana; padding: 20px;"),
                      fluidRow( #sidebarPanel(width=12,
                        shiny::column(12,
                                      
                                      div(
                                        div(
                                          DT::dataTableOutput("table_INDELs"), style = "overflow-x: scroll; padding: 25px; font-size: 83%; width = 70%;")
                                        
                                      ))
                      ),
                      
                      br(),
                      br(),
                      
                      
                      tags$h4("CN Data", style = "font-size:27px; color: black; font-family: Verdana; padding: 20px;"),
                      fluidRow( #sidebarPanel(width=12,
                        shiny::column(12,
                       
                                      
                                      div(
                                        div(
                                          DT::dataTableOutput("table_CN"),  style = "overflow-x: scroll; padding: 25px; font-size: 83%; width = 70%;")
                                        
                                      ))
                      )
                      
          
             ),
             
             
             tabPanel("MUTATIONS",
                      
                      
                      ### HTML appended to modify the background color of the sidepanel
                      
                      tags$head(tags$style(
                        HTML('
                                 #sidebar_mutation {
                                    background-color: #FFFFFF;
                                 }
                                
                        
                                body, label, input, button, select { 
                                  font-family: "Verdana"; color:black;
                                }')
                      )),
                      
                      
                      
                      
                      sidebarLayout(
                        
                        
                        sidebarPanel( width=3, id="sidebar_mutation",
                                      
                                      h1(id="mutation-data-heading", "Mutation Data"),
                                      tags$style(HTML("#mutation-data-heading{color: black;font-family: Verdana; font-size:30px;}")),
                                      
                                      
                                      br(),
                                      
                                      
                                      helpText("This page allows for the exploration of mutation data through INDEL and SNV data"),
                                      
                                      selectInput("var",
                                                  label = "Please click SNV or INDEL to view the data of your choosing: ",
                                                  choices = c("SNV",
                                                              "INDEL"),
                                                
                                                  selected = "SNV"),
                                      
                                    # uiOutput("chr_selection_circos"), 
                                      selectInput("donor_choice_mutation", multiple=TRUE,

                                                  label = "Please choose a donor to view the corresponding data: ",
                                                  
                                                  choices = c(sort(unique(d$donor_unique_id))),
                                                  
                                                  selected = "BCH_001_S4FU683F_S7EH61A2")
                                      
                                      
                        ),
                        
                        
                        
                        
                        
                        mainPanel(
                          
                          div(dataTableOutput("selected_mutation"), style = 'overflow-x: scroll; font-size: 83%; width = 69%')
                          
                          
                        )
                      )
                      
                      
                      
                      
                      
             ),
             tabPanel("cBioPortal",
                      
                    
                             
                             tags$a(
                               href="https://www.cbioportal.org/study/summary?id=mpnst_mskcc", 
                               tags$img(src="cbioportal_logo.png", 
                                       
                                        width="850",
                                        height="310")
                             )
                      
                      
                    
                  
                      
                      ),
             tabPanel("FAQ")
             
  )
  
 
)

