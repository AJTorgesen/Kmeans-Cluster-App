#Required Libraries
#1. Authentication---
library(googledrive)
library(googlesheets4)
#2. Data Manipulation---
library(tidyverse)
#3. Shiny---
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(waiter)
library(sever)
library(shinyjqui)
library(DT)
library(rmarkdown)
library(webshot)
library(factoextra)
library(FNN)

if(is.null(webshot:::find_phantom())){webshot::install_phantomjs()}



#https://github.com/rajkstats/git_discoverer_app/blob/master/app.R

#Google Sheets Authentication---

#Store initial token in cache
#Uncomment this line only once
#drive_auth(cache = ".secrets", 
#           email = TRUE)



#Allows those with access to the google sheet to make changes to the website.
#options(gargle_oauth_email = TRUE,
#        gargle_oauth_cache = ".secrets")

#gs4_auth(token = drive_token(),
#         scopes="https://www.googleapis.com/auth/spreadsheets.readonly")

#ssid <- as_sheets_id(drive_get("[Insert Sheetname]"))



#FUNCTIONS----
navbar_page_with_inputs <- function(...) {
    navbar <- shiny::navbarPage(...)
    return(navbar)
}


#UI----
shinyUI(fluidPage(
    
    use_waiter(),
    use_steward(),
    use_sever(),
    #Uncomment before publishing
    waiter_show_on_load(spin_hexdots()),
    useShinyjs(),
    
    # Google Analytics
    #tags$head(includeHTML(("www/google-analytics.html"))),
    
    
    # 1.0.0 HEAD----
    tagList(
        tags$head(HTML("<title>GroupR</title>")),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
        
    ),
    style = "padding:0px;",
    
    # 1.1.0 JUMBOTRON COMPONENT ----
    div(
        class = "container-fluid",
        style = "padding:0;",
        id = "jumbotron",
        div( # component
            class = "jumbotron",
            style = "background-size: cover; margin-bottom:0; background-image:url('jumbotron-01.png');", #include this in style: background-image:url('[insert image here]');
            div(
                class = "jumbotron-ui-box text-default bg-primary bg-default",
                style = "color:white; background-color:rgba(0,0,0,0.5); padding:25px;",
                h1("Group", tags$strong("R"), tags$small("Kmeans Cluster Tool", style = "color: white; font-size: 50%;"), style = "color: white;"),
                br()
            )
        )
    ),
    div(
        id = "app-content",
        style = 'font-weight:100; font-size:initial; font-family: "Open Sans script=all rev=2"',
        
        # 2.0.0 NAVBAR PAGE----
        navbar_page_with_inputs(
            id = "nav",
            # 2.1.0 Application Title----
            title = div(
                # adding share it buttons
                #Put this code wherever you want share buttons to go:
                #<div class="sharethis-inline-share-buttons"></div>
                tags$script(src= '//platform-api.sharethis.com/js/sharethis.js#property=5f36c534bccc40001298d5a5&product=inline-share-buttons',async='async'),
                uiOutput("page_title"),
                tags$style(HTML("
                        @import url('//fonts.googleapis.com/css2?family=Nunito+Sans&display=swap');
                        @import url('//fonts.googleapis.com/css2?family=Lato&display=swap');

                        h1{
                        font-family: 'Nunito Sans', sans-serif;
                        font-size: 30px;
                        font-weight: 400;
                        letter-spacing: -1px;
                        line-height: 1.2;
                        display:contents;
                        color: #eeeeee;
                        }
                        
                        h3{
                        font-family: 'Nunito Sans', sans-serif;
                        font-size: 24px;
                        font-weight: 400;
                        letter-spacing: -1px;
                        line-height: 1.2;
                        display:contents;
                        #color: #008B8B;
                        }
                        
                        body{
                        font-family: 'Lato', sans-serif;
                        }

                        ")) #style closed
                
            ),
            collapsible = TRUE,
            
            theme = shinytheme("paper"),
            
            # 2.2.0 PANEL START: Tools Chart ----
            tabPanel(
                title = "Cluster Tool",
                div(class = "row",
                    div(class = "col-sm-4", id = "sidebarToggle-lg",
                        div(id = "sidebarToggle",
                            span("Sidebar")
                        )
                    )
                ),
                div(class = "row",
                    # 2.2.1 Sidebar Panel ----
                    div(id = "sidebar", class = "col-sm-4",
                        div(class = "well",
                            
                            
                            #2.2.1.0 Data Input ----
                            div(
                                div(
                                    class = "sidebar-titles",
                                    id = "upload_1",
                                    h3("Upload Dataset")
                                ),
                                br(),
                                div(
                                    id = "upload_2",
                                    style = "margin: auto; width: fit-content;",
                                    switchInput(
                                        inputId = "header",
                                        label = "Header?",
                                        value = TRUE,
                                        onLabel = "Yes",
                                        offLabel = "No",
                                        onStatus = "success"),
                                    fileInput(
                                        inputId = "file1",
                                        label = NULL,
                                        multiple = TRUE,
                                        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                                    )
                                )
                            ),
                            br(),
                            # 2.2.1.1 Variable Selector ----
                            div(
                                div(
                                    class = "sidebar-titles",
                                    id = "variables_1",
                                    h3("Choose Relevant Variables")
                                ),
                                br(),
                                div(
                                    id = "variables_2",
                                    style = "margin: auto; width: fit-content;",
                                    uiOutput("variable_ui_1"),
                                    uiOutput("variable_ui_2")
                                )
                            ),
                            br(),
                            # 2.2.1.2 Number of Clusters
                            div(
                                div(
                                    class = "sidebar-titles",
                                    id = "variables_2",
                                    h3("How Would You Like to Cluster the Variables")
                                ),
                                br(),
                                div(
                                    id = "n_clusters_title",
                                    style = "margin: auto; width: fit-content;",
                                    radioGroupButtons(
                                        inputId = "cluster_type",
                                        label = NULL,
                                        choices = c("Optimal", "Choose Your Own"),
                                        selected = "Optimal",
                                        status = "primary",
                                        size = "lg"
                                    ),
                                    uiOutput("n_clusters_output")
                                )
                            ),
                            br(),
                            # 2.2.1.3 Cluster Button ----
                            div(
                                id = "cluster-button",
                                style = "margin: auto; width: fit-content;",
                                actionBttn(
                                    inputId = "ClusterBttn",
                                    label = "Cluster", 
                                    style = "material-flat",
                                    color = "primary"
                                )
                            ),
                            hr(),
                            # 2.2.1.4 Download ----
                            div(
                                div(
                                    class = "sidebar-titles",
                                    id = "download_info_1",
                                    h3("Download Graphic")
                                ),
                                br(),
                                div(
                                    id = "grade_2",
                                    style = "margin: auto; width: fit-content;",
                                    div(
                                        span(strong("File Name:"), style = "float: left; width: 33.333%"),
                                        div(style = "float: left; width: 66.666%",
                                            textInput(
                                                inputId = "download_name",
                                                label = NULL,
                                                value = "untitled"
                                            )
                                        )
                                    ),
                                    div(
                                        span(strong("File Type:"), style = "float: left; width: 33.333%"),
                                        div(style = "float:left; width: 66.666%",
                                            selectInput(
                                                inputId = "download_type",
                                                label = NULL,
                                                choices = c(".csv",".pdf",".png")
                                            )
                                        )
                                    ),
                                    div(style = "margin: auto; width: fit-content;",
                                        downloadBttn(
                                            outputId = "download_btn",
                                            label = "Download",
                                            color = "primary"
                                        )
                                    )
                                )
                            ),
                        )
                    ),
                    # 2.2.2 Main Panel ----
                    div(id = "mainpanel", class = "col-sm-8", style = "float: right; overflow-x: auto;",
                        dataTableOutput("datatable_output"),
                        plotOutput("cluster_plot_output")
                    )
                )
                
                
                
            ),
            # 2.2.x PANEL END: Tools Chart ----
            
            
            # 2.3.0 PANEL START: Definitions ----
            tabPanel(
                title = "New Data",
                h3("Enter new data to determine new point's cluster:"),
                br(),
                br(),
                div(
                    id = "def-panel",
                    uiOutput("new_data_output")
                ),
                br(),br(),
                div(
                    uiOutput("new_cluster_output")
                )
            )
            # 2.3.x PANEL END: Definitions ----
            
            
            
        )
        
        
    )
    
))