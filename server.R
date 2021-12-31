library(shiny)

shinyServer(function(input, output, session) {
    sever(
        opacity = 0.75
    )
    Sys.sleep(1) #  something that takes time
    
    waiter_hide()
    
    useSweetAlert()
    
    output$page_title <- renderUI({
        h1(input$nav)
    })
    
    
    #SIDEBAR POPOUT LOGIC----
    pop <- reactiveVal("in")
    
    popout <- function(direction)
    {
        if(direction == "out")
        {
            delay(200, jqui_switch_class("#mainpanel", removeClassName = "col-sm-12", addClassName = "col-sm-8", duration = 1000))
            delay(1250, jqui_show("#sidebar", effect = "blind", duration = 1000))
            pop("in")
            return()
        }
        else(direction == "in")
        {
            delay(200, jqui_hide("#sidebar", effect = "blind", duration = 1000))
            delay(1250, jqui_switch_class("#mainpanel", removeClassName = "col-sm-8", addClassName = "col-sm-12", duration = 1000))
            pop("out")
            return()
            
        }
    }
    
    shinyjs::onclick("sidebarToggle-lg", popout(pop()))
    
    data_output <- reactiveVal()
    
    filedata <- reactive({
        req(input$file1)
        if(input$header == TRUE)
        {
            df <- read.csv(input$file1$datapath,
                           header = TRUE,
                           sep = ",",
                           stringsAsFactors = FALSE)
        }
        else
        {
            df <- read.csv(input$file1$datapath,
                           header = FALSE,
                           sep = ",",
                           stringsAsFactors = FALSE)
        }
        data_output(df)
        return(df)
    })
    
    variable_names_list <- reactive({
        req(input$file1)
        df <- filedata()[ ,sapply(filedata(), is.numeric)]
        return(names(df))
        
    })
    
    
    output$variable_ui_1 <- renderUI({
        validate(need(input$file1, message = "Input a Dataset"))
        selectInput(
            inputId = "id_vars",
            label = "Choose ID Variable",
            choices = names(filedata()),
            selected = 1,
            multiple = FALSE
        )
    })
    
    output$variable_ui_2 <- renderUI({
        req(input$file1)
        selectInput(
            inputId = "rel_vars",
            label = "Choose Relevant Variable(s)",
            choices = variable_names_list(),
            multiple = TRUE
        )
    })
    
    
    output$n_clusters_output <- renderUI({
        req(input$cluster_type)
        if(input$cluster_type == "Choose Your Own")
        {
            selectInput(
                inputId = "n_clusters",
                label = "Number of Clusters",
                choices = 1:10,
                selected = 1,
                multiple = FALSE
            )
        }
        
        
    })
    
    output$datatable_output <- renderDataTable({
        req(input$file1)
        dt<- datatable(data_output(),
                       rownames = FALSE)
        if(input$ClusterBttn)
        {
            dt<- datatable(data_output(),
                           rownames = FALSE, escape = FALSE, selection = 'none')  %>%
                formatStyle('Cluster', target = 'row', backgroundColor = styleEqual(c(1,2,3,4,5), c("#cceff1","#d5ecf9", "#faf1cc", "#fedccd","#fae1fd")))
        }
        return(dt)
    })
    
    graphic_dwnld <- reactiveVal()
    
    output$cluster_plot_output <- renderPlot({
        req(kmean_output())
        plot_output <- fviz_cluster(kmean_output(), data = finaldata(),
                     palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07","#E872F4"),
                     ggtheme = theme_minimal(),
                     main = "Partitioning Clustering Plot"
        )
        graphic_dwnld(plot_output)
        return(plot_output)
    })
    
    
    observeEvent(input$AddnewBttn,{
        showModal(datamodal())
    })
    
    finaldata <- reactive({
        req(input$file1)
        req(input$id_vars)
        req(input$rel_vars)
        df <- data.frame(filedata()[input$id_vars], filedata()[input$rel_vars], row.names = input$id_vars)
        df <- scale(df)
        return(df)
    })
    
    kmean_output <- reactiveVal()
    
    observeEvent(input$ClusterBttn, {
        n_clusters <- NULL
        if(input$cluster_type == "Optimal")
        {
            optimal <- fviz_nbclust(mydata, kmeans, method = "silhouette")
            
            df <- optimal$data
            
            n_clusters <- as.numeric(df %>%
                                   filter(y == max(y)) %>%
                                   select(clusters))
        }
        else
        {
            n_clusters <- input$n_clusters
        }
        kmn <- kmeans(finaldata(), centers = n_clusters, nstart = 50)
        kmean_output(kmn)
        clusterCol <- as.data.frame(kmn$cluster)
        clusterCol$names <- rownames(clusterCol)
        names(clusterCol) <- c("Cluster", input$id_vars)
        dt <- data_output()
        if(is.null(dt$Cluster))
        {
            dt <- merge(dt, clusterCol, by= input$id_vars, all.y=FALSE)
        }
        else
        {
            dt$Cluster <- NULL
            dt <- merge(dt, clusterCol, by= input$id_vars, all.y=FALSE)
            
        }
            
        data_output(dt)
        
        
    })
    
    
    
    output$new_data_output <- renderUI({
        validate(need(input$ClusterBttn, message = "Input a Dataset in Cluster Tool Tab"))
        div(
            div(
                column(12,
                textInput(
                    inputId = "new_id",
                    label = h3(input$id_vars),
                    placeholder = "ie. Name"
                ))
                ,br(),
                lapply(seq_len(length(input$rel_vars)), function(i) {
                    column(3,
                    numericInput(
                        inputId = paste0("rel_var", i),
                        label = h3(input$rel_vars[i]),
                        value = 0
                    )
                    )
                })
            )
        )
    })
    
    cluster_output <- reactiveVal()
    
    observeEvent(input$addnew, {
        
        if(is.null(input$new_id))
        {
            cluster_output(paste("Please enter a value for:", input$id_vars))
        }
        else
        {
            df <- data.frame(filedata()[input$id_vars], filedata()[input$rel_vars], row.names = input$id_vars)
            
            new_list <- NULL
            for (i in 1:length(input$rel_vars)) {
                new_list <- c(new_list, input[[paste0("rel_var",i)]])
            }
            new_list2 <- data.frame()
            new_list <- rbind(new_list2,new_list)

            scale_vec <- lapply(seq_len(length(input$rel_vars)), function(i) {
                sd(df[,i])
            })
            center_vec <- lapply(seq_len(length(input$rel_vars)), function(i) {
                mean(df[,i])
            })
            new_list2 <- scale(new_list, center = center_vec, scale = scale_vec)
            pred.knn <- get.knnx(kmean_output()$center, new_list2, 1)
            cluster_output(paste0("Recommended Cluster for ", input$new_id, ": ", pred.knn$nn.index[[1]]))
        }
    })
    
    output$new_cluster_output <- renderUI({
        req(input$ClusterBttn)
        div(
            br(),
            column(12,
            hr(),
            actionButton(
                inputId = "addnew",
                label = "Add",
                style = "background-color: #1d89ff; color: white;"
            ),
            h3(cluster_output())
            )
        )
    })
    
    download_filename <- reactive({
        paste0(input$download_name,input$download_type)
    })
    
    output$download_btn <- downloadHandler(
        filename = function(){download_filename()},
        content = function(file){
            if(input$download_type == ".csv")
            {
                write.csv(data_output(), file, row.names = FALSE)
            }
            else if(input$download_type == ".pdf" || input$download_type == ".png")
            {
                saveWidget(graphic_dwnld(), "maintable.html")
                webshot("maintable.html", file = file)
            }
            
        }
    )
    
})
