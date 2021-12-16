# This script provides the interface and server functions for an web app
# for predicting assignment of the user-provided patient data set to risk
# clusters (semi-supervised clustering) and for predicting CT abnormalities
# and functional lung impairment following COVID-19

# data and tools ----

    library(plyr)
    library(tidyverse)
    library(stringi)
    library(shinyWidgets)

    source('app_tools.R')


# User interface -----

  ui <- fluidPage(
     
     ## some styling
     
     tags$head(
        tags$style(
           "
            .title {
                background: url('banner.png');
                background-repeat: no-repeat; 
                background-size: cover;
                font-size: 50px;
                color: #223854;
                font-family: Bahnschrift, Verdana, Helvetica;
                text-shadow: 1px 1px #e2e2e2; 
                padding-left: 3%;
                padding-top: 1%; 
                padding-bottom: 0.05%
            }
            
            h2 {
               font-size: 30;
               font-weight: bold; 
               font-family: Bahnschrift, Verdana, Helvetica
               }
            
             h3 {
               font-size: 26;
               font-weight: bold; 
               font-family: Bahnschrift, Verdana, Helvetica
               }
            
            h4 {
               font-size: 22; 
               font-family: Bahnschrift, Verdana, Helvetica
            }
            
            .shiny-text-output {
              font_size: 18, 
              font-family: Bahnschrift, Verdana, Helvetica
            }
            
            .shiny-html-output {
              font_size: 18, 
              font-family: Bahnschrift, Verdana, Helvetica
            }
            
            "
        )
     ),
      
     ## Title panel with the logos and names
     
     titlePanel(title =  HTML("<div class = 'title'>
                                 <strong>CovILD</strong>
                                 COVID-19 pulmonary recovery assessment tools
                                 <img src = '' width = 20%>
                                 <img src = 'mui_logo.png' width = 6%><br/>
                                 <div/>
                                 <hr style = 'height: 5px'>"), 
                windowTitle = 'CovILD Tools'), 
     
    ## Side panel with user's entries.
    ## Contains an upload handler
    
    sidebarLayout(
      
      sidebarPanel(h4('Step 1'),
                   h5(a('Download', href = 'input_template.xlsx', download = NA, target = '_blank'), ' and fill in the data entry form'), 
                   br(), 
                   h4('Step 2'), 
                   h5('Upload the complete data entry form'), 
                   br(), 
                   fileInput(inputId = 'entry_form', 
                             label = 'Choose the entry form file', 
                             multiple = FALSE, 
                             accept = '.xlsx'), 
                   br(), 
                   h4('Step 3 (optional)'),
                   h5('Adjust the Risk Cluster classifier'), 
                   br(), 
                   numericInput(inputId = 'k_input', 
                                label = 'k, nearest neighbors', 
                                value = 20, 
                                min = 3, 
                                max = 100), 
                   br(), 
                   h4('Step 4'), 
                   h5('Launch the analysis'), 
                   br(), 
                   actionButton(inputId = 'launcher', 
                                label = 'Launch'), 
                   width = 3),
      
      ## Main panel to hold the dynamic output
      
      mainPanel(
         tabsetPanel(tabPanel('General information', 
                              h3('Welcome to CovILD pulmonary recovery assessment tools!'), 
                              hr(), 
                              p('The risk modeling app was developed based on the preprint by Sonnweber et al. (DOI: 10.1101/2021.06.22.21259316). 
                                The goal is to provide a rounst and cost-effective screening tool to identify subjects at risk of 
                                incomplete pulmonary recovery (long-term radiological lung abnormalities and 
                                lung function impairment) based on easily accessible clinical, demographic and 
                                biochemical variables during early COVID-19 convalescence.'), 
                              br(), 
                              p(globals$method_text[[1]]), 
                              br(), 
                              p(globals$method_text[[2]]), 
                              br(), 
                              p('The app developers and publication authors carry no responsibility 
                                for correctness of the risk predictions and the app source code. 
                                This tool may not be used for diagnostic purposes. For scientific use only.'), 
                              hr(), 
                              em('By using the application you accept', 
                                 a('the terms of use and licensing', 
                                   href = 'readme.txt')), 
                              br(),
                              HTML("<div style =  'text-align: right'>
                                  <img src = '' width = 80%>
                                  <p>Powered by </p>
                                  <a href = 'http://www.daas.tirol'>
                                  <img src = 'logo_large.png' width = 60 alt = 'daas.tirol'>
                                  </a>
                                  <img src = '' width = 30>
                                   <img src = 'shiny_logo.png' width = 60>
                                   <img src = '' width = 30>
                                   <img src = 'index.png' width = 60></div>")), 
            tabPanel('Risk Clusters', 
                     h3('Frequency of CT lung abnormalities in the Risk Clusters'), 
                     br(), 
                     h4('Pulmonary abnormalities at the 180-day follow-up 
                       in the Risk Clusters in the training cohort'), 
                     br(), 
                     plotOutput('ct_outcome', 
                                width = '80%', 
                                height = '500px'), 
                     br(), 
                     strong('Frequency of the radiological and functional lung findings in the training cohort.'), 
                     p('Statistical significance was assessed with chi-squared test.
                        Numbers of the participants in the Risk Clusters are shown under the plots. 
                        LR: low risk cluster, IR: intermediate risk cluster, HR: high risk cluster'), 
                     hr(), 
                     h3('Risk Cluster prediction'), 
                     br(), 
                     h4('Clustering features in the user-provided test data set'), 
                     br(),
                     plotOutput('heat_map', 
                                width = '90%', 
                                height = '800px'),
                     br(), 
                     strong('Clustering feature frequency in the training and test data set presented as a heat map.'), 
                     p('Prediction of the Risk Cluster assignment in the test patient set was done by 
                        the k-NN label propagation algorithm. The test patients are highlighted. 
                        Numbers of the training subjects in the Risk Clusters are shown under the plots. 
                        LR: low risk cluster, IR: intermediate risk cluster, HR: high risk cluster'), 
                     hr(), 
                     h4('Risk cluster assignment'),
                     br(), 
                     tableOutput('assign_tbl'), 
                     hr(), 
                     downloadButton('download_assign', 
                                    label = 'Download table')), 
            tabPanel('Any CT abnormalities', 
                     h3('Any CT abnormalities at the 180-day follow-up'), 
                     br(), 
                     radioButtons(inputId = 'any_plottype', 
                                  label = NULL, 
                                  choices = c('prediction', 'risk'), 
                                  inline = TRUE), 
                     br(), 
                     plotlyOutput('any_ct_outcome', 
                                  width = '80%', 
                                  height = '600px'), 
                     br(), 
                     strong('Prediction of any CT abnormalities at the 180-day follow-up 
                            by single machine learning classifiers and their ensemble.'), 
                     p('Estimated risk is presented. Each point represents a separate classifier. 
                       Median risk estimates are presented as orange diamonds.'), 
                     hr(), 
                     downloadButton('download_any_ct', 
                                    label = 'Download results'), 
                     br()), 
            tabPanel('Moderate-to-severe CT abnormalities', 
                     h3('Moderate-to-severe CT abnormalities at the 180-day follow-up'), 
                     br(), 
                     radioButtons(inputId = 'severe_plottype', 
                                  label = NULL, 
                                  choices = c('prediction', 'risk'), 
                                  inline = TRUE), 
                     br(), 
                     plotlyOutput('severe_ct_outcome', 
                                  width = '80%', 
                                  height = '600px'), 
                     br(), 
                     strong('Prediction of moderate-to-severe CT abnormalities (CT severity score > 5) at the 180-day follow-up 
                            by single machine learning classifiers and their ensemble.'), 
                     p('Estimated risk is presented. Each point represents a separate classifier. 
                       Median risk estimates are presented as orange diamonds.'), 
                     hr(), 
                     downloadButton('download_severe_ct', 
                                    label = 'Download results'), 
                     br()), 
            tabPanel('Functional lung impairment', 
                     h3('Functional lung impairment at the 180-day follow-up'), 
                     br(), 
                     radioButtons(inputId = 'lufo_plottype', 
                                  label = NULL, 
                                  choices = c('prediction', 'risk'), 
                                  inline = TRUE), 
                     br(), 
                     plotlyOutput('lufo_outcome', 
                                  width = '80%', 
                                  height = '600px'), 
                     br(), 
                     strong('Prediction of functional lung impairment at the 180-day follow-up 
                            by single machine learning classifiers and their ensemble.'), 
                     p('Lung function was deemed impaired when at least one of the following criteria was met: (1) forced
                        vital capacity (FVC) < 80% predicted, (2) forced expiratory volume in 1 second (FEV1) < 80%
                        predicted, FEV1:FVC <70% predicted, total lung capacity (TLC) < 80% predicted or diffusing
                        capacity of carbon monoxide (DLCO) < 80% predicted.'), 
                     p('Estimated risk is presented. Each point represents a separate classifier. 
                       Median risk estimates are presented as orange diamonds.'), 
                     hr(), 
                     downloadButton('download_lufo', 
                                    label = 'Download results'), 
                     br())), width = 9)
      
    )
  )

# Define server logic ----

  server <- function(input, output) {
    
    ## Table with input parameters
    
    inp_param_tbl <- eventReactive(input$launcher, {
      
      file <- input$entry_form
      
      tryCatch(read_input(path = file$datapath), 
               error = function(e) stop('Please provide a completely filled data entry form'))
      
    })
    
    inp_qc <- observe({
      
      faults <- inp_param_tbl()$incomplete_records
      
      if(length(faults) > 0) {
        
        showNotification(paste('There are', 
                               length(faults), 
                               'incomplete patient records in the data entry form. The incomplete records will be skipped from the analysis.'), 
                         duration = NULL, 
                         closeButton = TRUE,
                         type = 'warning')
        
      }
      
    })
    
    ## Cluster prediction
    
    output$ct_outcome <- renderPlot({
      
      clust_patho
      
    })
    
    test_clustr <- reactive({
      
      predict_risk_clust(combi_analysis_object = part_clust$train_clust$clust_obj, 
                         newdata = inp_param_tbl()$clust_tbl, 
                         k = round(input$k_input))
      
    })
    
    output$heat_map <- renderPlot({
      
      plot_risk_clust(train_analysis_object = part_clust$train_clust$clust_obj,
                      test_analysis_object = test_clustr(),
                      interactive = FALSE)
      
    })
    
    output$assign_tbl <- renderTable({
      
      pred_risk_clust_output(test_clustr(), 
                             path = NULL)
      
    }, 
    width = '30%', 
    hover = TRUE)

   output$download_assign <- downloadHandler(
      
      ## defining the filename
      
      filename = function() {
         
         return(paste('risk_clust_assignment_', Sys.Date(), '.xlsx', sep=''))
         
      },
      
      ## calling the saving function
      
      content = function(con) {
         
        pred_risk_clust_output(test_analysis_object = test_clustr(), 
                               path = con)
         
      }
      
   )
   
   ## machine learning, any CT abnormality
   
   any_ct_prediction <- reactive({
     
     predict_ml(ml$models$CT_findings_V3, 
                newdata = inp_param_tbl()$ml_tbl)
     
   })
   
   output$any_ct_outcome <- renderPlotly({
     
     plot_ml(pred_tbl = any_ct_prediction(), 
             interactive = TRUE, 
             plot_type = input$any_plottype)
     
   })
   
   output$download_any_ct <- downloadHandler(
     
     ## defining the filename
     
     filename = function() {
       
       return(paste('any_ct_prediction_', Sys.Date(), '.xlsx', sep=''))
       
     },
     
     ## calling the saving function
     
     content = function(con) {
       
       format_prediction(pred_tbl = any_ct_prediction(), 
                         response_txt = 'Any CT abnormality @180-day FUP', 
                         path = con)
       
     })
     
   ## machine learning, moderate-to-severe abnormality
   
   severe_ct_prediction <- reactive({
     
     predict_ml(ml$models$CTsevabove5_V3, 
                newdata = inp_param_tbl()$ml_tbl)
     
   })
   
   output$severe_ct_outcome <- renderPlotly({
     
     plot_ml(pred_tbl = severe_ct_prediction(), 
             interactive = TRUE, 
             plot_type = input$severe_plottype)
     
   })
   
   output$download_severe_ct <- downloadHandler(
     
     ## defining the filename
     
     filename = function() {
       
       return(paste('mod_severe_ct_prediction_', Sys.Date(), '.xlsx', sep=''))
       
     },
     
     ## calling the saving function
     
     content = function(con) {
       
       format_prediction(pred_tbl = severe_ct_prediction(), 
                         response_txt = 'Moderate-severe CT abnormality @180-day FUP', 
                         path = con)
       
     })
   
   ## machine learning, lufo impairment
   
   lufo_prediction <- reactive({
     
     predict_ml(ml$models$lung_function_impaired_V3, 
                newdata = inp_param_tbl()$ml_tbl)
     
   })
   
   output$lufo_outcome <- renderPlotly({
     
     plot_ml(pred_tbl = lufo_prediction(), 
             interactive = TRUE, 
             plot_type = input$lufo_plottype)
     
   })
   
   output$download_lufo <- downloadHandler(
     
     ## defining the filename
     
     filename = function() {
       
       return(paste('lufo_prediction_', Sys.Date(), '.xlsx', sep=''))
       
     },
     
     ## calling the saving function
     
     content = function(con) {
       
       format_prediction(pred_tbl = lufo_prediction(), 
                         response_txt = 'Functional lung impairment @180-day FUP', 
                         path = con)
       
     })
  
  }

# Run the app ----

  shinyApp(ui = ui, server = server)