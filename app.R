library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(gridExtra)
library(corrplot)
library(caret)
library(plotly)
library(plyr)
#library(ggplot2)
library(gdata)
library(xgboost)
library(prettydoc)
library(magrittr)
library(RColorBrewer)
#library(ROSE)
library(pROC)
library(ggplot2) # to plot
library(gridExtra) # to put more
library(grid)
#install.packages("Ckmeans.1d.dp")
library(Ckmeans.1d.dp)
test <- read.csv('test.csv')
train <- read.csv('train.csv')
#submission <- read.csv('sample_submission.csv')
ks <- function (x) { number_format(accuracy = 1,
                                   scale = 1/1000,
                                   suffix = "k",
                                   big.mark = ",")(x)}
## Response by previous insurance
#train$Gender = as.factor(train$Gender)
#train$Driving_License = as.factor(train$Driving_License)
#train$Region_Code = as.factor(train$Region_Code)
#train$Previously_Insured = as.factor(train$Previously_Insured)
#train$Vehicle_Age = as.factor(train$Vehicle_Age)
#train$Vehicle_Damage = as.factor(train$Vehicle_Damage)
#train$Policy_Sales_Channel = as.factor(train$Policy_Sales_Channel)
In1 <- train %>% mutate(c_resp = factor(Response)) %>%
  mutate(c_resp = revalue(c_resp,c("0"= "Not Interested","1"="Interested")))

In2 <- train %>% mutate(c_resp=factor(Response),Insured=factor(Previously_Insured)) %>%
  mutate(c_resp=revalue(c_resp,c("0"='Not Interested',"1"='Interested')),Insured=revalue(Insured,c("0"='Not Insured',"1"='Insured')))

In3 <- train %>% mutate(c_resp=factor(Response)) %>%
  mutate(c_resp= revalue(c_resp,c("0"="Not Interested","1"="Interested")))

In4 <- train %>% mutate(c_resp=factor(Response)) %>%
  mutate(c_resp= revalue(c_resp,c("0"="Not Interested","1"="Interested")))

In5 <- train %>% mutate(c_resp=factor(Response)) %>%
  mutate(c_resp= revalue(c_resp,c("0"="Not Interested","1"="Interested")))

###############Model##################################################
set.seed(123)
print("XGB Train:")

best_tune <- data.frame (nrounds = 100,max_depth = 8, eta=0.03,
                         gamma=0, colsample_bytree=0.5,
                         min_child_weight=1,subsample=1)
train_data = train
test_data = test
# One hot encoding
dmy <- dummyVars(" ~ .", data = train_data)
train_data <- data.frame(predict(dmy, newdata = train_data))
# Select data & label
train_data <- as.matrix(train_data)
d_ata <- ncol(train_data)
data_train <- train_data[,1:d_ata-1]
label_train <- train_data[,d_ata]
# Transform data
data_train <- as.matrix(data_train)
label_train <- as.numeric(label_train)
#XGB Model
dtrain = xgb.DMatrix(data_train,label=label_train)
machine = xgboost(data= dtrain, objective = "binary:logistic",
                  # paramaters
                  max_depth = best_tune$max_depth,
                  nrounds=10,
                  colsample_bytree = best_tune$colsample_bytree,
                  gamma = best_tune$gamma,
                  min_child_weight = best_tune$min_child_weight,
                  eta = best_tune$eta,
                  subsample = best_tune$subsample,
                  print_every_n = 200,
                  scale_pos_weight=7.15,
                  max_delta_step=1,
                  # others
                  verbose=1,
                  nthread = 4,
                  eval.metric = "auc")
# Encoding Test data & Transform
dmy1 <- dummyVars(" ~ .", data = test_data)
test_data <- data.frame(predict(dmy1, newdata = test_data))
test_data <- as.matrix(test_data)
# Predictions
pred <- predict
################
train_re =  train

dmy1 <- dummyVars(" ~ .", data = train_re[1:11])
new_test_data <- data.frame(predict(dmy1, newdata = train_re[1:11]))
new_test_data <- as.matrix(new_test_data)
new_pred <- predict(machine, new_test_data)

new_pred <-  as.numeric(new_pred > 0.5)

library(caret)
confusionMatrix(factor(new_pred),factor(train_re$Response))

machine$evaluation_log
# Importance plot
# importance_matrix <- xgb.importance(colnames(data_train), model = machine)
# gg <- xgb.ggplot.importance(importance_matrix, rel_to_first = TRUE,xlab="Relative Importance")
# gg + ggplot2::ylab("Relative importance")

#################### APP CODE #####################################################################################3
ui <- dashboardPage(
  
  #Dashboard title
  #dashboardHeader(title = 'CROSS SELLING VEHICLE INSURANCE',titleWidth = 400),
  dashboardHeader(title = span('CROSS SELLING VEHICLE INSURANCE',style = "font-size: 12px"),titleWidth = 290),
  
  dashboardSidebar(width = 290,
                   sidebarMenu(menuItem("EDA & Insights", tabName = "Insightsplots", icon = icon('poll')),
                               menuItem("Plot", tabName = "plots", icon = icon('poll')),
                               
                               menuItem("Predict", tabName = "dash", 
                                        icon = icon('tachometer-alt')),
                               menuItem("Download Predictions", tabName = "download", 
                                        icon = icon('download'))
                               #menuItem(tags$em("Download Predictions",style="font-size:120%"),icon=icon("download"),tabName="download")
                   )),
  #Tabs layout
  dashboardBody(tags$head(tags$style(HTML('.main-header .logo {font-weight: bold;}'))),
                fluidRow(
                  tabItems(
                    #Plots tab content
                    tabItem('plots',
                            #Histogram filter
                            box(status = 'primary', title = 'Filter for the histogram plot',
                                selectInput('num', "Numerical variables:", c('Age', 'Annual_Premium','Vintage')),
                                footer = 'Histogram plot for numerical variables'),
                            #Frecuency plot filter
                            box(status = 'primary', title = 'Filter for the frequency plot',
                                selectInput('cat', 'Categorical variables:', c('Gender', 'Driving_License', 'Vehicle_Age', 'Vehicle_Damage')),
                                footer = 'Frequency plot for categorical variables'),
                            #Boxes to display the plots
                            box(plotOutput('histPlot')),
                            box(plotOutput('freqPlot'))),
                    ###########NEXT TAB##################
                    tabItem('Insightsplots',
                            
                            tags$h2("Understanding what characteristics makes a customer interested/not interested in buying a vehicle insurance", style="font-size:150%"),
                            box(plotOutput('In5Plot'),footer = 'Customers having new vehicles aged between 1-2 years are more interested in buying insurance than others. '),
                            #box(plotOutput('In1Plot'),footer="This shows that our data is imbalanced. Most of the customers are not interested."),
                            box(plotOutput('In2Plot'),footer = 'Customers who previously had a vehicle insurance are less interested in buying the new vehicle insurance. '),
                            box(plotOutput('In3Plot'),footer = 'Customers who had previous damages on their cars are interested in the new vehicle insurance as compared to the customers who did not have any damage before.                                                                                      '),
                            box(plotOutput('In4Plot'),footer = 'The ratio of male to female who are interested in buying vehicle insurance is the relatively close to that of customers not interested in buying. However, there is a slightly higher percentage of males who are interested in buying vehicel insurance.'),
                            
                    ),
                    
                    tabItem(tabName="dash",
                            
                            fluidRow(
                              # A static valueBox
                              valueBox('70%', "Accuracy", icon = icon("credit-card")),
                              valueBox('66%', "Sensitivity", icon = icon("credit-card")),
                              valueBox('92%', "Specificity", icon = icon("credit-card"))),
                            tags$h5("With this shiny prediction app, you can upload your data and get back predictions.
                                  The model is a XGBOOST with ~85% ROC AUC that predicts the probability of a customer to buy 
                                  vehicle insurance.", style="font-size:150%"),
                            tags$h5("To predict using this model, upload test data in csv format by using the button below.", style="font-size:150%"),
                            tags$h5("Then, go to the", tags$span("Download Predictions",style="color:red"),
                                    tags$span("section in the sidebar to  download the predictions."), style="font-size:150%"),
                            
                            column(width = 12,
                                   fileInput('file1', em('Upload test data in csv format ',style="text-align:center;color:blue;font-size:150%"),multiple = FALSE,
                                             accept=c('.csv'))),
                            
                                   dataTableOutput("sample_input_data")
                            
                                   #uiOutput("sample_input_data_heading"),
                                   #DataTableOutput("sample_input_data"),
                                   
                                   # uiOutput("sample_prediction_heading"),
                                   # tableOutput("sample_predictions"),
                                   # br(),
                                   # br(),
                                   # br(),
                                   # br()
                            ,
                            tags$h5("Please upload the file in the below format, keeping the columns in same order", style="font-size:150%"),
                            img(src = "SC.jpg" , height = 140, width = 800)#,
                            #HTML('<img src="Screen.png", height="400px" style="float:right"/>')
                            
                    ),
                    #################NEXT TAB##############
                    tabItem(tabName="download",
                            fluidRow(
                              column(width = 8,
                                     tags$h5("After you upload a test dataset, you can download the predictions in csv format by
                                    clicking the button below.",
                                             style="font-size:200%"),
                                     br()
                              )),
                            fluidRow(
                              
                              column(width = 7,
                                     downloadButton("downloadData", em('Download Predictions',style="text-align:center;color:blue;font-size:150%")),
                                     #plotOutput('plot_predictions')
                              ),
                              column(width = 12,
                                     uiOutput("sample_prediction_heading"),
                                     dataTableOutput("sample_predictions")
                              )
                              
                            ))
                    
                    
                  ))))


server <- function(input, output) {
  options(shiny.maxRequestSize = 800*1024^2)
  output$histPlot <- renderPlot({
    #Column name variable
    num_val = ifelse(input$num == 'Age', 'Age',
                     ifelse(input$num == 'Annual_Premium', 'Annual_Premium',
                            ifelse(input$num == 'Vintage', 'Vintage',
                            )))
    
    #Histogram plot
    ggplot(data = train, aes(x = train[[num_val]]))+
      geom_histogram(stat = "bin", fill = 'steelblue3',
                     color = 'lightgrey')+
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face = 'bold'))+
      labs(title = sprintf('Histogram plot of the variable %s', num_val),
           x = sprintf('%s', input$num),y = 'Frequency')+
      stat_bin(geom = 'text',
               aes(label = ifelse(..count.. == max(..count..), as.character(max(..count..)), '')),
               vjust = -0.6)
  })
  
  output$freqPlot <- renderPlot({
    #Column name variable
    cat_val = ifelse(input$cat == 'Gender', 'Gender',
                     ifelse(input$cat == 'Driving_License', 'Driving_License',
                            ifelse(input$cat == 'Vehicle_Age', 'Vehicle_Age',
                                   ifelse(input$cat == 'Vehicle_Damage', 'Vehicle_Damage',
                                   ))))
    
    #Frequency plot
    ggplot(data =train, aes(x = train[[cat_val]]))+
      geom_bar(stat = 'count', fill = 'mediumseagreen',
               width = 0.5)+
      stat_count(geom = 'text', size = 4,
                 aes(label = ..count..),
                 position = position_stack(vjust = 1.03))+
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face="bold"))+
      labs(title = sprintf('Frequency plot of the variable %s', cat_val),
           x = sprintf('%s', input$cat), y = 'Count')
    
  })
  ##########Insights ###################################  
  output$In1Plot <- renderPlot({
    ggplot(data = In1, aes(c_resp,fill=c_resp)) + 
      geom_bar() +
      scale_y_continuous(labels=ks) +
      labs(title='Customer Response',x='Customer response') + 
      theme(legend.title=element_blank()) #+
    #scale_fill_manual(values= c('#E69F00','#56B4E9'),name='') +
    #theme_classic()
    
  })
  
  
  
  output$In2Plot <- renderPlot({
    ggplot(data = In2, aes(c_resp,fill=Insured)) + 
      geom_bar(position='fill') + 
      scale_y_continuous(labels=percent) + 
      theme(legend.title = element_blank()) + 
      labs(x='Customer response',title='Response by previous insurances')# +
    #scale_fill_manual(values= c('#009E73','#56B4E9')) +
    #theme_classic()
  })
  
  output$In3Plot <- renderPlot({
    ggplot(data = In3, aes(c_resp,fill=Vehicle_Damage)) +
      geom_bar(position='fill') +
      scale_y_continuous(labels=percent) + 
      labs(x='Customer response',title='Response by vehicle history') #+
    #scale_fill_manual(values= c('#999999','#56B4E9'),name="Damaged?") +
    #theme_classic()
    
  })
  
  output$In4Plot <- renderPlot({
    ggplot(data = In4, aes(c_resp,fill=Gender)) +
      geom_bar(position='fill') +
      scale_y_continuous(labels=percent) + 
      labs(x='Customer response',title='Response by gender') #+
    ##scale_fill_manual(values= c('#9999CC','#66CC99'),name="Gender") +
    #theme_classic()
  }) 
  
  output$In5Plot <- renderPlot({
    ggplot(data = In5, aes(c_resp,fill=Vehicle_Age)) +
      geom_bar(position='fill') +
      scale_y_continuous(labels=percent) + 
      labs(x='Customer Response',title='Response by vehicle age')
    #scale_fill_manual(values= c('#D55E00','#F0E442',"#E69F00"),name='Vehicle age') +
    #theme_classic()
  })
  ###############################
  output$sample_input_data = renderDataTable({    # show sample of uploaded data
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      input_data =  readr::read_csv(input$file1$datapath, col_names = TRUE)
      
      #colnames(input_data) = c("Test1", "Test2", "Label")
      
      #input_data$Label = as.factor(input_data$Label )
      
      #levels(input_data$Label) <- c("Failed", "Passed")
      head(input_data)
    }
  },options = list(scrollX = TRUE))
  predictions<-reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      withProgress(message = 'Predictions in progress. Please wait ...', {
        input_data =  readr::read_csv(input$file1$datapath, col_names = TRUE)
        
        #colnames(input_data) = c("Test1", "Test2", "Label")
        
        #input_data$Label = as.factor(input_data$Label )
        
        #levels(input_data$Label) <- c("Failed", "Passed")
        
        #mapped = feature_mapping(input_data)
        
        #df_final = cbind(input_data, mapped)
        dmy1 <- dummyVars(" ~ .", data = input_data)
        input_data <- data.frame(predict(dmy1, newdata = input_data))
        input_data <- as.matrix(input_data)
        prediction = predict(machine, input_data)
        
        input_data_with_prediction = cbind(input_data,prediction )
        input_data_with_prediction
        
      })
    }
  })
  
  output$sample_prediction_heading = renderUI({  # show only if data has been uploaded
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Sample predictions')
    }
  })
  
  output$sample_predictions = renderDataTable({   # the last 6 rows to show
    pred = predictions()
    head(pred)
    
  },options = list(scrollX = TRUE))
  # #Downloading CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("input_data_with_predictions", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(predictions(), file, row.names = FALSE)
    })
}
shinyApp(ui, server)

