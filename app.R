library(dplyr)
library(gridExtra)
library(grid)

source("data/clean_data.R")

gender_options <- c("No Filter", "Female", "Male")
dep_options <- c("No Filter", "has Dependents", "no Dependents")
serv_options <- c("No Filter", "Yes", "No")

ui <- fluidPage(
  
  dashboardPage(
    dashboardHeader(title = "Credit card Demographic"),
    dashboardSidebar( width = 160,
                      sidebarMenu(style = "position: fixed; width:150px;",
                                  selectInput("gender", 
                                              label = "Gender:",
                                              choices = c(gender_options), 
                                              selected = "No Filter"),
                                  selectInput("dependents", 
                                              label = "Dependents:",
                                              choices = c(dep_options), 
                                              selected = "No Filter")
                                  
                      )
                      
    ),
    
    dashboardBody(
      fluidRow(
        box(title = "Credit card Demographic", width = 20, solidHeader = TRUE, status = "primary", color = "#286192",
            box(title = "This will contain the number of customer attrited and existing and according to what the customer chooses the graph will change", width = 20, status = "primary"),
            fluidRow(
              box(align = "center",
                  title = NULL, 
                  width = 12,
                  status = NULL,
                  box(width = 20, plotOutput("plot1", height = "150px")))
            ),
            fluidRow(
              column(width = 4,
                     box(title = "All Customers", width = 20, solidHeader = TRUE, status = "primary",
                         box(title = "This will contain the number of customer in the dataset", width = 100, status = "primary"),
                         box(title = "Number of Customers", width = 20, status = "primary", infoBoxOutput("customerCountNoFilter")),
                        # Info boxes give figures such as percent of total for the given datatypes.
                         ))
              
            )
        )),
      fluidRow(
        box(title = "Card Category", width = 20, solidHeader = TRUE, status = "primary",
            fluidRow(
              tabBox(width = 20, title = "Card Category", side = "right",
                     tabPanel("Card Category",
                              fluidRow(
                                box(width = 20, title = NULL, status = NULL, plotOutput("plot4"))
                              )
                     )
              )
            ),
            fluidRow(
              tabBox(width = 20, title = "Charges", side = "right",
                     tabPanel("Monthly Charges",
                              fluidRow(
                                box(width = 20, title = NULL, status = NULL, plotOutput("plot9"))
                              )
                     ),
                     tabPanel("Total Charges",
                              fluidRow(
                                box(width = 20, title = NULL, status = NULL, plotOutput("plot8"))
                              )
                     )
              )
            ),
            column(width = 4,
                   box(title = "All Customers", width = 20, solidHeader = TRUE, status = "primary",
                       box(title = "Total Monthly Charges", width = 20, status = "primary", infoBoxOutput("totalMonthlyChargesNoFilter")),
                       box(title = "Average Monthly Charges", width = 20, status = "primary", infoBoxOutput("avgMonthlyChargesNoFilter"))               ))
            
        )),
      fluidRow(
        box(title = "Months on book", width = 20, solidHeader = TRUE, status = "primary",
            fluidRow(
              tabBox(width = 20, title = "Months on book", side = "right",
                     tabPanel("Histogram",
                              fluidRow(
                                box(width = 12, title = "Months with Company", plotOutput("plot2"))
                              )
                     ),
                     tabPanel("Density Chart",
                              fluidRow(
                                box(width = 12, title = "Months with Company", plotOutput("plot5"))
                              )
                     )
              )
            ),
        )),
      fluidRow(
        box(title = "Education Level", width = 20, solidHeader = TRUE, status = "primary",
            fluidRow(
              tabBox(width = 20, title = "Type of education", side = "right",
                     tabPanel("popular education",
                              fluidRow(
                                box(width = 12, title = "popular education", plotOutput("plot12"))
                              )
                     ),
                     tabPanel("popular education",
                              fluidRow(
                                box(width = 12, title = "popular education", plotOutput("plot11"))
                              )
                     )
                     
              )
            )
        ))
    )
  )
)

server <- function(input, output) {
  # it is a reactive value, which means that it is updated every time the server runs
  rv = reactiveValues()
  rv$dataset=dataset
  
  observe({
    rv$dataset = dataset %>% filter(if(input$gender == 'No Filter'){gender %in% gender_options} else {gender == input$gender}) %>%
      filter(if(input$dependents == 'No Filter'){Dependents %in% dep_options} else {Dependents == input$dependents}) 
    
    rv$churn = filter(rv$dataset, Churn == 'Churn')
    rv$stayed = filter(rv$dataset, Churn == 'Stayed')
    
    rv$dim = dim(rv$dataset)
    rv$dimStayed = dim(rv$stayed)
    rv$dimChurn = dim(rv$churn)
  })
  
  output$customerCountNoFilter <- renderInfoBox({
    x = format(rv$dim[1], big.mark = ",")
    infoBox("Customer Count", x,
            color = "light-blue",
            icon = icon("users")
    )
  })
  output$percentTotalNoFilter <- renderInfoBox({
    x = format(round(rv$dim[1]/dim(dataset)*100, 2))
    infoBox( "Percent of Customers", paste(x, "%"),
             color = "light-blue",
             icon = icon("percentage")
    )
  })
  output$totalMonthlyChargesNoFilter <- renderInfoBox({
    x = format(round(sum(rv$dataset$MonthlyCharges),2), big.mark = ",")
    infoBox( "Total Monthly", paste("$",x),
             color = "light-blue",
             icon = icon("dollar-sign")
    )
  })
  output$avgMonthlyChargesNoFilter <- renderInfoBox({
    x = format(round(mean(rv$dataset$MonthlyCharges),2))
    infoBox("Average Monthly", paste("$",x),
            color = "light-blue",
            icon = icon("dollar-sign")
    )
  })
  output$avgMonthsNoFilter <- renderInfoBox({
    x = format(round(mean(rv$dataset$tenure),2))
    infoBox("Avg Months", paste(x, "months"),
            color = "light-blue",
            icon = icon("calendar-alt")
    )
  })
  
  ### Plots  
  output$plot1 <- renderPlot({
    
    plot1 <- ggplot(rv$dataset, aes(x = Churn, fill = Churn)) +
      geom_bar(position = "dodge", fill = c("#f0ad4e", "#5cb85c"), color = c("#ba7412", "#376e37")) +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$dataset)*100,2), "%")),
                stat = "count",
                position = 'dodge',
                hjust = -0.5,
                size = 3,
                inherit.aes = TRUE) +
      theme_minimal() +
      theme(legend.position = "none",
            axis.title=element_blank(),
            panel.border=element_blank())
    
    plot1
  })
  
  output$plot2 <- renderPlot({
    bar_fills <- c("orange", "#5cb85c")
    plot2 <- ggplot(rv$dataset, aes(x=tenure, fill= Churn))+
      geom_histogram(stat = 'bin',
                     bins = 50,
                     position = "dodge") +
      scale_fill_manual(values = bar_fills,
                        guide = "none") +
      xlab('Number of Months') +
      ylab('Number of customer') +
      theme_minimal()
    plot2
  })
  #
  output$plot4 <- renderPlot({
    
    p1 <- ggplot(rv$dataset, aes(x=PaymentMethod, fill = factor(PaymentMethod)))+
      geom_bar(position = "dodge",  fill = c("#99bcdb","#5b94c5","#337ab7","#286192"), color = "#1e496d") +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$dataset)*100,2), "%")),
                stat = "count", position = "dodge", hjust = -0.1) +
      labs(title = "All Customers", fill = '')+
      theme_minimal() +
      theme(legend.position = "none",
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    
    plot4 <- grid.arrange(p1, ncol = 1)
    plot4
  })
  
  output$plot5 <- renderPlot({
    bar_fills <- c("orange", "#5cb85c")
    plot5 <- ggplot(rv$dataset, aes(tenure, fill = Churn)) +
      geom_density(aes(alpha = 1/100)) +
      scale_fill_manual(values = bar_fills,
                        guide = "none") +
      xlab('Number of Months') +
      theme_minimal()
    
    plot5
  })
  
  output$plot8 <- renderPlot({
    plot8 <- ggplot(rv$dataset, aes(Churn %in% c("Stayed", "Churn"))) +
      geom_point(aes(x = tenure, y = TotalCharges, color= Churn)) +
      scale_color_manual(values =  c("orange", "#5cb85c")) +
      xlab("Months with company")+
      ylab("Total Charges (USD)") +
      theme_minimal()
    plot8
  })
  output$plot9 <- renderPlot({
    plot9 <- ggplot(rv$dataset, aes(Churn %in% c("Stayed", "Churn"))) +
      geom_point(aes(x = tenure, y = MonthlyCharges, color= Churn)) +
      scale_color_manual(values =  c("orange", "#5cb85c")) +
      xlab("Months with company")+
      ylab("Monthly Charges (USD)") +
      theme_minimal()
    plot9
  })
  
  output$plot11 <- renderPlot({
    p1 <- ggplot(rv$dataset, aes(x=PhoneServ, fill = factor(PhoneServ)))+
      geom_bar(position = "dodge",  fill = c("#99bcdb","#5b94c5","#286192"), color = "#1e496d") +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$dataset)*100,2), "%")),
                stat = "count", position = "dodge", hjust = -0.1) +
      labs(title = "All Customers", fill = '')+
      theme_minimal() +
      theme(legend.position = "none",
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    
    plot11 <- grid.arrange(p1, ncol = 1)
    plot11
  })
  output$plot12 <- renderPlot({
    p1 <- ggplot(rv$dataset, aes(x=Streaming, fill = factor(Streaming)))+
      geom_bar(position = "dodge",  fill = c("#99bcdb","#5b94c5","#337ab7","#286192"), color = "#1e496d") +
      coord_flip() +
      geom_text(aes(label = paste(round(..count../nrow(rv$dataset)*100,2), "%")),
                stat = "count", position = "dodge", hjust = -0.1) +
      labs(title = "All Customers", fill = '')+
      theme_minimal() +
      theme(legend.position = "none",
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    
    plot12 <- grid.arrange(p1, ncol = 1)
    plot12
  })
}

# Run the application 
shinyApp(ui, server)