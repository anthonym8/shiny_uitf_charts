
library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)
library(zoo)


rm(list = ls())
source("./functions/get_fund_matrix.R")
source("./functions/clean_fund_matrix.R")
source("./functions/get_navpu.R")
source("./functions/compute_rsi.R")
source("./functions/compute_macd.R")

analytics_prices <- data_frame()

shinyServer(function(input, output, session) {
    
    # Pull Fund Matrix at startup
    withProgress(message = 'Loading UITF data...', value = 0.4, {
        fund_matrix <- get_fund_matrix()
        incProgress(0.4)
        fund_matrix <- clean_fund_matrix(fund_matrix)
        incProgress(0.4)
    })
    
    
    
    # FUND MATRIX TAB
    
    # Render fund matrix table
    output$fund_matrix <- DT::renderDataTable({
        fund_matrix %>% 
            select(Bank, `Fund Name`, Classification, 
                   `Risk Classification`, Currency) %>% 
            datatable(rownames = FALSE,
                      filter = "top",
                      extensions = "Scroller",
                      class = "stripe hover nowrap cell-border",
                      selection = "none",
                      options = list(
                          pageLength = 10,
                          autoWidth = FALSE,
                          dom = "t",
                          scrollY = 400,
                          scroller = TRUE
                      ))
    })
    
    
    # ANALYTICS TAB
    
    # reactive values
    a_fund_selected <- reactive({ filter(fund_matrix, complete_name == input$analytics_select_fund) })
    a_inception_date <- reactive({ mdy(a_fund_selected()[["Inception Date"]]) })
    
    # Update choices on fund selection
    updateSelectInput(
        session = session,
        inputId = "analytics_select_fund",
        choices = c("None", fund_matrix$complete_name)
    )
    
    # Update choices on comparison select list
    observe({
        choices <- fund_matrix$complete_name
        choices <- choices[choices != input$analytics_select_fund]
        updateSelectInput(
            session = session,
            inputId = "analytics_comparison",
            choices = choices
        )
    })
    
    # Update allowable date range
    observe({
        updateDateRangeInput(
            session = session,
            inputId = "analytics_dateRange",
            min = a_inception_date(),
            max = Sys.Date()
        )
    })
    
    # Update date range after selecting DateRange Radio Button Group
    observeEvent(
        input$analytics_quickDateRange, {
            if(input$analytics_quickDateRange == "1M") start_date <- max(Sys.Date() - months(1), a_inception_date())
            else if(input$analytics_quickDateRange == "1Y") start_date <- max(Sys.Date() - years(1), a_inception_date())
            else if(input$analytics_quickDateRange == "5Y") start_date <- max(Sys.Date() - years(5), a_inception_date())
            else if(input$analytics_quickDateRange == "All") start_date <- a_inception_date()
            
            updateDateRangeInput(
                session = session,
                inputId = "analytics_dateRange",
                start = start_date,
                end = Sys.Date()
            )
        }
    )
    
    # Update displayed fund name
    output$analytics_fund_name <- renderText({
        if(input$analytics_select_fund == "None") return()
        a_fund_selected()[["Fund Name"]]
    })
    
    # Update displayed bank name
    output$analytics_bank_name <- renderText({
        if(input$analytics_select_fund == "None") return()
        a_fund_selected()[["bank_alias"]]
    })
    
    # Update Displayed Fund Details
    output$analytics_fund_profile <- DT::renderDataTable({
        if(input$analytics_select_fund == "None") return()
        a_fund_selected() %>% 
            select(-fund_id, -bank_id, -`Fund Name`, -bank_alias, -complete_name) %>% 
            gather(Key, Value) %>% 
            datatable(rownames = NULL,
                      colnames = list(NULL, NULL),
                      selection = "none",
                      class = "stripe hover cell-border",
                      options = list(
                          pageLength = -1,
                          autoWidth = TRUE,
                          dom = "t",
                          bSort = FALSE
                      ))
    })
    
    # reactive prices container
    a_prices <- reactive({
        if(input$analytics_select_fund == "None") return()
        withProgress(message = 'Loading  NAVpu data...', value = 1/(nrow(comparison_df())+1), {
            # get navpu data of selected UITF
            price_data <- get_navpu(a_fund_selected()[["fund_id"]],
                                    a_fund_selected()[["bank_id"]],
                                    input$analytics_dateRange[1],
                                    input$analytics_dateRange[2]) %>% 
                mutate(fund_name = a_fund_selected()[["complete_name"]])
            incProgress(1/(nrow(comparison_df())+1))
            
            # get navpu data for each UITF to compare
            if(length(input$analytics_comparison)>0){
                for(i in 1:nrow(comparison_df())){
                    other_price_data <- get_navpu(comparison_df()$fund_id[i],
                                                  comparison_df()$bank_id[i],
                                                  input$analytics_dateRange[1],
                                                  input$analytics_dateRange[2]) %>% 
                        mutate(fund_name = comparison_df()$complete_name[i])
                    price_data <- bind_rows(price_data, other_price_data)
                    incProgress(1/(nrow(comparison_df())+1))
                }
                # transform navpu to percent returns reshape data
                price_data <- price_data %>% 
                    group_by(fund_name) %>% 
                    arrange(date) %>%
                    mutate(navpu = (navpu/first(navpu)-1)*100) %>% 
                    spread(key = fund_name, value = navpu) %>% 
                    arrange(date)
            } else {
                # reshape data
                price_data <- price_data %>% 
                    group_by(fund_name) %>% 
                    arrange(date) %>%
                    spread(key = fund_name, value = navpu)
            }
        })
        return(price_data)
    })
    
    # reactive ticksuffix: decimal or percentage
    a_ticksuffix <- reactive({ ifelse(length(input$analytics_comparison)>0, "%", "")})
    
    # reactive container of UITFs for comparison
    comparison_df <- reactive({
        fund_matrix %>% 
            filter(complete_name %in% input$analytics_comparison) %>% 
            select(fund_id, bank_id, complete_name)
    })
    
    # NAVpu chart constructor
    navpu_chart <- reactive({
        if(input$analytics_select_fund == "None") return()
        
        # get names of UITFs for comparison
        traces <- names(a_prices())[-1]
        
        # construct line plot of main UITF
        main_chart <- plot_ly(x = a_prices()[["date"]],
                              y = na.locf(a_prices()[[traces[[1]]]], na.rm=F),
                              mode="lines", type="scatter",
                              name=traces[1]) %>% 
            layout(xaxis = list(title="", side="bottom")) %>% 
            layout(yaxis = list(title="", side="left", 
                                tickformat = ".2f",
                                ticksuffix = a_ticksuffix())) %>% 
            layout(margin = list(pad = 2)) %>% 
            config(displayModeBar = F) %>% 
            layout(showlegend = T, legend = list(x = 0.01, y = 0.99)) %>% 
            layout(hoverlabel = list(namelength=100))
        
        # add line plots of each UITF for comparison
        if(length(input$analytics_comparison) > 0) {
            for(i in 2:length(traces)){
                main_chart <- main_chart %>% 
                    add_trace(x = a_prices()[["date"]],
                              y = na.locf(a_prices()[[traces[[i]]]], na.rm=F),
                              name = traces[i],
                              mode="lines", type="scatter")
            }
        }
        return(main_chart)
    })
    
    # RSI Chart Constructor
    rsi_chart <- reactive({
        if(input$analytics_select_fund == "None") return()
        
        # extract navpu data of main UITF
        main_data <- a_prices() %>% 
            select(date, navpu=matches(input$analytics_select_fund)) %>% 
            mutate(navpu = navpu+1) %>% 
            drop_na()
        
        # compute RSI data
        rsi_data <- compute_rsi(main_data) %>% 
            drop_na()
        
        # construct RSI chart
        plot_ly(data=rsi_data, x=~date) %>% 
            add_trace(y=~RSI, name="RSI", mode="lines", type="scatter",
                      line = list(color = "orange"),
                      showlegend = F) %>% 
            add_trace(x = c(min(rsi_data$date), max(rsi_data$date)), 
                      y = c(30,30), mode = "lines",
                      line = list(color = "black"),
                      showlegend = F) %>% 
            add_trace(x = c(min(rsi_data$date), max(rsi_data$date)), 
                      y= c(70,70), mode = "lines",
                      line = list(color = "black"),
                      showlegend = F) %>% 
            layout(margin = list(pad = 2)) %>% 
            layout(yaxis = list(title = "", side = "left")) %>% 
            config(displayModeBar = F)
    })
    
    # MACD Chart Constructor
    macd_chart <- reactive({
        if(input$analytics_select_fund == "None") return()
        
        # extract navpu data of main UITF
        main_data <- a_prices() %>% 
            select(date, navpu=matches(input$analytics_select_fund)) %>% 
            mutate(navpu = navpu+1) %>% 
            drop_na()
        
        # compute MACD data
        macd_data <- compute_macd(main_data) %>% 
            drop_na()
        
        # construct MACD chart
        plot_ly(data=macd_data, x=~date) %>% 
            add_trace(y=~histogram, mode="none", fill="tozeroy", name="Divergence", showlegend = F) %>% 
            add_trace(y=~macd, mode="lines", name="MACD", line=list(color="red"), showlegend = F) %>% 
            add_trace(y=~signal, mode="lines", name="Signal", line=list(color="green"), showlegend = F) %>% 
            layout(margin = list(pad = 2)) %>% 
            layout(yaxis = list(title = "", side = "left")) %>% 
            config(displayModeBar = F)
    })
    
    # Render Composite Chart
    output$analytics_composite_chart <- renderPlotly({
        if(input$analytics_select_fund == "None") return()
        plot_list <- list(navpu = navpu_chart())
        if("RSI" %in% input$analytics_indicators) plot_list[["rsi"]] <- rsi_chart()
        if("MACD" %in% input$analytics_indicators) plot_list[["macd"]] <- macd_chart()
        subplot(plot_list, nrows=3, shareX=T, heights=c(0.4,0.3,0.3),
                which_layout=1) %>% 
            layout(xaxis = list(title="")) %>% 
            layout(plot_bgcolor = "rgb(250,250,250)") %>% 
            layout(legend = list(bgcolor="rgb(250,250,250)"))
    })

})
