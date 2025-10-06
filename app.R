library(shiny)
library(bslib)
library(highcharter)
library(dplyr)
library(bsicons)

# Define UI
ui <- page_navbar(
  title = "Old Tampa Bay Chlorophyll-a Dashboard",
  theme = bs_theme(
    version = 5,
    primary = "#1e806e",
    secondary = "#035172",
    base_font = font_google("Inter"),
    bg = "#f5f5f5",
    fg = "#5c524f"
  ),
  
  # NW Subsegment tab
  nav_panel(
    title = "NW Subsegment",
    icon = bs_icon("droplet-fill"),
    value = "NW",
    
    layout_columns(
      col_widths = c(4, 8),
      
      # Left column - Value boxes
      layout_columns(
        col_widths = 12,
        
        card(
          card_header(
            class = "bg-primary text-white",
            "Target Threshold"
          ),
          card_body(
            class = "text-center",
            h2("11.3 μg/L", style = "color: #1e806e; margin-bottom: 0;"),
            p("June-October Average Target", class = "text-muted")
          )
        ),
        
        value_box(
          title = "Jun-Oct Average",
          value = textOutput("nw_avg_value"),
          showcase = bs_icon("graph-up"),
          theme = "primary",
          textOutput("nw_avg_subtitle")
        ),
        
        value_box(
          title = "Remaining Months",
          value = textOutput("nw_remaining_value"),
          showcase = bs_icon("calendar3"),
          theme = "secondary",
          p("months to report (Jun-Oct)")
        ),
        
        value_box(
          title = "Max Allowable Avg",
          value = textOutput("nw_max_allowable_value"),
          showcase = bs_icon("exclamation-triangle"),
          theme = "warning",
          textOutput("nw_max_allowable_subtitle")
        )
      ),
      
      # Right column - Plots
      layout_columns(
        col_widths = 12,
        
        card(
          full_screen = TRUE,
          card_header("Monthly Chlorophyll-a Values"),
          card_body(
            highchartOutput("nw_monthly_plot", height = "400px")
          )
        ),
        
        card(
          full_screen = TRUE,
          card_header("June-October Values (Threshold Period)"),
          card_body(
            highchartOutput("nw_seasonal_plot", height = "400px")
          )
        )
      )
    )
  ),
  
  # CW Subsegment tab
  nav_panel(
    title = "CW Subsegment",
    icon = bs_icon("droplet-fill"),
    value = "CW",
    
    layout_columns(
      col_widths = c(4, 8),
      
      # Left column - Value boxes
      layout_columns(
        col_widths = 12,
        
        card(
          card_header(
            class = "bg-primary text-white",
            "Target Threshold"
          ),
          card_body(
            class = "text-center",
            h2("13.8 μg/L", style = "color: #1e806e; margin-bottom: 0;"),
            p("June-October Average Target", class = "text-muted")
          )
        ),
        
        value_box(
          title = "Jun-Oct Average",
          value = textOutput("cw_avg_value"),
          showcase = bs_icon("graph-up"),
          theme = "primary",
          textOutput("cw_avg_subtitle")
        ),
        
        value_box(
          title = "Remaining Months",
          value = textOutput("cw_remaining_value"),
          showcase = bs_icon("calendar3"),
          theme = "secondary",
          p("months to report (Jun-Oct)")
        ),
        
        value_box(
          title = "Max Allowable Avg",
          value = textOutput("cw_max_allowable_value"),
          showcase = bs_icon("exclamation-triangle"),
          theme = "warning",
          textOutput("cw_max_allowable_subtitle")
        )
      ),
      
      # Right column - Plots
      layout_columns(
        col_widths = 12,
        
        card(
          full_screen = TRUE,
          card_header("Monthly Chlorophyll-a Values"),
          card_body(
            highchartOutput("cw_monthly_plot", height = "400px")
          )
        ),
        
        card(
          full_screen = TRUE,
          card_header("June-October Values (Threshold Period)"),
          card_body(
            highchartOutput("cw_seasonal_plot", height = "400px")
          )
        )
      )
    )
  )

)

# Define server logic
server <- function(input, output, session) {
  
  # Define thresholds
  thresholds <- list(NW = 11.3, CW = 13.8)
  
  # Five-year historical averages
  historical_avg <- data.frame(
    Month = month.abb,
    NW = c(8.2, 7.8, 8.5, 9.2, 10.1, 11.5, 12.3, 11.8, 10.9, 10.2, 9.1, 8.4),
    CW = c(9.5, 9.1, 9.8, 10.5, 11.2, 13.0, 14.1, 13.5, 12.4, 11.6, 10.3, 9.7)
  )
  
  # Current year data (example data with some missing values)
  current_data <- list(
    NW = c(7.9, 7.5, 8.1, 9.0, 9.8, 10.8, 11.9, NA, NA, NA, NA, NA),
    CW = c(9.2, 8.9, 9.5, 10.2, 10.9, 12.5, 13.8, NA, NA, NA, NA, NA)
  )
  
  # Calculate June-October average
  calc_seasonal_avg <- function(segment) {
    june_oct_idx <- 6:10
    
    values <- sapply(june_oct_idx, function(i) {
      if (is.na(current_data[[segment]][i])) {
        historical_avg[[segment]][i]
      } else {
        current_data[[segment]][i]
      }
    })
    
    mean(values, na.rm = TRUE)
  }
  
  # Calculate maximum allowable average for remaining months
  calc_max_remaining <- function(segment) {
    june_oct_idx <- 6:10
    threshold <- thresholds[[segment]]
    
    actual_values <- current_data[[segment]][june_oct_idx]
    actual_sum <- sum(actual_values, na.rm = TRUE)
    remaining_count <- sum(is.na(actual_values))
    
    if (remaining_count == 0) {
      return(NULL)
    }
    
    max_remaining_total <- (threshold * 5) - actual_sum
    max_remaining_avg <- max_remaining_total / remaining_count
    
    return(max_remaining_avg)
  }
  
  # Calculate remaining months
  calc_remaining_months <- function(segment) {
    june_oct_idx <- 6:10
    sum(is.na(current_data[[segment]][june_oct_idx]))
  }
  
  # NW Value box outputs
  output$nw_avg_value <- renderText({
    sprintf("%.2f μg/L", calc_seasonal_avg("NW"))
  })
  
  output$nw_avg_subtitle <- renderText({
    threshold <- thresholds$NW
    is_exceeding <- calc_seasonal_avg("NW") > threshold
    paste0(ifelse(is_exceeding, "Exceeds", "Below"), 
           " threshold of ", threshold, " μg/L")
  })
  
  output$nw_remaining_value <- renderText({
    as.character(calc_remaining_months("NW"))
  })
  
  output$nw_max_allowable_value <- renderText({
    max_rem <- calc_max_remaining("NW")
    
    if (is.null(max_rem)) {
      "Complete"
    } else if (max_rem > 0) {
      sprintf("%.2f μg/L", max_rem)
    } else {
      "Exceeded"
    }
  })
  
  output$nw_max_allowable_subtitle <- renderText({
    max_rem <- calc_max_remaining("NW")
    
    if (is.null(max_rem)) {
      "All Jun-Oct data reported"
    } else if (max_rem > 0) {
      "for remaining months"
    } else {
      "Cannot meet threshold"
    }
  })
  
  # CW Value box outputs
  output$cw_avg_value <- renderText({
    sprintf("%.2f μg/L", calc_seasonal_avg("CW"))
  })
  
  output$cw_avg_subtitle <- renderText({
    threshold <- thresholds$CW
    is_exceeding <- calc_seasonal_avg("CW") > threshold
    paste0(ifelse(is_exceeding, "Exceeds", "Below"), 
           " threshold of ", threshold, " μg/L")
  })
  
  output$cw_remaining_value <- renderText({
    as.character(calc_remaining_months("CW"))
  })
  
  output$cw_max_allowable_value <- renderText({
    max_rem <- calc_max_remaining("CW")
    
    if (is.null(max_rem)) {
      "Complete"
    } else if (max_rem > 0) {
      sprintf("%.2f μg/L", max_rem)
    } else {
      "Exceeded"
    }
  })
  
  output$cw_max_allowable_subtitle <- renderText({
    max_rem <- calc_max_remaining("CW")
    
    if (is.null(max_rem)) {
      "All Jun-Oct data reported"
    } else if (max_rem > 0) {
      "for remaining months"
    } else {
      "Cannot meet threshold"
    }
  })
  
  # NW Monthly plot
  output$nw_monthly_plot <- renderHighchart({
    segment <- "NW"
    threshold <- thresholds[[segment]]
    
    # Prepare data
    actual_data <- current_data[[segment]]
    projected_data <- ifelse(is.na(actual_data), historical_avg[[segment]], NA)
    historical_data <- historical_avg[[segment]]
    
    highchart() %>%
      hc_chart(type = "line") %>%
      hc_xAxis(categories = month.abb) %>%
      hc_yAxis(
        title = list(text = "Chlorophyll-a (μg/L)"),
        plotLines = list(
          list(
            color = "#d9534f",
            width = 2,
            value = threshold,
            dashStyle = "Dash",
            label = list(text = paste("Threshold:", threshold, "μg/L"), style = list(color = "#d9534f", fontWeight = "bold"))
          )
        )
      ) %>%
      hc_add_series(
        name = "Actual Data",
        data = actual_data,
        color = "#1e806e",
        marker = list(radius = 5),
        lineWidth = 3
      ) %>%
      hc_add_series(
        name = "5-Yr Avg Projection",
        data = projected_data,
        color = "#d9d9d9",
        dashStyle = "Dash",
        marker = list(radius = 4),
        lineWidth = 2
      ) %>%
      hc_add_series(
        name = "Historical Avg",
        data = historical_data,
        color = "#5c524f",
        dashStyle = "Dot",
        marker = list(enabled = FALSE),
        lineWidth = 1
      ) %>%
      hc_tooltip(
        shared = TRUE,
        valueSuffix = " μg/L",
        crosshairs = TRUE
      ) %>%
      hc_legend(align = "center", verticalAlign = "bottom") %>%
      hc_credits(enabled = FALSE)
  })
  
  # NW Seasonal plot
  output$nw_seasonal_plot <- renderHighchart({
    segment <- "NW"
    threshold <- thresholds[[segment]]
    june_oct_idx <- 6:10
    
    seasonal_data <- sapply(june_oct_idx, function(i) {
      if (is.na(current_data[[segment]][i])) {
        historical_avg[[segment]][i]
      } else {
        current_data[[segment]][i]
      }
    })
    
    colors <- sapply(june_oct_idx, function(i) {
      if (is.na(current_data[[segment]][i])) {
        "#d9d9d9"  # Projected
      } else {
        "#1e806e"  # Actual
      }
    })
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = month.abb[june_oct_idx]) %>%
      hc_yAxis(
        title = list(text = "Chlorophyll-a (μg/L)"),
        plotLines = list(
          list(
            color = "#d9534f",
            width = 2,
            value = threshold,
            dashStyle = "Dash",
            label = list(text = paste("Threshold:", threshold, "μg/L"), style = list(color = "#d9534f", fontWeight = "bold"))
          )
        )
      ) %>%
      hc_add_series(
        name = "Chlorophyll-a",
        data = seasonal_data,
        colorByPoint = TRUE,
        colors = colors,
        showInLegend = FALSE
      ) %>%
      hc_plotOptions(
        column = list(
          dataLabels = list(enabled = TRUE, format = "{y:.1f}")
        )
      ) %>%
      hc_tooltip(valueSuffix = " μg/L") %>%
      hc_credits(enabled = FALSE)
  })
  
  # CW Monthly plot
  output$cw_monthly_plot <- renderHighchart({
    segment <- "CW"
    threshold <- thresholds[[segment]]
    
    # Prepare data
    actual_data <- current_data[[segment]]
    projected_data <- ifelse(is.na(actual_data), historical_avg[[segment]], NA)
    historical_data <- historical_avg[[segment]]
    
    highchart() %>%
      hc_chart(type = "line") %>%
      hc_xAxis(categories = month.abb) %>%
      hc_yAxis(
        title = list(text = "Chlorophyll-a (μg/L)"),
        plotLines = list(
          list(
            color = "#d9534f",
            width = 2,
            value = threshold,
            dashStyle = "Dash",
            label = list(text = paste("Threshold:", threshold, "μg/L"), style = list(color = "#d9534f", fontWeight = "bold"))
          )
        )
      ) %>%
      hc_add_series(
        name = "Actual Data",
        data = actual_data,
        color = "#1e806e",
        marker = list(radius = 5),
        lineWidth = 3
      ) %>%
      hc_add_series(
        name = "5-Yr Avg Projection",
        data = projected_data,
        color = "#d9d9d9",
        dashStyle = "Dash",
        marker = list(radius = 4),
        lineWidth = 2
      ) %>%
      hc_add_series(
        name = "Historical Avg",
        data = historical_data,
        color = "#5c524f",
        dashStyle = "Dot",
        marker = list(enabled = FALSE),
        lineWidth = 1
      ) %>%
      hc_tooltip(
        shared = TRUE,
        valueSuffix = " μg/L",
        crosshairs = TRUE
      ) %>%
      hc_legend(align = "center", verticalAlign = "bottom") %>%
      hc_credits(enabled = FALSE)
  })
  
  # CW Seasonal plot
  output$cw_seasonal_plot <- renderHighchart({
    segment <- "CW"
    threshold <- thresholds[[segment]]
    june_oct_idx <- 6:10
    
    seasonal_data <- sapply(june_oct_idx, function(i) {
      if (is.na(current_data[[segment]][i])) {
        historical_avg[[segment]][i]
      } else {
        current_data[[segment]][i]
      }
    })
    
    colors <- sapply(june_oct_idx, function(i) {
      if (is.na(current_data[[segment]][i])) {
        "#d9d9d9"  # Projected
      } else {
        "#1e806e"  # Actual
      }
    })
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = month.abb[june_oct_idx]) %>%
      hc_yAxis(
        title = list(text = "Chlorophyll-a (μg/L)"),
        plotLines = list(
          list(
            color = "#d9534f",
            width = 2,
            value = threshold,
            dashStyle = "Dash",
            label = list(text = paste("Threshold:", threshold, "μg/L"), style = list(color = "#d9534f", fontWeight = "bold"))
          )
        )
      ) %>%
      hc_add_series(
        name = "Chlorophyll-a",
        data = seasonal_data,
        colorByPoint = TRUE,
        colors = colors,
        showInLegend = FALSE
      ) %>%
      hc_plotOptions(
        column = list(
          dataLabels = list(enabled = TRUE, format = "{y:.1f}")
        )
      ) %>%
      hc_tooltip(valueSuffix = " μg/L") %>%
      hc_credits(enabled = FALSE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)