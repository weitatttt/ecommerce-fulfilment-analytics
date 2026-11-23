# ============================================================
# ETW2001 ASSIGNMENT 3
# E-Commerce Fulfilment Performance Dashboard
# ============================================================

library(shiny)
library(ggplot2)

# Meaningful interactions implemented for Task 3:
# 1. Date-range filtering
# 2. Shipping-mode filtering with comparison context retained in V2
# 3. Geographic-level switching (Market / Order Region / Order Country)
# 4. Customer-segment filtering
# 5. V3 point-click drill-down into V4 and V5
# 6. Clear-geography / reset-investigation context controls

# ============================================================
# DASHBOARD VISUAL THEME
# ============================================================

dashboard_plot_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15,
      colour = "#1f2937"
    ),
    plot.subtitle = element_text(
      size = 11,
      colour = "#5f6b7a",
      margin = margin(b = 8)
    ),
    plot.caption = element_text(
      size = 9,
      colour = "#6b7280",
      hjust = 0,
      margin = margin(t = 8)
    ),
    axis.title = element_text(
      size = 10.5,
      colour = "#374151"
    ),
    axis.text = element_text(
      colour = "#4b5563"
    ),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 12, 10, 10)
  )

# ============================================================
# 1. LOAD DATA
# ============================================================

data_raw <- read.csv(
  "data.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "latin1"
)

# ============================================================
# 2. DATE PREPARATION
# ============================================================

# Keep a datetime field for reference.
data_raw[["order_datetime"]] <- as.POSIXct(
  data_raw[["order date (DateOrders)"]],
  format = "%m/%d/%Y %H:%M",
  tz = "UTC"
)

# Create calendar date directly from the original text.
# This avoids timezone shifting at midnight.
data_raw[["order_date"]] <- as.Date(
  substr(data_raw[["order date (DateOrders)"]], 1, 10),
  format = "%m/%d/%Y"
)

# ============================================================
# 3. BUILD ONE-ROW-PER-ORDER DATA
# ============================================================

order_ids <- unique(data_raw[["Order Id"]])
representative_rows <- match(order_ids, data_raw[["Order Id"]])

order_data <- data.frame(
  "Order Id" = order_ids,
  "Delivery Status" = data_raw[["Delivery Status"]][representative_rows],
  "Late_delivery_risk" = data_raw[["Late_delivery_risk"]][representative_rows],
  "Shipping Mode" = data_raw[["Shipping Mode"]][representative_rows],
  "Days for shipping (real)" = data_raw[["Days for shipping (real)"]][representative_rows],
  "Days for shipment (scheduled)" = data_raw[["Days for shipment (scheduled)"]][representative_rows],
  "Market" = data_raw[["Market"]][representative_rows],
  "Order Region" = data_raw[["Order Region"]][representative_rows],
  "Order Country" = data_raw[["Order Country"]][representative_rows],
  "Customer Segment" = data_raw[["Customer Segment"]][representative_rows],
  order_datetime = data_raw[["order_datetime"]][representative_rows],
  order_date = data_raw[["order_date"]][representative_rows],
  check.names = FALSE
)

# ============================================================
# 4. COMMERCIAL MEASURES
# ============================================================

sum_preserve_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  sum(x, na.rm = TRUE)
}

order_sales <- tapply(
  data_raw[["Order Item Total"]],
  data_raw[["Order Id"]],
  sum_preserve_na
)

order_profit <- tapply(
  data_raw[["Order Profit Per Order"]],
  data_raw[["Order Id"]],
  sum_preserve_na
)

order_data[["Order Net Sales"]] <- as.numeric(
  order_sales[
    match(
      as.character(order_data[["Order Id"]]),
      names(order_sales)
    )
  ]
)

order_data[["Order Profit"]] <- as.numeric(
  order_profit[
    match(
      as.character(order_data[["Order Id"]]),
      names(order_profit)
    )
  ]
)

# ============================================================
# 5. FULFILMENT INDICATORS
# ============================================================

order_data$is_cancelled <- order_data[["Delivery Status"]] == "Shipping canceled"
order_data$is_fulfilled <- !order_data$is_cancelled
order_data$is_late <- order_data[["Late_delivery_risk"]] == 1 & order_data$is_fulfilled

# ============================================================
# 6. DASHBOARD DATE RANGE
# ============================================================

dashboard_min_date <- min(order_data[["order_date"]], na.rm = TRUE)
dashboard_max_date <- max(order_data[["order_date"]], na.rm = TRUE)

# ============================================================
# 7. START-UP CHECKS
# ============================================================

cat("\n============================================\n")
cat("ETW2001 DASHBOARD DATA CHECK\n")
cat("============================================\n")
cat("Date range:", format(dashboard_min_date), "to", format(dashboard_max_date), "\n")
cat("Orders:", nrow(order_data), "\n")
cat("Unique orders:", length(unique(order_data[["Order Id"]])), "\n")
cat("Fulfilled orders:", sum(order_data$is_fulfilled, na.rm = TRUE), "\n")
cat("Late fulfilled orders:", sum(order_data$is_late, na.rm = TRUE), "\n")
cat(
  "Late-delivery rate:",
  sprintf(
    "%.2f%%",
    100 * sum(order_data$is_late, na.rm = TRUE) /
      sum(order_data$is_fulfilled, na.rm = TRUE)
  ),
  "\n"
)
cat(
  "Late-order net sales exposure:",
  sprintf(
    "$%.2fM",
    sum(
      order_data[["Order Net Sales"]][order_data$is_late %in% TRUE],
      na.rm = TRUE
    ) / 1000000
  ),
  "\n"
)
cat("============================================\n\n")

# ============================================================
# 8. USER INTERFACE
# ============================================================

ui <- fluidPage(

  tags$head(
    tags$style(
      HTML(
        "
        body {
          background: #f4f6f9;
          color: #1f2937;
          font-family: 'Segoe UI', Arial, sans-serif;
        }

        .container-fluid {
          padding-left: 24px;
          padding-right: 24px;
        }

        .dashboard-header {
          background: linear-gradient(135deg, #17324d 0%, #24577a 100%);
          color: white;
          padding: 22px 28px;
          margin: 0 -24px 22px -24px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.12);
        }

        .dashboard-header h1 {
          margin: 0;
          font-size: 29px;
          font-weight: 650;
          letter-spacing: 0.1px;
        }

        .dashboard-header p {
          margin: 7px 0 0 0;
          color: #dce8f2;
          font-size: 14px;
        }

        .sidebar-card {
          background: white;
          border: 1px solid #e3e8ee;
          border-radius: 10px;
          padding: 18px;
          box-shadow: 0 2px 10px rgba(31,41,55,0.06);
          position: sticky;
          top: 12px;
        }

        .sidebar-card h4 {
          margin-top: 0;
          margin-bottom: 16px;
          font-weight: 650;
          color: #17324d;
        }

        .sidebar-card .form-group {
          margin-bottom: 16px;
        }

        .sidebar-card label {
          font-size: 12px;
          font-weight: 650;
          color: #465362;
        }

        .reset-btn {
          width: 100%;
          background: #17324d;
          color: white;
          border: none;
          border-radius: 6px;
          padding: 9px 12px;
          font-weight: 600;
        }

        .reset-btn:hover, .reset-btn:focus {
          background: #24577a;
          color: white;
        }

        .sidebar-help {
          margin-top: 16px;
          padding-top: 14px;
          border-top: 1px solid #e5e7eb;
          color: #6b7280;
          font-size: 12px;
          line-height: 1.5;
        }

        .kpi-card {
          background: white;
          border: 1px solid #e3e8ee;
          border-top: 4px solid #2f6b8a;
          border-radius: 9px;
          padding: 15px 18px 14px 18px;
          min-height: 122px;
          box-shadow: 0 2px 8px rgba(31,41,55,0.05);
          margin-bottom: 16px;
        }

        .kpi-card.exposure {
          border-top-color: #c58a32;
        }

        .kpi-label {
          font-size: 11px;
          text-transform: uppercase;
          letter-spacing: 0.55px;
          color: #6b7280;
          font-weight: 700;
          margin-bottom: 5px;
        }

        .kpi-value {
          font-size: 31px;
          line-height: 1.15;
          font-weight: 700;
          color: #17324d;
          margin: 0;
        }

        .kpi-note {
          margin-top: 7px;
          font-size: 10.5px;
          line-height: 1.35;
          color: #7a8491;
        }

        .story-strip {
          background: #eaf1f6;
          border-left: 4px solid #2f6b8a;
          border-radius: 7px;
          padding: 13px 16px;
          margin-bottom: 18px;
          color: #435363;
          font-size: 13px;
          line-height: 1.55;
        }

        .story-strip strong {
          color: #17324d;
        }

        .decision-path {
          margin-top: 5px;
          color: #667483;
          font-size: 11.5px;
        }

        .chart-card {
          background: white;
          border: 1px solid #e3e8ee;
          border-radius: 10px;
          padding: 18px 18px 11px 18px;
          margin-bottom: 20px;
          box-shadow: 0 2px 10px rgba(31,41,55,0.05);
        }

        .section-kicker {
          display: inline-block;
          font-size: 11px;
          font-weight: 750;
          letter-spacing: 0.65px;
          text-transform: uppercase;
          color: #2f6b8a;
          margin-bottom: 3px;
        }

        .section-title {
          font-size: 20px;
          font-weight: 650;
          color: #1f2937;
          margin: 0 0 4px 0;
        }

        .section-guide {
          color: #6b7280;
          font-size: 12px;
          line-height: 1.45;
          margin-bottom: 3px;
        }

        .geo-context {
          margin-top: 4px;
          padding: 9px 12px;
          border-radius: 6px;
          font-size: 11.5px;
          line-height: 1.45;
        }

        .geo-context.neutral {
          background: #f5f7f9;
          color: #667483;
          border: 1px solid #e4e8ed;
        }

        .geo-context.active {
          background: #fff7e8;
          color: #72531c;
          border: 1px solid #efd9ad;
        }

        .clear-selection-btn {
          margin-left: 10px;
          padding: 3px 9px;
          font-size: 11px;
          border-radius: 5px;
        }

        .action-card {
          background: #fffaf0;
          border: 1px solid #ecd7ad;
          border-left: 5px solid #c58a32;
          border-radius: 9px;
          padding: 17px 19px;
          margin-bottom: 14px;
          box-shadow: 0 2px 8px rgba(31,41,55,0.04);
        }

        .action-card h3 {
          margin: 0 0 9px 0;
          font-size: 19px;
          font-weight: 650;
          color: #5f481c;
        }

        .action-card ul {
          margin: 6px 0 0 18px;
          padding: 0;
        }

        .action-card li {
          margin-bottom: 6px;
          line-height: 1.5;
          color: #51493b;
          font-size: 12.5px;
        }

        .credibility-note {
          background: #f7f8fa;
          border: 1px solid #e3e7eb;
          border-radius: 7px;
          padding: 11px 14px;
          margin-bottom: 24px;
          color: #697481;
          font-size: 11px;
          line-height: 1.5;
        }

        .credibility-note strong {
          color: #465362;
        }

        .shiny-plot-output {
          width: 100% !important;
        }

        @media (max-width: 991px) {
          .sidebar-card {
            position: static;
            margin-bottom: 18px;
          }

          .dashboard-header h1 {
            font-size: 24px;
          }
        }
        "
      )
    )
  ),

  div(
    class = "dashboard-header",
    h1("E-Commerce Fulfilment Performance Dashboard"),
    p(
      "Regional Head of E-Commerce Operations | Evidence path: Monitor → Compare → Locate → Diagnose → Prioritise"
    )
  ),

  fluidRow(

    column(
      width = 3,
      div(
        class = "sidebar-card",
        h4("Investigation Controls"),

        dateRangeInput(
          inputId = "date_range",
          label = "Order date range",
          start = dashboard_min_date,
          end = dashboard_max_date,
          min = dashboard_min_date,
          max = dashboard_max_date
        ),

        selectInput(
          inputId = "shipping_mode",
          label = "Shipping mode",
          choices = c(
            "All",
            "Same Day",
            "First Class",
            "Second Class",
            "Standard Class"
          ),
          selected = "All"
        ),

        selectInput(
          inputId = "geo_level",
          label = "Geographic level",
          choices = c(
            "Order Region",
            "Market",
            "Order Country"
          ),
          selected = "Order Region"
        ),

        selectInput(
          inputId = "customer_segment",
          label = "Customer segment",
          choices = c(
            "All",
            "Consumer",
            "Corporate",
            "Home Office"
          ),
          selected = "All"
        ),

        actionButton(
          inputId = "reset_filters",
          label = "Reset Investigation",
          class = "reset-btn"
        ),

        div(
          class = "sidebar-help",
          tags$strong("How to investigate"),
          tags$br(),
          "1. Set the time, shipping and customer context.",
          tags$br(),
          "2. Change geographic level in V3.",
          tags$br(),
          "3. Click a V3 point to drill V4 and V5 into that geography.",
          tags$br(),
          "4. Reset to restore the full-scope view."
        )
      )
    ),

    column(
      width = 9,

      fluidRow(
        column(
          width = 4,
          div(
            class = "kpi-card",
            div(class = "kpi-label", "Late-delivery rate"),
            div(class = "kpi-value", textOutput("kpi_late_rate", inline = TRUE)),
            div(class = "kpi-note", "Fulfilled orders only; cancellations excluded")
          )
        ),
        column(
          width = 4,
          div(
            class = "kpi-card",
            div(class = "kpi-label", "Late fulfilled orders"),
            div(class = "kpi-value", textOutput("kpi_late_orders", inline = TRUE)),
            div(class = "kpi-note", "Unique fulfilled orders classified as late")
          )
        ),
        column(
          width = 4,
          div(
            class = "kpi-card exposure",
            div(class = "kpi-label", "Late-order net sales exposure"),
            div(class = "kpi-value", textOutput("kpi_late_sales", inline = TRUE)),
            div(class = "kpi-note", "Associated exposure; not a causal revenue-loss estimate")
          )
        )
      ),

      div(
        class = "story-strip",
        uiOutput("executive_story")
      ),

      div(
        class = "chart-card",
        div(class = "section-kicker", "1 · Monitor"),
        div(class = "section-title", "Is the delivery problem persistent?"),
        div(
          class = "section-guide",
          "Track the monthly late-delivery rate and distinguish persistent unreliability from temporary fluctuation."
        ),
        plotOutput("plot_v1", height = "345px")
      ),

      div(
        class = "chart-card",
        div(class = "section-kicker", "2 · Compare"),
        div(class = "section-title", "Which shipping modes miss their service levels?"),
        div(
          class = "section-guide",
          "Compare late-delivery rates with scheduled and average actual shipping times; all modes stay visible for context."
        ),
        plotOutput("plot_v2", height = "345px")
      ),

      div(
        class = "chart-card",
        div(class = "section-kicker", "3 · Locate"),
        div(class = "section-title", "Where is the business most exposed?"),
        div(
          class = "section-guide",
          "Combine delivery reliability with late-order net sales exposure. Click one point to drill the next two views into that geography."
        ),
        plotOutput("plot_v3", height = "400px", click = "v3_click"),
        uiOutput("geo_context")
      ),

      div(
        class = "chart-card",
        div(class = "section-kicker", "4 · Diagnose"),
        div(class = "section-title", "Which products and customers concentrate the exposure?"),
        div(
          class = "section-guide",
          "Identify the category and customer-segment combinations that account for the greatest late-order net sales exposure."
        ),
        plotOutput("plot_v4", height = "465px")
      ),

      div(
        class = "chart-card",
        div(class = "section-kicker", "5 · Prioritise"),
        div(class = "section-title", "Do commercial outcomes change the operational priority?"),
        div(
          class = "section-guide",
          "Compare order-level net sales and profit distributions for late versus on-time fulfilled orders without implying causation."
        ),
        plotOutput("plot_v5", height = "385px")
      ),

      div(
        class = "action-card",
        h3("What should management do next?"),
        uiOutput("management_implication")
      ),

      div(
        class = "credibility-note",
        tags$strong("Credibility and scope: "),
        "Delivery metrics are calculated at unique Order Id level. Shipping-cancelled orders are excluded from timeliness rates. Commercial values are aggregated to order level where appropriate. Late-order net sales represent commercial exposure, not proven revenue loss. The observed period runs from Jan 2015 to Jan 2018, with 2018 containing January only."
      )
    )
  )
)

# ============================================================
# 9. SERVER
# ============================================================

server <- function(input, output, session) {

  # ----------------------------------------------------------
  # Selected geography from V3
  # ----------------------------------------------------------

  selected_geo <- reactiveVal(NULL)

  # ----------------------------------------------------------
  # Main filtered order data
  # ----------------------------------------------------------

  filtered_orders <- reactive({
    req(input$date_range)

    dat <- order_data[
      !is.na(order_data[["order_date"]]) &
        order_data[["order_date"]] >= input$date_range[1] &
        order_data[["order_date"]] <= input$date_range[2],
    ]

    if (input$shipping_mode != "All") {
      dat <- dat[
        !is.na(dat[["Shipping Mode"]]) &
          dat[["Shipping Mode"]] == input$shipping_mode,
      ]
    }

    if (input$customer_segment != "All") {
      dat <- dat[
        !is.na(dat[["Customer Segment"]]) &
          dat[["Customer Segment"]] == input$customer_segment,
      ]
    }

    dat
  })

  # Clear geographic drill-down whenever main context changes.
  observeEvent(
    list(
      input$date_range,
      input$shipping_mode,
      input$customer_segment,
      input$geo_level
    ),
    {
      selected_geo(NULL)
    },
    ignoreInit = TRUE
  )

  # Data used by V4 and V5 after V3 drill-down.
  analysis_orders <- reactive({
    dat <- filtered_orders()
    geo_selected <- selected_geo()

    if (!is.null(geo_selected)) {
      geo_variable <- input$geo_level
      dat <- dat[
        !is.na(dat[[geo_variable]]) &
          as.character(dat[[geo_variable]]) == geo_selected,
      ]
    }

    dat
  })

  # ==========================================================
  # KPI 1 — LATE-DELIVERY RATE
  # ==========================================================

  output$kpi_late_rate <- renderText({
    dat <- filtered_orders()

    fulfilled_count <- sum(dat$is_fulfilled, na.rm = TRUE)

    if (fulfilled_count == 0) {
      return("N/A")
    }

    late_count <- sum(dat$is_late, na.rm = TRUE)
    late_rate <- (late_count / fulfilled_count) * 100

    sprintf("%.1f%%", late_rate)
  })

  # ==========================================================
  # KPI 2 — LATE FULFILLED ORDERS
  # ==========================================================

  output$kpi_late_orders <- renderText({
    dat <- filtered_orders()
    late_count <- sum(dat$is_late, na.rm = TRUE)

    format(
      late_count,
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    )
  })

  # ==========================================================
  # KPI 3 — LATE-ORDER NET SALES
  # ==========================================================

  output$kpi_late_sales <- renderText({
    dat <- filtered_orders()

    late_sales <- sum(
      dat[["Order Net Sales"]][dat$is_late %in% TRUE],
      na.rm = TRUE
    )

    sprintf("$%.2fM", late_sales / 1000000)
  })

  # ==========================================================
  # EXECUTIVE STORY — SETUP / INTENT ALIGNMENT
  # ==========================================================

  output$executive_story <- renderUI({
    dat <- filtered_orders()
    fulfilled <- dat[dat$is_fulfilled %in% TRUE, ]

    if (nrow(fulfilled) == 0) {
      return(
        tags$div(
          tags$strong("What is happening? "),
          "No fulfilled orders are available under the current selection."
        )
      )
    }

    late_count <- sum(fulfilled$is_late, na.rm = TRUE)
    late_rate <- (late_count / nrow(fulfilled)) * 100
    late_sales <- sum(
      fulfilled[["Order Net Sales"]][fulfilled$is_late %in% TRUE],
      na.rm = TRUE
    )

    full_scope <- (
      input$shipping_mode == "All" &&
        input$customer_segment == "All" &&
        as.Date(input$date_range[1]) == dashboard_min_date &&
        as.Date(input$date_range[2]) == dashboard_max_date
    )

    tags$div(
      tags$strong("What is happening? "),
      paste0(
        format(late_count, big.mark = ",", scientific = FALSE, trim = TRUE),
        " of ",
        format(nrow(fulfilled), big.mark = ",", scientific = FALSE, trim = TRUE),
        " fulfilled orders are late (",
        sprintf("%.1f%%", late_rate),
        "), with ",
        sprintf("$%.2fM", late_sales / 1000000),
        " of net sales associated with late fulfilled orders under the current selection."
      ),
      if (full_scope) {
        tags$span(
          " The full-scope view indicates a persistent operational problem rather than a short-lived exception."
        )
      },
      div(
        class = "decision-path",
        tags$strong("Decision path: "),
        "Monitor persistence → compare service reliability → locate exposure → diagnose concentrations → prioritise action."
      )
    )
  })

  # ==========================================================
  # V1 — MONITOR
  # ==========================================================

  output$plot_v1 <- renderPlot({
    dat <- filtered_orders()

    v1_data <- dat[
      dat$is_fulfilled %in% TRUE &
        !is.na(dat[["order_date"]]),
    ]

    validate(
      need(
        nrow(v1_data) > 0,
        "No fulfilled orders are available for the selected filters."
      )
    )

    v1_data[["order_month"]] <- as.Date(
      format(v1_data[["order_date"]], "%Y-%m-01")
    )

    monthly_fulfilled <- aggregate(
      rep(1, nrow(v1_data)),
      by = list(order_month = v1_data[["order_month"]]),
      FUN = sum
    )
    names(monthly_fulfilled)[2] <- "Fulfilled"

    monthly_late <- aggregate(
      as.integer(v1_data$is_late),
      by = list(order_month = v1_data[["order_month"]]),
      FUN = sum,
      na.rm = TRUE
    )
    names(monthly_late)[2] <- "Late"

    v1_summary <- merge(
      monthly_fulfilled,
      monthly_late,
      by = "order_month",
      all.x = TRUE
    )

    v1_summary$Late[is.na(v1_summary$Late)] <- 0
    v1_summary$Late_Rate <- (v1_summary$Late / v1_summary$Fulfilled) * 100

    filtered_late_rate <- (
      sum(v1_data$is_late, na.rm = TRUE) /
        nrow(v1_data)
    ) * 100

    ggplot(
      v1_summary,
      aes(x = order_month, y = Late_Rate)
    ) +
      geom_line(linewidth = 0.9, colour = "#2f6b8a") +
      geom_point(size = 2, colour = "#2f6b8a") +
      geom_hline(
        yintercept = filtered_late_rate,
        linetype = "dashed"
      ) +
      annotate(
        "text",
        x = max(v1_summary$order_month),
        y = filtered_late_rate,
        label = sprintf("Filtered average: %.1f%%", filtered_late_rate),
        hjust = 1,
        vjust = -0.7,
        size = 3.5
      ) +
      scale_x_date(
        date_breaks = "6 months",
        date_labels = "%b %Y"
      ) +
      labs(
        title = NULL,
        subtitle = "Monthly late-delivery rate among fulfilled orders",
        x = NULL,
        y = "Late-delivery rate (%)",
        caption = paste(
          "Cancelled orders are excluded.",
          "The dashed line represents the current filtered average."
        )
      ) +
      dashboard_plot_theme +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })

  # ==========================================================
  # V2 — COMPARE
  # ==========================================================

  output$plot_v2 <- renderPlot({
    req(input$date_range)

    # V2 intentionally retains all shipping modes for comparison.
    # Date and customer segment filters still apply.
    v2_data <- order_data[
      !is.na(order_data[["order_date"]]) &
        order_data[["order_date"]] >= input$date_range[1] &
        order_data[["order_date"]] <= input$date_range[2] &
        order_data$is_fulfilled %in% TRUE,
    ]

    if (input$customer_segment != "All") {
      v2_data <- v2_data[
        !is.na(v2_data[["Customer Segment"]]) &
          v2_data[["Customer Segment"]] == input$customer_segment,
      ]
    }

    validate(
      need(
        nrow(v2_data) > 0,
        "No fulfilled orders are available for the selected filters."
      )
    )

    shipping_order <- c(
      "Same Day",
      "First Class",
      "Second Class",
      "Standard Class"
    )

    v2_data[["Shipping Mode"]] <- factor(
      v2_data[["Shipping Mode"]],
      levels = shipping_order
    )

    fulfilled_summary <- aggregate(
      rep(1, nrow(v2_data)),
      by = list(Shipping_Mode = v2_data[["Shipping Mode"]]),
      FUN = sum
    )
    names(fulfilled_summary)[2] <- "Fulfilled"

    late_summary <- aggregate(
      as.integer(v2_data$is_late),
      by = list(Shipping_Mode = v2_data[["Shipping Mode"]]),
      FUN = sum,
      na.rm = TRUE
    )
    names(late_summary)[2] <- "Late"

    scheduled_summary <- aggregate(
      v2_data[["Days for shipment (scheduled)"]],
      by = list(Shipping_Mode = v2_data[["Shipping Mode"]]),
      FUN = function(x) {
        if (all(is.na(x))) {
          return(NA_real_)
        }
        mean(x, na.rm = TRUE)
      }
    )
    names(scheduled_summary)[2] <- "Scheduled_Days"

    actual_summary <- aggregate(
      v2_data[["Days for shipping (real)"]],
      by = list(Shipping_Mode = v2_data[["Shipping Mode"]]),
      FUN = function(x) {
        if (all(is.na(x))) {
          return(NA_real_)
        }
        mean(x, na.rm = TRUE)
      }
    )
    names(actual_summary)[2] <- "Actual_Days"

    v2_summary <- merge(
      fulfilled_summary,
      late_summary,
      by = "Shipping_Mode"
    )
    v2_summary <- merge(
      v2_summary,
      scheduled_summary,
      by = "Shipping_Mode"
    )
    v2_summary <- merge(
      v2_summary,
      actual_summary,
      by = "Shipping_Mode"
    )

    v2_summary$Shipping_Mode <- factor(
      v2_summary$Shipping_Mode,
      levels = shipping_order
    )
    v2_summary <- v2_summary[order(v2_summary$Shipping_Mode), ]

    v2_summary$Late_Rate <- (
      v2_summary$Late /
        v2_summary$Fulfilled
    ) * 100

    v2_summary$Service_Label <- paste0(
      "Scheduled: ",
      sprintf("%.1f", v2_summary$Scheduled_Days),
      "d\nActual avg: ",
      sprintf("%.1f", v2_summary$Actual_Days),
      "d"
    )

    if (input$shipping_mode == "All") {
      v2_summary$Bar_Alpha <- 1
    } else {
      v2_summary$Bar_Alpha <- ifelse(
        as.character(v2_summary$Shipping_Mode) == input$shipping_mode,
        1,
        0.35
      )
    }

    ggplot(
      v2_summary,
      aes(x = Shipping_Mode, y = Late_Rate)
    ) +
      geom_col(
        aes(alpha = Bar_Alpha),
        width = 0.7,
        fill = "#2f6b8a"
      ) +
      scale_alpha_identity() +
      geom_text(
        aes(label = sprintf("%.1f%%", Late_Rate)),
        vjust = -0.5,
        size = 4
      ) +
      geom_text(
        aes(
          y = Late_Rate / 2,
          label = Service_Label
        ),
        colour = "white",
        size = 3.5,
        lineheight = 0.95
      ) +
      scale_y_continuous(
        limits = c(0, 110),
        breaks = seq(0, 100, by = 20),
        labels = function(x) paste0(x, "%")
      ) +
      labs(
        title = NULL,
        subtitle = "Late-delivery rate with scheduled and average actual shipping times",
        x = "Shipping mode",
        y = "Late-delivery rate (%)",
        caption = paste(
          "Cancelled orders are excluded.",
          "All shipping modes remain visible for comparison."
        )
      ) +
      dashboard_plot_theme +
      theme(
        panel.grid.major.x = element_blank()
      )
  })

  # ==========================================================
  # V3 SUMMARY DATA
  # ==========================================================

  v3_summary_data <- reactive({
    dat <- filtered_orders()
    dat <- dat[dat$is_fulfilled %in% TRUE, ]

    geo_variable <- input$geo_level

    valid_geo <- (
      !is.na(dat[[geo_variable]]) &
        trimws(as.character(dat[[geo_variable]])) != ""
    )

    dat <- dat[valid_geo, ]

    validate(
      need(
        nrow(dat) > 0,
        "No fulfilled orders are available for the selected filters."
      )
    )

    geo_values <- as.character(dat[[geo_variable]])

    geo_fulfilled <- aggregate(
      rep(1, nrow(dat)),
      by = list(Geography = geo_values),
      FUN = sum
    )
    names(geo_fulfilled)[2] <- "Fulfilled"

    geo_late <- aggregate(
      as.integer(dat$is_late),
      by = list(Geography = geo_values),
      FUN = sum,
      na.rm = TRUE
    )
    names(geo_late)[2] <- "Late"

    late_sales_values <- ifelse(
      dat$is_late %in% TRUE &
        !is.na(dat[["Order Net Sales"]]),
      dat[["Order Net Sales"]],
      0
    )

    geo_sales <- aggregate(
      late_sales_values,
      by = list(Geography = geo_values),
      FUN = sum,
      na.rm = TRUE
    )
    names(geo_sales)[2] <- "Late_Net_Sales"

    summary_data <- merge(
      geo_fulfilled,
      geo_late,
      by = "Geography"
    )
    summary_data <- merge(
      summary_data,
      geo_sales,
      by = "Geography"
    )

    summary_data$Late_Rate <- (
      summary_data$Late /
        summary_data$Fulfilled
    ) * 100

    summary_data
  })

  # ==========================================================
  # V3 CLICK INTERACTION
  # ==========================================================

  observeEvent(input$v3_click, {
    clicked <- nearPoints(
      v3_summary_data(),
      input$v3_click,
      xvar = "Late_Rate",
      yvar = "Late_Net_Sales",
      threshold = 15,
      maxpoints = 1
    )

    if (nrow(clicked) == 1) {
      selected_geo(as.character(clicked$Geography[1]))
    }
  })

  # Context-preserving linked-selection status.
  output$geo_context <- renderUI({
    if (is.null(selected_geo())) {
      div(
        class = "geo-context neutral",
        tags$strong("Linked drill-down: "),
        "No geography selected. Click a point in V3 to focus V4 and V5 on that location."
      )
    } else {
      div(
        class = "geo-context active",
        tags$strong("Drill-down active: "),
        paste0(input$geo_level, " = ", selected_geo()),
        actionButton(
          inputId = "clear_geo",
          label = "Clear geography",
          class = "clear-selection-btn"
        )
      )
    }
  })

  observeEvent(
    input$clear_geo,
    {
      selected_geo(NULL)
    },
    ignoreInit = TRUE
  )

  # ==========================================================
  # V3 — LOCATE
  # ==========================================================

  output$plot_v3 <- renderPlot({
    v3_summary <- v3_summary_data()

    current_orders <- filtered_orders()
    current_orders <- current_orders[
      current_orders$is_fulfilled %in% TRUE,
    ]

    validate(
      need(
        nrow(current_orders) > 0,
        "No fulfilled orders are available."
      )
    )

    current_late_rate <- (
      sum(current_orders$is_late, na.rm = TRUE) /
        nrow(current_orders)
    ) * 100

    label_order <- order(
      v3_summary$Late_Net_Sales,
      decreasing = TRUE
    )

    label_count <- min(5, nrow(v3_summary))
    label_regions <- v3_summary[
      label_order[seq_len(label_count)],
    ]

    if (is.null(selected_geo())) {
      selection_caption <- paste(
        "Dashed line = current filtered late-delivery rate.",
        "Click a point to drill V4 and V5 into that geography."
      )
    } else {
      selection_caption <- paste0(
        "Selected ",
        input$geo_level,
        ": ",
        selected_geo(),
        ". V4 and V5 are drilled into this geography."
      )
    }

    p <- ggplot(
      v3_summary,
      aes(x = Late_Rate, y = Late_Net_Sales)
    ) +
      geom_point(size = 3, alpha = 0.8) +
      geom_vline(
        xintercept = current_late_rate,
        linetype = "dashed"
      ) +
      geom_text(
        data = label_regions,
        aes(label = Geography),
        vjust = -0.7,
        size = 3.2,
        show.legend = FALSE,
        check_overlap = TRUE
      ) +
      scale_x_continuous(
        labels = function(x) paste0(round(x, 1), "%")
      ) +
      scale_y_continuous(
        labels = function(x) {
          ifelse(
            abs(x) >= 1000000,
            paste0("$", round(x / 1000000, 1), "M"),
            paste0("$", round(x / 1000, 0), "K")
          )
        },
        expand = expansion(mult = c(0.05, 0.15))
      ) +
      labs(
        title = NULL,
        subtitle = paste0(
          input$geo_level,
          ": late-delivery rate versus late-order net sales"
        ),
        x = "Late-delivery rate",
        y = "Net sales associated with late fulfilled orders",
        caption = selection_caption
      ) +
      dashboard_plot_theme

    if (!is.null(selected_geo())) {
      selected_point <- v3_summary[
        v3_summary$Geography == selected_geo(),
      ]

      if (nrow(selected_point) > 0) {
        p <- p +
          geom_point(
            data = selected_point,
            size = 6,
            shape = 21,
            stroke = 1.4,
            fill = "gold"
          )
      }
    }

    p
  })

  # ==========================================================
  # V4 — DIAGNOSE
  # ==========================================================

  output$plot_v4 <- renderPlot({
    dat <- analysis_orders()

    late_order_ids <- dat[
      dat$is_fulfilled %in% TRUE &
        dat$is_late %in% TRUE,
      "Order Id"
    ]

    validate(
      need(
        length(late_order_ids) > 0,
        "No late fulfilled orders are available for this selection."
      )
    )

    v4_items <- data_raw[
      data_raw[["Order Id"]] %in% late_order_ids &
        !is.na(data_raw[["Category Name"]]) &
        trimws(as.character(data_raw[["Category Name"]])) != "" &
        !is.na(data_raw[["Customer Segment"]]),
    ]

    validate(
      need(
        nrow(v4_items) > 0,
        "No product data are available for this selection."
      )
    )

    category_totals <- aggregate(
      v4_items[["Order Item Total"]],
      by = list(Category = v4_items[["Category Name"]]),
      FUN = function(x) sum(x, na.rm = TRUE)
    )
    names(category_totals)[2] <- "Late_Net_Sales"

    category_totals <- category_totals[
      order(category_totals$Late_Net_Sales, decreasing = TRUE),
    ]

    top_n <- min(12, nrow(category_totals))
    top_categories <- category_totals$Category[seq_len(top_n)]

    top_items <- v4_items[
      v4_items[["Category Name"]] %in% top_categories,
    ]

    v4_summary <- aggregate(
      top_items[["Order Item Total"]],
      by = list(
        Category = top_items[["Category Name"]],
        Customer_Segment = top_items[["Customer Segment"]]
      ),
      FUN = function(x) sum(x, na.rm = TRUE)
    )
    names(v4_summary)[3] <- "Late_Net_Sales"

    if (input$customer_segment == "All") {
      segment_levels <- c(
        "Consumer",
        "Corporate",
        "Home Office"
      )
    } else {
      segment_levels <- input$customer_segment
    }

    full_grid <- expand.grid(
      Category = top_categories,
      Customer_Segment = segment_levels,
      stringsAsFactors = FALSE
    )

    v4_summary <- merge(
      full_grid,
      v4_summary,
      by = c("Category", "Customer_Segment"),
      all.x = TRUE
    )

    v4_summary$Late_Net_Sales[
      is.na(v4_summary$Late_Net_Sales)
    ] <- 0

    v4_summary$Category <- factor(
      v4_summary$Category,
      levels = rev(top_categories)
    )

    v4_summary$Customer_Segment <- factor(
      v4_summary$Customer_Segment,
      levels = segment_levels
    )

    v4_summary$Exposure_Label <- ifelse(
      v4_summary$Late_Net_Sales == 0,
      "",
      ifelse(
        v4_summary$Late_Net_Sales >= 1000000,
        paste0(
          "$",
          sprintf("%.1f", v4_summary$Late_Net_Sales / 1000000),
          "M"
        ),
        paste0(
          "$",
          round(v4_summary$Late_Net_Sales / 1000),
          "K"
        )
      )
    )

    positive_values <- v4_summary$Late_Net_Sales[
      v4_summary$Late_Net_Sales > 0
    ]

    if (length(positive_values) > 0) {
      text_threshold <- median(positive_values)
    } else {
      text_threshold <- 0
    }

    v4_summary$Text_Colour <- ifelse(
      v4_summary$Late_Net_Sales >= text_threshold &
        v4_summary$Late_Net_Sales > 0,
      "white",
      "black"
    )

    total_exposure <- sum(
      v4_items[["Order Item Total"]],
      na.rm = TRUE
    )

    top_exposure <- sum(
      category_totals$Late_Net_Sales[seq_len(top_n)],
      na.rm = TRUE
    )

    top_share <- ifelse(
      total_exposure > 0,
      (top_exposure / total_exposure) * 100,
      NA_real_
    )

    if (is.null(selected_geo())) {
      geo_text <- "Current dashboard filters"
    } else {
      geo_text <- paste0(
        input$geo_level,
        ": ",
        selected_geo()
      )
    }

    ggplot(
      v4_summary,
      aes(
        x = Customer_Segment,
        y = Category,
        fill = Late_Net_Sales
      )
    ) +
      geom_tile(
        colour = "white",
        linewidth = 0.8
      ) +
      geom_text(
        aes(
          label = Exposure_Label,
          colour = Text_Colour
        ),
        size = 3.4
      ) +
      scale_colour_identity() +
      scale_fill_gradient(
        low = "#dceaf4",
        high = "#21618c",
        labels = function(x) {
          ifelse(
            x >= 1000000,
            paste0("$", round(x / 1000000, 1), "M"),
            paste0("$", round(x / 1000), "K")
          )
        }
      ) +
      labs(
        title = NULL,
        subtitle = paste0(
          "Late-order net sales by product category and customer segment — ",
          geo_text
        ),
        x = "Customer segment",
        y = NULL,
        fill = "Late-order\nnet sales",
        caption = sprintf(
          "Top %d categories account for %.1f%% of late-order net sales exposure under the current selection.",
          top_n,
          top_share
        )
      ) +
      dashboard_plot_theme +
      theme(
        panel.grid = element_blank()
      )
  })

  # ==========================================================
  # V5 — PRIORITISE
  # ==========================================================

  output$plot_v5 <- renderPlot({
    dat <- analysis_orders()

    v5_data <- dat[
      dat$is_fulfilled %in% TRUE,
    ]

    validate(
      need(
        nrow(v5_data) > 0,
        "No fulfilled orders are available for this selection."
      )
    )

    v5_data$Fulfilment_Status <- ifelse(
      v5_data$is_late %in% TRUE,
      "Late",
      "On time"
    )

    v5_data$Fulfilment_Status <- factor(
      v5_data$Fulfilment_Status,
      levels = c("On time", "Late")
    )

    get_box_stats <- function(x) {
      x <- x[!is.na(x)]

      if (length(x) == 0) {
        return(NULL)
      }

      stats <- boxplot.stats(x)$stats

      data.frame(
        ymin = stats[1],
        lower = stats[2],
        middle = stats[3],
        upper = stats[4],
        ymax = stats[5]
      )
    }

    statuses <- c("On time", "Late")
    metrics <- c("Order Net Sales", "Order Profit")

    stats_rows <- list()
    counter <- 1

    for (status in statuses) {
      status_data <- v5_data[
        as.character(v5_data$Fulfilment_Status) == status,
      ]

      for (metric in metrics) {
        box_result <- get_box_stats(status_data[[metric]])

        if (!is.null(box_result)) {
          box_result$Fulfilment_Status <- status
          box_result$Metric <- metric
          stats_rows[[counter]] <- box_result
          counter <- counter + 1
        }
      }
    }

    validate(
      need(
        length(stats_rows) > 0,
        "No commercial outcome data are available for this selection."
      )
    )

    v5_box_stats <- do.call(rbind, stats_rows)

    v5_box_stats$Fulfilment_Status <- factor(
      v5_box_stats$Fulfilment_Status,
      levels = c("On time", "Late")
    )

    v5_box_stats$Metric <- factor(
      v5_box_stats$Metric,
      levels = c("Order Net Sales", "Order Profit")
    )

    if (is.null(selected_geo())) {
      geo_text <- "Current dashboard filters"
    } else {
      geo_text <- paste0(
        input$geo_level,
        ": ",
        selected_geo()
      )
    }

    ggplot(
      v5_box_stats,
      aes(
        x = Fulfilment_Status,
        fill = Fulfilment_Status
      )
    ) +
      geom_boxplot(
        aes(
          ymin = ymin,
          lower = lower,
          middle = middle,
          upper = upper,
          ymax = ymax
        ),
        stat = "identity",
        width = 0.6
      ) +
      facet_wrap(
        ~ Metric,
        scales = "free_y"
      ) +
      scale_fill_manual(
        values = c(
          "On time" = "#8fb9cf",
          "Late" = "#d97967"
        )
      ) +
      labs(
        title = NULL,
        subtitle = paste0(
          "Order-level net sales and profit among fulfilled orders — ",
          geo_text
        ),
        x = "Delivery status",
        y = "Order value ($)",
        fill = "Delivery status",
        caption = paste(
          "Boxplot statistics use all non-missing orders;",
          "extreme outlier points are not displayed.",
          "The comparison is observational and does not imply causation."
        )
      ) +
      dashboard_plot_theme +
      theme(
        legend.position = "none"
      )
  })

  # ==========================================================
  # MANAGEMENT IMPLICATION — RESOLUTION / NEXT ACTION
  # ==========================================================

  output$management_implication <- renderUI({
    req(input$date_range)

    # Preserve V2's comparison logic: date + customer segment apply,
    # while all shipping modes remain available for comparison.
    mode_data <- order_data[
      !is.na(order_data[["order_date"]]) &
        order_data[["order_date"]] >= input$date_range[1] &
        order_data[["order_date"]] <= input$date_range[2] &
        order_data$is_fulfilled %in% TRUE &
        !is.na(order_data[["Shipping Mode"]]),
    ]

    if (input$customer_segment != "All") {
      mode_data <- mode_data[
        !is.na(mode_data[["Customer Segment"]]) &
          mode_data[["Customer Segment"]] == input$customer_segment,
      ]
    }

    if (nrow(mode_data) == 0) {
      return(
        tags$div(
          "No fulfilled orders are available to generate a management implication under the current selection."
        )
      )
    }

    mode_summary <- aggregate(
      as.integer(mode_data$is_late),
      by = list(Shipping_Mode = mode_data[["Shipping Mode"]]),
      FUN = function(x) mean(x, na.rm = TRUE)
    )
    names(mode_summary)[2] <- "Late_Rate"
    mode_summary$Late_Rate <- mode_summary$Late_Rate * 100

    worst_mode_row <- mode_summary[
      which.max(mode_summary$Late_Rate),
    ]

    geo_summary <- v3_summary_data()
    top_geo_row <- geo_summary[
      which.max(geo_summary$Late_Net_Sales),
    ]

    if (is.null(selected_geo())) {
      geography_action <- paste0(
        "At the current ",
        input$geo_level,
        " level, ",
        top_geo_row$Geography[1],
        " carries the largest late-order net sales exposure (",
        sprintf("$%.2fM", top_geo_row$Late_Net_Sales[1] / 1000000),
        "). Use V3 to compare rate and scale, then drill V4 and V5 into the locations that warrant investigation."
      )
    } else {
      geography_action <- paste0(
        "A geographic drill-down is active for ",
        input$geo_level,
        " = ",
        selected_geo(),
        ". Use V4 to identify the product/customer concentrations inside this selected exposure and V5 to assess its commercial context."
      )
    }

    tags$ul(
      tags$li(
        tags$strong("Service reliability: "),
        paste0(
          "Start root-cause investigation with ",
          as.character(worst_mode_row$Shipping_Mode[1]),
          ", which has the highest late-delivery rate under the current date/customer comparison (",
          sprintf("%.1f%%", worst_mode_row$Late_Rate[1]),
          ")."
        )
      ),
      tags$li(
        tags$strong("Exposure targeting: "),
        geography_action
      ),
      tags$li(
        tags$strong("Decision caution: "),
        "Use late-order net sales to prioritise commercially important operational exposure. V5 is descriptive and does not establish that late delivery causes lower sales or profit."
      )
    )
  })

  # ==========================================================
  # RESET FILTERS
  # ==========================================================

  observeEvent(input$reset_filters, {
    selected_geo(NULL)

    updateDateRangeInput(
      session = session,
      inputId = "date_range",
      start = dashboard_min_date,
      end = dashboard_max_date,
      min = dashboard_min_date,
      max = dashboard_max_date
    )

    updateSelectInput(
      session = session,
      inputId = "shipping_mode",
      selected = "All"
    )

    updateSelectInput(
      session = session,
      inputId = "geo_level",
      selected = "Order Region"
    )

    updateSelectInput(
      session = session,
      inputId = "customer_segment",
      selected = "All"
    )
  })
}

# ============================================================
# 10. RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
