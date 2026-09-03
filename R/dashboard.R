# Shiny dashboard --------------------------------------------------------

#' Launch the interactive forensics dashboard
#'
#' Requires the \code{shiny} package (optional dependency).
#'
#' @param streams named list of data.frames (date + value), or a single one
#' @param value name of the numeric column
#' @param date name of the date column
#' @param launch run the app immediately (TRUE) or return the app object (FALSE)
#' @return a \code{shiny.appobj} (when \code{launch = FALSE})
#' @export
run_dashboard <- function(streams, value = "amount", date = "date",
                          launch = TRUE) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("package 'shiny' is required; install with install.packages('shiny')")
  }
  if (is.data.frame(streams)) streams <- list(stream = streams)
  if (is.null(names(streams))) names(streams) <- paste0("stream", seq_along(streams))
  stream_names <- names(streams)
  streams <- lapply(stream_names, function(nm) {
    s <- streams[[nm]]
    vcol <- .resolve_col(s, nm, value, default = "value")
    dcol <- .resolve_col(s, nm, date, default = "date")
    d <- data.frame(
      date = as.Date(s[[dcol]]),
      value = as.numeric(s[[vcol]])
    )
    d[is.finite(d$value), c("date", "value")]
  })
  names(streams) <- stream_names

  ui <- shiny::fluidPage(
    shiny::titlePanel("个人数据数字取证 · 体检台"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput("stream", "数据流", choices = names(streams)),
        shiny::sliderInput("k", "异常阈值 (MAD-z)", 2, 6, 3.5, step = 0.5),
        shiny::hr(),
        shiny::textOutput("verdict")
      ),
      shiny::mainPanel(
        shiny::plotOutput("weekly"),
        shiny::h4("异常周"),
        shiny::tableOutput("anomalies"),
        shiny::h4("Benford 首位数字"),
        shiny::plotOutput("benford", height = "260px")
      )
    )
  )

  server <- function(input, output, session) {
    data <- shiny::reactive({
      s <- streams[[input$stream]]
      wk <- weekly_totals(s, "date", "value")
      detect_anomalies(wk, "total", k = input$k)
    })
    output$verdict <- shiny::renderText({
      x <- streams[[input$stream]]$value
      x <- x[x > 0]
      if (length(x) < 50) return("样本不足 50 个,Benford 检验暂不可靠")
      sprintf("Benford 判定: %s (MAD = %.4f)", benford_test(x)$verdict,
              benford_test(x)$mad)
    })
    output$weekly <- shiny::renderPlot({
      d <- data()
      ggplot2::ggplot(d, ggplot2::aes(.data$week_start, .data$total)) +
        ggplot2::geom_line(ggplot2::aes(group = 1), colour = "#0284c7", linewidth = 0.8) +
        ggplot2::geom_point(ggplot2::aes(colour = .data$.anomaly), size = 3) +
        ggplot2::scale_colour_manual(values = c("FALSE" = "#38bdf8", "TRUE" = "#ef4444"),
                                     name = "异常周", labels = c("FALSE" = "正常", "TRUE" = "异常")) +
        ggplot2::labs(x = NULL, y = "周合计") +
        ggplot2::theme_minimal(base_size = 13, base_family = .plot_family()) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
    })
    output$anomalies <- shiny::renderTable({
      d <- data()
      d <- d[d$.anomaly, c("week", "week_start", "total")]
      if (nrow(d) == 0) d <- data.frame(备注 = "无异常周")
      d
    })
    output$benford <- shiny::renderPlot({
      x <- streams[[input$stream]]$value
      x <- x[x > 0]
      if (length(x) < 50) {
        return(ggplot2::ggplot() + ggplot2::labs(title = "样本不足") +
               ggplot2::theme_void())
      }
      bt <- benford_test(x)
      df <- data.frame(digit = names(bt$observed),
                       observed = as.numeric(bt$proportion),
                       expected = as.numeric(bt$expected) / bt$n)
      df$digit <- factor(df$digit, levels = df$digit)
      ggplot2::ggplot(df, ggplot2::aes(.data$digit)) +
        ggplot2::geom_col(ggplot2::aes(y = .data$observed), fill = "#38bdf8") +
        ggplot2::geom_line(ggplot2::aes(y = .data$expected, group = 1),
                           colour = "#0f172a") +
        ggplot2::geom_point(ggplot2::aes(y = .data$expected)) +
        ggplot2::labs(x = "首位数字", y = "比例") +
        ggplot2::theme_minimal(base_size = 13, base_family = .plot_family()) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
    })
  }

  app <- shiny::shinyApp(ui, server)
  if (launch) {
    shiny::runApp(app)
  } else {
    app
  }
}
