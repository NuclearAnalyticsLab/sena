# R/mobile_utils.R
# Mobile-specific utility functions for NUCLEARFF app

#' Detect Mobile Device
#'
#' Determines if the current session is on a mobile device
#'
#' @param session Shiny session object
#' @return Logical indicating if device is mobile
#' @export
is_mobile_device <- function(session) {
  user_agent <- session$request$HTTP_USER_AGENT
  if (is.null(user_agent)) return(FALSE)

  mobile_patterns <- c(
    "Mobile", "Android", "iPhone", "iPad", "iPod",
    "BlackBerry", "Windows Phone", "Opera Mini"
  )

  any(sapply(mobile_patterns, function(pattern) {
    grepl(pattern, user_agent, ignore.case = TRUE)
  }))
}

#' Get Device Information
#'
#' Extracts device information from user agent
#'
#' @param session Shiny session object
#' @return List with device information
#' @export
get_device_info <- function(session) {
  user_agent <- session$request$HTTP_USER_AGENT %||% ""

  # Detect device type
  device_type <- case_when(
    grepl("iPhone|iPod", user_agent, ignore.case = TRUE) ~ "iPhone",
    grepl("iPad", user_agent, ignore.case = TRUE) ~ "iPad",
    grepl("Android", user_agent, ignore.case = TRUE) ~ "Android",
    grepl("Windows Phone", user_agent, ignore.case = TRUE) ~ "Windows Phone",
    TRUE ~ "Desktop"
  )

  # Detect browser
  browser <- case_when(
    grepl("Chrome", user_agent) ~ "Chrome",
    grepl("Safari", user_agent) & !grepl("Chrome", user_agent) ~ "Safari",
    grepl("Firefox", user_agent) ~ "Firefox",
    grepl("Edge", user_agent) ~ "Edge",
    TRUE ~ "Unknown"
  )

  # Check for PWA installation
  is_pwa <- !is.null(session$request$HTTP_X_PWA) ||
            grepl("standalone", user_agent, ignore.case = TRUE)

  list(
    device_type = device_type,
    browser = browser,
    is_mobile = device_type != "Desktop",
    is_pwa = is_pwa,
    user_agent = user_agent,
    supports_touch = device_type != "Desktop"
  )
}

#' Create Mobile-Optimized DataTable
#'
#' Generates a DataTable with mobile-specific configurations
#'
#' @param data Data frame to display
#' @param page_length Number of rows per page (default: 10 for mobile)
#' @param scrollX Enable horizontal scrolling (default: TRUE)
#' @param responsive Enable responsive features (default: TRUE)
#' @param ... Additional DT::datatable arguments
#' @return DT datatable object
#' @export
mobile_datatable <- function(data,
                           page_length = 10,
                           scrollX = TRUE,
                           responsive = TRUE,
                           ...) {

  # Mobile-optimized options
  mobile_options <- list(
    responsive = responsive,
    scrollX = scrollX,
    scrollCollapse = TRUE,
    pageLength = page_length,
    lengthMenu = c(5, 10, 15, 25),
    dom = 'ftip', # Simplified layout for mobile
    columnDefs = list(
      list(
        targets = '_all',
        className = 'dt-center mobile-cell'
      )
    ),
    language = list(
      search = "Filter:",
      lengthMenu = "Show _MENU_",
      info = "_START_ to _END_ of _TOTAL_",
      paginate = list(
        first = "«",
        last = "»",
        next = "›",
        previous = "‹"
      )
    ),
    initComplete = JS(
      "function(settings, json) {",
      "  $(this.api().table().container()).addClass('mobile-table');",
      "  // Add touch-friendly styling",
      "  $('.dataTables_paginate .paginate_button').addClass('mobile-page-btn');",
      "}"
    )
  )

  DT::datatable(
    data,
    options = mobile_options,
    class = 'cell-border stripe compact mobile-optimized',
    rownames = FALSE,
    ...
  )
}

#' Mobile-Friendly Notification
#'
#' Shows notifications optimized for mobile devices
#'
#' @param title Notification title
#' @param text Notification text
#' @param type Type of notification ("success", "error", "warning", "info")
#' @param duration Duration in milliseconds (default: 3000)
#' @param position Position on screen (default: "top")
#' @export
mobile_notify <- function(title, text, type = "info", duration = 3000, position = "top") {

  # Map types to Framework7 colors
  color_map <- list(
    "success" = "green",
    "error" = "red",
    "warning" = "orange",
    "info" = "blue"
  )

  # Map types to icons
  icon_map <- list(
    "success" = "checkmark_circle_fill",
    "error" = "xmark_circle_fill",
    "warning" = "exclamationmark_triangle_fill",
    "info" = "info_circle_fill"
  )

  f7Notification(
    title = title,
    text = text,
    icon = f7Icon(icon_map[[type]] %||% "info_circle_fill"),
    closeTimeout = duration,
    closeButton = TRUE,
    closeOnClick = TRUE
  )
}

#' Mobile Loading Indicator
#'
#' Shows a mobile-optimized loading indicator
#'
#' @param text Loading text (default: "Loading...")
#' @param color Loading indicator color (default: "blue")
#' @return Shiny UI element
#' @export
mobile_loading <- function(text = "Loading...", color = "blue") {
  tags$div(
    class = "mobile-loading-container",
    style = "text-align: center; padding: 40px 20px;",

    f7Preloader(color = color, size = 42),
    tags$br(),
    tags$div(
      text,
      style = "margin-top: 15px; font-size: 16px; color: var(--f7-text-color);"
    )
  )
}

#' Mobile Action Sheet Helper
#'
#' Creates a mobile-friendly action sheet with common options
#'
#' @param title Action sheet title
#' @param actions List of actions with text, icon, color, and handler
#' @param cancel_text Text for cancel button (default: "Cancel")
#' @return Action sheet configuration
#' @export
create_mobile_action_sheet <- function(title, actions, cancel_text = "Cancel") {

  # Format actions for Framework7
  formatted_actions <- purrr::map(actions, function(action) {
    list(
      text = action$text,
      icon = if (!is.null(action$icon)) f7Icon(action$icon) else NULL,
      color = action$color %||% "blue",
      onClick = action$handler %||% ""
    )
  })

  # Add cancel button
  formatted_actions <- append(formatted_actions, list(
    list(text = cancel_text, color = "red")
  ))

  f7ActionSheet(
    title = title,
    buttons = formatted_actions
  )
}

#' Mobile Touch Feedback
#'
#' Adds haptic feedback for mobile interactions
#'
#' @param session Shiny session object
#' @param type Type of feedback ("light", "medium", "heavy")
#' @export
add_haptic_feedback <- function(session, type = "light") {

  # JavaScript for haptic feedback
  haptic_js <- sprintf(
    "if ('vibrate' in navigator) {
       var duration = %s;
       navigator.vibrate(duration);
     }
     if (window.navigator && window.navigator.vibrate) {
       window.navigator.vibrate(%s);
     }",
    switch(type,
      "light" = "10",
      "medium" = "20",
      "heavy" = "50",
      "10"
    ),
    switch(type,
      "light" = "10",
      "medium" = "[20]",
      "heavy" = "[50]",
      "10"
    )
  )

  session$sendCustomMessage(type = "haptic-feedback", message = haptic_js)
}

#' Mobile Swipe Gesture Handler
#'
#' Sets up swipe gesture detection for mobile
#'
#' @param element_id ID of element to attach swipe detection
#' @param left_handler JavaScript function for left swipe
#' @param right_handler JavaScript function for right swipe
#' @param threshold Minimum distance for swipe (default: 50px)
#' @return JavaScript code for swipe detection
#' @export
setup_mobile_swipe <- function(element_id, left_handler = NULL, right_handler = NULL, threshold = 50) {

  js_code <- sprintf("
    // Mobile swipe detection for element: %s
    (function() {
      var element = document.getElementById('%s');
      if (!element) return;

      var startX = 0;
      var startY = 0;
      var threshold = %d;

      element.addEventListener('touchstart', function(e) {
        startX = e.touches[0].clientX;
        startY = e.touches[0].clientY;
      });

      element.addEventListener('touchend', function(e) {
        var endX = e.changedTouches[0].clientX;
        var endY = e.changedTouches[0].clientY;

        var deltaX = endX - startX;
        var deltaY = endY - startY;

        // Check if horizontal swipe is dominant
        if (Math.abs(deltaX) > Math.abs(deltaY) && Math.abs(deltaX) > threshold) {
          if (deltaX > 0) {
            // Right swipe
            %s
          } else {
            // Left swipe
            %s
          }
        }
      });
    })();
  ",
  element_id,
  element_id,
  threshold,
  right_handler %||% "console.log('Right swipe detected');",
  left_handler %||% "console.log('Left swipe detected');"
  )

  tags$script(HTML(js_code))
}

#' Mobile Chart Configuration
#'
#' Returns ggplot2 theme optimized for mobile displays
#'
#' @param base_size Base font size (default: 12 for mobile)
#' @param dark_mode Whether to use dark mode theme
#' @return ggplot2 theme object
#' @export
mobile_chart_theme <- function(base_size = 12, dark_mode = FALSE) {

  base_theme <- ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      # Mobile-optimized text sizes
      text = ggplot2::element_text(size = base_size),
      axis.text = ggplot2::element_text(size = base_size - 2),
      axis.title = ggplot2::element_text(size = base_size - 1),
      plot.title = ggplot2::element_text(size = base_size + 2, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = base_size),
      legend.text = ggplot2::element_text(size = base_size - 2),
      legend.title = ggplot2::element_text(size = base_size - 1),

      # Mobile-friendly layout
      legend.position = "bottom",
      legend.direction = "horizontal",
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 10, 10, 10),

      # Increase line sizes for touch targets
      axis.ticks = ggplot2::element_line(size = 0.5),
      panel.grid.major = ggplot2::element_line(size = 0.3),

      # Better spacing for mobile
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 5)),
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 5)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10))
    )

  # Apply dark mode modifications
  if (dark_mode) {
    base_theme <- base_theme +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = "#1a1a1a", color = NA),
        plot.background = ggplot2::element_rect(fill = "#1a1a1a", color = NA),
        text = ggplot2::element_text(color = "white"),
        axis.text = ggplot2::element_text(color = "white"),
        axis.title = ggplot2::element_text(color = "white"),
        panel.grid.major = ggplot2::element_line(color = "#333333", size = 0.2),
        legend.background = ggplot2::element_rect(fill = "#1a1a1a", color = NA),
        legend.key = ggplot2::element_rect(fill = "#1a1a1a", color = NA)
      )
  }

  base_theme
}

#' Mobile Data Export
#'
#' Handles data export optimized for mobile file handling
#'
#' @param data Data frame to export
#' @param filename Base filename (without extension)
#' @param format Export format ("xlsx", "csv", "json")
#' @param session Shiny session object
#' @export
mobile_export_data <- function(data, filename, format = "xlsx", session) {

  # Validate inputs
  if (is.null(data) || nrow(data) == 0) {
    mobile_notify(
      title = "Export Failed",
      text = "No data available to export",
      type = "error"
    )
    return()
  }

  # Generate timestamped filename
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  full_filename <- paste0(filename, "_", timestamp, ".", format)

  tryCatch({

    # Export based on format
    if (format == "xlsx") {
      openxlsx::write.xlsx(data, full_filename)

    } else if (format == "csv") {
      readr::write_csv(data, full_filename)

    } else if (format == "json") {
      jsonlite::write_json(data, full_filename, pretty = TRUE)

    } else {
      stop("Unsupported format: ", format)
    }

    # Success notification
    mobile_notify(
      title = "Export Successful",
      text = paste("Data exported to", full_filename),
      type = "success"
    )

    # Add download handler if in Shiny environment
    if (!is.null(session)) {
      session$sendCustomMessage(
        type = "file-ready",
        message = list(filename = full_filename, format = format)
      )
    }

  }, error = function(e) {
    mobile_notify(
      title = "Export Failed",
      text = paste("Error:", e$message),
      type = "error"
    )
  })
}

#' Mobile Performance Monitor
#'
#' Tracks app performance metrics for mobile optimization
#'
#' @param session Shiny session object
#' @return Performance monitoring setup
#' @export
setup_mobile_performance_monitor <- function(session, input) {

  # JavaScript for performance monitoring
  perf_js <- "
    // Mobile performance monitoring
    window.mobilePerf = {
      startTime: performance.now(),
      metrics: {},

      // Track page load performance
      trackLoad: function() {
        window.addEventListener('load', function() {
          const loadTime = performance.timing.loadEventEnd - performance.timing.navigationStart;
          window.mobilePerf.metrics.loadTime = loadTime;
          console.log('Mobile app load time:', loadTime + 'ms');
        });
      },

      // Track memory usage (if available)
      trackMemory: function() {
        if (performance.memory) {
          setInterval(function() {
            window.mobilePerf.metrics.memory = {
              used: Math.round(performance.memory.usedJSHeapSize / 1024 / 1024),
              total: Math.round(performance.memory.totalJSHeapSize / 1024 / 1024),
              limit: Math.round(performance.memory.jsHeapSizeLimit / 1024 / 1024)
            };
          }, 5000);
        }
      },

      // Track user interactions
      trackInteractions: function() {
        let interactionCount = 0;
        ['click', 'touchstart', 'keydown'].forEach(function(eventType) {
          document.addEventListener(eventType, function() {
            interactionCount++;
            window.mobilePerf.metrics.interactions = interactionCount;
          });
        });
      },

      // Send metrics to Shiny server
      sendMetrics: function() {
        if (window.Shiny) {
          Shiny.setInputValue('mobile_performance', window.mobilePerf.metrics, {priority: 'event'});
        }
      }
    };

    // Initialize monitoring
    window.mobilePerf.trackLoad();
    window.mobilePerf.trackMemory();
    window.mobilePerf.trackInteractions();

    // Send metrics every 30 seconds
    setInterval(window.mobilePerf.sendMetrics, 30000);
  "

  # Add JavaScript to session
  session$sendCustomMessage(type = "eval", message = perf_js)

  # Set up server-side performance tracking
  observe({
    perf_data <- session$input$mobile_performance
    if (!is.null(perf_data)) {
      # Log performance metrics
      message("Mobile Performance Metrics: ", jsonlite::toJSON(perf_data))

      # Alert if performance issues detected
      if (!is.null(perf_data$loadTime) && perf_data$loadTime > 5000) {
        message("WARNING: Slow load time detected (", perf_data$loadTime, "ms)")
      }

      if (!is.null(perf_data$memory) && perf_data$memory$used > 100) {
        message("WARNING: High memory usage detected (", perf_data$memory$used, "MB)")
      }
    }
  })
}

#' Null Coalescing Operator
#'
#' Returns left-hand side unless it's NULL, then returns right-hand side
#'
#' @param lhs Left-hand side value
#' @param rhs Right-hand side value
#' @return Non-NULL value
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}
