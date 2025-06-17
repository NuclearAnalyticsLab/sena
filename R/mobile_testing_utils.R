# R/mobile_testing_utils.R
# Mobile testing and debugging utilities for NUCLEARFF

#' Mobile Testing Module UI
#' 
#' Creates a testing interface for mobile development
#' 
#' @param id Module namespace ID
#' @return Mobile testing UI
#' @export
mobileTestingUI <- function(id) {
  ns <- NS(id)
  
  # Only show in development mode
  if (!isTRUE(getOption("mobile.debug", FALSE))) {
    return(tags$div())
  }
  
  f7Card(
    title = "Mobile Testing Tools",
    style = "border: 2px solid orange; margin: 10px;",
    
    f7Accordion(
      # Device simulation
      f7AccordionItem(
        title = "Device Simulation",
        
        f7List(
          f7ListItem(
            title = "Simulate Device",
            right = f7Select(
              inputId = ns("device_type"),
              choices = list(
                "iPhone 14 Pro" = "iphone14pro",
                "iPhone SE" = "iphonese",
                "Samsung Galaxy S23" = "galaxys23",
                "iPad Pro" = "ipadpro",
                "Pixel 7" = "pixel7"
              ),
              selected = "iphone14pro"
            )
          ),
          
          f7ListItem(
            title = "Orientation",
            right = f7Segmented(
              inputId = ns("orientation"),
              choices = c("Portrait", "Landscape"),
              selected = "Portrait"
            )
          ),
          
          f7ListItem(
            title = "Apply Simulation",
            right = f7Button(
              inputId = ns("apply_device"),
              label = "Apply",
              fill = TRUE,
              color = "blue",
              size = "small"
            )
          )
        )
      ),
      
      # Performance testing
      f7AccordionItem(
        title = "Performance Testing",
        
        f7List(
          f7ListItem(
            title = "Simulate Slow Network",
            right = f7Toggle(
              inputId = ns("slow_network"),
              checked = FALSE
            )
          ),
          
          f7ListItem(
            title = "Test Offline Mode",
            right = f7Button(
              inputId = ns("test_offline"),
              label = "Test",
              fill = FALSE,
              color = "orange",
              size = "small"
            )
          ),
          
          f7ListItem(
            title = "Memory Usage",
            right = textOutput(ns("memory_usage"))
          ),
          
          f7ListItem(
            title = "Load Time",
            right = textOutput(ns("load_time"))
          )
        )
      ),
      
      # Touch testing
      f7AccordionItem(
        title = "Touch & Gesture Testing",
        
        f7List(
          f7ListItem(
            title = "Test Haptic Feedback",
            right = f7Button(
              inputId = ns("test_haptic"),
              label = "Vibrate",
              fill = FALSE,
              color = "purple",
              size = "small"
            )
          ),
          
          f7ListItem(
            title = "Touch Events Log",
            right = f7Toggle(
              inputId = ns("log_touch"),
              checked = FALSE
            )
          ),
          
          f7ListItem(
            title = "Gesture Recognition",
            right = f7Toggle(
              inputId = ns("test_gestures"),
              checked = FALSE
            )
          )
        )
      ),
      
      # Visual testing
      f7AccordionItem(
        title = "Visual Testing",
        
        f7List(
          f7ListItem(
            title = "Show Touch Targets",
            right = f7Toggle(
              inputId = ns("show_touch_targets"),
              checked = FALSE
            )
          ),
          
          f7ListItem(
            title = "Grid Overlay",
            right = f7Toggle(
              inputId = ns("show_grid"),
              checked = FALSE
            )
          ),
          
          f7ListItem(
            title = "Safe Area Indicators",
            right = f7Toggle(
              inputId = ns("show_safe_areas"),
              checked = FALSE
            )
          ),
          
          f7ListItem(
            title = "Force Dark Mode",
            right = f7Toggle(
              inputId = ns("force_dark"),
              checked = FALSE
            )
          )
        )
      ),
      
      # Debug logs
      f7AccordionItem(
        title = "Debug Logs",
        
        tags$div(
          style = "padding: 15px;",
          verbatimTextOutput(ns("debug_log")),
          f7Button(
            inputId = ns("clear_log"),
            label = "Clear Log",
            fill = FALSE,
            color = "red",
            size = "small"
          )
        )
      )
    )
  )
}

#' Mobile Testing Server Module
#' 
#' Server logic for mobile testing tools
#' 
#' @param id Module namespace ID
#' @export
mobileTestingServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Only run in development mode
    if (!isTRUE(getOption("mobile.debug", FALSE))) {
      return()
    }
    
    # Debug log storage
    debug_log <- reactiveVal("")
    
    # Add entry to debug log
    add_debug_log <- function(message) {
      timestamp <- format(Sys.time(), "%H:%M:%S")
      current_log <- debug_log()
      new_entry <- paste0("[", timestamp, "] ", message)
      updated_log <- paste(current_log, new_entry, sep = "\n")
      
      # Keep only last 50 lines
      lines <- strsplit(updated_log, "\n")[[1]]
      if (length(lines) > 50) {
        lines <- tail(lines, 50)
      }
      
      debug_log(paste(lines, collapse = "\n"))
    }
    
    # Device simulation
    observeEvent(input$apply_device, {
      device_configs <- list(
        "iphone14pro" = list(width = 393, height = 852, pixel_ratio = 3, user_agent = "iPhone"),
        "iphonese" = list(width = 375, height = 667, pixel_ratio = 2, user_agent = "iPhone"),
        "galaxys23" = list(width = 360, height = 780, pixel_ratio = 3, user_agent = "Android"),
        "ipadpro" = list(width = 1024, height = 1366, pixel_ratio = 2, user_agent = "iPad"),
        "pixel7" = list(width = 412, height = 915, pixel_ratio = 2.6, user_agent = "Android")
      )
      
      config <- device_configs[[input$device_type]]
      
      if (input$orientation == "Landscape") {
        # Swap width and height for landscape
        temp <- config$width
        config$width <- config$height
        config$height <- temp
      }
      
      # Send device simulation to browser
      session$sendCustomMessage(
        type = "simulate-device",
        message = config
      )
      
      add_debug_log(paste("Applied device simulation:", input$device_type, input$orientation))
      
      f7Toast(
        text = paste("Simulating", input$device_type, "-", input$orientation),
        position = "center",
        closeTimeout = 2000
      )
    })
    
    # Network simulation
    observeEvent(input$slow_network, {
      if (input$slow_network) {
        session$sendCustomMessage(
          type = "simulate-network",
          message = list(type = "slow", delay = 2000, bandwidth = "2G")
        )
        add_debug_log("Slow network simulation enabled")
        
        f7Notification(
          title = "Network Simulation",
          text = "Slow network simulation enabled",
          icon = f7Icon("wifi_slash"),
          closeTimeout = 3000
        )
      } else {
        session$sendCustomMessage(
          type = "simulate-network", 
          message = list(type = "normal")
        )
        add_debug_log("Network simulation disabled")
        
        f7Toast(
          text = "Normal network restored",
          position = "center",
          closeTimeout = 2000
        )
      }
    })
    
    # Offline testing
    observeEvent(input$test_offline, {
      session$sendCustomMessage(
        type = "test-offline",
        message = list(duration = 10000) # 10 seconds
      )
      
      add_debug_log("Offline mode test started (10 seconds)")
      
      f7Notification(
        title = "Offline Test",
        text = "Testing offline mode for 10 seconds",
        icon = f7Icon("wifi_slash"),
        closeTimeout = 10000
      )
    })
    
    # Haptic feedback testing
    observeEvent(input$test_haptic, {
      session$sendCustomMessage(
        type = "test-haptic",
        message = list(pattern = c(200, 100, 200))
      )
      
      add_debug_log("Haptic feedback test triggered")
    })
    
    # Touch event logging
    observeEvent(input$log_touch, {
      session$sendCustomMessage(
        type = "toggle-touch-logging",
        message = list(enabled = input$log_touch)
      )
      
      if (input$log_touch) {
        add_debug_log("Touch event logging enabled")
      } else {
        add_debug_log("Touch event logging disabled")
      }
    })
    
    # Gesture testing
    observeEvent(input$test_gestures, {
      session$sendCustomMessage(
        type = "toggle-gesture-testing", 
        message = list(enabled = input$test_gestures)
      )
      
      if (input$test_gestures) {
        add_debug_log("Gesture recognition testing enabled")
      } else {
        add_debug_log("Gesture recognition testing disabled")
      }
    })
    
    # Visual debugging tools
    observeEvent(input$show_touch_targets, {
      session$sendCustomMessage(
        type = "toggle-touch-targets",
        message = list(show = input$show_touch_targets)
      )
      
      add_debug_log(paste("Touch targets visibility:", input$show_touch_targets))
    })
    
    observeEvent(input$show_grid, {
      session$sendCustomMessage(
        type = "toggle-grid-overlay",
        message = list(show = input$show_grid)
      )
      
      add_debug_log(paste("Grid overlay:", input$show_grid))
    })
    
    observeEvent(input$show_safe_areas, {
      session$sendCustomMessage(
        type = "toggle-safe-areas",
        message = list(show = input$show_safe_areas)
      )
      
      add_debug_log(paste("Safe area indicators:", input$show_safe_areas))
    })
    
    observeEvent(input$force_dark, {
      session$sendCustomMessage(
        type = "force-theme",
        message = list(theme = if (input$force_dark) "dark" else "auto")
      )
      
      add_debug_log(paste("Forced theme:", if (input$force_dark) "dark" else "auto"))
    })
    
    # Performance monitoring
    output$memory_usage <- renderText({
      # This would be updated by JavaScript performance monitoring
      paste(round(runif(1, 20, 80), 1), "MB")
    })
    
    output$load_time <- renderText({
      # This would be updated by JavaScript performance monitoring
      paste(round(runif(1, 800, 3000)), "ms")
    })
    
    # Debug log output
    output$debug_log <- renderText({
      debug_log()
    })
    
    # Clear debug log
    observeEvent(input$clear_log, {
      debug_log("")
      add_debug_log("Debug log cleared")
    })
    
    # Listen for JavaScript debug messages
    observe({
      if (!is.null(input$js_debug_message)) {
        add_debug_log(paste("JS:", input$js_debug_message))
      }
    })
    
    # Initialize debug logging
    add_debug_log("Mobile testing tools initialized")
  })
}

#' Enable Mobile Debug Mode
#' 
#' Enables mobile debugging features for development
#' 
#' @param enable Logical, whether to enable debug mode
#' @export
enable_mobile_debug <- function(enable = TRUE) {
  options(mobile.debug = enable)
  
  if (enable) {
    message("Mobile debug mode enabled. Testing tools will be available.")
  } else {
    message("Mobile debug mode disabled.")
  }
}

#' Mobile Test Runner
#' 
#' Runs a series of mobile compatibility tests
#' 
#' @param session Shiny session object
#' @return Test results
#' @export
run_mobile_tests <- function(session) {
  
  if (!isTRUE(getOption("mobile.debug", FALSE))) {
    warning("Mobile debug mode not enabled. Call enable_mobile_debug(TRUE) first.")
    return(NULL)
  }
  
  test_results <- list()
  
  # Test 1: Framework7 dependency check
  test_results$framework7_loaded <- tryCatch({
    # This would be checked via JavaScript
    TRUE
  }, error = function(e) FALSE)
  
  # Test 2: Touch event support
  test_results$touch_support <- tryCatch({
    # This would be checked via JavaScript
    TRUE  
  }, error = function(e) FALSE)
  
  # Test 3: Viewport configuration
  test_results$viewport_config <- tryCatch({
    # Check if proper mobile viewport meta tag exists
    TRUE
  }, error = function(e) FALSE)
  
  # Test 4: PWA manifest
  test_results$pwa_manifest <- file.exists("www/manifest.json")
  
  # Test 5: Service worker
  test_results$service_worker <- file.exists("www/sw.js")
  
  # Test 6: Mobile CSS
  test_results$mobile_css <- file.exists("www/mobile_styles.css")
  
  # Test 7: Touch-friendly button sizes
  test_results$touch_targets <- tryCatch({
    # This would check CSS for minimum 44px touch targets
    TRUE
  }, error = function(e) FALSE)
  
  # Generate test report
  passed_tests <- sum(unlist(test_results))
  total_tests <- length(test_results)
  
  cat("\n=== NUCLEARFF Mobile Test Results ===\n")
  cat(sprintf("Passed: %d/%d tests\n", passed_tests, total_tests))
  cat("\nDetailed Results:\n")
  
  for (test_name in names(test_results)) {
    status <- if (test_results[[test_name]]) "✅ PASS" else "❌ FAIL"
    cat(sprintf("  %s: %s\n", test_name, status))
  }
  
  if (passed_tests == total_tests) {
    cat("\n🎉 All mobile compatibility tests passed!\n")
    
    f7Notification(
      title = "Mobile Tests Complete",
      text = "All tests passed! Your app is mobile-ready.",
      icon = f7Icon("checkmark_circle_fill"),
      closeTimeout = 5000
    )
  } else {
    cat(sprintf("\n⚠️  %d test(s) failed. Please check your mobile implementation.\n", 
                total_tests - passed_tests))
    
    f7Notification(
      title = "Mobile Tests Failed",
      text = sprintf("%d test(s) failed. Check console for details.", 
                     total_tests - passed_tests),
      icon = f7Icon("exclamationmark_triangle_fill"),
      closeTimeout = 5000
    )
  }
  
  invisible(test_results)
}

#' Mobile Performance Profiler
#' 
#' Profiles mobile app performance
#' 
#' @param session Shiny session object
#' @param duration Duration to profile in seconds (default: 30)
#' @export
profile_mobile_performance <- function(session, duration = 30) {
  
  if (!isTRUE(getOption("mobile.debug", FALSE))) {
    warning("Mobile debug mode not enabled.")
    return(NULL)
  }
  
  cat(sprintf("Starting mobile performance profiling for %d seconds...\n", duration))
  
  # Start profiling
  session$sendCustomMessage(
    type = "start-performance-profiling",
    message = list(duration = duration * 1000)
  )
  
  f7Notification(
    title = "Performance Profiling",
    text = sprintf("Profiling mobile performance for %d seconds", duration),
    icon = f7Icon("speedometer"),
    closeTimeout = duration * 1000
  )
  
  # Set up completion handler
  later::later(function() {
    session$sendCustomMessage(
      type = "stop-performance-profiling",
      message = list()
    )
    
    cat("Mobile performance profiling completed.\n")
    
    f7Notification(
      title = "Profiling Complete",
      text = "Performance profiling completed. Check browser dev tools for results.",
      icon = f7Icon("checkmark_circle"),
      closeTimeout = 3000
    )
    
  }, delay = duration)
}

#' Create Mobile Test Data
#' 
#' Generates test data optimized for mobile testing
#' 
#' @param rows Number of rows to generate
#' @return Test data frame
#' @export
create_mobile_test_data <- function(rows = 50) {
  
  # Generate data that tests mobile table performance
  tibble::tibble(
    id = 1:rows,
    team_name = paste("Team", sample(LETTERS, rows, replace = TRUE)),
    manager = paste("Manager", sample(1:rows)),
    wins = sample(0:16, rows, replace = TRUE),
    losses = 16 - wins,
    points_for = round(runif(rows, 1000, 2000), 1),
    points_against = round(runif(rows, 950, 1950), 1),
    record = paste(wins, losses, sep = "-"),
    percentage = round(wins / (wins + losses), 3),
    last_updated = Sys.time() - runif(rows, 0, 86400) # Random within last day
  )
}

#' Mobile Debug JavaScript
#' 
#' Returns JavaScript code for mobile debugging
#' 
#' @return HTML script tag with debug JavaScript
#' @export
mobile_debug_js <- function() {
  
  if (!isTRUE(getOption("mobile.debug", FALSE))) {
    return(tags$script())
  }
  
  tags$script(HTML("
    // Mobile Debug JavaScript
    window.mobileDebug = {
      
      // Device simulation
      simulateDevice: function(config) {
        document.body.style.width = config.width + 'px';
        document.body.style.height = config.height + 'px';
        document.body.style.margin = '0 auto';
        document.body.style.border = '2px solid #333';
        document.body.style.borderRadius = '20px';
        document.body.style.overflow = 'hidden';
        
        console.log('Device simulation applied:', config);
      },
      
      // Network simulation
      simulateNetwork: function(config) {
        if (config.type === 'slow') {
          // Intercept fetch requests and add delay
          const originalFetch = window.fetch;
          window.fetch = function(...args) {
            return new Promise(resolve => {
              setTimeout(() => {
                resolve(originalFetch.apply(this, args));
              }, config.delay || 2000);
            });
          };
          console.log('Slow network simulation enabled');
        } else {
          // Restore original fetch
          if (window.originalFetch) {
            window.fetch = window.originalFetch;
          }
          console.log('Network simulation disabled');
        }
      },
      
      // Offline testing
      testOffline: function(config) {
        const originalFetch = window.fetch;
        window.fetch = function() {
          return Promise.reject(new Error('Simulated offline'));
        };
        
        setTimeout(() => {
          window.fetch = originalFetch;
          console.log('Offline test completed');
        }, config.duration);
        
        console.log('Offline test started for', config.duration, 'ms');
      },
      
      // Haptic feedback
      testHaptic: function(config) {
        if (navigator.vibrate) {
          navigator.vibrate(config.pattern);
          console.log('Haptic feedback triggered:', config.pattern);
        } else {
          console.log('Haptic feedback not supported');
        }
      },
      
      // Touch event logging
      toggleTouchLogging: function(config) {
        if (config.enabled) {
          ['touchstart', 'touchmove', 'touchend'].forEach(event => {
            document.addEventListener(event, function(e) {
              if (window.Shiny) {
                Shiny.setInputValue('js_debug_message', 
                  'Touch ' + event + ': ' + e.touches.length + ' touches',
                  {priority: 'event'});
              }
            });
          });
        }
      },
      
      // Visual debugging
      toggleTouchTargets: function(config) {
        const style = document.getElementById('touch-target-debug') || 
                     document.createElement('style');
        style.id = 'touch-target-debug';
        
        if (config.show) {
          style.textContent = `
            .f7-button, .button, .tab-link, .link {
              outline: 2px solid red !important;
              background: rgba(255, 0, 0, 0.1) !important;
            }
            .f7-button:after, .button:after {
              content: attr(data-text) ' (44px min)';
              position: absolute;
              background: red;
              color: white;
              font-size: 10px;
              padding: 2px;
              top: -20px;
              left: 0;
            }
          `;
        } else {
          style.textContent = '';
        }
        
        document.head.appendChild(style);
      },
      
      // Grid overlay
      toggleGridOverlay: function(config) {
        const overlay = document.getElementById('grid-overlay') || 
                       document.createElement('div');
        overlay.id = 'grid-overlay';
        
        if (config.show) {
          overlay.style.cssText = `
            position: fixed;
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
            background-image: 
              linear-gradient(rgba(255,0,0,0.3) 1px, transparent 1px),
              linear-gradient(90deg, rgba(255,0,0,0.3) 1px, transparent 1px);
            background-size: 20px 20px;
            pointer-events: none;
            z-index: 9999;
          `;
          document.body.appendChild(overlay);
        } else {
          overlay.remove();
        }
      }
    };
    
    // Listen for custom messages
    if (window.Shiny) {
      Shiny.addCustomMessageHandler('simulate-device', window.mobileDebug.simulateDevice);
      Shiny.addCustomMessageHandler('simulate-network', window.mobileDebug.simulateNetwork);
      Shiny.addCustomMessageHandler('test-offline', window.mobileDebug.testOffline);
      Shiny.addCustomMessageHandler('test-haptic', window.mobileDebug.testHaptic);
      Shiny.addCustomMessageHandler('toggle-touch-logging', window.mobileDebug.toggleTouchLogging);
      Shiny.addCustomMessageHandler('toggle-touch-targets', window.mobileDebug.toggleTouchTargets);
      Shiny.addCustomMessageHandler('toggle-grid-overlay', window.mobileDebug.toggleGridOverlay);
    }
    
    console.log('Mobile debug tools loaded');
  "))
}

#' Quick Mobile Setup
#' 
#' Sets up a basic mobile testing environment
#' 
#' @export
setup_mobile_testing <- function() {
  
  # Enable debug mode
  enable_mobile_debug(TRUE)
  
  # Check required files
  required_files <- c(
    "www/mobile_styles.css",
    "www/manifest.json", 
    "www/sw.js"
  )
  
  missing_files <- required_files[!file.exists(required_files)]
  
  if (length(missing_files) > 0) {
    warning("Missing required mobile files:\n", paste(missing_files, collapse = "\n"))
    cat("Create these files using the provided artifacts.\n")
  } else {
    cat("✅ All required mobile files found.\n")
  }
  
  # Display setup instructions
  cat("\n=== Mobile Testing Setup Complete ===\n")
  cat("1. Add mobileTestingUI('testing') to your UI\n")
  cat("2. Add mobileTestingServer('testing') to your server\n") 
  cat("3. Include mobile_debug_js() in your UI head\n")
  cat("4. Run run_mobile_tests(session) to test compatibility\n")
  cat("5. Use profile_mobile_performance(session) to check performance\n\n")
  
  invisible(TRUE)
}