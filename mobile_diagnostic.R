# mobile_diagnostic.R
# Comprehensive diagnostic tool for mobile implementation

#' Mobile Implementation Diagnostic Tool
#'
#' Runs comprehensive checks on your mobile Shiny app implementation
#'
#' @param app_dir Directory containing your Shiny app (default: current directory)
#' @param fix_issues Whether to attempt automatic fixes (default: FALSE)
#' @return Diagnostic report
#' @export
diagnose_mobile_implementation <- function(app_dir = ".", fix_issues = FALSE) {
  cat("🔍 NUCLEARFF Mobile Implementation Diagnostic\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  results <- list()

  # Check 1: Required packages
  cat("📦 Checking required packages...\n")
  required_packages <- c(
    "shiny", "shinyMobile", "htmltools", "jsonlite",
    "dplyr", "tibble", "purrr", "ggplot2", "DT"
  )

  package_results <- sapply(required_packages, function(pkg) {
    available <- requireNamespace(pkg, quietly = TRUE)
    if (available) {
      cat("  ✅", pkg, "(", as.character(packageVersion(pkg)), ")\n")
    } else {
      cat("  ❌", pkg, "- NOT INSTALLED\n")
      if (fix_issues) {
        cat("    🔧 Installing", pkg, "...\n")
        install.packages(pkg)
      }
    }
    available
  })

  results$packages <- package_results

  # Check 2: File structure
  cat("\n📁 Checking file structure...\n")
  required_files <- list(
    "app.R" = "Main application file",
    "R/mobile_module_home.R" = "Mobile home module",
    "R/mobile_module_data.R" = "Mobile data module",
    "R/mobile_module_viz.R" = "Mobile visualization module",
    "R/mobile_utils.R" = "Mobile utility functions",
    "www/mobile_styles.css" = "Mobile CSS styles",
    "www/manifest.json" = "PWA manifest",
    "www/sw.js" = "Service worker"
  )

  file_results <- sapply(names(required_files), function(file_path) {
    full_path <- file.path(app_dir, file_path)
    exists <- file.exists(full_path)

    if (exists) {
      size <- file.size(full_path)
      cat("  ✅", file_path, "(", round(size / 1024, 1), "KB )\n")
    } else {
      cat("  ❌", file_path, "- MISSING\n")
      cat("      ↳ Expected at:", normalizePath(full_path, mustWork = FALSE), "\n")
      cat("      ", required_files[[file_path]], "\n")
    }
    exists
  })

  results$files <- file_results

  # Check 3: Content validation
  cat("\n📝 Validating file contents...\n")

  if (file.exists(file.path(app_dir, "app.R"))) {
    app_content <- readLines(file.path(app_dir, "app.R"))
    app_lines <- tibble::tibble(line = seq_along(app_content), text = app_content)

    mobile_checks <- list(
      shinyMobile = grep("library\\(shinyMobile\\)|require\\(shinyMobile\\)", app_lines$text),
      f7Page = grep("f7Page", app_lines$text),
      f7TabLayout = grep("f7TabLayout", app_lines$text),
      mobile_css = grep("mobile_styles\\.css", app_lines$text)
    )

    for (check_name in names(mobile_checks)) {
      lines_found <- mobile_checks[[check_name]]
      if (length(lines_found) > 0) {
        cat("  ✅", check_name, "found at line(s):", paste(lines_found, collapse = ", "), "\n")
      } else {
        cat("  ❌", check_name, "NOT FOUND in app.R\n")
      }
    }

    results$app_content <- sapply(mobile_checks, function(x) length(x) > 0)
  }

  # Check manifest.json validity
  if (file.exists(file.path(app_dir, "www/manifest.json"))) {
    tryCatch(
      {
        manifest <- jsonlite::fromJSON(file.path(app_dir, "www/manifest.json"))
        required_fields <- c("name", "short_name", "start_url", "display", "icons")

        manifest_checks <- sapply(required_fields, function(field) {
          exists <- field %in% names(manifest)
          if (exists) {
            cat("  ✅ manifest.json has", field, "\n")
          } else {
            cat("  ❌ manifest.json MISSING field:", field, "\n")
          }
          exists
        })

        results$manifest <- manifest_checks
      },
      error = function(e) {
        cat("  ❌ manifest.json - INVALID JSON\n")
        cat("      Error:", e$message, "\n")
        results$manifest <- FALSE
      }
    )
  }

  # Check CSS for mobile optimizations
  if (file.exists(file.path(app_dir, "www/mobile_styles.css"))) {
    css_content <- readLines(file.path(app_dir, "www/mobile_styles.css"))

    css_checks <- c(
      "touch_targets" = any(grepl("44px", css_content)),
      "responsive" = any(grepl("@media", css_content)),
      "mobile_vars" = any(grepl("--mobile-", css_content)),
      "framework7" = any(grepl("f7-", css_content))
    )

    for (check_name in names(css_checks)) {
      status <- if (css_checks[[check_name]]) "✅" else "❌"
      desc <- switch(check_name,
        "touch_targets" = "Touch-friendly button sizes (44px)",
        "responsive" = "Responsive media queries",
        "mobile_vars" = "Mobile CSS variables",
        "framework7" = "Framework7 component styles"
      )
      cat("  ", status, desc, "\n")
    }

    results$css <- css_checks
  }

  # Check 4: Conflicts
  cat("\n🔍 Checking for potential conflicts...\n")
  if (file.exists(file.path(app_dir, "app.R"))) {
    full_content <- paste(readLines(file.path(app_dir, "app.R")), collapse = "\n")

    conflicts <- c(
      "fluidPage + f7Page" = grepl("fluidPage", full_content) && grepl("f7Page", full_content),
      "navbarPage + f7TabLayout" = grepl("navbarPage", full_content) && grepl("f7TabLayout", full_content),
      "Bootstrap + Framework7" = grepl("bslib::", full_content) && grepl("f7", full_content)
    )

    for (name in names(conflicts)) {
      if (conflicts[[name]]) {
        cat("  ⚠️  Potential conflict:", name, "\n")
      } else {
        cat("  ✅ No conflict:", name, "\n")
      }
    }

    results$conflicts <- conflicts
  }

  # Check 5: Mobile assets
  cat("\n🎨 Checking mobile assets...\n")
  asset_paths <- c(
    "www/nuclearff/nuclearff-navbar-icon-color.png",
    "www/nuclearff/nuclearff-navbar-icon-dark.png"
  )

  asset_results <- sapply(asset_paths, function(path) {
    full <- file.path(app_dir, path)
    if (file.exists(full)) {
      cat("  ✅", basename(path), "\n")
      TRUE
    } else {
      cat("  ❌", basename(path), "- MISSING\n")
      FALSE
    }
  })

  results$assets <- asset_results

  # Summary
  cat("\n📊 DIAGNOSTIC SUMMARY\n")
  cat(paste(rep("=", 30), collapse = ""), "\n")

  flat_results <- unlist(results, recursive = TRUE, use.names = TRUE)
  check_names <- names(flat_results)
  total <- length(flat_results)
  passed <- sum(flat_results, na.rm = TRUE)

  cat("Overall Status:", passed, "/", total, "checks passed\n")

  if (passed == total) {
    cat("🎉 EXCELLENT! Your mobile implementation looks great!\n\n")
    cat("🚀 Ready for next steps:\n")
    cat("  1. Run: source('run_mobile_dev.R')\n")
    cat("  2. Test on device\n")
    cat("  3. Deploy securely\n\n")
  } else {
    cat("⚠️  Issues found that need attention:\n\n")

    if (!all(results$packages %||% TRUE)) {
      missing <- names(results$packages)[!results$packages]
      cat("📦 Install missing packages:\n")
      cat("   install.packages(c(", paste0("'", missing, "'", collapse = ", "), "))\n\n")
    }

    if (!all(results$files %||% TRUE)) {
      missing <- names(results$files)[!results$files]
      cat("📁 Create missing files:\n")
      for (f in missing) {
        cat("   -", f, "\n")
      }
      cat("\n")
    }

    if (!all(results$app_content %||% TRUE)) {
      cat("🔧 Update app.R to include mobile components:\n")
      if (!results$app_content[["f7Page"]]) cat("   - ❌ f7Page() not found\n")
      if (!results$app_content[["f7TabLayout"]]) cat("   - ❌ f7TabLayout() not found\n")
      if (!results$app_content[["mobile_css"]]) cat("   - ❌ mobile_styles.css not linked\n")
      if (!results$app_content[["shinyMobile"]]) cat("   - ❌ shinyMobile package not used in app.R\n")
      cat("\n")
    }

    if (!all(results$manifest %||% TRUE)) {
      missing <- names(results$manifest)[!results$manifest]
      cat("🧾 Fix manifest.json:\n")
      for (f in missing) {
        cat("   - ❌ Missing field:", f, "\n")
      }
      cat("\n")
    }

    if (!all(results$css %||% TRUE)) {
      missing <- names(results$css)[!results$css]
      cat("🎨 Improve mobile_styles.css:\n")
      for (f in missing) {
        cat("   - ❌ Missing:", f, "\n")
      }
      cat("\n")
    }

    if (any(unlist(results$conflicts %||% FALSE))) {
      cat("⚠️  Resolve UI conflicts:\n")
      cat("   - Don't mix Shiny and Framework7 UIs\n")
      cat("   - Use f7Page + f7TabLayout exclusively\n")
      cat("\n")
    }

    if (!all(results$assets %||% TRUE)) {
      missing <- names(results$assets)[!results$assets]
      cat("🖼️  Add missing mobile icons:\n")
      for (f in missing) {
        cat("   -", f, "\n")
      }
      cat("\n")
    }

    cat("💡 Tip: Run `mobile_setup_wizard()` to fix issues step-by-step.\n\n")
  }

  invisible(results)
}


#' Quick Mobile Health Check
#'
#' Runs a quick diagnostic focused on the most critical issues
#'
#' @param app_dir Directory containing your Shiny app
#' @return Boolean indicating if app is mobile-ready
#' @export
quick_mobile_check <- function(app_dir = ".") {
  cat("⚡ Quick Mobile Health Check\n")
  cat("========================\n")

  # Critical checks only
  critical_checks <- c(
    "shinyMobile package" = requireNamespace("shinyMobile", quietly = TRUE),
    "app.R exists" = file.exists(file.path(app_dir, "app.R")),
    "Mobile CSS exists" = file.exists(file.path(app_dir, "www/mobile_styles.css")),
    "PWA manifest exists" = file.exists(file.path(app_dir, "www/manifest.json"))
  )

  for (check_name in names(critical_checks)) {
    status <- if (critical_checks[[check_name]]) "✅" else "❌"
    cat(status, check_name, "\n")
  }

  all_passed <- all(critical_checks)

  cat("\n")
  if (all_passed) {
    cat("🎉 Quick check PASSED! Your app has the mobile essentials.\n")
    cat("Run diagnose_mobile_implementation() for detailed analysis.\n")
  } else {
    cat("❌ Quick check FAILED! Critical mobile components missing.\n")
    cat("Follow the step-by-step implementation guide.\n")
  }

  invisible(all_passed)
}

#' Interactive Mobile Setup Wizard
#'
#' Guides user through mobile implementation step-by-step
#'
#' @param app_dir Directory containing your Shiny app
#' @export
mobile_setup_wizard <- function(app_dir = ".") {
  if (!interactive()) {
    cat("Setup wizard requires an interactive R session.\n")
    return(invisible(FALSE))
  }

  cat("🧙 NUCLEARFF Mobile Setup Wizard\n")
  cat("==============================\n\n")

  cat("This wizard will guide you through converting your Shiny app to mobile.\n")
  cat("It will check your current setup and help you implement the necessary changes.\n\n")

  # Step 1: Check current state
  if (readline("Ready to start? (y/n): ") %in% c("y", "Y", "yes", "YES")) {
    cat("\n🔍 Step 1: Checking current setup...\n")
    current_state <- quick_mobile_check(app_dir)

    if (!current_state) {
      cat("\n🛠️  Step 2: Installing required packages...\n")
      if (readline("Install missing packages? (y/n): ") %in% c("y", "Y", "yes", "YES")) {
        required_packages <- c("shinyMobile", "htmltools", "jsonlite")
        install.packages(required_packages)
        cat("✅ Packages installed.\n")
      }
    }

    cat("\n📁 Step 3: Creating file structure...\n")
    if (readline("Create mobile directory structure? (y/n): ") %in% c("y", "Y", "yes", "YES")) {
      dirs_to_create <- c("R", "www", "www/icons")
      for (dir in dirs_to_create) {
        if (!dir.exists(file.path(app_dir, dir))) {
          dir.create(file.path(app_dir, dir), recursive = TRUE)
          cat("  Created:", dir, "\n")
        }
      }
      cat("✅ Directory structure ready.\n")
    }

    cat("\n📝 Step 4: Next steps...\n")
    cat("Now you need to create the mobile files using the provided artifacts:\n\n")
    cat("Required files to create:\n")
    cat("  1. R/mobile_module_home.R (from Mobile-Optimized Home Module artifact)\n")
    cat("  2. R/mobile_module_data.R (from Mobile-Optimized Data Module artifact)\n")
    cat("  3. R/mobile_module_viz.R (from Mobile-Optimized Visualization Module artifact)\n")
    cat("  4. R/mobile_utils.R (from Mobile Utility Functions artifact)\n")
    cat("  5. www/mobile_styles.css (from Mobile-Optimized CSS Styles artifact)\n")
    cat("  6. www/manifest.json (from PWA Manifest artifact)\n")
    cat("  7. www/sw.js (from Service Worker artifact)\n")
    cat("  8. app.R (from Mobile-Optimized app.R artifact)\n\n")

    cat("After creating these files, run:\n")
    cat("  diagnose_mobile_implementation()\n\n")

    cat("🎯 Happy mobile development!\n")
  } else {
    cat("Setup cancelled. You can run the wizard again anytime.\n")
  }

  invisible(TRUE)
}

#' Test Mobile App Performance
#'
#' Runs performance tests on your mobile app
#'
#' @param port Port your app is running on (default: 8080)
#' @export
test_mobile_performance <- function(port = 8080) {
  cat("⚡ Mobile Performance Test\n")
  cat("========================\n")

  app_url <- paste0("http://localhost:", port)

  cat("Testing app at:", app_url, "\n\n")

  # Test 1: App accessibility
  cat("🌐 Testing app accessibility...\n")
  tryCatch(
    {
      response <- httr::GET(app_url, httr::timeout(10))
      if (httr::status_code(response) == 200) {
        cat("  ✅ App accessible at", app_url, "\n")
      } else {
        cat("  ❌ App returned status:", httr::status_code(response), "\n")
      }
    },
    error = function(e) {
      cat("  ❌ Cannot connect to app. Make sure it's running.\n")
      cat("     Start with: runApp(host='0.0.0.0', port=", port, ")\n")
    }
  )

  # Test 2: Mobile assets
  cat("\n📱 Testing mobile assets...\n")
  mobile_endpoints <- c(
    "/mobile_styles.css",
    "/manifest.json",
    "/sw.js"
  )

  for (endpoint in mobile_endpoints) {
    asset_url <- paste0(app_url, endpoint)
    tryCatch(
      {
        response <- httr::GET(asset_url, httr::timeout(5))
        status <- if (httr::status_code(response) == 200) "✅" else "❌"
        cat("  ", status, endpoint, "\n")
      },
      error = function(e) {
        cat("  ❌", endpoint, "- Error:", e$message, "\n")
      }
    )
  }

  cat("\n📊 Performance Tips:\n")
  cat("  • Test on real mobile devices\n")
  cat("  • Use Chrome DevTools mobile simulation\n")
  cat("  • Monitor network requests in DevTools\n")
  cat("  • Test with slow network conditions\n")
  cat("  • Verify PWA installation works\n\n")
}

# Helper function for string repetition (if not available)
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs) || length(lhs) == 0) rhs else lhs
}
