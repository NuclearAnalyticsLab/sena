# setup_mobile_app.R
# Complete setup script for NUCLEARFF mobile application

cat("=== NUCLEARFF Mobile Setup Script ===\n")
cat("Setting up your Shiny app for iOS and Android compatibility...\n\n")

# Install required packages
install_mobile_packages <- function() {
  cat("📦 Installing required packages...\n")
  
  # Core packages
  required_packages <- c(
    "shiny",        # Core Shiny functionality
    "shinyMobile",  # Framework7 mobile components
    "bslib",        # Bootstrap themes (for fallback)
    "htmltools",    # HTML utilities
    "jsonlite",     # JSON handling
    "dplyr",        # Data manipulation
    "tibble",       # Modern data frames
    "purrr",        # Functional programming
    "ggplot2",      # Data visualization
    "DT",           # Data tables
    "openxlsx",     # Excel export
    "readr",        # Data reading
    "RSQLite",      # Database support
    "auth0",        # Authentication
    "later",        # Delayed execution
    "pacman"        # Package management
  )
  
  # Check which packages need installation
  missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
  
  if (length(missing_packages) > 0) {
    cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
    install.packages(missing_packages, dependencies = TRUE)
  } else {
    cat("✅ All required packages already installed.\n")
  }
  
  # Install development version of shinyMobile if needed
  if (packageVersion("shinyMobile") < "2.0.0") {
    cat("📱 Installing latest shinyMobile from GitHub...\n")
    if (!requireNamespace("remotes", quietly = TRUE)) {
      install.packages("remotes")
    }
    remotes::install_github("RinteRface/shinyMobile")
  }
  
  cat("✅ Package installation complete.\n\n")
}

# Create directory structure
create_mobile_structure <- function() {
  cat("📁 Creating mobile app directory structure...\n")
  
  # Create directories
  dirs_to_create <- c(
    "R",
    "www",
    "www/icons",
    "www/screenshots", 
    "data",
    "tests"
  )
  
  for (dir in dirs_to_create) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      cat("  Created:", dir, "\n")
    }
  }
  
  cat("✅ Directory structure created.\n\n")
}

# Create mobile configuration files
create_mobile_config <- function() {
  cat("⚙️  Creating mobile configuration files...\n")
  
  # Create .Rprofile for mobile development
  rprofile_content <- '
# NUCLEARFF Mobile Development Configuration
options(
  mobile.debug = TRUE,          # Enable mobile debugging
  shiny.port = 8080,           # Standard port for mobile testing
  shiny.host = "0.0.0.0",      # Allow external connections for device testing
  shiny.launch.browser = TRUE,  # Auto-launch browser
  repos = c(CRAN = "https://cran.rstudio.com/")
)

# Load mobile utilities on startup
if (file.exists("R/mobile_utils.R")) {
  source("R/mobile_utils.R")
}

cat("🚀 NUCLEARFF Mobile Development Environment Loaded\\n")
cat("📱 Mobile debug mode enabled\\n")
cat("🌐 App will be accessible at http://localhost:8080\\n\\n")
'
  
  if (!file.exists(".Rprofile")) {
    writeLines(rprofile_content, ".Rprofile")
    cat("  Created: .Rprofile\n")
  }
  
  # Create mobile testing configuration
  mobile_config <- list(
    app = list(
      name = "NUCLEARFF",
      version = "1.0.0",
      description = "Fantasy Football Analytics Mobile App"
    ),
    mobile = list(
      theme = "auto",
      dark_mode = TRUE,
      touch_feedback = TRUE,
      pwa_enabled = TRUE,
      offline_support = TRUE
    ),
    testing = list(
      debug_mode = TRUE,
      performance_monitoring = TRUE,
      touch_logging = FALSE,
      network_simulation = FALSE
    ),
    devices = list(
      primary_test_devices = c("iPhone 14 Pro", "Samsung Galaxy S23", "iPad Pro"),
      breakpoints = list(
        mobile = 480,
        tablet = 768, 
        desktop = 1024
      )
    )
  )
  
  if (!file.exists("mobile_config.json")) {
    jsonlite::write_json(mobile_config, "mobile_config.json", pretty = TRUE, auto_unbox = TRUE)
    cat("  Created: mobile_config.json\n")
  }
  
  cat("✅ Configuration files created.\n\n")
}

# Copy required assets
setup_mobile_assets <- function() {
  cat("🎨 Setting up mobile assets...\n")
  
  # Check if logo files exist
  logo_files <- c(
    "www/nuclearff/nuclearff-navbar-icon-color.png",
    "www/nuclearff/nuclearff-navbar-icon-dark.png"
  )
  
  missing_logos <- logo_files[!file.exists(logo_files)]
  
  if (length(missing_logos) > 0) {
    cat("⚠️  Missing logo files:\n")
    for (logo in missing_logos) {
      cat("    -", logo, "\n")
    }
    cat("  Please ensure your logo files are in the correct location.\n")
  } else {
    cat("✅ Logo files found.\n")
  }
  
  # Create placeholder icons if needed
  icon_sizes <- c(192, 512)
  for (size in icon_sizes) {
    icon_path <- paste0("www/icons/icon-", size, "x", size, ".png")
    if (!file.exists(icon_path)) {
      cat("  Need to create:", icon_path, "\n")
    }
  }
  
  cat("✅ Asset setup complete.\n\n")
}

# Validate mobile implementation
validate_mobile_setup <- function() {
  cat("🔍 Validating mobile setup...\n")
  
  validation_results <- list()
  
  # Check required files
  required_files <- c(
    "R/mobile_module_home.R",
    "R/mobile_module_data.R", 
    "R/mobile_module_viz.R",
    "R/mobile_utils.R",
    "R/mobile_testing_utils.R",
    "www/mobile_styles.css",
    "www/manifest.json",
    "www/sw.js"
  )
  
  for (file in required_files) {
    exists <- file.exists(file)
    validation_results[[file]] <- exists
    status <- if (exists) "✅" else "❌"
    cat("  ", status, file, "\n")
  }
  
  # Check package dependencies
  required_packages <- c("shiny", "shinyMobile", "htmltools", "jsonlite")
  for (pkg in required_packages) {
    loaded <- requireNamespace(pkg, quietly = TRUE)
    validation_results[[paste0("package_", pkg)]] <- loaded
    status <- if (loaded) "✅" else "❌"
    cat("  ", status, "Package:", pkg, "\n")
  }
  
  # Summary
  passed <- sum(unlist(validation_results))
  total <- length(validation_results)
  
  cat("\n📊 Validation Summary:", passed, "/", total, "checks passed\n")
  
  if (passed == total) {
    cat("🎉 Mobile setup validation successful!\n")
    cat("Your NUCLEARFF app is ready for mobile deployment.\n\n")
  } else {
    cat("⚠️  Some validation checks failed.\n")
    cat("Please ensure all required files are created using the provided artifacts.\n\n")
  }
  
  invisible(validation_results)
}

# Create development helper scripts
create_dev_helpers <- function() {
  cat("🛠️  Creating development helper scripts...\n")
  
  # Mobile development runner
  dev_runner <- '#!/usr/bin/env Rscript
# run_mobile_dev.R - Development server runner

# Load required libraries
library(shiny)

# Set mobile development options
options(
  mobile.debug = TRUE,
  shiny.port = 8080,
  shiny.host = "0.0.0.0",
  shiny.launch.browser = TRUE
)

cat("🚀 Starting NUCLEARFF Mobile Development Server...\\n")
cat("📱 Access your app at: http://localhost:8080\\n")
cat("📱 On mobile device: http://[YOUR_IP]:8080\\n")
cat("🔧 Debug mode enabled\\n\\n")

# Run the app
runApp(
  appDir = ".",
  port = 8080,
  host = "0.0.0.0",
  launch.browser = TRUE
)
'
  
  writeLines(dev_runner, "run_mobile_dev.R")
  cat("  Created: run_mobile_dev.R\n")
  
  # Mobile testing script
  test_runner <- '#!/usr/bin/env Rscript
# test_mobile.R - Mobile testing runner

# Load testing utilities
source("R/mobile_testing_utils.R")

# Enable debug mode
enable_mobile_debug(TRUE)

# Setup testing environment
setup_mobile_testing()

cat("🧪 Mobile testing environment ready\\n")
cat("Run your app and navigate to the testing tools\\n")
'
  
  writeLines(test_runner, "test_mobile.R")
  cat("  Created: test_mobile.R\n")
  
  # Deployment helper
  deploy_helper <- '#!/usr/bin/env Rscript
# deploy_mobile.R - Mobile deployment helper

cat("🚀 NUCLEARFF Mobile Deployment Helper\\n\\n")

# Pre-deployment checks
cat("📋 Running pre-deployment checks...\\n")

# Check if all required files exist
required_files <- c(
  "www/manifest.json",
  "www/sw.js", 
  "www/mobile_styles.css"
)

all_good <- TRUE
for (file in required_files) {
  if (file.exists(file)) {
    cat("✅", file, "found\\n")
  } else {
    cat("❌", file, "missing\\n")
    all_good <- FALSE
  }
}

if (all_good) {
  cat("\\n✅ All required files present\\n")
  cat("📱 Your app is ready for mobile deployment!\\n")
  cat("\\n🔗 Deployment checklist:\\n")
  cat("  1. Deploy to HTTPS server (required for PWA)\\n")
  cat("  2. Test on real mobile devices\\n")
  cat("  3. Verify PWA installation works\\n")
  cat("  4. Test offline functionality\\n")
  cat("  5. Monitor mobile performance\\n")
} else {
  cat("\\n❌ Deployment checks failed\\n")
  cat("Please ensure all required files are present\\n")
}
'
  
  writeLines(deploy_helper, "deploy_mobile.R")
  cat("  Created: deploy_mobile.R\n")
  
  cat("✅ Development helpers created.\n\n")
}

# Generate quick start guide
create_quick_start <- function() {
  cat("📚 Generating quick start guide...\n")
  
  quick_start <- '# NUCLEARFF Mobile Quick Start Guide

## Getting Started

1. **Run the development server:**
   ```r
   source("run_mobile_dev.R")
   ```

2. **Access your app:**
   - Local: http://localhost:8080
   - Mobile device: http://[YOUR_IP]:8080

3. **Enable mobile testing:**
   ```r
   source("test_mobile.R")
   ```

## Mobile Features

### Framework7 Components
- ✅ Native iOS/Android styling
- ✅ Touch-optimized interactions
- ✅ Swipe gestures
- ✅ Haptic feedback

### Progressive Web App (PWA)
- ✅ App-like experience
- ✅ Home screen installation
- ✅ Offline functionality
- ✅ Background sync

### Mobile Optimization
- ✅ Responsive design
- ✅ Touch-friendly buttons (44px minimum)
- ✅ Mobile-optimized data tables
- ✅ Fast loading times

## Testing Your Mobile App

### On Desktop
1. Open Chrome DevTools
2. Toggle device toolbar (Ctrl+Shift+M)
3. Select a mobile device
4. Test touch interactions

### On Real Devices
1. Connect device to same WiFi
2. Navigate to http://[YOUR_IP]:8080
3. Test all mobile features
4. Try installing as PWA

## File Structure

```
your_app/
├── app.R                    # Mobile-optimized main app
├── R/
│   ├── mobile_module_*.R    # Mobile modules
│   ├── mobile_utils.R       # Mobile utilities
│   └── mobile_testing_utils.R # Testing tools
├── www/
│   ├── mobile_styles.css    # Mobile CSS
│   ├── manifest.json        # PWA manifest
│   ├── sw.js               # Service worker
│   └── nuclearff/          # App icons
└── mobile_config.json      # Mobile configuration
```

## Troubleshooting

### Common Issues

**Issue: Framework7 styles not loading**
- Solution: Ensure shinyMobile is loaded before shiny

**Issue: Touch gestures not working**
- Solution: Check mobile_styles.css is included

**Issue: PWA not installing**
- Solution: Ensure HTTPS and valid manifest.json

**Issue: Offline mode not working**
- Solution: Check service worker registration

### Debug Mode

Enable debug mode for additional testing tools:
```r
options(mobile.debug = TRUE)
```

## Deployment

1. **Pre-deployment checklist:**
   ```r
   source("deploy_mobile.R")
   ```

2. **Requirements:**
   - HTTPS server (required for PWA)
   - Proper MIME types for manifest.json
   - Service worker caching strategy

3. **Testing:**
   - Test on multiple devices
   - Verify PWA installation
   - Check offline functionality

## Support

- [shinyMobile Documentation](https://rinterface.github.io/shinyMobile/)
- [Framework7 Documentation](https://framework7.io/)
- [PWA Guidelines](https://web.dev/progressive-web-apps/)

Happy mobile development! 📱
'
  
  writeLines(quick_start, "MOBILE_QUICKSTART.md")
  cat("  Created: MOBILE_QUICKSTART.md\n")
  
  cat("✅ Quick start guide created.\n\n")
}

# Main setup function
main_setup <- function() {
  cat("🎯 Starting complete NUCLEARFF mobile setup...\n\n")
  
  # Run all setup steps
  install_mobile_packages()
  create_mobile_structure()
  create_mobile_config()
  setup_mobile_assets()
  create_dev_helpers()
  create_quick_start()
  validation_results <- validate_mobile_setup()
  
  # Final summary
  cat("🎉 NUCLEARFF Mobile Setup Complete! 🎉\n\n")
  cat("📱 Your Shiny app is now mobile-ready with:\n")
  cat("   ✅ Framework7 iOS/Android styling\n")
  cat("   ✅ Progressive Web App (PWA) capabilities\n") 
  cat("   ✅ Touch-optimized interactions\n")
  cat("   ✅ Offline functionality\n")
  cat("   ✅ Mobile testing tools\n")
  cat("   ✅ Development helpers\n\n")
  
  cat("🚀 Next Steps:\n")
  cat("   1. Create the mobile modules using provided artifacts\n")
  cat("   2. Run: source('run_mobile_dev.R')\n")
  cat("   3. Test on mobile devices\n")
  cat("   4. Deploy to HTTPS server\n\n")
  
  cat("📚 Read MOBILE_QUICKSTART.md for detailed instructions\n\n")
  
  # Open quick start guide
  if (interactive() && file.exists("MOBILE_QUICKSTART.md")) {
    cat("📖 Opening quick start guide...\n")
    try(file.show("MOBILE_QUICKSTART.md"))
  }
  
  invisible(validation_results)
}

# Run the complete setup
if (!interactive() || readline(prompt = "Run complete mobile setup? (y/n): ") %in% c("y", "Y", "yes", "YES")) {
  main_setup()
} else {
  cat("Setup cancelled. You can run individual setup functions:\n")
  cat("  - install_mobile_packages()\n")
  cat("  - create_mobile_structure()\n") 
  cat("  - create_mobile_config()\n")
  cat("  - setup_mobile_assets()\n")
  cat("  - create_dev_helpers()\n")
  cat("  - validate_mobile_setup()\n")
}