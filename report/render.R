# Build report/AB Test Report.pdf from report/report.Rmd.
#
# Run from the project root:
#     Rscript report/render.R
#
# The R Markdown is knitted to HTML, then printed to PDF by Chrome or Edge in
# headless mode, so no LaTeX installation is needed. Set CHROME_PATH to use a
# specific browser.

output_pdf <- file.path(normalizePath("report", winslash = "/"), "AB Test Report.pdf")

# Outside RStudio, Rscript may not find pandoc; RStudio ships its own copy
if (!rmarkdown::pandoc_available()) {
  bundled <- c("C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
               "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools",
               "/usr/lib/rstudio/resources/app/bin/quarto/bin/tools")
  found <- bundled[dir.exists(bundled)]
  if (length(found) == 0) stop("pandoc not found: install pandoc or RStudio")
  Sys.setenv(RSTUDIO_PANDOC = found[1])
}

find_browser <- function() {
  candidates <- c(
    Sys.getenv("CHROME_PATH"),
    "C:/Program Files/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
    "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    Sys.which(c("google-chrome", "chromium", "chromium-browser", "microsoft-edge"))
  )
  found <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(found) == 0) stop("No Chrome or Edge found: set CHROME_PATH")
  found[1]
}

# Knit to a self-contained HTML file in a temporary folder
build_dir <- tempfile("report_")
dir.create(build_dir)
html <- rmarkdown::render("report/report.Rmd", output_dir = build_dir,
                          intermediates_dir = build_dir, knit_root_dir = getwd(),
                          envir = new.env(), quiet = TRUE)

# Print the HTML to PDF
if (file.exists(output_pdf)) invisible(file.remove(output_pdf))
status <- system2(find_browser(), c(
  "--headless=new", "--disable-gpu", "--no-pdf-header-footer",
  shQuote(paste0("--print-to-pdf=", output_pdf)),
  shQuote(paste0("file:///", normalizePath(html, winslash = "/")))
), stdout = FALSE, stderr = FALSE)

if (!file.exists(output_pdf)) stop("PDF was not created (browser exit status ", status, ")")
cat("Report written to", output_pdf, "\n")
