###############################################################################
# 13b_export_displays.R — copy the manuscript's reported display items into
# displays/ under their manuscript labels (Figures 1-4, eFigures 1-5, Table 1,
# eTables 1-5), keeping only the reported worksheet(s) of each workbook.
# In:  output/figs/*.pdf, output/tabs/*.xlsx (the pipeline's publication
#      outputs named in the mapping table below)
# Out: $proj/displays/Figure1.pdf ... eTable5.xlsx
###############################################################################

proj_env <- Sys.getenv("MEPS_PROTECTION_PROJ", unset = "")
proj <- if (nzchar(proj_env)) proj_env else getwd()
if (!file.exists(file.path(proj, "code", "00_master.do"))) {
  stop("Cannot locate project root. Set MEPS_PROTECTION_PROJ or cd to repo root.")
}

log_file <- file.path(proj, "code", "logs", "13b_export_displays.log")
log_con  <- file(log_file, open = "wt")
sink(log_con, type = "output", split = TRUE)
sink(log_con, type = "message", append = TRUE)

cat("=== Step 13b: Export Manuscript Display Items ===\n")
cat("Start:", format(Sys.time()), "\n\n")

# ---------------------------------------------------------------------------
# Display-item mapping (display file <- source; sheets NA for PDFs and for
# whole-workbook copies, else the kept sheet names joined by "; ", in order).
# ---------------------------------------------------------------------------
displays <- data.frame(
  display = c(
    "Figure1.pdf", "Figure2.pdf", "Figure3.pdf", "Figure4.pdf",
    "eFigure1.pdf", "eFigure2.pdf", "eFigure3.pdf",
    "eFigure4.pdf", "eFigure5.pdf",
    "Table1.xlsx", "eTable1.xlsx", "eTable2.xlsx", "eTable3.xlsx",
    "eTable4.xlsx", "eTable5.xlsx"
  ),
  source = c(
    "output/figs/oop_burden_distribution.pdf",
    "output/figs/main_predicted.pdf",
    "output/figs/hardship_states.pdf",
    "output/figs/che_conditional.pdf",
    "figures_source/sample_flow_diagram.pdf",
    "output/figs/main_heatmap.pdf",
    "output/figs/main_forest.pdf",
    "output/figs/config_distribution.pdf",
    "output/figs/intrahh.pdf",
    "output/tabs/characteristics_by_multimorbidity.xlsx",
    "output/tabs/characteristics_by_insurance.xlsx",
    "output/tabs/hardship_states.xlsx",
    "output/tabs/sens_nonpos_income.xlsx",
    "output/tabs/sens_covid_period.xlsx",
    "output/tabs/multimorb_4cat.xlsx"
  ),
  sheets = c(
    NA, NA, NA, NA,
    NA, NA, NA, NA, NA,
    NA, NA,
    "states; ratios; differences",
    "exclusion_results; states_positive_income; ratios_positive_income",
    "prepost_margins; period_differences",
    "3-level vs 4-level comparison"
  ),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# Every source must exist before anything is written.
# ---------------------------------------------------------------------------
src_paths <- file.path(proj, displays$source)
missing <- src_paths[!file.exists(src_paths)]
if (length(missing) > 0) {
  stop("Step 13b: missing source file(s):\n  ", paste(missing, collapse = "\n  "))
}

displays_dir <- file.path(proj, "displays")
dir.create(displays_dir, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------------
# openxlsx declares a drawing and a vmlDrawing relationship in every
# worksheet's .rels file whether or not those parts were written. No <drawing>
# element references them, so Excel and LibreOffice open the workbook, but a
# strict reader that walks the relationship graph (openpyxl, and pandas
# through it) stops with "no item named xl/drawings/drawing1.xml". Because
# displays/ is the reader-facing copy, drop every relationship
# whose target part is absent from the archive, and rebuild the file with
# fixed timestamps so repeated runs stay byte-identical.
# ---------------------------------------------------------------------------
resolve_part <- function(base, target) {
  parts <- c(strsplit(base, "/", fixed = TRUE)[[1]],
             strsplit(target, "/", fixed = TRUE)[[1]])
  out <- character(0)
  for (piece in parts) {
    if (piece == "" || piece == ".") next
    if (piece == "..") {
      if (length(out) > 0) out <- out[-length(out)]
    } else {
      out <- c(out, piece)
    }
  }
  paste(out, collapse = "/")
}

drop_dangling_rels <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)   # zip::zip() resolves a relative zipfile inside root
  entries <- zip::zip_list(path)$filename
  work <- file.path(tempdir(), paste0("xlsxfix_", basename(path)))
  unlink(work, recursive = TRUE)
  dir.create(work, recursive = TRUE)
  utils::unzip(path, exdir = work)

  removed <- 0L
  for (rel in grep("_rels/[^/]*\\.rels$", entries, value = TRUE)) {
    rel_file <- file.path(work, rel)
    if (!file.exists(rel_file)) next
    txt <- paste(readLines(rel_file, warn = FALSE), collapse = "")
    tags <- regmatches(txt, gregexpr("<Relationship\\b[^>]*/>", txt))[[1]]
    base <- dirname(dirname(rel))
    for (tag in tags) {
      if (grepl('TargetMode="External"', tag, fixed = TRUE)) next
      target <- sub('.*\\bTarget="([^"]*)".*', "\\1", tag)
      if (identical(target, tag)) next
      if (grepl("^[a-z]+:", target)) next
      if (resolve_part(base, target) %in% entries) next
      txt <- sub(tag, "", txt, fixed = TRUE)
      removed <- removed + 1L
    }
    writeLines(txt, rel_file)
  }
  if (removed == 0L) return(0L)

  # Re-archive exactly the original parts, in their original order, with a
  # fixed mtime so repeated runs stay byte-identical.
  missing_parts <- entries[!file.exists(file.path(work, entries))]
  if (length(missing_parts) > 0) {
    stop("Step 13b: parts not extracted from ", basename(path), ": ",
         paste(missing_parts, collapse = ", "))
  }
  invisible(lapply(file.path(work, entries),
                   Sys.setFileTime, time = as.POSIXct("2020-01-01", tz = "UTC")))
  unlink(path)
  zip::zip(zipfile = path, files = entries, root = work,
           mode = "mirror", include_directories = FALSE)   # mirror keeps the part paths
  unlink(work, recursive = TRUE)
  if (!setequal(zip::zip_list(path)$filename, entries)) {
    stop("Step 13b: archive part list changed while repairing ", basename(path))
  }
  removed
}

# Fail loudly if any relationship still points at a part that is not there.
assert_rels_resolve <- function(path) {
  entries <- zip::zip_list(path)$filename
  core <- c("[Content_Types].xml", "_rels/.rels", "xl/workbook.xml")
  if (!all(core %in% entries)) {
    stop("Step 13b: ", basename(path), " lacks core part(s): ",
         paste(setdiff(core, entries), collapse = ", "))
  }
  for (rel in grep("_rels/[^/]*\\.rels$", entries, value = TRUE)) {
    con <- unz(path, rel)
    txt <- paste(readLines(con, warn = FALSE), collapse = "")
    close(con)
    base <- dirname(dirname(rel))
    tags <- regmatches(txt, gregexpr("<Relationship\\b[^>]*/>", txt))[[1]]
    for (tag in tags) {
      if (grepl('TargetMode="External"', tag, fixed = TRUE)) next
      target <- sub('.*\\bTarget="([^"]*)".*', "\\1", tag)
      if (identical(target, tag) || grepl("^[a-z]+:", target)) next
      part <- resolve_part(base, target)
      if (!part %in% entries) {
        stop("Step 13b: ", basename(path), " declares relationship to a missing part: ", part)
      }
    }
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Copy / subset each display item.
# ---------------------------------------------------------------------------
for (i in seq_len(nrow(displays))) {
  display     <- displays$display[i]
  source_rel  <- displays$source[i]
  sheets_keep <- displays$sheets[i]
  src <- file.path(proj, source_rel)
  dst <- file.path(displays_dir, display)

  if (is.na(sheets_keep)) {
    ok <- file.copy(src, dst, overwrite = TRUE)
    if (!ok) stop("Step 13b: file.copy failed for ", display, " (source: ", src, ")")
    if (!identical(unname(tools::md5sum(src)), unname(tools::md5sum(dst)))) {
      stop("Step 13b: md5 mismatch after copying ", display, " (source: ", src, ")")
    }
    sheets_field <- "all"
  } else {
    keep <- strsplit(sheets_keep, "; ", fixed = TRUE)[[1]]
    # Subset with readxl + writexl: an openxlsx load/save round-trip rewrites
    # every cell whose text ends in a space, so the copy would not match its source.
    available <- readxl::excel_sheets(src)
    if (!all(keep %in% available)) {
      stop("Step 13b: ", display, " -- source lacks sheet(s) [",
           paste(setdiff(keep, available), collapse = ", "), "]")
    }
    sheet_data <- lapply(keep, function(sh)
      readxl::read_excel(src, sheet = sh, .name_repair = "minimal"))
    names(sheet_data) <- keep
    writexl::write_xlsx(sheet_data, path = dst)
    kept <- readxl::excel_sheets(dst)
    if (!identical(kept, keep)) {
      stop("Step 13b: sheet mismatch after subsetting ", display,
           " -- expected [", paste(keep, collapse = ", "),
           "], got [", paste(kept, collapse = ", "), "]")
    }
    # Every kept cell must survive the subset unchanged.
    for (sh in keep) {
      before <- readxl::read_excel(src, sheet = sh, .name_repair = "minimal")
      after  <- readxl::read_excel(dst, sheet = sh, .name_repair = "minimal")
      if (!isTRUE(all.equal(as.data.frame(before), as.data.frame(after),
                            tolerance = 1e-12))) {
        stop("Step 13b: ", display, " -- sheet '", sh,
             "' changed while subsetting the workbook")
      }
    }
    sheets_field <- sheets_keep
  }

  if (grepl("\\.xlsx$", display)) {
    dropped <- drop_dangling_rels(dst)
    if (dropped > 0L) {
      cat(sprintf("  repaired %s: dropped %d dangling relationship(s)\n",
                  display, dropped))
    }
    assert_rels_resolve(dst)
  }

  cat(sprintf("%-12s <- %s [%s]\n", display, source_rel, sheets_field))

}

# ---------------------------------------------------------------------------
# Delete any file in displays/ that is not one of the 15 display items.
# ---------------------------------------------------------------------------
stale <- setdiff(list.files(displays_dir), displays$display)
for (f in stale) {
  stale_path <- file.path(displays_dir, f)
  cat("Deleting stale file:", stale_path, "\n")
  unlink(stale_path, recursive = TRUE)
}

cat("\n13b done: 15 display items exported\n")

cat("\n=== Step 13b complete ===\n")
cat("End:", format(Sys.time()), "\n")

sink(type = "message")
sink(type = "output")
close(log_con)
