# Render LaTeX paper outputs

command_exists <- function(command) {
  nzchar(Sys.which(command))
}

command_status <- function(result) {
  status <- attr(result, "status")
  if (is.null(status)) 0 else status
}

quote_arg <- function(arg) {
  if (.Platform$OS.type == "windows") {
    shQuote(arg, type = "cmd")
  } else {
    shQuote(arg)
  }
}

run_latex_command <- function(command, args, allow_failure = FALSE) {
  result <- system2(command, args = args, stdout = TRUE, stderr = TRUE)
  status <- command_status(result)

  if (!allow_failure && status != 0) {
    stop(
      "LaTeX command failed: ", command, " ", paste(args, collapse = " "),
      "\n\n", paste(result, collapse = "\n"),
      call. = FALSE
    )
  }

  invisible(result)
}

configure_tinytex_repository <- function() {
  repo <- Sys.getenv(
    "WOWAIR_TEXLIVE_REPOSITORY",
    "https://mirror.ctan.org/systems/texlive/tlnet"
  )

  if (!requireNamespace("tinytex", quietly = TRUE) ||
      !"tlmgr_repo" %in% getNamespaceExports("tinytex")) {
    return(invisible(FALSE))
  }

  result <- try(tinytex::tlmgr_repo(repo), silent = TRUE)

  if (inherits(result, "try-error")) {
    warning(
      "Could not set TinyTeX repository to ", repo,
      ". Continuing with the current TinyTeX repository.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }

  invisible(TRUE)
}

render_latex_file <- function(tex_file, paper_dir = "Paper") {
  if (!dir.exists(paper_dir)) {
    stop("Paper directory does not exist: ", paper_dir, call. = FALSE)
  }

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(paper_dir)

  tex_file <- basename(tex_file)

  if (!file.exists(tex_file)) {
    stop("LaTeX file does not exist: ", file.path(paper_dir, tex_file), call. = FALSE)
  }

  if (requireNamespace("tinytex", quietly = TRUE)) {
    configure_tinytex_repository()

    result <- try(tinytex::latexmk(tex_file, engine = "pdflatex"), silent = TRUE)

    if (inherits(result, "try-error")) {
      error_text <- as.character(result)
      tlmgr_hint <- if (grepl("tlmgr|repository|pubkey|package installation", error_text, ignore.case = TRUE)) {
        paste0(
          "If the error mentions tlmgr, repository verification, or package installation, ",
          "run this once in R and then rerun the render script:\n\n",
          "tinytex::tlmgr_repo('https://mirror.ctan.org/systems/texlive/tlnet')\n",
          "tinytex::tlmgr_update()\n\n"
        )
      } else {
        ""
      }

      stop(
        "TinyTeX failed while rendering ", tex_file, ". ",
        "See ", file.path(paper_dir, paste0(tools::file_path_sans_ext(tex_file), ".log")),
        " for LaTeX details.\n\n",
        tlmgr_hint,
        error_text,
        call. = FALSE
      )
    }
  } else {
    if (!command_exists("pdflatex")) {
      stop("pdflatex is not available on PATH.", call. = FALSE)
    }

    tex_arg <- quote_arg(tex_file)
    aux_base <- tools::file_path_sans_ext(tex_file)
    aux_file <- paste0(aux_base, ".aux")

    run_latex_command(
      "pdflatex",
      c("-interaction=nonstopmode", "-halt-on-error", tex_arg)
    )

    if (file.exists(aux_file) && command_exists("bibtex")) {
      run_latex_command("bibtex", quote_arg(aux_base))
    }

    run_latex_command(
      "pdflatex",
      c("-interaction=nonstopmode", "-halt-on-error", tex_arg)
    )
    run_latex_command(
      "pdflatex",
      c("-interaction=nonstopmode", "-halt-on-error", tex_arg)
    )
  }

  pdf_file <- file.path(paper_dir, paste0(tools::file_path_sans_ext(tex_file), ".pdf"))
  message("Rendered LaTeX output: ", pdf_file)
  invisible(pdf_file)
}

render_latex_outputs <- function() {
  c(
    anonymized = render_latex_file("Main.tex"),
    non_anonymized = render_latex_file("Main Non-Anonymized.tex")
  )
}

if (!identical(tolower(Sys.getenv("WOWAIR_RENDER_LATEX", "true")), "false")) {
  latex_outputs <- render_latex_outputs()
}
