
# {usethis} helpers ------------------------------------------------------------

#' @export
edit_user_rprofile <- poof::new_trick(
  "Edit user '.Rprofile'",
  ~ selection_is_empty(),
  ~ usethis::edit_r_profile()
)

#' @export
edit_project_rprofile <- poof::new_trick(
  "Edit project '.Rprofile'",
  ~ selection_is_empty(),
  ~ usethis::edit_r_profile(scope = "project")
)

#' @export
edit_renviron <- poof::new_trick(
  "Edit '.Renviron'",
  ~ selection_is_empty(),
  ~ usethis::edit_r_environ()
)

#' @export
edit_rstudio_snippets <- poof::new_trick(
  "Edit RStudio snippets",
  ~ selection_is_empty(),
  ~ usethis::edit_rstudio_snippets()
)

#' @export
initialise_git <- poof::new_trick(
  "Use git",
  ~ selection_is_empty() && project_is_package() && !project_uses_git(),
  ~ usethis::use_git()
)

#' @export
browse_cran_page <- poof::new_trick(
  "visit CRAN page of '{current_selection()}' package",
  ~ selection_is_cran_package(),
  ~ usethis::browse_cran(current_selection())
)

#' @export
browse_github_page <- poof::new_trick(
  "visit github page of '{current_selection()}' package",
  ~ selection_is_cran_package(),
  ~ usethis::browse_github(current_selection())
)

# {covr} helpers ---------------------------------------------------------------

#' @export
calculate_coverage <- poof::new_trick(
  "Calculate package test coverage with `covr::report()`",
  ~ selection_is_empty() && project_is_package(),
  ~ covr::report()
)

#' @export
install_from_cran <- poof::new_trick(
  "Install '{current_selection()}' package from CRAN",
  ~ selection_is_cran_package(),
  ~ remotes::install_cran(current_selection(), upgrade = "ask")
)

#' @export
install_from_github <- poof::new_trick(
  "Install '{current_selection()}' package  from github",
  ~ selection_is_cran_package(),
  ~ {
    url <- usethis:::github_url(current_selection())
    repo <- paste(strsplit(url, "/")[[1]][4:5], collapse = "/")
    remotes::install_github(repo, upgrade = "ask")
    }
)

# reprex -----------------------------------------------------------------------

#' @export
reprex_selection <- poof::new_trick(
  "Reprex selection",
  ~ selection_is_parsable(symbol_ok = FALSE),
  ~ call_addin("reprex", "Reprex selection")
)

# styler -----------------------------------------------------------------------

#' @export
style_selection <- poof::new_trick(
  "Style selection wih 'styler'",
  ~ selection_is_parsable(symbol_ok = FALSE),
  ~ {ƒ
    sel <- current_selection()
    replace_selection(
      paste(styler::style_text(sel), collapse = "\n"))
  }
)

# no dependency ----------------------------------------------------------------

#' @export
call_debugonce <- poof::new_trick(
  "debugonce({current_selection()})",
  ~ selection_is_function(),
  ~ debugonce(.(current_call()))
)

#' @export
lookup_rdocumentation <- poof::new_trick(
  "Lookup '{current_selection()}' on 'rdocumentation.org",
  ~ selection_is_symbol(),
  ~ browseURL(paste0("https://www.rdocumentation.org/search?q=", current_selection()))
)

#' @export
initiate_for_loop <- poof::new_trick(
  "Initiate `for` loop",
  ~ selection_matches("^for\\((.*?) in (.*?)\\)$"),
  ~ local({
    i_chr <- sub('^for\\((.*?) in (.*?)\\)$', '\\1', current_selection())
    seq_chr <- sub('^for\\((.*?) in (.*?)\\)$', '\\2', current_selection())
    seq_lng <- str2lang(seq_chr)
    assign(i_chr, value = eval(seq_lng, .GlobalEnv)[[1]], envir = .GlobalEnv)
  })
)

#' @export
allign_assignments <- poof::new_trick(
  "allign `<-` assignments",
  ~ selection_matches("<-", n_min = 2, target = "lines"),
  ~ local({
    code <- current_lines()
    pos <- regexpr("<-", code)
    pad <- max(pos) - pos
    code <- mapply(function(x, n) sub("<-", paste0(strrep(" ", n), "<-"), x), code, pad)
    replace_current_lines(code)
  })
)

#' @export
allign_commas <- poof::new_trick(
  "allign commas",
  ~ selection_matches(",", n_min = 2, target = "lines"),
  ~ local({
    code <- current_lines()
    pos <- regexpr(",", code)
    pad <- max(pos) - pos
    code <- mapply(function(x, n) sub(",", paste0(strrep(" ", n), ","), x), code, pad)
    replace_current_lines(code)
  })
)

#' @export
allign_comments <- poof::new_trick(
  "allign comments",
  ~ selection_matches("#", n_min = 2, target = "lines"),
  ~ local({
    code <- current_lines()
    pos <- regexpr("#", code)
    pad <- max(pos) - pos
    code <- mapply(function(x, n) sub("#", paste0(strrep(" ", n), "#"), x), code, pad)
    replace_current_lines(code)
  })
)
