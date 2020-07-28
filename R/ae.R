base_url <- "https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/"

links_from_url <- function(url) {
    # extracts all the <a> tags from the 'main-content' element. This
    # avoids getting menus, sidebar and various dupes.
    xml2::read_html(url) %>%
        rvest::html_nodes(css = "#main-content a")
}

strip_base_url <- function(url) {
    substr(url, nchar(base_url) + 1, nchar(url) - 1)
}

##' @title Create the MONSTR defaults
##' @param download_root Root of directory hierarchy.
##' @return an augmented metadata
##' @author Neale Swinnerton <neale@mastodonc.com>
##' @export
##' @import here
pipeline_defaults <- function(download_root="") {
    basedir <- "{{download_root}}/data"
    filepath <- "{{datasource}}/{{dataset}}/{{edition}}/{{dataset}}-v{{version}}.{{format}}"

    metadata <- dplyr::tibble(download_filename_template=c(sprintf("%s/raw/%s",
                                                                   basedir,
                                                                   filepath)),
                              clean_filename_template = c(sprintf("%s/clean/%s",
                                                                  basedir,
                                                                  filepath)))

    if (missing(download_root)) {
        metadata$download_root = here::here() # TODO here supposedly for
                                            # interactive use?
    } else {
        metadata$download_root = download_root
    }
    metadata
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param defaults
##' @return
##' @author Neale Swinnerton <neale@mastodonc.com>
##' @export
##' @examples
##' \dontrun{
##' ae_datasets_setup(monstr_pipeline_defaults()) # rooted in current project
##' }
ae_datasets_setup <- function(defaults) {
    links <- links_from_url(base_url) %>%
        purrr::map(~ xml2::xml_attr(., "href")) %>%
        purrr::keep(~ grepl("ae-attendances-and-emergency-admissions", .)) %>%
        purrr::reduce(~ if (.y %in% .x) {.x} else {append(.x, .y)})

    ids <- links %>% purrr::modify(strip_base_url)

    items=tibble(id=ids,link=links)

    tibble(items=list(tibble(items)), monstr=list(defaults))
}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param metadata
##' @return dataframe of available datasets
##' @export
##' @author Neale Swinnerton <neale@mastodonc.com>
ae_available_datasets <- function(metadata) {
  metadata$items[[1]] %>% dplyr::select(id)
}

##' Return a list of available editions
##'
##' .. content for \details{} ..##' @title
##' @param metadata
##' @param id identifier for the dataset
##' @return
##' @export
##' @author Neale Swinnerton <neale@mastodonc.com>
ae_available_editions <- function(metadata, id) {
  # TODO hardcoded for now
    tibble(edition=c("timeseries", "ae-by-provider"))
}

parse_month <- function(s) {
    ## Implementation Note: We can do this more efficiently with a
    ## hairy regex, but we choose to do the simple dumb thing for
    ## readability. This is inefficient (multiple calls to grep*), but
    ## we don't expect this to be called a lot. If it *is* called a
    ## lot this implementation can be revisited
    if (grepl("\\bJan(?:uary)?\\b", s, ignore.case = TRUE)) {
        version <- "january"
    } else if (grepl("\\bFeb(?:ruary)?\\b", s, ignore.case = TRUE)) {
        version <- "february"
    } else if (grepl("\\bMar(?:ch)?\\b", s, ignore.case = TRUE)) {
        version <- "march"
    } else if (grepl("\\bApr(?:il)?\\b", s, ignore.case = TRUE)) {
        version <- "april"
    } else if (grepl("\\bMay\\b", s, ignore.case = TRUE)) {
        version <- "may"
    } else if (grepl("\\bJun(?:e)?\\b", s, ignore.case = TRUE)) {
        version <- "june"
    } else if (grepl("\\bJul(?:y)?\\b", s, ignore.case = TRUE)) {
        version <- "july"
    } else if (grepl("\\bAug(?:ust)?\\b", s, ignore.case = TRUE)) {
        version <- "august"
    } else if (grepl("\\bSep(?:tember)?\\b", s, ignore.case = TRUE)) {
        version <- "september"
    } else if (grepl("\\bOct(?:ober)?\\b", s, ignore.case = TRUE)) {
        version <- "october"
    } else if (grepl("\\bNov(?:ember)?\\b", s, ignore.case = TRUE)) {
        version <- "november"
    } else if (grepl("\\bDec(?:ember)?\\b", s, ignore.case = TRUE)) {
        version <- "december"
    } else {
        version <- NA
    }
    version
}

parse_year <- function(s) {
  re <- regexec("(?<!\\d)\\d{4}(?!\\d)", s, perl = TRUE) # 4 digits NOT preceded by or followed by a digit
  l <- attr(re[[1]], "match.length")[1]
  if (l != -1) {
    match <- re[[1]][1]
    ystr <- substring(s, match, match + l - 1)
    year <- as.numeric(ystr)
  } else {
    year <- NA
  }
  year
}

parse_version <- function(s) {
    quarter <- parse_quarter(s)
    month <- parse_month(s)
    year <- parse_year(s)
    dplyr::tibble(quarter = c(quarter), month = c(month), year = c(year))
}

parse_release_frequency <- function(s) {
    if (grepl("monthly", s, ignore.case = TRUE)) {
        "monthly"
    } else if (grepl("quarterly", s, ignore.case = TRUE)) {
        "quarterly"
    } else {
        NA
    }
}

parse_adjusted <- function(s) {
    if (grepl("\\badjusted\\b", s, ignore.case = TRUE)) {
        TRUE
    } else if (grepl("\\bunadjusted\\b", s, ignore.case = TRUE)) {
        FALSE
    } else {
        NA
    }
}

parse_edition <- function(s) {
  if (grepl("Timeseries", s, ignore.case = TRUE)) {
    "timeseries"
  } else if (grepl("by[_-]provider", s, ignore.case = TRUE)) {
    "by-provider"
  } else {
      NA
  }
}

parse_quarter <- function(s) {
    re <- regexec("\\bQ(?:uarter)?[-_]?([1-4])\\b", s, perl=TRUE)
    l <- attr(re[[1]], "match.length")
    if (l[1] != -1) {
        l <- l[[2]]
        match <- re[[1]][2]
        qstr <- substring(s, match, match+l-1)
        quarter<- paste0("Q", qstr)
    } else {
        quarter<- NA
    }
    quarter
}

build_id <- function(parent, edition, release_frequency, month, quarter, year, debug) {
    id <- edition
    if(!missing(release_frequency) && !is.na(release_frequency)) {
        id <- paste0(id, "_", release_frequency)
    }
    if(!missing(month) && !is.na(month)) {
        id <- paste0(id, "_", month)
    }
    if(!missing(quarter) && !is.na(quarter)) {
        id <- paste0(id, "_", quarter)
    }
    if(!missing(year) && !is.na(year) ) {
        id <- paste0(id, "_", year)
    }

    if (!missing(debug)) {
        id <- paste0(id, ">>>", debug, "<<<")
    }

    id
}

ae_href_parser <- function(link_node, parent) {
    href <- link_node$href
    match <- regexec(".*/(.*?)$", href)[[1]]
    if (match[1] == -1) {
        stop("match error")
    }

    s <- substring(href, match[2])

    result <- dplyr::tibble(
                         version = parse_version(s),
                         edition = parse_edition(s),
                         description = c(link_node$description),
                         href = c(href),
                         adjusted = parse_adjusted(s),
                         release_frequency = parse_release_frequency(s))
    result$version$id <- build_id(edition = result$edition,
                                  release_frequency = result$release_frequency,
                                  quarter = result$version$quarter,
                                  month = result$version$month,
                                  year = result$version$year)
    result}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param metadata
##' @param id
##' @param edition
##' @return
##' @author Neale Swinnerton <neale@mastodonc.com>
ae_available_versions <- function(metadata, id, edition) {
    edition_regex <- sprintf("(%s)", paste(ae_available_editions()$edition, collapse = "|"))
    versions <- links_from_url(sprintf("%s/%s/", base_url, id)) %>%
        purrr::map(~ list(href = xml2::xml_attr(., "href"), description = xml2::xml_text(.)))  %>%
        purrr::keep(~ grepl(edition_regex, .$href, ignore.case = TRUE)) %>%
        purrr::modify(ae_href_parser, id)
    versions
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param metadata
##' @param id identifier for the dataset
##' @param edition
##' @return
##' @export
##' @author Neale Swinnerton <neale@mastodonc.com>
##'
ae_available_version_ids <- function(metadata, id, edition) {
    ae_available_versions(metadata, id, edition) %>%
        purrr::modify(~ .$version$id)
}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param metadata
##' @param id
##' @return metadata describing the given dataset
##' @export
##' @author Neale Swinnerton <neale@mastodonc.com>
ae_dataset_by_id <- function(metadata, id, edition, version) {
    versions <- ae_available_versions(metadata, id, edition)

    metadata$dataset <- versions[versions$id %>% purrr::detect_index(~ .version$id == id)]

    metadata
}

##' Download
##'
##' \code{ae_download} retrieves the data described by the given df
##' @param metadata data describing the download
##' @param format a valid format for the download
##' @export
##' @import logger
ae_download <- function(metadata,
                        format = "csv") {
    validate_file <- function(f) {
        TRUE ## TODO
    }

    try (if(!(format %in% c('xls'))) stop('Format not allowed'))

    logger::log_info(sprintf("Downloading data from %s", metadata$href))

    destfile <-  generate_download_filename(template=metadata$monstr$download_filename_template,
                                            root=metadata$monstr$download_root,
                                            data=metadata$monstr)

    if (safe_download(url = c(metadata$href),
                      destfile = destfile,
                      fvalidate = validate_file)) {
        write_metadata(metadata, sprintf("%s.meta.json", destfile))
        logger::log_info(sprintf("File created at %s ", destfile))
    }

    if (metadata$monstr$is_latest) {

        version <- metadata$monstr$version
        metadata$monstr$version <- "LATEST"

        linkfile <- generate_download_filename(template=metadata$monstr$download_filename_template,
                                               root=metadata$monstr$download_root,
                                               data=metadata$monstr)

        metadata$monstr$version <- version
        if (file.exists(linkfile)) {
            file.remove(linkfile)
        }

        file.symlink(destfile,
                     linkfile)
        log_info("Create symlink to LATEST file")
    }

    metadata$monstr$destfile <- destfile
    metadata

}
