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

maybe_append <- function(s, v, sep) {
    print(s)
    print(v)

    if (is.na(v)) {
        s
    } else {
        if(is.na(s) || stringr::str_length(s) == 0) {
            paste0("", v)
        } else {
            paste0(s, sep, v)
        }
    }
}

##' @title Create the pipeline defaults
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
##' ae_datasets_setup(pipeline_defaults()) # rooted in current project
##' }
ae_datasets_setup <- function(defaults) {
    links <- links_from_url(base_url) %>%
        purrr::map(~ xml2::xml_attr(., "href")) %>%
        purrr::keep(~ grepl("ae-attendances-and-emergency-admissions", .)) %>%
        purrr::reduce(~ if (.y %in% .x) {.x} else {append(.x, .y)})

    ids <- links %>% purrr::modify(strip_base_url)

    items=dplyr::tibble(id=ids,link=links)
    pipeline__ <- defaults
    pipeline__$datasource <- "A_and_E"

    dplyr::tibble(items=list(dplyr::tibble(items)), pipeline__=list(pipeline__))
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
    dplyr::tibble(edition=c("timeseries", "ae-by-provider"))
}

grep_bounded_word <- function(pattern, s) {
# grep for the word surrounded by word boundaries, but consider a _ to
# be a word boundary (unlike \b in pcre)
    grepl(paste0("(?<![A-Za-z\\A])",pattern,"(?![A-Za-z]\\Z)"), s, ignore.case = TRUE, perl = TRUE)
}

parse_month <- function(s) {
    ## Implementation Note: We can do this more efficiently with a
    ## hairy regex, but we choose to do the simple dumb thing for
    ## readability. This is inefficient (multiple calls to grep*), but
    ## we don't expect this to be called a lot. If it *is* called a
    ## lot this implementation can be revisited
    if (grep_bounded_word("Jan(?:uary)?", s)) {
        version <- "january"
    } else if (grep_bounded_word("Feb(?:ruary)?", s)) {
        version <- "february"
    } else if (grep_bounded_word("Mar(?:ch)?", s)) {
        version <- "march"
    } else if (grep_bounded_word("Apr(?:il)?", s)) {
        version <- "april"
    } else if (grep_bounded_word("May", s)) {
        version <- "may"
    } else if (grep_bounded_word("Jun(?:e)?", s)) {
        version <- "june"
    } else if (grep_bounded_word("Jul(?:y)?", s)) {
        version <- "july"
    } else if (grep_bounded_word("Aug(?:ust)?", s)) {
        version <- "august"
    } else if (grep_bounded_word("Sep(?:tember)?", s)) {
        version <- "september"
    } else if (grep_bounded_word("Oct(?:ober)?", s)) {
        version <- "october"
    } else if (grep_bounded_word("Nov(?:ember)?", s)) {
        version <- "november"
    } else if (grep_bounded_word("Dec(?:ember)?", s)) {
        version <- "december"
    } else {
        version <- NA
    }
    version
}

parse_year <- function(s) {
    re <- regexec("(?<![\\d\\A])\\d{4}(?![\\d\\Z])", s, perl = TRUE) # 4 digits NOT preceded by or followed by a digit
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
    version <- "" %>%
        maybe_append(parse_quarter(s), "_") %>%
        maybe_append(parse_month(s), "_") %>%
        maybe_append(parse_year(s), "_")


    dplyr::tibble(version = c(version), is_latest=c(FALSE))
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
    "by_provider"
  } else {
      NA
  }
}

parse_quarter <- function(s) {
    re <- regexec("(?<![\\d\\A])Q(?:uarter)?[-_]?([1-4])(?![\\d\\Z])", s, perl=TRUE) # Match Q1 or Quarter_1 like things but no Q23 etc.
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
    if(!missing(release_frequency)) {
        id <- maybe_append(id, "_", release_frequency)
    }
    if(!missing(month)) {
        id <- maybe_append(id, "_", month)
    }
    if(!missing(quarter)) {
        id <- maybe_append(id, "_", quarter)
    }
    if(!missing(year)) {
        id <- maybe_append(id, "_", year)
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
    purrr::flatten(result)}

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
    versions <- dplyr::tibble(version=links_from_url(sprintf("%s/%s/", base_url, id)) %>%
        purrr::map(~ list(href = xml2::xml_attr(., "href"), description = xml2::xml_text(.)))  %>%
        purrr::keep(~ grepl(edition_regex, .$href, ignore.case = TRUE)) %>%
        purrr::modify(ae_href_parser, id))
    tidyr::unnest_wider(versions, "version")
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
    ae_available_versions(metadata, id, edition) %>% select(id, description)
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

    dataset <- versions %>% dplyr::filter(id == version)

    if (length(dataset) == 0) {
        stop(sprintf("No dataset with version "%s" found", version))
    } else {
        metadata$pipeline__[[1]]$dataset <- list(dataset)
        metadata
    }
}

pipeline_metadata <- function(metadata) {
    metadata$pipeline__[[1]]
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

    try (if(!(format %in% c("xls"))) stop("Format not allowed"))

    logger::log_info(sprintf("Downloading data from %s", metadata$href))

    pipeline__ <- pipeline_metadata(metadata)
    destfile <-  generate_download_filename(template=pipeline__$download_filename_template,
                                            root=pipeline__$download_root,
                                            data=pipeline__$dataset)

    if (safe_download(url = c(pipeline__$dataset[[1]]$href),
                      destfile = destfile,
                      fvalidate = validate_file)) {
        write_metadata(metadata, sprintf("%s.meta.json", destfile))
        logger::log_info(sprintf("File created at %s ", destfile))
    }

    if (metadata$pipeline__$is_latest) {

        version <- metadata$pipeline__$version
        metadata$pipeline__$version <- "LATEST"

        linkfile <- generate_download_filename(template=metadata$pipeline__$download_filename_template,
                                               root=metadata$pipeline__$download_root,
                                               data=metadata$pipeline__)

        metadata$pipeline__$version <- version
        if (file.exists(linkfile)) {
            file.remove(linkfile)
        }

        file.symlink(destfile,
                     linkfile)
        log_info("Create symlink to LATEST file")
    }

    metadata$pipeline__$destfile <- destfile
    metadata

}
