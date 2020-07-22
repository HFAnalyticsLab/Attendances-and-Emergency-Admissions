base_url <- "https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/"

links_from_url <- function(url) {
    xml2::read_html(url) %>%
        rvest::html_nodes(css="#main-content a")
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param defaults
##' @return
##' @author neale
##' @export
##' @examples
##' \dontrun{
##' ae_datasets_setup(monstr_pipeline_defaults()) # rooted in current project
##' }
ae_datasets_setup <- function(defaults) {
    links <- links_from_url(base_url) %>%
        purrr::map(~ xml2::xml_attr(., "href")) %>%
        purrr::keep(~ grepl("ae-attendances-and-emergency-admissions", .)) %>%
        purrr::reduce(~ if (.y %in% .x) {.x} else{append(.x, .y)})
    ids <- links %>%
        purrr::modify(~ substr(.,nchar(base_url)+1, nchar(.)-1))

    ## TODO - should we have a column per dataset? allowing different meta per ds?
    dplyr::tibble(id=ids, href=links)
}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param metadata
##' @return dataframe of available datasets
##' @export
##' @author neale
ae_available_datasets <- function(metadata) {
    metadata %>% dplyr::select(id)
}

##' Return a list of available editions
##'
##' .. content for \details{} ..##' @title
##' @param metadata
##' @param id identifier for the dataset
##' @return
##' @export
##' @author neale
ae_available_editions <- function(metadata, id) {
    # TODO hardcoded for now
    c("timeseries", "ae-by-provider")
}

parse_month <- function(s) {
    ## Implementation Note: We can do this more efficiently with a
    ## hairy regex, but we choose to do the simple dumb thing for
    ## readability. This is inefficient (multiple calls to grep*), but
    ## we don't expect this to be called a lot. If it *is* called a
    ## lot this implementation can be revisited
    if (grepl("\\bJan(?:uary)?\\b", s, ignore.case=TRUE)) {
        version <- "january"
    } else if (grepl("\\bFeb(?:ruary)?\\b", s, ignore.case=TRUE)) {
        version <- "february"
    } else if (grepl("\\bMar(?:ch)?\\b", s, ignore.case=TRUE)) {
        version <- "march"
    } else if (grepl("\\bApr(?:il)?\\b", s, ignore.case=TRUE)) {
        version <- "april"
    } else if (grepl("\\bMay\\b", s, ignore.case=TRUE)) {
        version <- "may"
    } else if (grepl("\\bJun(?:e)?\\b", s, ignore.case=TRUE)) {
        version <- "june"
    } else if (grepl("\\bJul(?:y)?\\b", s, ignore.case=TRUE)) {
        version <- "july"
    } else if (grepl("\\bAug(?:ust)?\\b", s, ignore.case=TRUE)) {
        version <- "august"
    } else if (grepl("\\bSep(?:tember)?\\b", s, ignore.case=TRUE)) {
        version <- "september"
    } else if (grepl("\\bOct(?:ober)?\\b", s, ignore.case=TRUE)) {
        version <- "october"
    } else if (grepl("\\bNov(?:ember)?\\b", s, ignore.case=TRUE)) {
        version <- "november"
    } else if (grepl("\\bDec(?:ember)?\\b", s, ignore.case=TRUE)) {
        version <- "december"
    } else {
        version <- NA
    }
    version
}

parse_year <- function(s) {
    re <- regexec("(?<!\\d)\\d{4}(?!\\d)", s, perl=TRUE) # 4 digits NOT preceded by or followed by a digit
    l <- attr(re[[1]],"match.length")
    if (l != -1) {
        match <- re[[1]][1]
        ystr <- substring(s, match, match+l-1)
        year <- as.numeric(ystr)
    } else {
        year <- NA
    }
    year
}

parse_version <- function(s) {
    month<- parse_month(s)
    year <- parse_year(s)
    dplyr::tibble(month=c(month), year=c(year))
}

parse_release_frequency <- function(s) {
    if(grepl("monthly", s, ignore.case=TRUE)) {
        "monthly"
    } else if(grepl("quarterly", s, ignore.case=TRUE)) {
        "quarterly"
    } else {
        NA
    }
}

parse_adjusted <- function(s) {
    if(grepl("\\badjusted\\b", s, ignore.case=TRUE)) {
        TRUE
    } else if (grepl("\\bunadjusted\\b", s, ignore.case=TRUE)) {
        FALSE
    } else {
        NA
    }
}

parse_edition <- function(s) {
    if (grepl("Timeseries", s, ignore.case=TRUE)) {
        "timeseries"
    } else {
        NA
    }
}

ae_href_parser <- function(link_node, parent) {
    href<- link_node$href
    match <- regexec(".*/(.*?)$", href)[[1]]
    if (match[1] == -1) {
        stop("match error")
    }

    s <- substring(href,match[2])

    result <- dplyr::tibble(version=parse_version(s),
                            edition=parse_edition(s),
                            description=c(link_node$description),
                            href=c(href),
                            adjusted=parse_adjusted(s),
                            release_frequency=parse_release_frequency(s))
    result$id <- sprintf("%s_%s_%s_%s_%s", parent, result$edition, result$release_frequency, result$version$month, result$version$year)
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
##' @author neale
ae_available_versions <- function(metadata, id, edition) {
    edition_regex <- sprintf("(%s)", paste(ae_available_editions(), collapse="|"))
    versions <- links_from_url(sprintf("%s/%s/", base_url, id)) %>%
        purrr::map(~ dplyr::tibble(href=xml2::xml_attr(., "href"), description=xml2::xml_text(.))) %>%
        purrr::keep(~ grepl(edition_regex, .$href, ignore.case=TRUE)) %>%
        purrr::modify(ae_href_parser, id)

    versions
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param metadata
##' @param id
##' @return metadata describing the given dataset
##' @export
##' @author neale
ae_dataset_by_id <- function(metadata, id, edition, version) {
    links <- links_from_url(sprintf("%s/%s/", base_url, id)) %>%
        purrr::map(~ xml2::xml_attr(., "href")) %>%
        purrr::keep(~ grepl(edition, ., ignore.case=TRUE)) %>%
        purrr::reduce(~ if (.y %in% .x) {.x} else{append(.x, .y)})
    links
}


##' Download
##'
##' \code{ae_download} retrieves the data described by the given df
##' @param metadata data describing the download
##' @param format a valid format for the download
##' @export
##' @import logger
ae_download <- function(metadata,
                        format="csv" ) {
}
