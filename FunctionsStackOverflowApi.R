# install.packages("httr", dependencies = TRUE)
# install.packages("xml2", dependencies = TRUE)
# install.packages("urltools", dependencies = TRUE)
# install.packages("jsonlite", dependencies = TRUE)
# install.packages("anytime", dependencies = TRUE)
library(httr)
library(xml2)
library(urltools)
library(jsonlite)
library(anytime)
library(stringr)

so_base_url <- "https://api.stackexchange.com/2.2/search/advanced"

# Search for items matching the query string from StackOverflow
get_data <- function(filter = "withbody", page = NULL) {
  api_url <- so_base_url

  api_url <- paste(api_url, "?key=", so_api_key, sep = "", collapse = "")
  api_url <- paste(api_url, "&site=stackoverflow", sep = "", collapse = "")
  api_url <- paste(api_url, "&order=", "desc", sep = "", collapse = "")
  api_url <- paste(api_url, "&sort=", "activity", sep = "", collapse = "")
  api_url <- paste(api_url, "&filter=", filter, sep = "", collapse = "")
  api_url <- paste(api_url, "&q=", query_string, sep = "", collapse = "")

  if (!(is.null(page))) {
    api_url <- paste(api_url, "&page=", page,  sep = "", collapse = "")
  }

  response <- GET(URLencode(api_url))
  content(response)
}


get_stackoverflow_data <- function(query_string) {
  if (!exists("so_api_key")) {
    stop("Please set the StackOverflow API key in the script")
  }

  dataset <- data.frame()
  page_number <- 1

  total_count <- get_data(filter = "total")

  repeat {
    request_data <- get_data(page = page_number)
    if (length(request_data$items) == 0) {
      break
    }

    dataset_size <- nrow(dataset) + length(request_data$items)

    print(
      paste(
        "Page:", str_pad(as.character(page_number), 3, side = "left"),
        "| Items:", dataset_size,
        "/", total_count$total,
        "| Results: ", length(request_data$items),
        sep = " ",
        collapse = ""
      )
    )

    for (item in request_data$items) {
      tag_number <- 0
      tags <- NULL

      for (tag in item$tags) {
        if (tag_number == 0) {
          tags <- tag
          tag_number <- 1
        } else {
          tags <- paste(tags, tag, sep = ";", collapse = "")
          tag_number <- tag_number + 1
        }
      }

      date_cr <- anydate(item$creation_date)
      date_la <- anydate(item$last_activity_date)

      row <- data.frame(
        AuthorId = ifelse(is.null(item$owner$user_id), 0, item$owner$user_id),
        Q_id = ifelse(is.null(item$question_id), "", item$question_id),
        Title = ifelse(is.null(item$title), "", item$title),
        Abstract = ifelse(is.null(item$body), "", item$body),
        Views = ifelse(is.null(item$view_count), 0, item$view_count),
        Answers = ifelse(is.null(item$answer_count), 0, item$answer_count),
        Cites = ifelse(is.null(item$score), 0, item$score),
        Tags_n = tag_number,
        Tags = ifelse(is.null(tags), "", tags),
        Date = ifelse(is.null(date_cr), 0, as.character(date_cr)),
        CR_Date = ifelse(is.null(date_cr), 0, as.character(date_cr)),
        LA_Date = ifelse(is.null(date_la), 0, as.character(date_la)),
        stringsAsFactors = F
      )

      dataset <- rbind(dataset, row)
    }

    if (page_number %% 10 == 0) {
      Sys.sleep(1)
    }
    page_number <- page_number + 1
  }

  dataset
}
