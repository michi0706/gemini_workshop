library(httr)
library(jsonlite)

# JSONテキストの解析と階層的表示のための関数
parse_gemini_content <- function(data) {
  text <- data$candidates[[1]]$content$parts[[1]]$text
  # JSONパターンを抽出
  json_patterns <- regmatches(text, gregexpr("```json\\n(.*?)\\n```", text, perl=TRUE))
  parsed_jsons <- lapply(json_patterns[[1]], function(pattern) {
    cleaned <- gsub("```json\\n|```", "", pattern)
    fromJSON(cleaned)
  })
  return(parsed_jsons)
}

# 既存の関数群
escape_quotes = function(txt, double_quotes=TRUE, single_quotes=TRUE) {
  if (single_quotes) {
    txt = gsub("'","\\'",txt, fixed=TRUE)
  }
  if (double_quotes) {
    txt = gsub('"','\\"',txt, fixed=TRUE)
  }
  txt
}

gemini_result_to_df = function(res, ...) {
  if (!is.null(res$error)) {
    li = c(
      list(...),
      res[c("model","json_mode","temperature")],
      list(
        error = res$error$message,
        finishReason = "error",
        content = NA
      )
    )
  } else {
    li = c(
      list(...),
      res[c("model","json_mode","temperature")],
      list(
        error = "",
        finishReason = res$candidates$finishReason[1],
        content = paste0(unlist(res$candidates$content,use.names = FALSE), collapse="")
      )
    )
  }
  # JSONの解析と整形を追加
  if (!is.na(li$content)) {
    parsed_content <- try(parse_gemini_content(res), silent = TRUE)
    if (!inherits(parsed_content, "try-error")) {
      li$parsed_json <- toJSON(parsed_content, pretty=TRUE, auto_unbox=TRUE)
    }
  }
  return(as.data.frame(li))
}

run_gemini = function(prompt, api_key, model="gemini-1.5-flash", json_mode=FALSE, temperature=0.1, add_prompt=FALSE, verbose=TRUE) {
  url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model,":generateContent?key=", api_key)
  
  generationConfig = list(
    temperature = temperature
  )
  if (json_mode) {
    generationConfig$response_mime_type = "application/json"
  }
  
  response <- POST(
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model,":generateContent"),
    query = list(key = api_key),
    content_type_json(),
    encode = "json",
    body = list(
      contents = list(
        parts = list(
          list(text = prompt)
        )
      ),
      generationConfig = generationConfig
    )
  )
  
  status_code = status_code(response)
  json = content(response, "text")
  
  if (verbose) {
    cat("\n\nResult:\n",nchar(json), " characters:\n\n",json)
  }
  
  res = try(fromJSON(json), silent = TRUE)
  if (is(res, "try-error")) {
    res = list(status_code = status_code, parse_error=TRUE, json=json)
    return(res)
  }
  
  res$status_code = status_code
  res$parse_error = FALSE
  if (add_prompt) {
    res$prompt = prompt
  }
  res$model = model
  res$json_mode = json_mode
  res$temperature = temperature
  res
}

[run_gemini_embedding関数は変更なし]

# 使用例
example = function() {
  data <- readRDS("path/to/your.Rds")
  result <- gemini_result_to_df(data)
  if (!is.null(result$parsed_json)) {
    cat(result$parsed_json)
  }
}
