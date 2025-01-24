library(httr)
library(jsonlite)

parse_gemini_content <- function(data) {
 tryCatch({
   text <- data$candidates[[1]]$content$parts[[1]]$text
   if(is.null(text) || nchar(text) == 0) return(NULL)
   
   json_patterns <- regmatches(text, gregexpr("```json\\n(.*?)\\n```", text, perl=TRUE))
   if(length(json_patterns) > 0 && length(json_patterns[[1]]) > 0) {
     parsed_jsons <- lapply(json_patterns[[1]], function(pattern) {
       cleaned <- gsub("```json\\n|```", "", pattern)
       fromJSON(cleaned)
     })
     return(toJSON(parsed_jsons, pretty=TRUE, auto_unbox=TRUE))
   }
   return(NULL)
 }, error = function(e) return(NULL))
}

escape_quotes = function(txt, double_quotes=TRUE, single_quotes=TRUE) {
 if (single_quotes) txt = gsub("'","\\'",txt, fixed=TRUE)
 if (double_quotes) txt = gsub('"','\\"',txt, fixed=TRUE)
 txt
}

gemini_result_to_df = function(res, ...) {
 if (!is.null(res$error)) {
   li = c(list(...), res[c("model","json_mode","temperature")],
          list(error = res$error$message, finishReason = "error", content = NA))
 } else {
   content = res$candidates$content$parts[[1]]$text
   li = c(list(...), res[c("model","json_mode","temperature")],
          list(error = "", finishReason = res$candidates$finishReason[1], 
               content = content))
   if (!is.na(content)) {
     json_str = parse_gemini_content(res)
     if (!is.null(json_str)) {
       li$parsed_json = json_str
     }
   }
 }
 as.data.frame(li)
}

run_gemini = function(prompt,api_key, model="gemini-1.5-flash", json_mode=FALSE, temperature=0.1, add_prompt=FALSE, verbose=TRUE) {
 url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model,":generateContent?key=", api_key)
 
 generationConfig = list(temperature = temperature)
 if (json_mode) generationConfig$response_mime_type = "application/json"

 response <- POST(
   url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model,":generateContent"),
   query = list(key = api_key),
   content_type_json(),
   encode = "json",
   body = list(
     contents = list(parts = list(list(text = prompt))),
     generationConfig = generationConfig
   )
 )

 status_code = status_code(response)
 json = content(response, "text")
 
 if (verbose) cat("\n\nResult:\n",nchar(json), " characters:\n\n",json)
 
 res = try(fromJSON(json),silent = TRUE)
 if (is(res, "try-error")) {
   return(list(status_code = status_code,parse_error=TRUE, json=json))
 }
 
 res$status_code = status_code
 res$parse_error = FALSE
 if (add_prompt) res$prompt = prompt
 res$model = model
 res$json_mode = json_mode
 res$temperature = temperature
 res
}

run_gemini_embedding = function(text,api_key, model="gemini-1.5-flash", add_text=FALSE, verbose=TRUE) {
 cat("\nCreate embedding:\n")
 
 response <- POST(
   url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model,":generateEmbeddings"),
   query = list(key = api_key),
   content_type_json(),
   encode = "json",
   body = list(contents = list(text = list(text=text)))
 )

 status_code = status_code(response)
 json = content(response, "text")
 
 if (verbose) cat("\n\nResult:\n",nchar(json), " characters:\n\n",json)
 
 res = try(fromJSON(json),silent = TRUE)
 if (is(res, "try-error")) {
   return(list(status_code = status_code,parse_error=TRUE, json=json))
 }
 
 res$status_code = status_code
 res$parse_error = FALSE
 if (add_text) res$text = text
 res$model = model
 res$json_mode = json_mode
 res$temperature = temperature
 res
}
