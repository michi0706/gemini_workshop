MAX_RUNTIME_SEC = 5*60*60 # Max runtime on Github
MIN_SEC_PER_PROMPT = 5 # Minimum number of seconds between prompts

perform_analysis = function() {
  library(dplyr)
  start_time = as.numeric(Sys.time())
  API_KEY = Sys.getenv("API_KEY")
  setwd("~") 
  outdir = "/root/output"
  if (.Platform$OS.type == "windows") {
    setwd("C:/libraries/gpt/gemini/gemini_ex")
  }
  if (FALSE) {
    setwd("~/repbox/gemini/gemini_gha")  
  }
  source("scripts/gemini_tools.R")
  config_df = load_prompt_configs()

  # 論文ファイルとテンプレートファイルの取得
  paper_files = list.files("prompts/papers", glob2rx("*.txt"), full.names = TRUE)
  template_files = list.files("prompts/templates", glob2rx("*.txt"), full.names = TRUE)

  # 各論文に対して処理
  for (paper_file in paper_files) {
    paper_text = paste0(readLines(paper_file, warn = FALSE), collapse="\n")
    paper_name = tools::file_path_sans_ext(basename(paper_file))
    
    # 各テンプレートに対して処理
    for (template_file in template_files) {
      prompt_start_time = as.numeric(Sys.time())
      template_name = tools::file_path_sans_ext(basename(template_file))
      cat("\n\n****", paper_name, "-", template_name, "***\n")
      
      # テンプレートの読み込みとプロンプト生成
      template = paste0(readLines(template_file, warn = FALSE), collapse="\n")
      prompt = gsub("{paper_text}", paper_text, template, fixed=TRUE)
      
      # プロンプトを直接run_geminiに渡す
      config = get_prompt_config(template_file, config_df)
      res = run_gemini(prompt, api_key=API_KEY, 
                      json_mode=config$json_mode, 
                      model=config$model, 
                      temperature=config$temperature)
      
      # 結果の保存
      out_file = paste0(outdir, "/", paper_name, "_", template_name, ".Rds")
      saveRDS(res, out_file)

      cur_time = as.numeric(Sys.time())
      # 実行時間のチェック
      if (cur_time - start_time > MAX_RUNTIME_SEC) {
        cat("\nStop because total runtime exceeded ", MAX_RUNTIME_SEC, " seconds.\n")
        return()
      }
      wait_sec = MIN_SEC_PER_PROMPT-(cur_time - prompt_start_time)
      if (wait_sec > 0) {
        cat("\nWait for ", round(wait_sec), "seconds...")
        Sys.sleep(wait_sec)
      }
    }
  }
  cat("\n\nFINISHED\n\n")
}

get_prompt_config = function(file, config_df) {
  def_config = config_df[config_df$prompt_type=="_default",]
  prompt_id = tools::file_path_sans_ext(basename(file))
  row = which(prompt_id==config_df$prompt_type)
  if (length(row)==0) {
    row = which(startsWith(prompt_id, config_df$prompt_type))
  }
  if (length(row)==0) return(def_config)
  config = config_df[row[1],]
  fields = names(config)
  fields = fields[!sapply(config,is.na)]
  def_config[1, fields] = config[1, fields]
  def_config[1,]
}

load_prompt_configs = function() {
  library(stringi)
  library(dplyr)
  files = list.files("config", glob2rx("*.yml"), full.names = TRUE)
  config_df = bind_rows(lapply(files, function(config_file) {
    res = yaml::yaml.load_file(config_file)
    prompt_type = tools::file_path_sans_ext(basename(config_file))
    res$prompt_type = prompt_type
    res = as.data.frame(res)
  }))
  return(config_df)
}
