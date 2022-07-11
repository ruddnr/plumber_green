#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#
source("green_ftn.R", encoding = "UTF-8")

library(plumber)
library(jsonlite)
dat_list <- readRDS("sim_dat.RDS")
walk(names(dat_list), ~assign(.x, value = dat_list[[.x]], envir = .GlobalEnv))

#* @apiTitle Plumber Example API
#* @apiDescription Plumber example description.

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
    list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot a histogram
#* @serializer png
#* @get /plot
function() {
    rand <- rnorm(100)
    hist(rand)
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
    as.numeric(a) + as.numeric(b)
}



# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}

#* Calculate portfolio weight and return
#* @param sim_start start date
#* @param sim_end end date
#* @param include_sector sector to include
#* @param include_theme theme to include
#* @param value_adj:numeric value factor adjustment score
#* @param size_adj:numeric size factor adjustment score
#* @param quality_adj:numeric quality factor adjustment score
#* @param em_adj:numeric earnings momentum factor adjustment score
#* @param pm_adj:numeric price momentum factor adjustment score
#* @param weight_add_tbl sector weight adjustment
#* @param num_select:int number of stocks to include for each theme
#* @post /calc_port_weight

function(
    sim_start, sim_end, 
    include_sector, include_theme,
    # include_sector = toJSON(c("2차전지", "전기차", "대체에너지")),
    # include_theme = toJSON(c("폐기물")),
    value_adj, size_adj, quality_adj, em_adj, pm_adj,
    # weight_add_vec = toJSON(c(0.05, -0.05, 0, 0)),
    weight_add_vec,
    num_select) {
  
  sim_start <- ymd(sim_start)
  sim_end <- ymd(sim_end)
  value_adj <- as.numeric(value_adj)
  size_adj <- as.numeric(size_adj)
  quality_adj <- as.numeric(quality_adj)
  em_adj <- as.numeric(em_adj)
  pm_adj <- as.numeric(pm_adj)
  num_select <- as.numeric(num_select)
  include_sector <- fromJSON(include_sector)
  include_theme <- fromJSON(include_theme)
  weight_add_tbl <- tibble(
    sector = unique(ticker_tbl$sector), add_weight_sector = fromJSON(weight_add_vec)
  )
  
  
  # 포트폴리오 비중 계산 ----
  # step 1. 유니버스 필터링
  universe_filtered <- universe_tbl %>% 
    filter_universe(sim_start = sim_start, sim_end = sim_end,
                    include_sector = include_sector, include_theme = include_theme)
  
  # step 2. 펙터스코어 가중치 조절
  factor_score <- universe_filtered %>% 
    get_factor_score(
      value_adj = value_adj, size_adj = size_adj, 
      quality_adj = quality_adj, em_adj = em_adj, pm_adj = pm_adj
    )
  
  # step 3. 섹터비중
  sector_weight <- get_sector_weight(factor_score, weight_add_tbl)
  
  # step 4. 개별종목 스코어
  indv_score <- get_indv_score(factor_score, num_select)
  
  # step 5. 포트비중
  port_weight <- indv_score %>% 
    left_join(sector_weight) %>% 
    mutate(weight = theme_weight * indv_weight) %>% 
    group_by(td) %>% 
    mutate(weight = weight / sum(weight)) %>% 
    ungroup()
  
  port_return <- get_port_return(port_weight, daily_rtn)
  
  return(list(port_weight = port_weight, port_return = port_return))
}
