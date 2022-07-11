library(tidyverse)
library(lubridate)
library(tidyquant)
library(timetk)
library(slider)
library(tidymodels)
dat_list <- readRDS("sim_dat.RDS")

# 0. 포트폴리오 시뮬레이션 함수 ----
# 포트폴리오 동일가중 수익률 계산
get_port_return <- function(port_weight, rtn_tbl, trd_cost = 0.003) {
  
  term_tbl <- tibble(
    td = unique(rtn_tbl$td)
  ) %>%
    filter(td >= min(port_weight$td)) %>% 
    left_join(port_weight %>% distinct(td) %>% mutate(term = td), by = "td") %>% 
    fill(term, .direction = "down") %>% 
    mutate(rebal = td == term) %>% 
    mutate(td = lead(td, 1)) %>% 
    na.omit()
  
  port_return <- rtn_tbl %>% 
    filter(td >= min(term_tbl$td), td <= max(term_tbl$td)) %>% 
    left_join(term_tbl, by = "td") %>% 
    left_join(
      port_weight %>% rename(term = td), 
      by = c("term", "code")
    ) %>% 
    arrange(td) %>% 
    
    # Drifting weight 계산
    group_by(term, code) %>% 
    # fill(weight, .direction = "down") %>% 
    mutate(weight = replace_na(weight, 0)) %>%
    mutate(weight2 = cumprod(1 + rtn) * weight) %>%
    mutate(weight2 = lag(weight2)) %>% 
    mutate(weight = if_else(is.na(weight2), weight, weight2)) %>% 
    ungroup() %>% 
    select(-weight2) %>% 
    
    # 비중 노멀라이즈
    group_by(td) %>% 
    mutate(weight = weight / sum(weight)) %>% 
    ungroup() %>% 
    
    group_by(code) %>% 
    mutate(diff = if_else(rebal == TRUE, weight - lag(weight), 0)) %>% 
    # 분석초기 weight_chg 0으로 취급
    mutate(diff = replace_na(diff, 0)) %>% 
    ungroup() %>% 
    
    group_by(td) %>% 
    summarise(
      rtn = sum(rtn * weight, na.rm = TRUE),
      diff = sum(abs(diff), na.rm  = TRUE) / 2
    ) %>% 
    mutate(rtn_with_cost = rtn - diff * trd_cost) %>% 
    ungroup()
  
  return(port_return)
}

get_port_sim <- function(port_return) {
  bind_cols(
    tq_performance(port_return, rtn_with_cost, performance_fun = table.AnnualizedReturns),
    tq_performance(port_return, rtn_with_cost, performance_fun = maxDrawdown),
    port_return %>% summarise(avg_turnover = sum(diff)/(nrow(.)/52))
  )
}

get_rtn_cum <- function(dat, rtn_var) {
  dat %>% 
    mutate(rtn_cum = cumprod({{rtn_var}} + 1) - 1) %>% 
    select(td, rtn = {{rtn_var}}, rtn_cum)
}

# 0.2. 포트폴리오 시각화 함수 ----
get_plot_dat <- function(port_return, bm_return = green_rtn) {
  start_date <- max(min(port_return$td), min(bm_return$td))
  end_date <- min(max(port_return$td), max(bm_return$td))
  
  port_return <- port_return %>% filter(td >= start_date, td <= end_date)
  bm_return <- bm_return %>% filter(td >= start_date, td <= end_date)
  
  dat <- bind_rows(
    get_rtn_cum(port_return, rtn_with_cost) %>% mutate(type = "port"),
    get_rtn_cum(bm_return, rtn) %>% mutate(type = "bm")
  )
  
  return(dat)
}

plot_cum_return <- function(dat) {
  
  g1 <- dat %>% 
    mutate(text = str_glue("{td} : {scales::percent(rtn, accuracy = 0.01)}
                           type = {type}")) %>% 
    ggplot(aes(td, rtn_cum, color = type, label =  text)) +
    geom_line() +
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::percent_format()) + 
    labs(x = "", y = "") + 
    theme(legend.position = "none")


  return(ggplotly(g1, tooltip = "label"))
}

plot_monthly_return <- function(dat) {
  # 월별 수익률
  dat2 <- dat %>% 
    mutate(yearmon = as.yearmon(td)) %>% 
    group_by(type, yearmon) %>% 
    summarise(rtn = prod(rtn + 1) -1) %>% 
    ungroup()
  
  dat2 <- dat2 %>% 
    pivot_wider(names_from = type, values_from = rtn) %>% 
    mutate(rtn = port - bm) %>% 
    select(yearmon, rtn) %>% 
    mutate(type = "diff")
  
  
  g2 <- dat2 %>% 
    mutate(text = str_glue("{yearmon} : {scales::percent(rtn, 0.01)}")) %>% 
    ggplot(aes(yearmon, rtn, fill = type, text = text)) +
    geom_col(position = "dodge") + 
    scale_fill_tq() + 
    theme_tq() +
    scale_y_continuous(labels = scales::percent_format()) + 
    labs(x = "", y = "") + 
    theme(legend.position = "none")
  
  return(ggplotly(g2, tooltip = "text"))
}

plot_weight_ts <- function(port_weight) {
  g <- port_weight %>% 
    group_by(td, theme) %>% 
    summarise(theme_weight = sum(weight)) %>% 
    ungroup() %>% 
    mutate(weight = str_glue("{td}
                                 {theme} : {percent(theme_weight, accuracy = .01)}")) %>% 
    ggplot(aes(td, theme_weight, fill = theme, label = weight)) + 
    geom_area() + 
    scale_fill_tq() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
    theme_tq() +
    theme(legend.position = "right") + 
    labs(x = "", y = "")
  ggplotly(g, tooltip = "label")
}

plot_weight_pie <- function(port_weight, target_td){
  port_weight_filtered <- port_weight %>% 
    filter(td == target_td) %>% 
    group_by(theme) %>% 
    summarise(weight = sum(weight)) %>% 
    ungroup()
  
  fig <- plot_ly(port_weight_filtered, labels = ~theme, values = ~weight, type = "pie",
                 textposition = "inside", 
                 textinfo = "label+percent",
                 hoverinfo = "label+percent",
                 showlegend = FALSE)
  
  return(fig)
}



filter_universe <- function(universe_tbl, 
                            sim_start = "2015-01-03", sim_end = "2022-04-29",
                            include_sector, include_theme) {
  universe_tbl %>% 
    filter(sector %in% include_sector | theme %in% include_theme) %>% 
    filter(td >= sim_start, td <= sim_end)
}


# step2. 팩터스코어 ----
# 팩터스코어 가중치 조절 (-100% ~ 100%)

# adj 값은 -1~1
get_factor_score <- function(universe_filtered, value_adj = 0, size_adj = 0, 
                             quality_adj = 0, em_adj = 0, pm_adj = 0) {
  factor_joined <- 
    universe_filtered %>% 
    left_join(factor_tbl) %>%
    group_by(td, col) %>% 
    mutate(val = if_else(ord == "ASC", percent_rank(-val), percent_rank(val))) %>% 
    ungroup() %>% 
    
    # NA인 경우 0으로 대체
    mutate(val = replace_na(val, 0)) %>% 
    
    select(td, code, name,  val, col, sector, theme) %>% 
    pivot_wider(names_from = col, values_from = val) %>% 
    
    # 팩터별 스코어 계산
    group_by(td, code, name, sector, theme) %>% 
    summarise(
      value = mean(VAL_TR_PER, VAL_FW_PER, VAL_TR_PBR),
      size = MKT_MCAP,
      quality = mean(VAL_TR_ROE, QUL_RTN_VOL_3M, QUL_EPS_VISIB_6Y, QUL_FCF_STAB),
      earnings_momentum = mean(EPSFY1_MT1M, EPSFY2_MT1M, VAL_FW_EPS_G),
      price_momentum = 0.5 * PM1M + 0.4 * PM3M + 0.3 * PM6M + 0.2 * PM1Y - 0.4 * PM1W
    ) %>% 
    ungroup() %>% 
    
    group_by(td) %>% 
    mutate(across(where(is.numeric), ~percent_rank(.))) %>% 
    ungroup() 
  
  factor_score <- factor_joined %>% 
    mutate(
      value = value * (value_adj + 1),
      size = size * (size_adj + 1), 
      quality = quality * (quality_adj + 1), 
      earnings_momentum = earnings_momentum * (em_adj + 1), 
      price_momentum = price_momentum * (pm_adj + 1)
    ) %>% 
    mutate(score = rowSums(across(where(is.numeric)))) %>% 
    group_by(td) %>% 
    # 정규화
    mutate(score = (score - mean(score)) / sd(score)) %>% 
    # 분포함수로 0 ~ 1사이의 스코어로 변환
    mutate(score = pnorm(score)) %>% 
    ungroup() 
  
  return(factor_score)
}



# step3. 섹터, 테마 비중 ----


get_sector_weight <- function(factor_score, weight_add_tbl) {
  assertthat::assert_that(
    near(sum(weight_add_tbl$add_weight_sector), 0),
    length(unique(weight_add_tbl$sector)) == 4
  )
  
  theme_score <- factor_score %>% 
    group_by(td, sector, theme) %>% 
    summarise(theme_score = mean(score)) %>% 
    mutate(theme_weight = theme_score / sum(theme_score)) %>% 
    ungroup() %>% 
    group_by(td) %>% 
    mutate(theme_rank = row_number(-theme_score)) %>% 
    ungroup()
  
  sector_score <- factor_score %>% 
    group_by(td, sector) %>% 
    # 구성종목의 평균
    summarise(sector_score = mean(score)) %>% 
    # 스코어 값 비율로 비중 계산
    mutate(sector_weight = sector_score / sum(sector_score)) %>% 
    ungroup() 
  
  sector_weight <- sector_score %>% 
    left_join(weight_add_tbl, by = "sector") %>% 
    mutate(add_weight_sector = replace_na(add_weight_sector, 0)) %>% 
    mutate(add_weight_sector = if_else(sector_weight + add_weight_sector < 0, -sector_weight, add_weight_sector)) %>% 
    group_by(td) %>% 
    mutate(sector_weight = sector_weight + add_weight_sector) %>% 
    mutate(sector_weight = sector_weight / sum(sector_weight)) %>% 
    ungroup()
  
  sector_weight <- 
    sector_weight %>% 
    left_join(theme_score) %>% 
    mutate(theme_weight = sector_weight * theme_weight) %>% 
    group_by(td) %>% 
    mutate(theme_weight = theme_weight / sum(theme_weight)) %>% 
    ungroup()
  
  # theme_weight_add <- sector_score %>%
  #   left_join(theme_score) %>% 
  #   mutate(weight = sector_weight * theme_weight) %>%
  #   select(td, sector, theme, weight) %>%
  #   left_join(weight_add_tbl, by = "theme") %>%
  #   mutate(add_weight_theme = replace_na(add_weight_theme, 0)) %>%
  #   # 비중을 감소시키는데 최종 비중이 음수가 된다면, 해당 테마 비중을 0으로 처리
  #   mutate(add_weight_theme = if_else(weight + add_weight_theme < 0, -weight, add_weight_theme)) %>%
  #   group_by(td, sector) %>%
  #   mutate(add_weight_sector = sum(add_weight_theme)) %>%
  #   ungroup()
  
  
  # 사용자 정의 비중 변동 합계를 구하고 = sum(add_weight_theme)
  # 1에서 이를 뺀 나머지로 sector_weight_resid를 구함
  # sector_weight_resid 만큼에 대해 섹터비중을 안분
  # 여기에 theme_weight를 곱하고
  # add_weight_theme을 더해줌
  
  # sector_weight <- sector_score %>% 
  #   left_join(theme_weight_add) %>% 
  #   group_by(td) %>% 
  #   mutate(
  #     sector_weight_resid = 1 - sum(add_weight_theme),
  #     sector_weight2 = sector_weight * sector_weight_resid,
  #     theme_weight_mod = theme_weight * sector_weight2 + add_weight_theme
  #   ) %>% 
  #   mutate(theme_weight_mod = theme_weight_mod / sum(theme_weight_mod)) %>% 
  #   ungroup()
  
  return(sector_weight)
}

# step 4. 개별종목 스코어 ---- 
get_indv_score <- function(factor_score, num_select) {
  indv_score <- factor_score %>% 
    group_by(td, sector, theme) %>% 
    slice_max(score, n = num_select) %>% 
    mutate(indv_weight = score / sum(score)) %>% 
    ungroup() 
  
  return(indv_score)
}



