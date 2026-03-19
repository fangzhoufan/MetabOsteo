library(dplyr)
library(ggplot2)
library(patchwork)
library(stringr)

#' @title 药物靶点模式图 (仿 LDSC 风格：散点/火山自动切换)
#' @description 
#' 针对每个靶点：
#' 1. 如果 Meta 和 UKB 都有数据 -> 画散点图 (Beta vs Beta)
#' 2. 如果只有单侧数据 -> 画火山图 (Beta vs -log10 P)
#' 3. 颜色仅基于 P < 0.05 的方向一致性
mytheme <- theme_bw() + ggplot_themes(text_size = 12)+
  theme(
    panel.grid.major = element_line(color = "grey90"), 
    text = element_text(family = "Times New Roman"),
    legend.position = 'right',
    plot.title = element_text(face = "bold", size = 12)
  )
plot_drug_patterns_strict <- function(df, target_list) {
  
  # --- 0. 基础设置 ---
  # 定义统一的颜色和因子水平
  all_levels <- c("Positive",  "Not Sig","Negative")
  color_palette <- c(
    "Positive" = "#ED0000FF", # 红
    "Negative" = "#00468BFF", # 蓝
    "Not Sig"  = "grey80"     # 灰
  )
  p_threshold <- 0.05 # 您设定的阈值
  
  # 定义一个简洁的主题
  my_theme <- theme_bw() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 7),
      legend.position = "none" # 隐藏图例，靠颜色直观区分
    )
  
  # 存储所有子图的列表
  plot_list <- list()
  
  # --- 1. 循环每个靶点绘图 ---
  for (t in target_list) {
    
    # 1.1 筛选当前靶点数据 (模糊匹配，处理空格和后缀)
    # 先把数据里的 exposure 清洗一下以便匹配
    sub_df <- df %>%
      mutate(exposure_clean = exposure) %>%
      filter(exposure_clean == t)
    
    # 如果没数据，跳过
    if (nrow(sub_df) == 0) next
    
    # 1.2 判断数据完整性
    # 检查 b_Meta / b_Ukb 列是否存在且非全 NA
    has_meta <- !all(is.na(sub_df$b_Meta))
    has_ukb  <- !all(is.na(sub_df$b_Ukb))
    
    # ---------------------------------------------------------
    # === 模式 A: 散点图 (Meta vs UKB) ===
    # ---------------------------------------------------------
    if (has_meta && has_ukb) {
      
      plot_df <- sub_df %>%
        filter(!is.na(b_Meta) & !is.na(b_Ukb)) %>%
        mutate(
          # 【逻辑复刻】双重显著 + 方向一致
          MR = case_when(
            pval_Meta < p_threshold & pval_Ukb < p_threshold & b_Meta > 0 & b_Ukb > 0 ~ "Positive",
            pval_Meta < p_threshold & pval_Ukb < p_threshold & b_Meta < 0 & b_Ukb < 0 ~ "Negative",
            TRUE ~ "Not Sig"
          ),
          # 强制因子顺序
          MR = factor(MR, levels = all_levels)
        )
      
      # 计算标注位置 (左上角)
      x_min <- min(plot_df$b_Meta, na.rm = TRUE)
      y_max <- max(plot_df$b_Ukb, na.rm = TRUE)
      # 动态调整行高
      y_range <- diff(range(plot_df$b_Ukb, na.rm=T))
      line_height <- ifelse(y_range == 0, 0.1, y_range * 0.15)
      
      p <- ggplot(plot_df, aes(x = b_Meta, y = b_Ukb, color = MR)) +
        # 辅助线
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
        geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "grey50") +
        
        # 散点与拟合
        geom_point(size = 2, alpha = 0.8) +
        geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.5) +
        
        # 颜色映射 (drop = FALSE)
        scale_color_manual(values = color_palette, breaks = all_levels, drop = FALSE) +
        
        labs(title = t, x = "Meta Beta", y = "UKB Beta",family="Times New Roman") + mytheme + theme(legend.position='none')
      
      # 手动标注计数
      current_y <- y_max
      for(lbl in c("Positive", "Negative")) {
        n <- sum(as.character(plot_df$MR) == lbl) 
        if(n > 0) {
          p <- p + annotate("text", x = x_min, y = current_y, label = paste0(lbl, ": ", n),
                            color = color_palette[lbl], hjust = 0, vjust = 1, fontface="bold", size = 4,family="Times New Roman")
          current_y <- current_y - line_height
        }
      }
      
      plot_list[[t]] <- p
      
    } 
    # ---------------------------------------------------------
    # === 模式 B: 火山图 (单数据源) ===
    # ---------------------------------------------------------
    else {
      # 确定用哪一列
      prefix <- ifelse(has_meta, "Meta", "Ukb")
      col_b   <- paste0("b_", prefix)
      col_p   <- paste0("pval_", prefix)
      
      plot_df <- sub_df %>%
        rename(eff = !!sym(col_b), pval_raw = !!sym(col_p)) %>%
        filter(!is.na(eff) & !is.na(pval_raw)) %>%
        mutate(
          logP = -log10(pval_raw),
          # 【逻辑复刻】单侧显著 + 方向
          MR = case_when(
            pval_raw < p_threshold & eff > 0 ~ "Positive",
            pval_raw < p_threshold & eff < 0 ~ "Negative",
            TRUE ~ "Not Sig"
          ),
          MR = factor(MR, levels = all_levels)
        )
      
      # 计算标注位置
      x_min <- min(plot_df$eff, na.rm = TRUE)
      y_max <- max(plot_df$logP, na.rm = TRUE)
      y_range <- diff(range(plot_df$logP, na.rm=T))
      line_height <- ifelse(y_range == 0, 1, y_range * 0.15)
      
      p <- ggplot(plot_df, aes(x = eff, y = logP, color = MR)) +
        # 辅助线
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
        geom_hline(yintercept = -log10(p_threshold), linetype = "dashed", color = "grey60") +
        
        # 散点
        geom_point(size = 2, alpha = 0.8) +
        
        scale_color_manual(values = color_palette, breaks = all_levels, drop = FALSE) +
        
        labs(title = t, x = "Beta", y = "-log10(P)",family="Times New Roman") + mytheme
      
      # 手动标注计数
      current_y <- y_max
      for(lbl in c("Positive", "Negative")) {
        n <- sum(as.character(plot_df$MR) == lbl)
        if(n > 0) {
          p <- p + annotate("text", x = x_min, y = current_y, label = paste0(lbl, ": ", n),
                            color = color_palette[lbl], hjust = 0, vjust = 1, fontface="bold", size = 4,family="Times New Roman")
          current_y <- current_y - line_height
        }
      }
      
      plot_list[[t]] <- p
    }
  }
  
  return(plot_list)
}