library(tidyverse)
library(patchwork)

# --- 1. 样式与配色 ---
color_palette <- c(
  "Positive" = "#ED0000FF", 
  "Not Sig" = "grey70",
  "Negative" = "#00468BFF"
)

my_theme <- theme_bw() + 
  theme(
    panel.grid.major = element_line(color = "grey90"), 
    panel.grid.minor = element_blank(),
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(face = "bold", size = 12)
  )

# --- 2. 修正后的绘图函数 ---
generate_plots <- function(long_data, p_threshold = 0.05) {
  
  # 转宽格式
  wide_data <- long_data %>%
    select(trait1, trait2, rg, rg_p, source) %>%
    pivot_wider(names_from = source, values_from = c(rg, rg_p), names_sep = "_")
  
  plot_list <- list()
  trait_list <- unique(long_data$trait1)
  
  # 【关键】定义统一的 Factor Level，确保顺序和内容一致
  all_levels <- c("Positive", "Not Sig","Negative")
  
  for (t in trait_list) {
    sub_df <- wide_data %>% filter(trait1 == t)
    
    has_meta <- "rg_Meta" %in% names(sub_df) && sum(!is.na(sub_df$rg_Meta)) > 0
    has_ukb  <- "rg_Ukb"  %in% names(sub_df) && sum(!is.na(sub_df$rg_Ukb)) > 0
    
    p <- NULL
    
    # === 模式 A: 散点图 ===
    if (has_meta && has_ukb) {
      plot_df <- sub_df %>%
        filter(!is.na(rg_Meta) & !is.na(rg_Ukb)) %>%
        mutate(
          class = case_when(
            rg_p_Meta < p_threshold & rg_p_Ukb < p_threshold & rg_Meta > 0 & rg_Ukb > 0 ~ "Positive",
            rg_p_Meta < p_threshold & rg_p_Ukb < p_threshold & rg_Meta < 0 & rg_Ukb < 0 ~ "Negative",
            TRUE ~ "Not Sig"
          ),
          # 【关键修改1】强制转换为统一的 Factor，确保 metadata 一致
          class = factor(class, levels = all_levels)
        )
      
      x_min <- min(plot_df$rg_Meta, na.rm = TRUE)
      y_max <- max(plot_df$rg_Ukb, na.rm = TRUE)
      line_height <- diff(range(plot_df$rg_Ukb, na.rm=T)) * 0.15
      if(line_height == 0) line_height <- 0.1
      
      p <- ggplot(plot_df, aes(x = rg_Meta, y = rg_Ukb, color = class)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
        geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "grey50") +
        geom_point(size = 3, alpha = 0.8) +
        geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.5) +
        
        # 【关键修改2】 drop = FALSE 强制显示所有图例项，即使数据不存在
        scale_color_manual(
          values = color_palette, 
          breaks = all_levels, # 强制规定图例顺序
          drop = FALSE         # 核心：哪怕这张图没有Positive，图例也要显示Positive
        ) +
        
        labs(title = t, x = "Meta rg", y = "Ukb rg",color = "LDSC") +
        my_theme +theme(legend.position = 'none') 
      
      # 标注保持不变
      current_y <- y_max
      for(lbl in c("Positive", "Negative")) {
        # 注意：这里需要把 factor 转回字符来统计数量
        n <- sum(as.character(plot_df$class) == lbl) 
        if(n > 0) {
          p <- p + annotate("text", x = x_min, y = current_y, label = paste0(lbl, ": ", n),
                            color = color_palette[lbl], hjust = 0, vjust = 1, fontface="bold", family="Times New Roman")
          current_y <- current_y - line_height
        }
      }
    } 
    # === 模式 B: 火山图 ===
    else {
      source_name <- ifelse(has_meta, "Meta", "Ukb")
      col_rg <- ifelse(has_meta, "rg_Meta", "rg_Ukb")
      col_p  <- ifelse(has_meta, "rg_p_Meta", "rg_p_Ukb")
      
      plot_df <- sub_df %>%
        rename(eff = !!sym(col_rg), pval = !!sym(col_p)) %>%
        filter(!is.na(eff) & !is.na(pval)) %>%
        mutate(
          logP = -log10(pval),
          class = case_when(
            pval < p_threshold & eff > 0 ~ "Positive",
            pval < p_threshold & eff < 0 ~ "Negative",
            TRUE ~ "Not Sig"
          ),
          # 【关键修改1】强制 Factor
          class = factor(class, levels = all_levels)
        )
      
      x_min <- min(plot_df$eff, na.rm = TRUE)
      y_max <- max(plot_df$logP, na.rm = TRUE)
      line_height <- diff(range(plot_df$logP, na.rm=T)) * 0.15
      if(line_height == 0) line_height <- 1
      
      p <- ggplot(plot_df, aes(x = eff, y = logP, color = class)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
        geom_hline(yintercept = -log10(p_threshold), linetype = "dashed", color = "grey60") +
        geom_point(size = 2.5, alpha = 0.8) +
        
        # 【关键修改2】 drop = FALSE
        scale_color_manual(
          values = color_palette, 
          breaks = all_levels,
          drop = FALSE 
        ) +
        
        labs(title = t, x = "rg", y = "-log10(P)",color="LDSC") +
        my_theme
      
      current_y <- y_max
      for(lbl in c("Positive", "Negative")) {
        n <- sum(as.character(plot_df$class) == lbl)
        if(n > 0) {
          p <- p + annotate("text", x = x_min, y = current_y, label = paste0(lbl, ": ", n),
                            color = color_palette[lbl], hjust = 0, vjust = 1, fontface="bold", family="Times New Roman")
          current_y <- current_y - line_height
        }
      }
    }
    
    if(!is.null(p)) plot_list[[t]] <- p
  }
  return(plot_list)
}
