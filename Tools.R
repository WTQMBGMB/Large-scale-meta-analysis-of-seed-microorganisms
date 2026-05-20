#0-1标准化
standard_01 = function(otutab){
  otutab_01 <- as.data.frame(lapply(otutab, function(x) ifelse(x > 0, 1, 0)))
  rownames(otutab_01) <- rownames(otutab)
  return(otutab_01)
}

# Check the input table and convert it to the correct format.
check_and_set_rownames <- function(df) {
  # 输入检查
  if (!is.data.frame(df) && !is.matrix(df)) {
    warning("Input is not a data frame or matrix")
    return(df)
  }
  
  # 检查是否是空数据框
  if (nrow(df) == 0 || ncol(df) == 0) {
    warning("The data frame is empty.")
    return(df)
  }
  
  # 判断是否为纯数值数据框
  is_all_numeric <- all(sapply(df, is.numeric))
  
  if (is_all_numeric) {
    message("Data is qualified.")
    return(df)
  }
  
  # 检查除了第一列，其余列是否全为数值
  if (ncol(df) > 1) {
    other_cols_numeric <- all(sapply(df[ ,-1, drop = FALSE], is.numeric))
    
    if (other_cols_numeric) {
      # 检查第一列是否有重复值
      first_col <- df[[1]]
      has_duplicates <- any(duplicated(first_col))
      if (!has_duplicates) {
        # 设置行名
        rownames(df) = NULL
        first_colname= colnames(df)[1]
        df = tibble::column_to_rownames(df,first_colname)
        message("Set the first column as the row name")
        return(df)
      } else {
        warning("It is impossible to set the first column as the row name and detect duplicate elements.")
        return(df)
      }
    }
  }
  
  # 其他情况
  warning("数据格式有误")
  return(df)
}
#---------------------------------------------------------------------------


# A function for merging data by rows and columns--------------------------------
merge_datafame = function(datafame,
                          col_group = FALSE,
                          col_method = "mean",
                          row_group = FALSE,
                          row_method = "sum"){
  library(dplyr)
  library(tidyr)
  datafame = check_and_set_rownames(datafame)
  
  if (class(col_group)=="data.frame"){
    if (ncol(col_group) == 2){
      colnames(col_group) = c("old_colname","Group")
      if (setequal(colnames(datafame),col_group$old_colname)){
        message(paste0("The list is currently being merged. The merging method is:",col_method))
        datafame = merge(col_group, as.data.frame(t(datafame)), by.x = "old_colname", by.y = "row.names")[ ,-1] %>%
          group_by(Group) %>%
          summarise(across(everything(), ~ do.call(col_method, list(.x, na.rm = TRUE)))) %>%
          tibble::column_to_rownames(var = "Group") %>%
          t() %>%
          as.data.frame()
      }else{message("The original column names in the input col_group do not match the column names in the dataframe.")}
    }else{print("The col_group must consist of 2 columns.")}
  }
  if (class(row_group)=="data.frame"){
    if (ncol(row_group) == 2){
      colnames(row_group) = c("old_rowname","Group")
      if (setequal(rownames(datafame),row_group$old_rowname)){
        message(paste0("The rows are currently being merged. The merging method is:",row_method))
        datafame = merge(row_group, datafame, by.x = "old_rowname", by.y = "row.names")[ ,-1] %>%
          group_by(Group) %>%
          summarise(across(everything(), ~ do.call(row_method, list(.x, na.rm = TRUE)))) %>%
          tibble::column_to_rownames(var = "Group")
      }else{message("The original row_group name input does not match the row name of the dataframe.")}
    }else{print("The row_group must consist of 2 columns.")}
  }
  return(datafame)
}
#-----------------------------------------------------------------------------


# This is a function for replacing the value of "na".------------------------------------------------------
replace_nas <- function(df) {
  for (col in names(df)) {
    if (is.numeric(df[[col]])) {
      df[[col]][is.na(df[[col]])] <- -1
    } else {
      df[[col]][is.na(df[[col]])] <- "Unknown"
    }
  }
  return(df)
}
#--------------------------------------------------------------------------


# This is a function for identifying generalists and specialists--------------------------------------------------
# The output consists of two parts: one is the comparison between the actual distribution and the predicted distribution, and the other is the classification result
# Utilizes multi-threading to optimize the computing speed
classify_gos <- function(spe,n_iter) {
  spe = check_and_set_rownames(spe)
  tspe=t(spe)
  nrows=ncol(tspe) ###otu数目
  ncols=nrow(tspe) ###样本数
  ###0-1矩阵的转置，计算OTU出现次数
  spe01=decostand(spe,"pa")
  spe01$sum <- rowSums(spe01) 
  ###每个样品的OTU在样品中出现的次数
  aa<-  colSums(spe01)
  aa=as.data.frame(aa)
  aa=aa[,1]
  #这个向量中储存了每个样品下出现的otu数量
  nb_of_elems=aa[1:(length(aa)-1)]
  ###模拟赋值计算
  #定义一个nrows行n_iter列的空矩阵
  table_sums <- matrix(data=c(0), nrow=nrows, ncol=n_iter) # table used to store rows sums
  to_sample=seq(1, nrows)
  
  library(parallel)
  
  # 检测可用核心数
  n_cores <- 8
  
  # 将单次迭代封装为函数
  one_iter <- function(dummy, nrows, ncols, nb_of_elems) {
    col_offsets <- (0:(ncols - 1)) * nrows
    indices <- unlist(lapply(seq_len(ncols), function(j) {
      sample.int(nrows, size = nb_of_elems[j], replace = FALSE) + col_offsets[j]
    }))
    table <- integer(nrows * ncols)  # 用向量代替矩阵，更轻量
    table[indices] <- 1L
    # 直接累加每行
    rowSums(matrix(table, nrow = nrows, ncol = ncols))
  }
  
  # 并行执行
  cl <- makeCluster(n_cores)
  results <- parLapply(cl, seq_len(n_iter), one_iter,
                       nrows = nrows, ncols = ncols, nb_of_elems = nb_of_elems)
  stopCluster(cl)
  
  # 整合结果
  table_sums <- do.call(cbind, results)
  
  ###模拟值与实际值分布直方图
  #将table_sums中的所有值作为值来绘制直方图
  pre_hist=hist(table_sums,breaks=c(0:ncols))
  real_hist=hist(spe01$sum,breaks=c(0:ncols))
  ###G&S统计计算
  #pre_hist$counts为每个频率对应的出现次数，除10000就是最终模拟预期值
  pre_num=(pre_hist$counts)/n_iter
  real_num = real_hist$counts 
  hist_df = data.frame(Frequency = seq(1, ncols),
                       real_num = real_num,
                       pre_num = pre_num)
  #judge_num为每个频率的预期值减去实际值（相当于两个直方图每个柱子相减），为负意思是这个频率下现实的出现次数大于预期值
  judge_num=pre_num-real_num
  judge_num=as.data.frame(pre_num-real_num)
  rownames(judge_num) <- rownames(tspe)
  names(judge_num) <- "diff_valu"
  judge_num$numbers <- c(1:ncols)
  #提取预期值大于显示值的频率
  cc=which(judge_num[,1]>0)
  count_min=cc[1]
  count_max=cc[length(cc)]
  spe01 <- rownames_to_column(spe01,var = "taxid")
  spe01$GOS <- ifelse(spe01$sum > count_max, 
                      "generalist",
                      ifelse(spe01$sum < count_min, 
                             "specialist", 
                             "comm"))
  result <- spe01[, c("taxid", "GOS")]
  colnames(result) <- c("taxid", "GOS")
  table(result$GOS)
  # return(result)
  return(list(result,hist_df))
}
#----------------------------------------------------------------------------


# This is a function for constructing a species classification tree file---------------------
class_tree = function(tax_table,outdir,lable){
  library(data.tree)
  library(ape)
  tax_table <- read.delim(tax_table, stringsAsFactors = FALSE, check.names = FALSE)
  colnames(tax_table)[colnames(tax_table) == lable] = "lable"
  # 构建完整路径（处理NA值为"unknown"）
  build_full_path <- function(row) {
    # 提取分类层级并替换NA
    taxa <- row[c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus","lable")]
    taxa[is.na(taxa)] <- "unknown"
    
    # 创建唯一路径字符串（添加层级前缀避免名称冲突）
    path <- paste(
      paste0("K_", taxa[1]),
      paste0("P_", taxa[2]),
      paste0("C_", taxa[3]),
      paste0("O_", taxa[4]),
      paste0("F_", taxa[5]),
      paste0("G_", taxa[6]),
      paste0("L_", taxa[7]),
      sep = "/"
    )
    return(path)
  }
  # 为每个OTU创建带分类层级的路径
  tax_table$pathString <- apply(tax_table, 1, function(row) {
    paste("root", build_full_path(row), row["lable"], sep = "/")
  })
  
  # 创建树结构
  tree <- as.Node(tax_table, pathDelimiter = "/")
  
  # 转换为phylo对象（ape兼容格式）
  phylo_tree <- as.phylo.Node(tree)
  # 输出Newick格式树文件
  write.tree(phylo_tree, file = outdir)
}
#----------------------------------------------------------------------------------------


#This is a function for drawing scatter box plots with multiple tests.----------------------------------------------------
tukey_boxplot <- function(df, group_col, value_col, fill_color,point_color, point_alpha, HSD = TRUE, xlab = "x", ylab = "y", tlab = "title",tilted = TRUE,force = TRUE, MAXLAB = FALSE,rmargin=30,show_n = TRUE,order = TRUE) {
  if (!require(ggplot2, quietly = TRUE)) stop("请先安装ggplot2包: install.packages('ggplot2')")
  if (!require(multcompView, quietly = TRUE)) stop("请先安装multcompView包: install.packages('multcompView')")
  if (!group_col %in% names(df)) stop("分组列名不存在于数据框中")
  if (!value_col %in% names(df)) stop("数值列名不存在于数据框中")
  if (!is.factor(df[[group_col]])) df[[group_col]] <- as.factor(df[[group_col]])
  group_means <- tapply(df[[value_col]], df[[group_col]], mean, na.rm = TRUE)
  if (order){
    ordered_levels <- names(rev(sort(group_means))) 
    df[[group_col]] <- factor(df[[group_col]], levels = ordered_levels)
  }
  p <- ggplot(df, aes(x = .data[[group_col]], y = .data[[value_col]])) +
    geom_boxplot(aes(fill = .data[[group_col]]), show.legend = FALSE, outlier.shape = NA) + 
    geom_jitter(width = 0.2, alpha = point_alpha, color = point_color, size = 1.5) +
    scale_fill_manual(values = rep(fill_color, nlevels(df[[group_col]]))) +
    labs(
      x = xlab,
      y = ylab,
      title = tlab
    ) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(
      plot.margin = margin(t = 10,
                           r = rmargin, 
                           b = 10,
                           l = 10,
                           unit = "pt")
    )
  if (HSD){
    formula <- as.formula(paste(value_col, "~", group_col))
    aov_model <- aov(formula, data = df)
    tukey_result <- TukeyHSD(aov_model)
    tukey_p <- tukey_result[[1]]
    p_adj <- tukey_p[, "p adj"]
    p_vec <- setNames(p_adj, rownames(tukey_p))
    letters_obj <- multcompView::multcompLetters(p_vec, threshold = 0.05)
    sig_letters <- letters_obj$Letters
    if (force) {
      sig_letters2=character(0)
      times = 1
      for (i in ordered_levels){
        sig_letters2[i] = sig_letters[times]
        times = times+1
      }
      sig_letters <- sig_letters2
    }
    

    if (MAXLAB) {
      sig_letters <- data.frame(
        group = names(sig_letters),
        letter = as.character(sig_letters),
        stringsAsFactors = FALSE
      )
      y_max <- df %>%
        group_by(.data[[group_col]]) %>%
        summarise(y = max(.data[[value_col]], na.rm = TRUE)) %>%
        ungroup()
      names(y_max)[1] = "group"
      label_df = merge(sig_letters,y_max,by="group")
    }else{
      y_max <- max(df[[value_col]], na.rm = TRUE) * 1.05
      label_df <- data.frame(
        group = names(sig_letters),
        letter = sig_letters,
        y = y_max
      )
    }

    label_df$group <- factor(label_df$group, levels = levels(df[[group_col]]))
    p <- p+
      geom_text(
        data = label_df,
        aes(x = group, y = y, label = letter),
        size = 5,
        color = "black",
        inherit.aes = FALSE
      ) 
  }else{tukey_result = "please set HSD as TRUE"}
  if (show_n){
    label_n = as.data.frame(table(df[[group_col]]))
    y_min <- min(df[[value_col]], na.rm = TRUE) * 0.95
    label_n$y = y_min
    if (length(unique(df[[group_col]]))>10){
      angle_num = 90
      }else{
        label_n$Freq = paste("n=",label_n$Freq,sep = "")
        angle_num = 0
      }
    p <- p+
      geom_text(
        data = label_n,
        aes(x = Var1, y = y, label = Freq),
        size = 5,
        color = "black",
        inherit.aes = FALSE,
        angle = angle_num
      ) 
  }
  p = p +
    theme_classic() + 
    theme(
      legend.position = "top" ,
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
      plot.title = element_text(
        size = 20,
        face = "bold",
        color = "black",
        hjust = 0.5,
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        size = 12,
        color = "black",
        hjust = 0.5,
        lineheight = 1.2,
        margin = margin(b = 15)
      ),
      axis.text.x = element_text(size = 14, color = "black"),
      axis.title.x = element_text(size = 14, color = "black", face = "bold"), 
      axis.text.y = element_text(size = 14, color = "black", angle = 0), 
      axis.title.y = element_text(size = 14, color = "black", face = "bold"),
      legend.title = element_text(size = 14, color = "black"), 
      legend.text = element_text(size = 14, color = "black"), 
      plot.margin = margin(t = 10, 
                           r = rmargin,
                           b = 10,
                           l = 10,
                           unit = "pt")
    )
  if (tilted){
    p = p + theme(axis.text.x = element_text(size = 14, color = "black", angle = -45, hjust = 0, vjust = 1))
  }
  return(list(tukey_result = tukey_result, plot = p))
}
#-------------------------------------------------------------------------------


#This is a function for drawing a pie chart-------------------------------------

draw_pie_chart <- function(dataframe, 
                           data_col,
                           label = TRUE,
                           pre = TRUE, 
                           legend_t = "legend title", 
                           limit_n = 26,
                           color_list = c("#F7E09B","#93CEDF","#9EB395", "#E9BDA0", "#D5ABBF", "#846992", "#D98267", "#73BEA0", "#E4A644", "#A23F25",
                                          "#3E4069","#517D7E","#CA820A","#C1D2E2","#808DAA","#E5A984","#552379","#B24057","#A49BC6","#D35A21",
                                          "#F1D20B","#547FB6","#2D659E", "#c89dc8","#7acdc4", "#c78980",
                                          "#6A994E","#BC6C25","#3F37C9","#4895EF","#F72585","#4CC9F0","#F8961E","#90A959","#F9844A","#9970AB",
                                          "#F9C74F","#90BE6D","#43AA8B","#577590","#F8961E","#277DA1","#F94144","#4D908E","#F9844A","#9067BE"),
                           other_name = c("unknown","Unassigned")
                           ) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    install.packages("dplyr")
  }
  
  library(ggplot2)
  library(dplyr)
  
  if (!is.numeric(limit_n) || limit_n <= 0 || limit_n != as.integer(limit_n)) {
    stop("limit_n必须是正整数")
  }
  limit_n <- as.integer(limit_n)
  freq_table <- dataframe %>%
    count(!!sym(data_col)) %>%
    mutate(percentage = n / sum(n) * 100,
           label_percent = paste0(!!sym(data_col)," (",round(percentage, 2), "%)")) %>% 
    arrange(desc(percentage))
  
  names(freq_table)[1] = "names"
  other_rows <- freq_table[freq_table$names %in% other_name, ]
  keep_rows <- freq_table[!freq_table$names %in% other_name, ]
  freq_table <- rbind(keep_rows, other_rows)
  if (nrow(freq_table) > limit_n) {
    # 提取前n个类别
    top_n <- freq_table[1:limit_n, ]
    # 计算剩余类别的总和
    other_sum <- freq_table[(limit_n + 1):nrow(freq_table), ] %>%
      summarise(
        n = sum(n),
        percentage = sum(percentage),
        label_percent = paste0("other (", round(sum(percentage), 2), "%)"),
        names = "other"
      )
    # 合并数据
    freq_table <- bind_rows(top_n, other_sum)
  }
  
  freq_table$names <- factor(freq_table$names, levels = c(as.character(freq_table$names), "other")[1:nrow(freq_table)])
  freq_table$label_percent <- factor(freq_table$label_percent, levels = c(as.character(freq_table$label_percent))[1:nrow(freq_table)])
  p <- ggplot(freq_table, aes(x = "", y = n, fill = names)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    labs(fill = legend_t) +
    theme(legend.position = "none")
  
  if (label) {
    if (pre) {
      p <- p + 
        geom_text(aes(label = label_percent), 
                  position = position_stack(vjust = 0.5), size = 5, colour = "black")
    } else {
      p <- p + 
        geom_text(aes(label = names), 
                  position = position_stack(vjust = 0.5), size = 5, colour = "black")
    }
  } else {
    if (pre) {
      p <- ggplot(freq_table, aes(x = "", y = n, fill = label_percent)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        theme_void() +
        labs(fill = legend_t) +
        theme(legend.position = "right")
    } else {
      p <- ggplot(freq_table, aes(x = "", y = n, fill = names)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        theme_void() +
        labs(fill = legend_t) +
        theme(legend.position = "right")
    }
  }
  
  # 颜色列表（扩展颜色数量，避免颜色不足）
  color_list = color_list
  
  # 确保颜色数量足够（循环使用颜色列表）
  color_values <- rep(color_list, length.out = nrow(freq_table))
  if (nrow(freq_table)>=limit_n){
    color_values[nrow(freq_table)]="#eeeeee"
  }
  p <- p + scale_fill_manual(values = color_values)
  
  return(p)
}

#This is a function for extracting sequences.
get_target_seq <- function(target_indices, source_fa, output_file){
  # 筛选序列
  silva_names <- names(source_fa)
  target_indices = intersect(target_indices, silva_names)
  target_sequences <- source_fa[target_indices]
  # 筛选出的序列写入新的FASTA文件
  message("The selected sequences are currently being written to the file：", output_file)
  writeXStringSet(target_sequences, file = output_file, format = "fasta")
}

#这是一个绘制花瓣图的代码
flower_plot <- function(sample, num, core, start = 90, a = 0.5, b = 2, 
                        r = 1, ellipse_col, circle_col = "white", alpha,
                        nameSize = 0.9, numSize = 0.8, coreSize = 1.2) {
  ellipse_col = adjustcolor(ellipse_col, alpha.f = alpha)
  
  # 设置图形参数：无边框/坐标轴/边距，强制正方形输出
  par(bty = 'n', ann = F, xaxt = 'n', yaxt = 'n', mar = c(1,1,1,1), pty = "s")
  
  # 创建标准化坐标系统(0-10范围保证绘图比例)
  plot(c(0,10), c(0,10), type = 'n', asp = 1)  # asp=1强制坐标轴等比例
  
  n <- length(sample)
  deg <- 360 / n  # 计算每个样本的间隔角度
  
  # 循环绘制每个样本的椭圆和标签
  lapply(seq_len(n), function(t) {
    # 计算当前椭圆中心坐标(极坐标转换)
    angle <- (start + deg * (t - 1)) * pi / 180
    x_center <- 5 + cos(angle)
    y_center <- 5 + sin(angle)
    
    # 绘制椭圆花瓣
    plotrix::draw.ellipse( 
      x = x_center,
      y = y_center,
      col = ellipse_col[t],
      border = ellipse_col[t],
      a = a, b = b, 
      angle = deg * (t - 1)  # 椭圆旋转角度
    )
    
    # 添加数量标签(内层文字) - 独有微生物数量
    text(
      x = x_center + (b-0.5) * cos(angle),  # 0.5为偏移系数
      y = y_center + (b-0.5) * sin(angle),
      labels = num[t],
      cex = numSize
    )
    
    # 添加样本名称标签(外层文字)
    text(
      x = 5 + (b+1.3)  * cos(angle),
      y = 5 + (b+1.3)  * sin(angle),
      labels = sample[t],
      srt = ifelse(deg*(t-1) < 180 && deg*(t-1) > 0, 
                   deg*(t-1) - start, 
                   deg*(t-1) + start),  # 自动调整文字旋转角度
      adj = ifelse(deg*(t-1) < 180 && deg*(t-1) > 0, 1, 0),  # 自动对齐方式
      cex = nameSize
    )
  })
  
  # 绘制中心圆 - 共存微生物数量
  plotrix::draw.circle(5,  5, r, col = circle_col, border = NA)
  text(5, 5, paste('Core:', core), cex = coreSize, font = 2)
}
#-------------------------------------------------------------------------------

# This is a function for drawing legends----------------------------------------
# The main reason is to match the legend order for the evolutionary tree
create_color_legend <- function(data, save_path = "legend.pdf", shape = "circle", 
                                layout = "col", n = 2, title = "title", 
                                label_size = 4, title_size = 6, ifsave = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("请先安装ggplot2包: install.packages('ggplot2')")
  }
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("请先安装grid包: install.packages('grid')")
  }
  
  library(ggplot2)
  library(grid)
  
  # 数据预处理与检查
  unique_data <- unique(data)
  if (any(duplicated(unique_data[,1]))) {
    warning("去重后标签列仍有重复元素，请检查数据")
  }
  if (any(duplicated(unique_data[,2]))) {
    warning("去重后颜色列仍有重复元素，请检查数据")
  }
  colnames(unique_data) <- c("category", "color")
  unique_data$color <- as.character(unique_data$color)
  color_check <- grepl("^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", unique_data$color)
  if (!all(color_check)) {
    stop("颜色列包含非16进制格式的颜色值")
  }
  title_size = title_size*4
  n_items <- nrow(unique_data)
  
  # 关键增强：计算最长标签字符数，并动态调整色块间距
  max_label_length <- max(nchar(as.character(unique_data$category)))
  # 根据规则计算间距调整量：每10个字符增加1单位间距
  width_adjust_factor <- floor(max_label_length / 5) 
  # 基础间距 + 动态调整量
  base_width_per_col <- 1.5
  adjusted_width_per_col <- base_width_per_col + width_adjust_factor
  
  # 根据布局模式计算参数
  if (layout == "col") {
    ncol <- n
    nrow <- ceiling(n_items / ncol)
    # 使用动态调整后的间距计算图形宽度
    width <- ncol * (2+width_adjust_factor*0.5)
    height <- nrow * 1 + 1.0
    legend_direction <- "vertical"
  } else if (layout == "row") {
    nrow <- n
    ncol <- ceiling(n_items / nrow)
    width <- ncol * (1.5+width_adjust_factor*0.5)
    height <- nrow * 1.5 + 1.0
    legend_direction <- "horizontal"
  } else {
    stop("layout参数必须是'col'或'row'")
  }
  
  # 设置图形符号
  shapes <- list(
    circle = 21,
    square = 22,
    diamond = 23,
    triangle = 24
  )
  
  if (!shape %in% names(shapes)) {
    stop("不支持的形状，请选择: ", paste(names(shapes), collapse = ", "))
  }
  shape_code <- shapes[[shape]]
  
  # 创建图例数据框
  legend_df <- data.frame(
    category = unique_data$category,
    color = unique_data$color,
    x = rep(1:ncol, length.out = n_items),
    y = rep(nrow:1, each = ncol)[1:n_items]
  )
  
  # 关键增强：在col模式下，根据调整后的间距重新计算x轴位置
  if (layout == "col") {
    legend_df <- data.frame(
      category = unique_data$category,
      color = unique_data$color,
      x = rep(1:ncol, each = nrow)[1:n_items],
      y = rep(nrow:1, length.out = n_items)
    )
    # 计算每个色块的新x坐标，考虑动态间距
    x_positions <- numeric(ncol)
    for (i in 1:ncol) {
      x_positions[i] <- (i - 1) * adjusted_width_per_col + 1
    }
    x_value = c()
    timess = 0
    for (cols in c(1:ncol)){
      timess = timess+nrow
      if (timess<n_items){
        x_value = c(x_value,rep(x_positions[[cols]],nrow))
      }else{
        rest = n_items + nrow - timess
        x_value = c(x_value,rep(x_positions[[cols]],rest))
      }
    }
    legend_df$x <- x_value
  }
  
  # 创建图形
  p <- ggplot(legend_df, aes(x = x, y = y, fill = category)) +
    geom_point(shape = shape_code, size = 8, color = "black", aes(fill = category)) +
    scale_fill_manual(values = setNames(unique_data$color, unique_data$category)) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = title_size, hjust = 0.5, face = "bold")
    ) +
    ggtitle(title) +
    coord_fixed(ratio = 1)
  
  # 添加标签
  if (layout == "col") {
    p <- p + geom_text(aes(label = category, x = x+0.3*ncol, y = y), 
                       size = label_size, hjust = 0, vjust = 0.5)+
      theme(plot.margin = margin(t = 10,  # 顶部边距
                                 r = 10+adjusted_width_per_col*5,  # 右侧边距
                                 b = 10,  # 底部边距
                                 l = 10,  # 左侧边距
                                 unit = "pt")  # 单位，如"pt"（磅）、"cm"（厘米）
      )+coord_cartesian(xlim =c(0.7, max(legend_df$x)+adjusted_width_per_col*0.8))
  } else if (layout == "row") {
    p <- p + geom_text(aes(label = category, x = x, y = y - 0.3), 
                       size = label_size, hjust = 0.5, vjust = 1)+
      coord_cartesian(xlim =c(0.5, max(legend_df$x)+adjusted_width_per_col*0.1),ylim =c(0.5, max(legend_df$y)))+
      theme(plot.margin = margin(t = 10,  # 顶部边距
                                 r = 10+adjusted_width_per_col*7,  # 右侧边距
                                 b = 10+adjusted_width_per_col*2,  # 底部边距
                                 l = 10+adjusted_width_per_col*7,  # 左侧边距
                                 unit = "pt"))
  }
  p
  # 保存图形
  width = width/1.4
  height = height/2
  if (ifsave){
    ggsave(save_path, p, width = width, height = height, units = "in", dpi = 300)
    message("图例已保存至: ", normalizePath(save_path))
    message("图例尺寸: ", round(width, 2), " x ", round(height, 2), " 英寸")
  }
  message("包含 ", n_items, " 个图例项")
  message("最长标签字符数: ", max_label_length, "，间距调整量: +", width_adjust_factor)
  
  return(p)
}
#--------------------------------------------------------------------------------

#This is a function for constructing a species classification tree file---------
class_tree = function(tax_table,outdir,lable){
  library(data.tree)
  library(ape)
  colnames(tax_table)[colnames(tax_table) == lable] = "lable"
  # 构建完整路径（处理NA值为"unknown"）
  build_full_path <- function(row) {
    # 提取分类层级并替换NA
    taxa <- row[c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus","lable")]
    taxa[is.na(taxa)] <- "unknown"
    
    # 创建唯一路径字符串（添加层级前缀避免名称冲突）
    path <- paste(
      paste0("K_", taxa[1]),
      paste0("P_", taxa[2]),
      paste0("C_", taxa[3]),
      paste0("O_", taxa[4]),
      paste0("F_", taxa[5]),
      paste0("G_", taxa[6]),
      paste0("L_", taxa[7]),
      sep = "/"
    )
    return(path)
  }
  # 为每个OTU创建带分类层级的路径
  tax_table$pathString <- apply(tax_table, 1, function(row) {
    paste("root", build_full_path(row), row["lable"], sep = "/")
  })
  
  # 创建树结构
  tree <- as.Node(tax_table, pathDelimiter = "/")
  
  # 转换为phylo对象（ape兼容格式）
  phylo_tree <- as.phylo.Node(tree)
  # 输出Newick格式树文件
  write.tree(phylo_tree, file = outdir)
}
#-------------------------------------------------------------------------------



#This is a function for drawing a stacked bar chart-----------------------------
make_mutibar = function(datain,
                        out_dir = "output.pdf",
                        x_name = "x",
                        y_name = "y",
                        legend_name = "legend",
                        limit_num = 10,
                        width = 15,
                        height = 7,
                        other_list = c("Unassigned","Incertae Sedis"),
                        colorlist = c("#FCB366", "#FE816F", "#90D7C7","#93BCD0", "#AF3F4F", 
                                      "#F3BB34", "#F8AD9A", "#5163B5", "#613767", "#56B394",
                                      "#DC6D73", "#B2B896", "#E97DAE","#FB7A64","#EDCE35", 
                                      "#E5C28C", "#CA8BB6", "#1A6C92", "#EAB46E", "#8CC3BD", 
                                      "#4E56AB","#0FBFBC", "#E9A564", "#ADD565", "#ADADAD", 
                                      "#8F9EC5", "#6C77B5", "#138A8E", "#4B9D77", "#8E60A8")){
  colnames(datain) = c("groups","levels")
  groups_levels = datain %>%
    count(groups,levels,name = "levels_count") %>%
    arrange(groups,levels_count)%>%
    group_by(groups) %>%
    mutate(rank = row_number(desc(levels_count)))%>%
    ungroup()
  # groups_levels = groups_levels[groups_levels$groups != 0, ]
  names(groups_levels)[1] = "category"
  #若rank大于5则使其为other
  levels_counts <- groups_levels %>%
    mutate(levels = ifelse(rank > limit_num, "other", levels))
  levels_counts$levels[levels_counts$levels %in% other_list] <- "other"
  #去除rank列
  levels_counts <-levels_counts[,-4]
  # 对levels_counts合并同类项
  levels_counts <- aggregate(levels_count ~ category + levels, data = levels_counts, FUN = sum)
  #构建宽数据
  wide_data <- spread(levels_counts, key = "category",
                      value = "levels_count")
  #消除缺失值
  wide_data[is.na(wide_data)] <- 0
  aa = wide_data# head(aa)n = ncol(aa)#增加一行，为整列的均值，计算每一列的均值，2就是表示列
  n = ncol(aa)
  #末尾加一列求和
  aa[n+1]=apply(aa[,c(2:ncol(aa))],1,sum)
  colnames(aa)[ncol(aa)] <- "jiangxu"#aa按照最后列求和
  
  bb<- arrange(aa, jiangxu)
  head(bb)
  #只要bb的第一和最后列
  bb = bb[c(1,ncol(bb))]
  colnames(bb)[ncol(bb)] <- "jiangxu"
  #因子排序变量查看
  bb$levels = as.character(bb$levels)
  bb$levels = as.factor(bb$levels)
  other_rows <- bb[bb$levels == "other", ]
  non_other_rows <- bb[bb$levels != "other", ]
  bb <- rbind(other_rows, non_other_rows)
  bb$levels
  levels_counts$levels = factor(levels_counts$levels,order = T,levels = bb$levels)
  #对levels_counts2降序排列
  levels_counts2 = plyr::arrange(levels_counts,desc(levels))
  head(levels_counts2)
  levels_counts2 <- levels_counts2%>%
    group_by(category) %>%
    mutate(CC = (levels_count / sum(levels_count))*100 ) %>%
    ungroup()
  desort_colname = sort(unique(c(levels_counts2$category)))
  levels_counts2$category <- factor(levels_counts2$category, levels = desort_colname)
  levels_num = length(unique(levels_counts2$levels))
  
  colorlist_rev=rev(colorlist[1:levels_num])
  if ("other" %in% levels_counts2$levels){
    colorlist_rev[1] = "#eeeeee"
  }
  p = ggplot(data = levels_counts2,aes(x=category,y=CC,fill=levels, order = levels))+
    geom_bar(stat = "identity",
             position = "stack")+ 
    labs(x = x_name, y = y_name, fill = legend_name)+
    theme_bw()+
    scale_fill_manual(values=colorlist_rev)+
    scale_y_continuous(
      labels = function(b) paste0(b, "%")  # 在数字后添加百分号
    )+
    theme_classic() + 
    theme(
      # legend.position = "top" ,
      panel.background = element_blank(), # 确保面板背景透明（可选，theme_bw 已是白色）
      panel.border = element_rect(colour = "black", fill = NA, size = 1.2), # 加粗面板边框
      plot.title = element_text(
        size = 20,
        face = "bold",
        color = "black",
        hjust = 0.5,  # 主标题居中
        margin = margin(b = 10)  # 主标题下方留白
      ),
      plot.subtitle = element_text(
        size = 12,
        color = "black",
        hjust = 0.5,  # 副标题居中
        lineheight = 1.2,  # 设置行高，使多行文本更易读
        margin = margin(b = 15)  # 副标题下方留白
      ),
      # 1. 自定义X轴刻度文字（axis.text.x）
      # axis.text.x = element_text(size = 11, color = "black", angle = -45, hjust = 0, vjust = 1), # 大小14px，深蓝色
      axis.text.x = element_text(size = 14, color = "black"),
      # 2. 自定义X轴标题（axis.title.x）
      axis.title.x = element_text(size = 14, color = "black", face = "bold"), # 大小16px，红色，加粗
      # 3. 自定义Y轴刻度文字（axis.text.y）
      axis.text.y = element_text(size = 14, color = "black", angle = 0), # 大小12px，棕色，不旋转
      # 4. 自定义Y轴标题（axis.title.y）
      axis.title.y = element_text(size = 14, color = "black", face = "bold"), # 大小16px，橙色，旋转90度
      # 5. 自定义图例标题（legend.title）- 同时影响color和size两个图例的标题
      legend.title = element_text(size = 14, color = "black"), # 大小12px，紫色
      # 6. 自定义图例文字（legend.text）- 同时影响color和size两个图例的标签
      legend.text = element_text(size = 12, color = "black"), # 大小10px，深绿色
      plot.margin = margin(t = 10,  # 顶部边距
                           r = 10,  # 右侧边距
                           b = 10,  # 底部边距
                           l = 10,  # 左侧边距
                           unit = "pt")  # 单位，如"pt"（磅）、"cm"（厘米）
    )
  ggsave(filename = out_dir, plot = p, width = width,height = height,units = "in")
  return(p)
}
#-------------------------------------------------------------------------------


# A small function used to add significance indicators----------------------
ifsig = function(p){
  p = as.numeric(p)
  if (p<0.001){
    sig = "***"
  } else if(p<0.01){
    sig = "**"
  } else if(p<0.05){
    sig = "*"
  } else{
    sig = "N.S."
  }
  return(sig)
}
#-----------------------------------------------------------------------


# A small function used for calculating correlation----------------------
cul_cor_df = function(indata,iv){
  dv = colnames(indata)
  numeric_cols <- names(indata)[sapply(indata, is.numeric)]
  dv = intersect(dv,numeric_cols)
  dv = dv[dv!=iv]
  result_df = data.frame(row.names = dv,
                         spearman_coe = rep(NA,length(dv)),
                         spearman_p = rep(NA,length(dv)),
                         pearson_coe = rep(NA,length(dv)),
                         pearson_p = rep(NA,length(dv)))
  i = 1
  for (dvi in dv){
    spearman_result = cor.test(indata[[iv]], indata[[dvi]], 
                               method = "spearman", 
                               exact = FALSE)
    spearman_coe = spearman_result$estimate
    spearman_P = spearman_result$p.value
    pearson_result = cor.test(indata[[iv]], indata[[dvi]], 
                              method = "pearson", 
                              exact = FALSE)
    pearson_coe = pearson_result$estimate
    pearson_P = pearson_result$p.value
    merge_result = c(spearman_coe,spearman_P,pearson_coe,pearson_P)
    result_df[i,] = merge_result
    i = i+1
  }
  result_df$direction = ifelse(result_df$spearman_coe>0,"pos","neg")
  return(result_df)
}
#-------------------------------------------------------------------

#This is a function for calculating correlation---------------------------------
#iv must be a numerical variable, while dv is a categorical variable."
Calculate_correlation = function(indata,iv,dv){
  colnames(indata)[colnames(indata)==iv] = "independent_variable"
  colnames(indata)[colnames(indata)==dv] = "dependent_variable"
  result_prop <- indata %>%
    group_by(independent_variable, dependent_variable) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(independent_variable) %>%
    mutate(proportion = count / sum(count)) %>%
    select(-count) %>%
    pivot_wider(names_from = dependent_variable, values_from = proportion, values_fill = 0)
  result_prop[] <- lapply(result_prop, as.numeric)
  cor_results <- data.frame(
    Variable = character(),      # 因变量名称
    Direction = character(),     # 正负性
    Pearson_p = numeric(),       # Pearson相关性p值
    Spearman_p = numeric()       # Spearman相关性p值
  )
  # 提取自变量（第一列）
  independent_var <- result_prop[[1]]
  independent_name <- names(result_prop)[1]
  
  # 循环计算每个因变量与自变量的相关性和显著性
  for (i in 2:ncol(result_prop)) {
    dependent_var <- result_prop[[i]]
    var_name <- names(result_prop)[i]
    
    # 1. 计算Pearson相关性和p值
    pearson_test <- cor.test(independent_var, dependent_var, 
                             method = "pearson", 
                             exact = FALSE)  # exact=FALSE避免样本量大时的问题
    
    # 2. 计算Spearman相关性和p值
    spearman_test <- cor.test(independent_var, dependent_var, 
                              method = "spearman", 
                              exact = FALSE)  # exact=FALSE避免样本量大时的问题
    
    # 3. 确定正负性（基于Pearson相关系数）
    direction <- ifelse(pearson_test$estimate > 0, "positive", "negative")
    
    # 4. 将结果添加到数据框
    cor_results <- rbind(cor_results, data.frame(
      Variable = var_name,
      Direction = direction,
      Pearson_p = pearson_test$p.value,
      Spearman_p = spearman_test$p.value
    ))
  }
  return(cor_results)
}
#-------------------------------------------------------------------------------


# Used to mark duplicate elements---------------------------------------------
# When duplicate elements occur, they will be numbered from 1 to n
make_unique_sequential <- function(x) {
  if (!is.character(x)) {
    stop("输入必须是字符向量")
  }
  
  # 创建一个计数器来跟踪每个元素的出现次数
  count_table <- list()
  result <- character(length(x))
  
  for (i in seq_along(x)) {
    element <- x[i]
    
    # 如果是第一次遇到这个元素
    if (is.null(count_table[[element]])) {
      count_table[[element]] <- 0
      result[i] <- element
    } else {
      # 增加计数并添加后缀
      count_table[[element]] <- count_table[[element]] + 1
      result[i] <- paste0(element, "_", count_table[[element]])
    }
  }
  
  return(result)
}
#---------------------------------------------------------------------------------