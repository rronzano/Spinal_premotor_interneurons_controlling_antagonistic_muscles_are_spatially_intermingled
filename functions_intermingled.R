## Functions to read source data ##

Data_building <- function(data_all, titer_n, method_n) {
  
  
  files <- list.files(pattern="xlsx", no.. = FALSE, full.names = FALSE, all.files = FALSE, ) 
  files_wo_format <- tools::file_path_sans_ext(files)
  files <- str_remove_all(files, "[~$]")
  files <- files[!duplicated(files)]
  files_wo_format <- str_remove_all(files_wo_format , "[~$]")
  files_wo_format <- files_wo_format[!duplicated(files_wo_format )]
  
  for (i in 1:length(files)) {
    a <- as.character(files[i])
    b1 <- read.xlsx(xlsxFile = a, sheet = 1, skipEmptyRows = TRUE, colNames=FALSE) #sheet one is the coordinate of INs
    c <- as.character(files_wo_format[i])
    transmitter <- "NA"
    if (grepl("_n_rmb", c)) { #The lab of origin is coded in the name of the files. Grepl recognize the presence of the string in
      # name file and add the lab in lab_n variable
      lab_n <- "UCL"
    }
    if (grepl("_n_djm", c)) {
      lab_n <- "UoG"
    } 
    if (grepl("_n_nz", c)) {
      lab_n <- "MDC"
    }
    if (grepl("_n_slp", c)) {
      lab_n <- "Salk"
    }
    if (grepl("exc", c)){
      transmitter <- "GlyT2-off"
      lab_n <- "UCL"
    }
    if (grepl("inh", c)){
      transmitter <- "GlyT2-on"
      lab_n <- "UCL"
    }
    t <- rep(transmitter, times=nrow(b1))
    sampl <- rep(c,times=nrow(b1))
    sampl <- str_remove_all(sampl, "gs_") #remove the name of the muscle from the samples' name. 
    sampl <- str_remove_all(sampl, "ta_")
    sampl <- str_remove_all(sampl, "mg_")
    sampl <- str_remove_all(sampl, "pl_")
    sampl <- str_remove_all(sampl, "lg_")
    sampl <- str_remove_all(sampl, "exc_") 
    sampl <- str_remove_all(sampl, "inh_")
    muscle_inj <- substr(c, start = 1, stop = 2) #the muscle injected is the two first letters of the file name
    muscle <- rep(muscle_inj, times=nrow(b1))
    b1 <- cbind(b1, sampl, muscle, t) #add to the data frame of the sample a column with the injected muscle, the sample name and the neurotransmitter if known. 
    colnames(b1) <-  c("x","y","z", "animal", "muscle", "GlyT2")
    b1$x <- b1$x*1700 #data are between -0.5 and 0.5, there are normalized in the range used. 
    b1$y <- b1$y*900
    lab = rep(lab_n,times=nrow(b1))  #add to the data frame a column with the lab of origin
    method = rep(method_n,times=nrow(b1)) #add to the data frame a column with the method
    titer = rep(titer_n,times=nrow(b1)) #add to the data frame a column with the lab of origin
    identity = rep("IN",times=nrow(b1)) #add to the identity interneuron or motoneuron. Here I took the dataframe with INs' position. 
    b1 <- cbind(b1, lab, titer, method)
    b1 <- cbind(b1, identity)
    data_all <- rbind(data_all, b1) #data frame with the coordinates of all labelled neurons for all animals all muscles 
    
    b2 <- read.xlsx(xlsxFile = a, sheet = 2, skipEmptyRows = TRUE, colNames=FALSE) #sheet with the coordinates of MNs
    if (is.null(b2)) { 
    }
    else {
      t <- rep("NA",times=nrow(b2))
      sampl <- rep(c,times=nrow(b2))
      sampl <- str_remove_all(sampl, "gs_")
      sampl <- str_remove_all(sampl, "ta_")
      sampl <- str_remove_all(sampl, "mg_")
      sampl <- str_remove_all(sampl, "pl_")
      sampl <- str_remove_all(sampl, "lg_")
      muscle <- rep(muscle_inj,times=nrow(b2))
      b2 <- cbind(b2, sampl, muscle, t)
      colnames(b2) <-  c("x","y","z", "animal", "muscle", "GlyT2")
      b2$x <- b2$x*1700
      b2$y <- b2$y*900
      
      lab = rep(lab_n ,times=nrow(b2))
      method = rep(method_n ,times=nrow(b2))
      titer = rep(titer_n ,times=nrow(b2))
      identity = rep("MN",times=nrow(b2))
      b2 <- cbind(b2, lab, titer, method)
      b2 <- cbind(b2, identity)
      data_all <- rbind(data_all, b2) #data frame with the coordinates of all labelled neurons for all animals all muscles 
    }
    
  }
  return(data_all)
}

## Functions to plot xy ##

xy_dot_density_center <- function(spinal, data, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, Size) {
  
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  
  M1 <-  data %>% filter(animal == Animal[1] & muscle == m1 & identity == IN | animal == Animal[1] & muscle == m1 & identity == MN) 
  M2 <-  data %>% filter(animal == Animal[1] & muscle == m2 & identity == IN | animal == Animal[1] & muscle == m2 & identity == MN)
  if (nrow(M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  p <- ggplot() +
    annotate("point", x = 0, y = 0, size = 4, colour = "black") +
    geom_path(data = spinal, aes(x = V1, y = V2), size = 1.5) + 
    geom_point(data = M1, aes(x = x, y = y, shape = identity, size = identity, alpha = identity), colour = ext_colors[1], show.legend = FALSE) +
    geom_point(data = M2, aes(x = x, y = y, shape = identity, size = identity, alpha = identity), colour = flex_colors[1], show.legend = FALSE) 
  
  if (2 <= length(Animal)) { 
    for (i in 2:length(Animal)) {
      M1 <-  data %>% filter(animal == Animal[i] & muscle == m1 & identity == IN | animal == Animal[i] & muscle == m1 & identity == MN) 
      M2 <-  data %>% filter(animal == Animal[i] & muscle == m2 & identity == IN | animal == Animal[i] & muscle == m2 & identity == MN)
      if (nrow(M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        p <- p +
          geom_point(data = M1, aes(x = x, y = y, shape = identity, size = identity, alpha = identity), colour = ext_colors[as.numeric(indice_col_ext)], show.legend = FALSE) 
      } 
      if (nrow(M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        p <- p +
          geom_point(data = M2, aes(x = x, y = y, shape = identity, size = identity, alpha = identity), colour = flex_colors[as.numeric(indice_col_flex)], show.legend = FALSE) 
      }
      
    }
  }
  
  p <- p +
    scale_size_manual(values = Size) +
    scale_alpha_manual(values = c(0.6, 1)) +
    scale_y_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    ylab("") +  
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"))
  
  p
}
xy_dot_density_center_concatenate <- function(spinal, data, Animal, m1, m2, color1, color2, IN, MN, Size) {
  
  
  M1 <-  data %>% filter(animal == Animal[1] & muscle == m1 & identity == IN | animal == Animal[1] & muscle == m1 & identity == MN) 
  M2 <-  data %>% filter(animal == Animal[1] & muscle == m2 & identity == IN | animal == Animal[1] & muscle == m2 & identity == MN)
  
  p <- ggplot() +
    annotate("point", x = 0, y = 0, size = 4, colour = "black") +
    geom_path(data = spinal, aes(x = V1, y = V2), size = 1.5) + 
    geom_point(data = M1, aes(x = x, y = y, shape = identity, size = identity, alpha = identity), colour = color1, show.legend = FALSE) +
    geom_point(data = M2, aes(x = x, y = y, shape = identity, size = identity, alpha = identity), colour = color2, show.legend = FALSE) 
  
  if (2 <= length(Animal)) { 
    for (i in 2:length(Animal)) {
      M1 <-  data %>% filter(animal == Animal[i] & muscle == m1 & identity == IN | animal == Animal[i] & muscle == m1 & identity == MN) 
      M2 <-  data %>% filter(animal == Animal[i] & muscle == m2 & identity == IN | animal == Animal[i] & muscle == m2 & identity == MN)
      if (nrow(M1) >= 1) {
        p <- p +
          geom_point(data = M1, aes(x = x, y = y, shape = identity, size = identity, alpha = identity), colour = color1, show.legend = FALSE) 
      } 
      if (nrow(M2) >= 1) {  
        p <- p +
          geom_point(data = M2, aes(x = x, y = y, shape = identity, size = identity, alpha = identity), colour = color2, show.legend = FALSE) 
      }
      
    }
  }
  
  p <- p +
    scale_size_manual(values = Size) +
    scale_alpha_manual(values = c(0.6, 1)) +
    scale_y_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    ylab("") +  
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"))
  
  p
}
xy_dot_density_center_concatenate_by_method <- function(spinal, data, m1, m2, color_pal1, color_pal2, nb_methods_ext, nb_methods_flex, IN, MN, Size, method_used) {
  
  if (nb_methods_ext >= 1) {
    ext_colors <- color_pal1(nb_methods_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_methods_flex >= 1) {
    flex_colors <- color_pal2(nb_methods_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  M1 <-  data_all %>% filter(method == method_used[1] & muscle == m1 & identity == IN | method == method_used[1] & muscle == m1 & identity == MN) 
  M2 <-  data_all %>% filter(method == method_used[1] & muscle == m2 & identity == IN | method == method_used[1] & muscle == m2 & identity == MN)
  if (nrow(M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  p <- ggplot() +
    annotate("point", x = 0, y = 0, size = 4, colour = "black") +
    geom_path(data = spinal, aes(x = V1, y = V2), size = 1.5) + 
    geom_point(data = M1, aes(x = x, y = y, shape = identity, size = identity, alpha = identity), colour = ext_colors[1], show.legend = FALSE) +
    geom_point(data = M2, aes(x = x, y = y, shape = identity, size = identity, alpha = identity), colour = flex_colors[1], show.legend = FALSE) 
  
  if (2 <= length(method_used)) { 
    for (i in 2:length(method_used)) {
      M1 <-  data_all %>% filter(method == method_used[i] & muscle == m1 & identity == IN | method == method_used[i] & muscle == m1 & identity == MN) 
      M2 <-  data_all %>% filter(method == method_used[i] & muscle == m2 & identity == IN | method == method_used[i] & muscle == m2 & identity == MN)
      if (nrow(M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        p <- p +
          geom_point(data = M1, aes(x = x, y = y, shape = identity, size = identity, alpha = identity), colour = ext_colors[as.numeric(indice_col_ext)], show.legend = FALSE) 
      } 
      if (nrow(M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        p <- p +
          geom_point(data = M2, aes(x = x, y = y, shape = identity, size = identity, alpha = identity), colour = flex_colors[as.numeric(indice_col_flex)], show.legend = FALSE) 
      }
    }
  }
  
  p <- p +
    scale_size_manual(values = Size) +
    scale_alpha_manual(values = c(0.6, 1)) +
    scale_y_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    ylab("") +  
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"))
  
  p
}
xy_dot_density_center_concatenate_by_titer <- function(spinal, data, m1, m2, color_pal1, color_pal2, nb_titers_ext, nb_titers_flex, IN, MN, Size, titer_used, method_used) {
  
  if (nb_titers_ext >= 1) {
    ext_colors <- color_pal1(nb_titers_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_titers_flex >= 1) {
    flex_colors <- color_pal2(nb_titers_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  M1 <-  data_all %>% filter(titer == titer_used[1] & muscle == m1 & identity == IN & method == method_used | titer == titer_used[1] & muscle == m1 & identity == MN & method == method_used ) 
  M2 <-  data_all %>% filter(titer == titer_used[1] & muscle == m2 & identity == IN & method == method_used | titer == titer_used[1] & muscle == m2 & identity == MN & method == method_used )
  if (nrow(M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  p <- ggplot() +
    annotate("point", x = 0, y = 0, size = 4, colour = "black") +
    geom_path(data = spinal, aes(x = V1, y = V2), size = 1.5) + 
    geom_point(data = M1, aes(x = x, y = y, shape = identity, size = identity, alpha = identity), colour = ext_colors[1], show.legend = FALSE) +
    geom_point(data = M2, aes(x = x, y = y, shape = identity, size = identity, alpha = identity), colour = flex_colors[1], show.legend = FALSE) 
  
  if (2 <= length(titer_used)) { 
    for (i in 2:length(titer_used)) {
      M1 <-  data_all %>% filter(titer == titer_used[i] & muscle == m1 & identity == IN & method == method_used | titer == titer_used[i] & muscle == m1 & identity == MN & method == method_used) 
      M2 <-  data_all %>% filter(titer == titer_used[i] & muscle == m2 & identity == IN & method == method_used | titer == titer_used[i] & muscle == m2 & identity == MN & method == method_used)
      if (nrow(M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        p <- p +
          geom_point(data = M1, aes(x = x, y = y, shape = identity, size = identity, alpha = identity), colour = ext_colors[as.numeric(indice_col_ext)], show.legend = FALSE) 
      } 
      if (nrow(M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        p <- p +
          geom_point(data = M2, aes(x = x, y = y, shape = identity, size = identity, alpha = identity), colour = flex_colors[as.numeric(indice_col_flex)], show.legend = FALSE) 
      }
    }
  }
  
  p <- p +
    scale_size_manual(values = Size) +
    scale_alpha_manual(values = c(0.6, 1)) +
    scale_y_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    ylab("") +  
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"))
  
  p
}

x_ventral_1d_density <- function(data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, xlimit) {
  
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  data <- data_all %>% filter(animal == Animal[1] & identity == "IN")
  
  x_ventral_M2 <- data %>% filter(y <= 0, muscle == m2)
  x_ventral_M1 <- data %>% filter(y <= 0, muscle == m1)
  if (nrow(x_ventral_M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(x_ventral_M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_ventral_M2 <- nrow(x_ventral_M2)/nrow(l_M2)
  scale_ventral_M1 <- nrow(x_ventral_M1)/nrow(l_M1)
  bw_x_M1 <- sd(l_M1$x)/kernel
  bw_x_M2 <- sd(l_M2$x)/kernel
  
  x_ventral_1d_density <- ggplot() +
    stat_density(data = x_ventral_M1, aes(x = x, y=stat(density)*scale_ventral_M1), bw = bw_x_M1, kernel = "gaussian", geom = "line", colour = ext_colors[1], position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = x_ventral_M2, aes(x = x, y=stat(density)*scale_ventral_M2), bw = bw_x_M2, kernel = "gaussian", geom = "line", colour = flex_colors[1], position = "identity", size = 1, show.legend = FALSE)
  
  if (2 <= length(Animal)) { 
    for (i in 2:length(Animal)) {
      data <- data_all %>% filter(animal == Animal[i] & identity == "IN")
      x_ventral_M2 <- data %>% filter(y <= 0, muscle == m2)
      x_ventral_M1 <- data %>% filter(y <= 0, muscle == m1)
      
      
      if (nrow(x_ventral_M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        l_M1 <-  data %>% filter(muscle == m1)
        scale_ventral_M1 <- nrow(x_ventral_M1)/nrow(l_M1)
        bw_x_M1 <- sd(l_M1$x)/kernel
        x_ventral_1d_density <- x_ventral_1d_density +
          stat_density(data = x_ventral_M1, aes(x = x, y=stat(density)*scale_ventral_M1), bw = bw_x_M1, kernel = "gaussian", geom = "line", colour = ext_colors[indice_col_ext], position = "identity", size = 1, show.legend = FALSE) 
      } 
      if (nrow(x_ventral_M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        l_M2 <-  data %>% filter(muscle == m2) 
        scale_ventral_M2 <- nrow(x_ventral_M2)/nrow(l_M2)
        bw_x_M2 <- sd(l_M2$x)/kernel
        x_ventral_1d_density <- x_ventral_1d_density +
          stat_density(data = x_ventral_M2, aes(x = x, y=stat(density)*scale_ventral_M2), bw = bw_x_M2, kernel = "gaussian", geom = "line", colour = flex_colors[indice_col_flex], position = "identity", size = 1, show.legend = FALSE)
      }
      
    }
  }
  if (xlimit == 0) {
    x_ventral_1d_density <- x_ventral_1d_density +
      scale_y_reverse(limits=c(NA, 0), expand = c(.2, 0, 0, 0))
  }
  if (xlimit != 0) {
    x_ventral_1d_density <- x_ventral_1d_density +
      scale_y_reverse(limits=c(xlimit, 0), expand = c(.2, 0, 0, 0)) 
  }
  x_ventral_1d_density <- x_ventral_1d_density +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    theme_classic() +
    ylab("") +
    xlab("M-L (μm)") +
    theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(), axis.title.x = element_text(vjust=13),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent"))
  x_ventral_1d_density
}
x_ventral_1d_density_concatenate <- function(data_all, Animal, m1, m2, color1, color2, kernel, xlimit){
  
  data <- data_all %>% filter(animal == Animal[1] & identity == "IN")
  if (2 <= length(Animal)) {
    for (i in 2:length(Animal)) {
      df <- data_all %>% filter(animal == Animal[i] & identity == "IN")
      data <- rbind(data, df)
    }
  }
  x_ventral_M2 <- data %>% filter(y <= 0, muscle == m2)
  x_ventral_M1 <- data %>% filter(y <= 0, muscle == m1)
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_ventral_M2 <- nrow(x_ventral_M2)/nrow(l_M2)
  scale_ventral_M1 <- nrow(x_ventral_M1)/nrow(l_M1)
  bw_x_M1 <- sd(l_M1$x)/kernel
  bw_x_M2 <- sd(l_M2$x)/kernel
  
  x_ventral_1d_density <- ggplot() +
    stat_density(data = x_ventral_M1, aes(x = x, y=stat(density)*scale_ventral_M1), bw = bw_x_M1, kernel = "gaussian", geom = "line", colour = color1, position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = x_ventral_M2, aes(x = x, y=stat(density)*scale_ventral_M2), bw = bw_x_M2, kernel = "gaussian", geom = "line", colour = color2, position = "identity", size = 1, show.legend = FALSE) +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) 
  if (xlimit == 0) {
    x_ventral_1d_density <- x_ventral_1d_density +
      scale_y_reverse(limits=c(NA, 0), expand = c(.2, 0, 0, 0))
  }
  if (xlimit != 0) {
    x_ventral_1d_density <- x_ventral_1d_density +
      scale_y_reverse(limits=c(xlimit, 0), expand = c(.2, 0, 0, 0)) 
  }
  x_ventral_1d_density <- x_ventral_1d_density +
    ylab("") +
    xlab("M-L (μm)") +
    theme_classic() +
    theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(), axis.title.x = element_text(vjust=13),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent"))
  
  x_ventral_1d_density
}
x_ventral_1d_density_concatenate_by_method <- function(data_all,  method_used, m1, m2, color_pal1, color_pal2, nb_methods_ext, nb_methods_flex, kernel, xlimit){
  
  if (nb_methods_ext >= 1) {
    ext_colors <- color_pal1(nb_methods_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_methods_flex >= 1) {
    flex_colors <- color_pal2(nb_methods_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  data <- data_all %>% filter(method == method_used[1] & identity == "IN")
  
  x_ventral_M2 <- data %>% filter(y <= 0, muscle == m2)
  x_ventral_M1 <- data %>% filter(y <= 0, muscle == m1)
  if (nrow(x_ventral_M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(x_ventral_M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_ventral_M2 <- nrow(x_ventral_M2)/nrow(l_M2)
  scale_ventral_M1 <- nrow(x_ventral_M1)/nrow(l_M1)
  bw_x_M1 <- sd(l_M1$x)/kernel
  bw_x_M2 <- sd(l_M2$x)/kernel
  
  x_ventral_1d_density <- ggplot() +
    stat_density(data = x_ventral_M1, aes(x = x, y=stat(density)*scale_ventral_M1), bw = bw_x_M1, kernel = "gaussian", geom = "line", colour = ext_colors[1], position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = x_ventral_M2, aes(x = x, y=stat(density)*scale_ventral_M2), bw = bw_x_M2, kernel = "gaussian", geom = "line", colour = flex_colors[1], position = "identity", size = 1, show.legend = FALSE)
  
  if (2 <= length(method_used)) { 
    for (i in 2:length(method_used)) {
      data <- data_all %>% filter(method == method_used[i] & identity == "IN")
      x_ventral_M2 <- data %>% filter(y <= 0, muscle == m2)
      x_ventral_M1 <- data %>% filter(y <= 0, muscle == m1)
      
      
      if (nrow(x_ventral_M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        l_M1 <-  data %>% filter(muscle == m1)
        scale_ventral_M1 <- nrow(x_ventral_M1)/nrow(l_M1)
        bw_x_M1 <- sd(l_M1$x)/kernel
        x_ventral_1d_density <- x_ventral_1d_density +
          stat_density(data = x_ventral_M1, aes(x = x, y=stat(density)*scale_ventral_M1), bw = bw_x_M1, kernel = "gaussian", geom = "line", colour = ext_colors[indice_col_ext], position = "identity", size = 1, show.legend = FALSE) 
      } 
      if (nrow(x_ventral_M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        l_M2 <-  data %>% filter(muscle == m2) 
        scale_ventral_M2 <- nrow(x_ventral_M2)/nrow(l_M2)
        bw_x_M2 <- sd(l_M2$x)/kernel
        x_ventral_1d_density <- x_ventral_1d_density +
          stat_density(data = x_ventral_M2, aes(x = x, y=stat(density)*scale_ventral_M2), bw = bw_x_M2, kernel = "gaussian", geom = "line", colour = flex_colors[indice_col_flex], position = "identity", size = 1, show.legend = FALSE)
      }
      
    }
  }
  if (xlimit == 0) {
    x_ventral_1d_density <- x_ventral_1d_density +
      scale_y_reverse(limits=c(NA, 0), expand = c(.2, 0, 0, 0))
  }
  if (xlimit != 0) {
    x_ventral_1d_density <- x_ventral_1d_density +
      scale_y_reverse(limits=c(xlimit, 0), expand = c(.2, 0, 0, 0)) 
  }
  x_ventral_1d_density <- x_ventral_1d_density +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    theme_classic() +
    ylab("") +
    xlab("M-L (μm)") +
    theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(), axis.title.x = element_text(vjust=13),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent"))
  x_ventral_1d_density
}
x_ventral_1d_density_concatenate_by_titer <- function(data_all,  titer_used, m1, m2, color_pal1, color_pal2, nb_titers_ext, nb_titers_flex, color2, method_used, kernel, xlimit){
  
  if (nb_titers_ext >= 1) {
    ext_colors <- color_pal1(nb_titers_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_titers_flex >= 1) {
    flex_colors <- color_pal2(nb_titers_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  data <- data_all %>% filter(titer == titer_used[1] & identity == "IN" & method == method_used )
  
  x_ventral_M2 <- data %>% filter(y <= 0, muscle == m2)
  x_ventral_M1 <- data %>% filter(y <= 0, muscle == m1)
  if (nrow(x_ventral_M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(x_ventral_M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_ventral_M2 <- nrow(x_ventral_M2)/nrow(l_M2)
  scale_ventral_M1 <- nrow(x_ventral_M1)/nrow(l_M1)
  bw_x_M1 <- sd(l_M1$x)/kernel
  bw_x_M2 <- sd(l_M2$x)/kernel
  
  x_ventral_1d_density <- ggplot() +
    stat_density(data = x_ventral_M1, aes(x = x, y=stat(density)*scale_ventral_M1), bw = bw_x_M1, kernel = "gaussian", geom = "line", colour = ext_colors[1], position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = x_ventral_M2, aes(x = x, y=stat(density)*scale_ventral_M2), bw = bw_x_M2, kernel = "gaussian", geom = "line", colour = flex_colors[1], position = "identity", size = 1, show.legend = FALSE)
  
  if (2 <= length(titer_used)) { 
    for (i in 2:length(titer_used)) {
      data <- data_all %>% filter(titer == titer_used[i] & identity == "IN" & method == method_used)
      x_ventral_M2 <- data %>% filter(y <= 0, muscle == m2)
      x_ventral_M1 <- data %>% filter(y <= 0, muscle == m1)
      
      
      if (nrow(x_ventral_M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        l_M1 <-  data %>% filter(muscle == m1)
        scale_ventral_M1 <- nrow(x_ventral_M1)/nrow(l_M1)
        bw_x_M1 <- sd(l_M1$x)/kernel
        x_ventral_1d_density <- x_ventral_1d_density +
          stat_density(data = x_ventral_M1, aes(x = x, y=stat(density)*scale_ventral_M1), bw = bw_x_M1, kernel = "gaussian", geom = "line", colour = ext_colors[indice_col_ext], position = "identity", size = 1, show.legend = FALSE) 
      } 
      if (nrow(x_ventral_M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        l_M2 <-  data %>% filter(muscle == m2) 
        scale_ventral_M2 <- nrow(x_ventral_M2)/nrow(l_M2)
        bw_x_M2 <- sd(l_M2$x)/kernel
        x_ventral_1d_density <- x_ventral_1d_density +
          stat_density(data = x_ventral_M2, aes(x = x, y=stat(density)*scale_ventral_M2), bw = bw_x_M2, kernel = "gaussian", geom = "line", colour = flex_colors[indice_col_flex], position = "identity", size = 1, show.legend = FALSE)
      }
      
    }
  }
  if (xlimit == 0) {
    x_ventral_1d_density <- x_ventral_1d_density +
      scale_y_reverse(limits=c(NA, 0), expand = c(.2, 0, 0, 0))
  }
  if (xlimit != 0) {
    x_ventral_1d_density <- x_ventral_1d_density +
      scale_y_reverse(limits=c(xlimit, 0), expand = c(.2, 0, 0, 0)) 
  }
  x_ventral_1d_density <- x_ventral_1d_density +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    theme_classic() +
    ylab("") +
    xlab("M-L (μm)") +
    theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(), axis.title.x = element_text(vjust=13),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) 
  
  x_ventral_1d_density
}

x_dorsal_1d_density <- function(data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, color1, color2, name_M1, name_M2, kernel, xlimit){
  
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  data <- data_all %>% filter(animal == Animal[1] & identity == "IN")
  
  x_dorsal_M1 <- data %>% filter(y >= 0, muscle == m1)
  x_dorsal_M2 <- data %>% filter(y >= 0, muscle == m2)
  if (nrow(x_dorsal_M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(x_dorsal_M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_dorsal_M1 <- nrow(x_dorsal_M1)/nrow(l_M1)
  scale_dorsal_M2 <- nrow(x_dorsal_M2)/nrow(l_M2)
  bw_x_M1 <- sd(l_M1$x)/kernel
  bw_x_M2 <- sd(l_M2$x)/kernel
  
  x_dorsal_1d_density <- ggplot() +
    stat_density(data = x_dorsal_M1, aes(x = x, y=stat(density)*scale_dorsal_M1), bw = bw_x_M1, kernel = "gaussian", geom = "line", colour = ext_colors[1], position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = x_dorsal_M2, aes(x = x, y=stat(density)*scale_dorsal_M2), bw = bw_x_M2, kernel = "gaussian", geom = "line", colour = flex_colors[1], position = "identity", size = 1, show.legend = FALSE)
  
  if (2 <= length(Animal)) { 
    for (i in 2:length(Animal)) {
      data <- data_all %>% filter(animal == Animal[i] & identity == "IN")
      x_dorsal_M1 <- data %>% filter(y >= 0, muscle == m1)
      x_dorsal_M2 <- data %>% filter(y >= 0, muscle == m2)
      
      
      if (nrow(x_dorsal_M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        l_M1 <-  data %>% filter(muscle == m1)
        scale_dorsal_M1 <- nrow(x_dorsal_M1)/nrow(l_M1)
        bw_x_M1 <- sd(l_M1$x)/kernel
        x_dorsal_1d_density <- x_dorsal_1d_density +
          stat_density(data = x_dorsal_M1, aes(x = x, y=stat(density)*scale_dorsal_M1), bw = bw_x_M1, kernel = "gaussian", geom = "line", colour = ext_colors[indice_col_ext], position = "identity", size = 1, show.legend = FALSE) 
      } 
      if (nrow(x_dorsal_M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        l_M2 <-  data %>% filter(muscle == m2) 
        scale_dorsal_M2 <- nrow(x_dorsal_M2)/nrow(l_M2)
        bw_x_M2 <- sd(l_M2$x)/kernel
        x_dorsal_1d_density <- x_dorsal_1d_density +
          stat_density(data = x_dorsal_M2, aes(x = x, y=stat(density)*scale_dorsal_M2), bw = bw_x_M2, kernel = "gaussian", geom = "line", colour = flex_colors[indice_col_flex], position = "identity", size = 1, show.legend = FALSE)
      }
      
    }
  }
  if (xlimit == 0) {
    x_dorsal_1d_density <- x_dorsal_1d_density +
      scale_y_continuous(limits=c(0, NA), expand = c(0, 0, .2, 0)) 
  }
  if (xlimit != 0) {
    x_dorsal_1d_density <- x_dorsal_1d_density +
      scale_y_continuous(limits=c(0, xlimit), expand = c(0, 0, .2, 0)) 
  }
  x_dorsal_1d_density <- x_dorsal_1d_density +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    theme_classic() +
    ylab("") +
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_), # necessary to avoid drawing plot outline
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) 
  x_dorsal_1d_density
}
x_dorsal_1d_density_concatenate <- function(data_all, Animal, m1, m2, color1, color2, kernel, xlimit){
  data <- data_all %>% filter(animal == Animal[1] & identity == "IN")
  if (2 <= length(Animal)) {
    for (i in 2:length(Animal)) {
      df <- data_all %>% filter(animal == Animal[i] & identity == "IN")
      data <- rbind(data, df)
    }
  }
  x_dorsal_M2 <- data %>% filter(y >= 0, muscle == m2)
  x_dorsal_M1 <- data %>% filter(y >= 0, muscle == m1)
  l_M1 <-  data %>% filter(muscle == m1)
  l_M2 <-  data %>% filter(muscle == m2)
  scale_dorsal_M2 <- nrow(x_dorsal_M2)/nrow(l_M2)
  scale_dorsal_M1 <- nrow(x_dorsal_M1)/nrow(l_M1)
  bw_x_M1 <- sd(l_M1$x)/kernel
  bw_x_M2 <- sd(l_M2$x)/kernel
  
  x_dorsal_1d_density <- ggplot() +
    stat_density(data = x_dorsal_M1, aes(x = x, y=stat(density)*scale_dorsal_M1), bw = bw_x_M1, kernel = "gaussian", geom = "line", colour = color1, position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = x_dorsal_M2, aes(x = x, y=stat(density)*scale_dorsal_M2), bw = bw_x_M2, kernel = "gaussian", geom = "line", colour = color2, position = "identity", size = 1, show.legend = FALSE) +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) 
  if (xlimit == 0) {
    x_dorsal_1d_density <- x_dorsal_1d_density +
      scale_y_continuous(limits=c(0, NA), expand = c(0, 0, .2, 0)) 
  }
  if (xlimit != 0) {
    x_dorsal_1d_density <- x_dorsal_1d_density +
      scale_y_continuous(limits=c(0, xlimit), expand = c(0, 0, .2, 0)) 
  }
  x_dorsal_1d_density <- x_dorsal_1d_density +
    ylab("") +  
    xlab("") +
    theme_classic() +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_), # necessary to avoid drawing plot outline
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) 
  
  x_dorsal_1d_density
}
x_dorsal_1d_density_concatenate_by_method <- function(data_all,  method_used, m1, m2, color_pal1, color_pal2, nb_methods_ext, nb_methods_flex, kernel, xlimit){
  
  if (nb_methods_ext >= 1) {
    ext_colors <- color_pal1(nb_methods_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_methods_flex >= 1) {
    flex_colors <- color_pal2(nb_methods_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  data <- data_all %>% filter(method == method_used[1] & identity == "IN")
  
  x_dorsal_M1 <- data %>% filter(y >= 0, muscle == m1)
  x_dorsal_M2 <- data %>% filter(y >= 0, muscle == m2)
  if (nrow(x_dorsal_M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(x_dorsal_M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_dorsal_M1 <- nrow(x_dorsal_M1)/nrow(l_M1)
  scale_dorsal_M2 <- nrow(x_dorsal_M2)/nrow(l_M2)
  bw_x_M1 <- sd(l_M1$x)/kernel
  bw_x_M2 <- sd(l_M2$x)/kernel
  
  x_dorsal_1d_density <- ggplot() +
    stat_density(data = x_dorsal_M1, aes(x = x, y=stat(density)*scale_dorsal_M1), bw = bw_x_M1, kernel = "gaussian", geom = "line", colour = ext_colors[1], position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = x_dorsal_M2, aes(x = x, y=stat(density)*scale_dorsal_M2), bw = bw_x_M2, kernel = "gaussian", geom = "line", colour = flex_colors[1], position = "identity", size = 1, show.legend = FALSE)
  
  if (2 <= length(method_used)) { 
    for (i in 2:length(method_used)) {
      data <- data_all %>% filter(method == method_used[i] & identity == "IN")
      x_dorsal_M1 <- data %>% filter(y >= 0, muscle == m1)
      x_dorsal_M2 <- data %>% filter(y >= 0, muscle == m2)
      
      
      if (nrow(x_dorsal_M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        l_M1 <-  data %>% filter(muscle == m1)
        scale_dorsal_M1 <- nrow(x_dorsal_M1)/nrow(l_M1)
        bw_x_M1 <- sd(l_M1$x)/kernel
        x_dorsal_1d_density <- x_dorsal_1d_density +
          stat_density(data = x_dorsal_M1, aes(x = x, y=stat(density)*scale_dorsal_M1), bw = bw_x_M1, kernel = "gaussian", geom = "line", colour = ext_colors[indice_col_ext], position = "identity", size = 1, show.legend = FALSE) 
      } 
      if (nrow(x_dorsal_M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        l_M2 <-  data %>% filter(muscle == m2) 
        scale_dorsal_M2 <- nrow(x_dorsal_M2)/nrow(l_M2)
        bw_x_M2 <- sd(l_M2$x)/kernel
        x_dorsal_1d_density <- x_dorsal_1d_density +
          stat_density(data = x_dorsal_M2, aes(x = x, y=stat(density)*scale_dorsal_M2), bw = bw_x_M2, kernel = "gaussian", geom = "line", colour = flex_colors[indice_col_flex], position = "identity", size = 1, show.legend = FALSE)
      }
      
    }
  }
  if (xlimit == 0) {
    x_dorsal_1d_density <- x_dorsal_1d_density +
      scale_y_continuous(limits=c(0, NA), expand = c(0, 0, .2, 0)) 
  }
  if (xlimit != 0) {
    x_dorsal_1d_density <- x_dorsal_1d_density +
      scale_y_continuous(limits=c(0, xlimit), expand = c(0, 0, .2, 0)) 
  }
  x_dorsal_1d_density <- x_dorsal_1d_density +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    theme_classic() +
    ylab("") +
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_), # necessary to avoid drawing plot outline
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) 
  
  x_dorsal_1d_density
}
x_dorsal_1d_density_concatenate_by_titer <- function(data_all,  titer_used, m1, m2, color_pal1, color_pal2, nb_titers_ext, nb_titers_flex, color1, method_used, kernel, xlimit){
  
  if (nb_titers_ext >= 1) {
    ext_colors <- color_pal1(nb_titers_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_titers_flex >= 1) {
    flex_colors <- color_pal2(nb_titers_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  data <- data_all %>% filter(titer == titer_used[1] & identity == "IN" & method == method_used)
  
  x_dorsal_M1 <- data %>% filter(y >= 0, muscle == m1)
  x_dorsal_M2 <- data %>% filter(y >= 0, muscle == m2)
  if (nrow(x_dorsal_M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(x_dorsal_M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_dorsal_M1 <- nrow(x_dorsal_M1)/nrow(l_M1)
  scale_dorsal_M2 <- nrow(x_dorsal_M2)/nrow(l_M2)
  bw_x_M1 <- sd(l_M1$x)/kernel
  bw_x_M2 <- sd(l_M2$x)/kernel
  
  x_dorsal_1d_density <- ggplot() +
    stat_density(data = x_dorsal_M1, aes(x = x, y=stat(density)*scale_dorsal_M1), bw = bw_x_M1, kernel = "gaussian", geom = "line", colour = ext_colors[1], position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = x_dorsal_M2, aes(x = x, y=stat(density)*scale_dorsal_M2), bw = bw_x_M2, kernel = "gaussian", geom = "line", colour = flex_colors[1], position = "identity", size = 1, show.legend = FALSE)
  
  if (2 <= length(titer_used)) { 
    for (i in 2:length(titer_used)) {
      data <- data_all %>% filter(titer == titer_used[i] & identity == "IN" & method == method_used)
      x_dorsal_M1 <- data %>% filter(y >= 0, muscle == m1)
      x_dorsal_M2 <- data %>% filter(y >= 0, muscle == m2)
      
      
      if (nrow(x_dorsal_M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        l_M1 <-  data %>% filter(muscle == m1)
        scale_dorsal_M1 <- nrow(x_dorsal_M1)/nrow(l_M1)
        bw_x_M1 <- sd(l_M1$x)/kernel
        x_dorsal_1d_density <- x_dorsal_1d_density +
          stat_density(data = x_dorsal_M1, aes(x = x, y=stat(density)*scale_dorsal_M1), bw = bw_x_M1, kernel = "gaussian", geom = "line", colour = ext_colors[indice_col_ext], position = "identity", size = 1, show.legend = FALSE) 
      } 
      if (nrow(x_dorsal_M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        l_M2 <-  data %>% filter(muscle == m2) 
        scale_dorsal_M2 <- nrow(x_dorsal_M2)/nrow(l_M2)
        bw_x_M2 <- sd(l_M2$x)/kernel
        x_dorsal_1d_density <- x_dorsal_1d_density +
          stat_density(data = x_dorsal_M2, aes(x = x, y=stat(density)*scale_dorsal_M2), bw = bw_x_M2, kernel = "gaussian", geom = "line", colour = flex_colors[indice_col_flex], position = "identity", size = 1, show.legend = FALSE)
      }
      
    }
  }
  if (xlimit == 0) {
    x_dorsal_1d_density <- x_dorsal_1d_density +
      scale_y_continuous(limits=c(0, NA), expand = c(0, 0, .2, 0)) 
  }
  if (xlimit != 0) {
    x_dorsal_1d_density <- x_dorsal_1d_density +
      scale_y_continuous(limits=c(0, xlimit), expand = c(0, 0, .2, 0)) 
  }
  x_dorsal_1d_density <- x_dorsal_1d_density +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    theme_classic() +
    ylab("") +
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_), # necessary to avoid drawing plot outline
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent"))  
  
  x_dorsal_1d_density
}

y_contra_1d_density <- function(data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, ylimit){
  
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  data <- data_all %>% filter(animal == Animal[1] & identity == "IN")
  
  y_contra_M1 <- data %>% filter(x <= 0, muscle == m1)
  y_contra_M2 <- data %>% filter(x <= 0, muscle == m2)
  if (nrow(y_contra_M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(y_contra_M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_contra_M1 <- nrow(y_contra_M1)/nrow(l_M1)
  scale_contra_M2 <- nrow(y_contra_M2)/nrow(l_M2)
  bw_y_M1 <- sd(l_M1$y)/kernel
  bw_y_M2 <- sd(l_M2$y)/kernel
  
  y_contra_1d_density <- ggplot() +
    stat_density(data = y_contra_M1, aes(x = y, y=stat(density)*scale_contra_M1), bw = bw_y_M1, kernel = "gaussian", geom = "line", colour = ext_colors[1], position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = y_contra_M2, aes(x = y, y=stat(density)*scale_contra_M2), bw = bw_y_M2, kernel = "gaussian", geom = "line", colour = flex_colors[1], position = "identity", size = 1, show.legend = FALSE)
  
  if (2 <= length(Animal)) { 
    for (i in 2:length(Animal)) {
      data <- data_all %>% filter(animal == Animal[i] & identity == "IN")
      y_contra_M1 <- data %>% filter(x <= 0, muscle == m1)
      y_contra_M2 <- data %>% filter(x <= 0, muscle == m2)
      
      
      if (nrow(y_contra_M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        l_M1 <-  data %>% filter(muscle == m1)
        scale_contra_M1 <- nrow(y_contra_M1)/nrow(l_M1)
        bw_y_M1 <- sd(l_M1$y)/kernel
        y_contra_1d_density <- y_contra_1d_density +
          stat_density(data = y_contra_M1, aes(x = y, y=stat(density)*scale_contra_M1), bw = bw_y_M1, kernel = "gaussian", geom = "line", colour = ext_colors[indice_col_ext], position = "identity", size = 1, show.legend = FALSE) 
      } 
      if (nrow(y_contra_M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        l_M2 <-  data %>% filter(muscle == m2) 
        scale_contra_M2 <- nrow(y_contra_M2)/nrow(l_M2)
        bw_y_M2 <- sd(l_M2$y)/kernel
        y_contra_1d_density <- y_contra_1d_density +
          stat_density(data = y_contra_M2, aes(x = y, y=stat(density)*scale_contra_M2), bw = bw_y_M2, kernel = "gaussian", geom = "line", colour = flex_colors[indice_col_flex], position = "identity", size = 1, show.legend = FALSE)
      }
      
    }
  }
  
  y_contra_1d_density <- y_contra_1d_density +
    scale_x_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    scale_y_reverse(limits=c(as.numeric(ylimit), 0),  expand = c(0.2, 0, 0, 0))+
    theme_classic() +
    ylab("") +  
    xlab("V-D (μm)")+
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(), axis.title.y = element_text(vjust=-16), panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate() 
  y_contra_1d_density
}
y_contra_1d_density_concatenate <- function(data_all, Animal, m1, m2, color1, color2, kernel, ylimit){
  data <- data_all %>% filter(animal == Animal[1] & identity == "IN")
  if (2 <= length(Animal)) {
    for (i in 2:length(Animal)) {
      df <- data_all %>% filter(animal == Animal[i] & identity == "IN")
      data <- rbind(data, df)
    }
  }
  y_contra_M2 <- data %>% filter(x <= 0, muscle == m2)
  y_contra_M1 <- data %>% filter(x <= 0, muscle == m1)
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_contra_M2 <- nrow(y_contra_M2)/nrow(l_M2)
  scale_contra_M1 <- nrow(y_contra_M1)/nrow(l_M1)
  bw_y_M1 <- sd(l_M1$y)/kernel
  bw_y_M2 <- sd(l_M2$y)/kernel
  
  y_contra_1d_density <- ggplot() +
    stat_density(data = y_contra_M1, aes(x = y, y=stat(density)*scale_contra_M1), bw = bw_y_M1, kernel = "gaussian", geom = "line", colour = color1, position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = y_contra_M2, aes(x = y, y=stat(density)*scale_contra_M2), bw = bw_y_M2, kernel = "gaussian", geom = "line", colour = color2, position = "identity", size = 1, show.legend = FALSE) +
    scale_x_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    scale_y_reverse(limits=c(ylimit, 0),  expand = c(0.2, 0, 0, 0))+
    theme_classic() +
    ylab("") +  
    xlab("V-D (μm)") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(), axis.title.y = element_text(vjust=-16), panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate() 
  
  y_contra_1d_density
}
y_contra_1d_density_concatenate_by_method <- function(data_all,  method_used, m1, m2, color_pal1, color_pal2, nb_methods_ext, nb_methods_flex, kernel, ylimit){
  
  if (nb_methods_ext >= 1) {
    ext_colors <- color_pal1(nb_methods_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_methods_flex >= 1) {
    flex_colors <- color_pal2(nb_methods_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  data <- data_all %>% filter(method == method_used[1] & identity == "IN")
  
  y_contra_M1 <- data %>% filter(x <= 0, muscle == m1)
  y_contra_M2 <- data %>% filter(x <= 0, muscle == m2)
  if (nrow(y_contra_M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(y_contra_M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_contra_M1 <- nrow(y_contra_M1)/nrow(l_M1)
  scale_contra_M2 <- nrow(y_contra_M2)/nrow(l_M2)
  bw_y_M1 <- sd(l_M1$y)/kernel
  bw_y_M2 <- sd(l_M2$y)/kernel
  
  y_contra_1d_density <- ggplot() +
    stat_density(data = y_contra_M1, aes(x = y, y=stat(density)*scale_contra_M1), bw = bw_y_M1, kernel = "gaussian", geom = "line", colour = ext_colors[1], position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = y_contra_M2, aes(x = y, y=stat(density)*scale_contra_M2), bw = bw_y_M2, kernel = "gaussian", geom = "line", colour = flex_colors[1], position = "identity", size = 1, show.legend = FALSE)
  
  if (2 <= length(method_used)) { 
    for (i in 2:length(method_used)) {
      data <- data_all %>% filter(method == method_used[i] & identity == "IN")
      y_contra_M1 <- data %>% filter(x <= 0, muscle == m1)
      y_contra_M2 <- data %>% filter(x <= 0, muscle == m2)
      
      
      if (nrow(y_contra_M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        l_M1 <-  data %>% filter(muscle == m1)
        scale_contra_M1 <- nrow(y_contra_M1)/nrow(l_M1)
        bw_y_M1 <- sd(l_M1$y)/kernel
        y_contra_1d_density <- y_contra_1d_density +
          stat_density(data = y_contra_M1, aes(x = y, y=stat(density)*scale_contra_M1), bw = bw_y_M1, kernel = "gaussian", geom = "line", colour = ext_colors[indice_col_ext], position = "identity", size = 1, show.legend = FALSE) 
      } 
      if (nrow(y_contra_M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        l_M2 <-  data %>% filter(muscle == m2) 
        scale_contra_M2 <- nrow(y_contra_M2)/nrow(l_M2)
        bw_y_M2 <- sd(l_M2$y)/kernel
        y_contra_1d_density <- y_contra_1d_density +
          stat_density(data = y_contra_M2, aes(x = y, y=stat(density)*scale_contra_M2), bw = bw_y_M2, kernel = "gaussian", geom = "line", colour = flex_colors[indice_col_flex], position = "identity", size = 1, show.legend = FALSE)
      }
      
    }
  }
  
  y_contra_1d_density <- y_contra_1d_density +
    scale_x_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    scale_y_reverse(limits=c(ylimit, 0),  expand = c(0.2, 0, 0, 0))+
    theme_classic() +
    ylab("") +  
    xlab("V-D (μm)")+
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(), axis.title.y = element_text(vjust=-16), panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate() 
  y_contra_1d_density
}
y_contra_1d_density_concatenate_by_titer <- function(data_all,  titer_used, m1, m2, color_pal1, color_pal2, nb_titers_ext, nb_titers_flex, method_used, kernel, ylimit){
  
  if (nb_titers_ext >= 1) {
    ext_colors <- color_pal1(nb_titers_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_titers_flex >= 1) {
    flex_colors <- color_pal2(nb_titers_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  data <- data_all %>% filter(titer == titer_used[1] & identity == "IN" & method == method_used)
  
  y_contra_M1 <- data %>% filter(x <= 0, muscle == m1)
  y_contra_M2 <- data %>% filter(x <= 0, muscle == m2)
  if (nrow(y_contra_M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(y_contra_M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_contra_M1 <- nrow(y_contra_M1)/nrow(l_M1)
  scale_contra_M2 <- nrow(y_contra_M2)/nrow(l_M2)
  bw_y_M1 <- sd(l_M1$y)/kernel
  bw_y_M2 <- sd(l_M2$y)/kernel
  
  y_contra_1d_density <- ggplot() +
    stat_density(data = y_contra_M1, aes(x = y, y=stat(density)*scale_contra_M1), bw = bw_y_M1, kernel = "gaussian", geom = "line", colour = ext_colors[1], position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = y_contra_M2, aes(x = y, y=stat(density)*scale_contra_M2), bw = bw_y_M2, kernel = "gaussian", geom = "line", colour = flex_colors[1], position = "identity", size = 1, show.legend = FALSE)
  
  if (2 <= length(titer_used)) { 
    for (i in 2:length(titer_used)) {
      data <- data_all %>% filter(titer == titer_used[i] & identity == "IN" & method == method_used)
      y_contra_M1 <- data %>% filter(x <= 0, muscle == m1)
      y_contra_M2 <- data %>% filter(x <= 0, muscle == m2)
      
      
      if (nrow(y_contra_M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        l_M1 <-  data %>% filter(muscle == m1)
        scale_contra_M1 <- nrow(y_contra_M1)/nrow(l_M1)
        bw_y_M1 <- sd(l_M1$y)/kernel
        y_contra_1d_density <- y_contra_1d_density +
          stat_density(data = y_contra_M1, aes(x = y, y=stat(density)*scale_contra_M1), bw = bw_y_M1, kernel = "gaussian", geom = "line", colour = ext_colors[indice_col_ext], position = "identity", size = 1, show.legend = FALSE) 
      } 
      if (nrow(y_contra_M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        l_M2 <-  data %>% filter(muscle == m2) 
        scale_contra_M2 <- nrow(y_contra_M2)/nrow(l_M2)
        bw_y_M2 <- sd(l_M2$y)/kernel
        y_contra_1d_density <- y_contra_1d_density +
          stat_density(data = y_contra_M2, aes(x = y, y=stat(density)*scale_contra_M2), bw = bw_y_M2, kernel = "gaussian", geom = "line", colour = flex_colors[indice_col_flex], position = "identity", size = 1, show.legend = FALSE)
      }
      
    }
  }
  
  y_contra_1d_density <- y_contra_1d_density +
    scale_x_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    scale_y_reverse(limits=c(ylimit, 0),  expand = c(0.2, 0, 0, 0))+
    theme_classic() +
    ylab("") +  
    xlab("V-D (μm)")+
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(), axis.title.y = element_text(vjust=-16), panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate() 
  y_contra_1d_density
}

y_ipsi_1d_density <- function(data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel){
  
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  data <- data_all %>% filter(animal == Animal[1] & identity == "IN")
  
  y_ipsi_M1 <- data %>% filter(x >= 0, muscle == m1)
  y_ipsi_M2 <- data %>% filter(x >= 0, muscle == m2)
  if (nrow(y_ipsi_M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(y_ipsi_M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_ipsi_M1 <- nrow(y_ipsi_M1)/nrow(l_M1)
  scale_ipsi_M2 <- nrow(y_ipsi_M2)/nrow(l_M2)
  bw_y_M1 <- sd(l_M1$y)/kernel
  bw_y_M2 <- sd(l_M2$y)/kernel
  
  y_ipsi_1d_density <- ggplot() +
    stat_density(data = y_ipsi_M1, aes(x = y, y=stat(density)*scale_ipsi_M1), bw = bw_y_M1, kernel = "gaussian", geom = "line", colour = ext_colors[1], position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = y_ipsi_M2, aes(x = y, y=stat(density)*scale_ipsi_M2), bw = bw_y_M2, kernel = "gaussian", geom = "line", colour = flex_colors[1], position = "identity", size = 1, show.legend = FALSE)
  
  if (2 <= length(Animal)) { 
    for (i in 2:length(Animal)) {
      data <- data_all %>% filter(animal == Animal[i] & identity == "IN")
      y_ipsi_M1 <- data %>% filter(x >= 0, muscle == m1)
      y_ipsi_M2 <- data %>% filter(x >= 0, muscle == m2)
      if (nrow(y_ipsi_M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        l_M1 <-  data %>% filter(muscle == m1)
        scale_ipsi_M1 <- nrow(y_ipsi_M1)/nrow(l_M1)
        bw_y_M1 <- sd(l_M1$y)/kernel
        y_ipsi_1d_density <- y_ipsi_1d_density +
          stat_density(data = y_ipsi_M1, aes(x = y, y=stat(density)*scale_ipsi_M1), bw = bw_y_M1, kernel = "gaussian", geom = "line", colour = ext_colors[indice_col_ext], position = "identity", size = 1, show.legend = FALSE) 
      } 
      if (nrow(y_ipsi_M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        l_M2 <-  data %>% filter(muscle == m2) 
        scale_ipsi_M2 <- nrow(y_ipsi_M2)/nrow(l_M2)
        bw_y_M2 <- sd(l_M2$y)/kernel
        y_ipsi_1d_density <- y_ipsi_1d_density +
          stat_density(data = y_ipsi_M2, aes(x = y, y=stat(density)*scale_ipsi_M2), bw = bw_y_M2, kernel = "gaussian", geom = "line", colour = flex_colors[indice_col_flex], position = "identity", size = 1, show.legend = FALSE)
      }
      
    }
  }
  
  y_ipsi_1d_density <- y_ipsi_1d_density +
    scale_x_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    scale_y_continuous(limits= c(0, NA), expand = c(0, 0, .2, 0)) +
    #expand_limits(y=c(0) +
    theme_classic() +
    ylab("") +  
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate()
  y_ipsi_1d_density
}
y_ipsi_1d_density_concatenate <- function(data_all, Animal, m1, m2, color1, color2, kernel){
  data <- data_all %>% filter(animal == Animal[1] & identity == "IN")
  if (2 <= length(Animal)) {
    for (i in 2:length(Animal)) {
      df <- data_all %>% filter(animal == Animal[i] & identity == "IN")
      data <- rbind(data, df)
    }
  }
  y_ipsi_M2 <- data %>% filter(x >= 0, muscle == m2)
  y_ipsi_M1 <- data %>% filter(x >= 0, muscle == m1)
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_ipsi_M2 <- nrow(y_ipsi_M2)/nrow(l_M2)
  scale_ipsi_M1 <- nrow(y_ipsi_M1)/nrow(l_M1)
  bw_y_M1 <- sd(l_M1$y)/kernel
  bw_y_M2 <- sd(l_M2$y)/kernel
  
  y_ipsi_1d_density <- ggplot() +
    stat_density(data = y_ipsi_M1, aes(x = y, y=stat(density)*scale_ipsi_M1), bw = bw_y_M1, kernel = "gaussian", geom = "line", colour = color1, position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = y_ipsi_M2, aes(x = y, y=stat(density)*scale_ipsi_M2), bw = bw_y_M2, kernel = "gaussian", geom = "line", colour = color2, position = "identity", size = 1, show.legend = FALSE) +
    scale_x_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    scale_y_continuous(limits= c(0, NA), expand = c(0, 0, .2, 0)) +
    theme_classic() +
    ylab("") +  
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate()
  
  y_ipsi_1d_density
}
y_ipsi_1d_density_concatenate_by_method <- function(data_all,  method_used, m1, m2, color_pal1, color_pal2, nb_methods_ext, nb_methods_flex, kernel){
  
  if (nb_methods_ext >= 1) {
    ext_colors <- color_pal1(nb_methods_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_methods_flex >= 1) {
    flex_colors <- color_pal2(nb_methods_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  data <- data_all %>% filter(method == method_used[1] & identity == "IN")
  
  y_ipsi_M1 <- data %>% filter(x >= 0, muscle == m1)
  y_ipsi_M2 <- data %>% filter(x >= 0, muscle == m2)
  if (nrow(y_ipsi_M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(y_ipsi_M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_ipsi_M1 <- nrow(y_ipsi_M1)/nrow(l_M1)
  scale_ipsi_M2 <- nrow(y_ipsi_M2)/nrow(l_M2)
  bw_y_M1 <- sd(l_M1$y)/kernel
  bw_y_M2 <- sd(l_M2$y)/kernel
  
  y_ipsi_1d_density <- ggplot() +
    stat_density(data = y_ipsi_M1, aes(x = y, y=stat(density)*scale_ipsi_M1), bw = bw_y_M1, kernel = "gaussian", geom = "line", colour = ext_colors[1], position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = y_ipsi_M2, aes(x = y, y=stat(density)*scale_ipsi_M2), bw = bw_y_M2, kernel = "gaussian", geom = "line", colour = flex_colors[1], position = "identity", size = 1, show.legend = FALSE)
  
  if (2 <= length(method_used)) { 
    for (i in 2:length(method_used)) {
      data <- data_all %>% filter(method == method_used[i] & identity == "IN")
      y_ipsi_M1 <- data %>% filter(x >= 0, muscle == m1)
      y_ipsi_M2 <- data %>% filter(x >= 0, muscle == m2)
      if (nrow(y_ipsi_M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        l_M1 <-  data %>% filter(muscle == m1)
        scale_ipsi_M1 <- nrow(y_ipsi_M1)/nrow(l_M1)
        bw_y_M1 <- sd(l_M1$y)/kernel
        y_ipsi_1d_density <- y_ipsi_1d_density +
          stat_density(data = y_ipsi_M1, aes(x = y, y=stat(density)*scale_ipsi_M1), bw = bw_y_M1, kernel = "gaussian", geom = "line", colour = ext_colors[indice_col_ext], position = "identity", size = 1, show.legend = FALSE) 
      } 
      if (nrow(y_ipsi_M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        l_M2 <-  data %>% filter(muscle == m2) 
        scale_ipsi_M2 <- nrow(y_ipsi_M2)/nrow(l_M2)
        bw_y_M2 <- sd(l_M2$y)/kernel
        y_ipsi_1d_density <- y_ipsi_1d_density +
          stat_density(data = y_ipsi_M2, aes(x = y, y=stat(density)*scale_ipsi_M2), bw = bw_y_M2, kernel = "gaussian", geom = "line", colour = flex_colors[indice_col_flex], position = "identity", size = 1, show.legend = FALSE)
      }
      
    }
  }
  
  y_ipsi_1d_density <- y_ipsi_1d_density +
    scale_x_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    scale_y_continuous(limits= c(0, NA), expand = c(0, 0, .2, 0)) +
    theme_classic() +
    ylab("") +  
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate()
  y_ipsi_1d_density
}
y_ipsi_1d_density_concatenate_by_titer <- function(data_all,  titer_used, m1, m2, color_pal1, color_pal2, nb_titers_ext, nb_titers_flex, method_used, kernel){
  
  if (nb_titers_ext >= 1) {
    ext_colors <- color_pal1(nb_titers_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_titers_flex >= 1) {
    flex_colors <- color_pal2(nb_titers_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  data <- data_all %>% filter(titer == titer_used[1] & identity == "IN" & method == method_used)
  
  y_ipsi_M1 <- data %>% filter(x >= 0, muscle == m1)
  y_ipsi_M2 <- data %>% filter(x >= 0, muscle == m2)
  if (nrow(y_ipsi_M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(y_ipsi_M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_ipsi_M1 <- nrow(y_ipsi_M1)/nrow(l_M1)
  scale_ipsi_M2 <- nrow(y_ipsi_M2)/nrow(l_M2)
  bw_y_M1 <- sd(l_M1$y)/kernel
  bw_y_M2 <- sd(l_M2$y)/kernel
  
  y_ipsi_1d_density <- ggplot() +
    stat_density(data = y_ipsi_M1, aes(x = y, y=stat(density)*scale_ipsi_M1), bw = bw_y_M1, kernel = "gaussian", geom = "line", colour = ext_colors[1], position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = y_ipsi_M2, aes(x = y, y=stat(density)*scale_ipsi_M2), bw = bw_y_M2, kernel = "gaussian", geom = "line", colour = flex_colors[1], position = "identity", size = 1, show.legend = FALSE)
  
  if (2 <= length(titer_used)) { 
    for (i in 2:length(titer_used)) {
      data <- data_all %>% filter(titer == titer_used[i] & identity == "IN" & method == method_used)
      y_ipsi_M1 <- data %>% filter(x >= 0, muscle == m1)
      y_ipsi_M2 <- data %>% filter(x >= 0, muscle == m2)
      if (nrow(y_ipsi_M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        l_M1 <-  data %>% filter(muscle == m1)
        scale_ipsi_M1 <- nrow(y_ipsi_M1)/nrow(l_M1)
        bw_y_M1 <- sd(l_M1$y)/kernel
        y_ipsi_1d_density <- y_ipsi_1d_density +
          stat_density(data = y_ipsi_M1, aes(x = y, y=stat(density)*scale_ipsi_M1), bw = bw_y_M1, kernel = "gaussian", geom = "line", colour = ext_colors[indice_col_ext], position = "identity", size = 1, show.legend = FALSE) 
      } 
      if (nrow(y_ipsi_M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        l_M2 <-  data %>% filter(muscle == m2) 
        scale_ipsi_M2 <- nrow(y_ipsi_M2)/nrow(l_M2)
        bw_y_M2 <- sd(l_M2$y)/kernel
        y_ipsi_1d_density <- y_ipsi_1d_density +
          stat_density(data = y_ipsi_M2, aes(x = y, y=stat(density)*scale_ipsi_M2), bw = bw_y_M2, kernel = "gaussian", geom = "line", colour = flex_colors[indice_col_flex], position = "identity", size = 1, show.legend = FALSE)
      }
      
    }
  }
  
  y_ipsi_1d_density <- y_ipsi_1d_density +
    scale_x_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    scale_y_continuous(limits= c(0, NA), expand = c(0, 0, .2, 0)) +
    theme_classic() +
    ylab("") +  
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate()
  y_ipsi_1d_density
}

xy_density_arrange_1d <- function(x_dorsal, y_contra, p, y_ipsi, x_ventral, panel){
  p_arrange <- ggarrange(NULL, NULL,x_dorsal ,NULL,NULL,
                         NULL,NULL,NULL,NULL,NULL,
                         y_contra,NULL,p,NULL, y_ipsi,
                         NULL,NULL,NULL,NULL,NULL,
                         NULL,NULL, x_ventral ,NULL,NULL,
                         ncol = 5, nrow = 5,  align = "hv",
                         widths = c(1.5,-0.6,3.5,-0.6,1.5), heights = c(1.5,-0.55,3,-0.55,1.5),
                         common.legend = TRUE, labels = as.character(panel), font.label = list(size = 28, color = "black")) 
  
  return(p_arrange)
}

complete_graph_xy_density <- function(spinal, density, concatenate, data_all, Animal, m1, m2, color_pal1, color_pal2, color1, color2, nb_samples_ext, 
                                      nb_samples_flex, IN, MN, Size, name_M1, name_M2, kernel, bin, method_used, titer_used, panel){
  xlimit <- 0
  if (concatenate == 1) {
    if (density == 0 & length(method_used) == 1 & length(titer_used) == 1){
      p <- xy_dot_density_center_concatenate(spinal, data_all, Animal, m1, m2, color1, color2, IN, MN, Size) 
      x_dorsal <- x_dorsal_1d_density_concatenate(data_all, Animal, m1, m2, color1, color2, kernel, xlimit)
      x_ventral <- x_ventral_1d_density_concatenate(data_all, Animal, m1, m2, color1, color2, kernel, xlimit)
      xlimits1 <- layer_scales(x_dorsal)$y$get_limits() #to get the xlim of the axis
      xlimit1 <- max(xlimits1)
      xlimits2 <- layer_scales(x_ventral)$y$get_limits() #to get the xlim of the axis
      xlimit2 <- max(- xlimits2)
      if (xlimit2 < xlimit1) {
        x_ventral <- x_ventral_1d_density_concatenate(data_all, Animal, m1, m2, color1, color2, kernel, xlimit1)
        x_dorsal <- x_dorsal +
          annotation_custom(textGrob(as.character(name_M1), gp = gpar(col = as.character(color1), fontface = 'bold', fontsize = 20)), 
                            xmin = -700, xmax = -600, ymin = 0.00, ymax = xlimit1) +
          annotation_custom(textGrob(as.character(name_M2), gp = gpar(col = as.character(color2), fontface = 'bold', fontsize = 20)), 
                            xmin = -400, xmax = -300, ymin = 0.00, ymax = xlimit1)
      }
      if (xlimit1 < xlimit2){
        x_dorsal <- x_dorsal_1d_density_concatenate(data_all, Animal, m1, m2, color1, color2, kernel, xlimit2) +
          annotation_custom(textGrob(as.character(name_M1), gp = gpar(col = as.character(color1), fontface = 'bold', fontsize = 20)), 
                            xmin = -700, xmax = -600, ymin = 0.00, ymax = xlimit2) +
          annotation_custom(textGrob(as.character(name_M2), gp = gpar(col = as.character(color2), fontface = 'bold', fontsize = 20)), 
                            xmin = -400, xmax = -300, ymin = 0.00, ymax = xlimit2)
      }
      y_ipsi <- y_ipsi_1d_density_concatenate(data_all, Animal, m1, m2, color1, color2, kernel)
      ylimits <- layer_scales(y_ipsi)$y$get_limits() #to get the ylim of the axis
      ylimit <- max(ylimits)
      y_contra <- y_contra_1d_density_concatenate(data_all, Animal, m1, m2, color1, color2, kernel, ylimit)
    }
    
    if (density == 0 & length(method_used) > 1 & length(titer_used) == 1){
      p <- xy_dot_density_center_concatenate_by_method(spinal, data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, Size, method_used) 
      x_ventral <- x_ventral_1d_density_concatenate_by_method(data_all, method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, xlimit)
      x_dorsal <- x_dorsal_1d_density_concatenate_by_method(data_all, method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, xlimit)
      xlimits1 <- layer_scales(x_dorsal)$y$get_limits() #to get the xlim of the axis
      xlimit1 <- max(xlimits1)
      xlimits2 <- layer_scales(x_ventral)$y$get_limits() #to get the xlim of the axis
      xlimit2 <- max(- xlimits2)
      if (xlimit2 < xlimit1) {
        x_ventral <- x_ventral_1d_density_concatenate_by_method(data_all, method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, xlimit1) 
      }
      if (xlimit1 < xlimit2){
        x_dorsal <- x_dorsal_1d_density_concatenate_by_method(data_all, method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, xlimit2)
      }
      y_ipsi <- y_ipsi_1d_density_concatenate_by_method(data_all, method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel)
      ylimits <- layer_scales(y_ipsi)$y$get_limits() #to get the ylim of the axis
      ylimit <-  max(ylimits)
      y_contra <- y_contra_1d_density_concatenate_by_method(data_all, method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, ylimit)
    }
    
    if (density == 0 & length(method_used) > 1 & length(titer_used) > 1){
      data_all$method <- str_c(data_all$method, "_", data_all$titer)
      method_used <- paste(rep(method_used, each = length(titer_used)), titer_used, sep = "_")
      p <- xy_dot_density_center_concatenate_by_method(spinal, data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, Size, method_used) 
      x_ventral <- x_ventral_1d_density_concatenate_by_method(data_all, method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, xlimit)
      x_dorsal <- x_dorsal_1d_density_concatenate_by_method(data_all, method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, xlimit)
      xlimits1 <- layer_scales(x_dorsal)$y$get_limits() #to get the xlim of the axis
      xlimit1 <- max(xlimits1)
      xlimits2 <- layer_scales(x_ventral)$y$get_limits() #to get the xlim of the axis
      xlimit2 <- max(- xlimits2)
      if (xlimit2 < xlimit1) {
        x_ventral <- x_ventral_1d_density_concatenate_by_method(data_all, method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, xlimit1) 
      }
      if (xlimit1 < xlimit2){
        x_dorsal <- x_dorsal_1d_density_concatenate_by_method(data_all, method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, xlimit2)
      }
      y_ipsi <- y_ipsi_1d_density_concatenate_by_method(data_all, method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel)
      ylimits <- layer_scales(y_ipsi)$y$get_limits() #to get the ylim of the axis
      ylimit <-  max(ylimits)
      y_contra <- y_contra_1d_density_concatenate_by_method(data_all, method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, ylimit)
    }
    if (density == 0 & length(method_used) == 1 & length(titer_used) > 1){
      p <- xy_dot_density_center_concatenate_by_titer(spinal, data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, Size, titer_used, method_used) 
      x_ventral <- x_ventral_1d_density_concatenate_by_titer(data_all,  titer_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, color2, method_used, kernel, xlimit)
      x_dorsal <- x_dorsal_1d_density_concatenate_by_titer(data_all,  titer_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, color1, method_used, kernel, xlimit)
      xlimits1 <- layer_scales(x_dorsal)$y$get_limits() #to get the xlim of the axis
      xlimit1 <- max(xlimits1)
      xlimits2 <- layer_scales(x_ventral)$y$get_limits() #to get the xlim of the axis
      xlimit2 <- max(- xlimits2)
      if (xlimit2 < xlimit1) {
        x_ventral <- x_ventral_1d_density_concatenate_by_titer(data_all, titer_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, color2, method_used, kernel, xlimit1) +
          annotation_custom(textGrob(as.character(name_M2), gp = gpar(col = as.character(color2), fontsize = 16)), 
                            xmin = -700, xmax = -200, ymin = 0.00, ymax = - xlimit1 * 1.2)
        x_dorsal <- x_dorsal +
          annotation_custom(textGrob(as.character(name_M1), gp = gpar(col = as.character(color1), fontsize = 16)), 
                            xmin = -700, xmax = -200, ymin = 0.00, ymax = xlimit1)
      }
      if (xlimit1 < xlimit2){
        x_dorsal <- x_dorsal_1d_density_concatenate_by_titer(data_all, titer_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, color1, method_used, kernel, xlimit2) +
          annotation_custom(textGrob(as.character(name_M1), gp = gpar(col = as.character(color1), fontsize = 16)), 
                            xmin = -700, xmax = -200, ymin = 0.00, ymax = xlimit2)
        x_ventral <- x_ventral +
          annotation_custom(textGrob(as.character(name_M2), gp = gpar(col = as.character(color2), fontsize = 16)), 
                            xmin = -700, xmax = -200, ymin = 0.00, ymax = - xlimit2 * 1.2)
        
      }
      y_ipsi <- y_ipsi_1d_density_concatenate_by_titer(data_all, titer_used, m1, m2, color_pal1, color_pal2,nb_samples_ext, nb_samples_flex, method_used, kernel)
      ylimits <- layer_scales(y_ipsi)$y$get_limits() #to get the ylim of the axis
      ylimit <-  max(ylimits)
      y_contra <- y_contra_1d_density_concatenate_by_titer(data_all,  titer_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, method_used, kernel, ylimit)
    }
    
    if (density == 1 & length(method_used) == 1 & length(titer_used) == 1){
      p <- density_kernel_xy_concatenate(spinal, data_all, Animal, m1, m2, color1, color2, IN, bin) 
      x_ventral <- x_ventral_1d_density_concatenate(data_all, Animal, m1, m2, color1, color2, kernel, xlimit)
      x_dorsal <- x_dorsal_1d_density_concatenate(data_all, Animal, m1, m2, color1, color2, kernel, xlimit)
      xlimits1 <- layer_scales(x_dorsal)$y$get_limits() #to get the xlim of the axis
      xlimit1 <- max(xlimits1)
      xlimits2 <- layer_scales(x_ventral)$y$get_limits() #to get the xlim of the axis
      xlimit2 <- max(- xlimits2)
      if (xlimit2 < xlimit1) {
        x_ventral <- x_ventral_1d_density_concatenate(data_all, Animal, m1, m2, color1, color2, kernel,xlimit1)
        x_dorsal <- x_dorsal +
          annotation_custom(textGrob(as.character(name_M1), gp = gpar(col = as.character(color1), fontface = 'bold', fontsize = 20)), 
                            xmin = -700, xmax = -600, ymin = 0.00, ymax = xlimit1) +
          annotation_custom(textGrob(as.character(name_M2), gp = gpar(col = as.character(color2), fontface = 'bold', fontsize = 20)), 
                            xmin = -400, xmax = -300, ymin = 0.00, ymax = xlimit1)
      }
      if (xlimit1 < xlimit2){
        x_dorsal <- x_dorsal_1d_density_concatenate(data_all, Animal, m1, m2, color1, color2, kernel, xlimit2) +
          annotation_custom(textGrob(as.character(name_M1), gp = gpar(col = as.character(color1), fontface = 'bold', fontsize = 20)), 
                            xmin = -700, xmax = -600, ymin = 0.00, ymax = xlimit2) +
          annotation_custom(textGrob(as.character(name_M2), gp = gpar(col = as.character(color2), fontface = 'bold', fontsize = 20)), 
                            xmin = -400, xmax = -300, ymin = 0.00, ymax = xlimit2)
      }
      y_ipsi <- y_ipsi_1d_density_concatenate(data_all, Animal, m1, m2, color1, color2, kernel)
      ylimits <- layer_scales(y_ipsi)$y$get_limits() #to get the ylim of the axis
      ylimit <-  max(ylimits)
      y_contra <- y_contra_1d_density_concatenate(data_all, Animal, m1, m2, color1, color2, kernel, ylimit)
    }
    
    if (density == 1 & length(method_used) > 1 & length(titer_used) == 1){
      p <- density_kernel_xy_by_method(spinal, data_all, method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, bin) 
      x_ventral <- x_ventral_1d_density_concatenate_by_method(data_all,  method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, xlimit)
      x_dorsal <- x_dorsal_1d_density_concatenate_by_method(data_all,  method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, xlimit)
      xlimits1 <- layer_scales(x_dorsal)$y$get_limits() #to get the xlim of the axis
      xlimit1 <- max(xlimits1)
      xlimits2 <- layer_scales(x_ventral)$y$get_limits() #to get the xlim of the axis
      xlimit2 <- max(- xlimits2)
      if (xlimit2 < xlimit1) {
        x_ventral <- x_ventral_1d_density_concatenate_by_method(data_all,  method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, xlimit1) 
      }
      if (xlimit1 < xlimit2){
        x_dorsal <- x_dorsal_1d_density_concatenate_by_method(data_all,  method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, xlimit2)
      }
      y_ipsi <- y_ipsi_1d_density_concatenate_by_method(data_all,  method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel)
      ylimits <- layer_scales(y_ipsi)$y$get_limits() #to get the ylim of the axis
      ylimit <-  max(ylimits)
      y_contra <- y_contra_1d_density_concatenate_by_method(data_all,  method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, ylimit)
    }
    
    if (density == 1 & length(method_used) > 1 & length(titer_used) > 1){
      data_all$method <- str_c(data_all$method, "_", data_all$titer)
      method_used <- paste(rep(method_used, each = length(titer_used)), titer_used, sep = "_")
      p <- density_kernel_xy_by_method(spinal, data_all, method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, bin) 
      x_ventral <- x_ventral_1d_density_concatenate_by_method(data_all, method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, xlimit)
      x_dorsal <- x_dorsal_1d_density_concatenate_by_method(data_all, method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, xlimit)
      xlimits1 <- layer_scales(x_dorsal)$y$get_limits() #to get the xlim of the axis
      xlimit1 <- max(xlimits1)
      xlimits2 <- layer_scales(x_ventral)$y$get_limits() #to get the xlim of the axis
      xlimit2 <- max(- xlimits2)
      if (xlimit2 < xlimit1) {
        x_ventral <- x_ventral_1d_density_concatenate_by_method(data_all, method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, xlimit1) 
      }
      if (xlimit1 < xlimit2){
        x_dorsal <- x_dorsal_1d_density_concatenate_by_method(data_all, method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, xlimit2)
      }
      y_ipsi <- y_ipsi_1d_density_concatenate_by_method(data_all, method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel)
      ylimits <- layer_scales(y_ipsi)$y$get_limits() #to get the ylim of the axis
      ylimit <-  max(ylimits)
      y_contra <- y_contra_1d_density_concatenate_by_method(data_all, method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, ylimit)
    }
    
    if (density == 1 & length(method_used) == 1 & length(titer_used) > 1){
      p <- density_kernel_xy_by_titer(spinal, data_all, titer_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, bin, method_used) 
      x_ventral <- x_ventral_1d_density_concatenate_by_titer(data_all, titer_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, color2, method_used, kernel, xlimit)
      x_dorsal <- x_dorsal_1d_density_concatenate_by_titer(data_all, titer_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, color1, method_used, kernel, xlimit)
      xlimits1 <- layer_scales(x_dorsal)$y$get_limits() #to get the xlim of the axis
      xlimit1 <- max(xlimits1)
      xlimits2 <- layer_scales(x_ventral)$y$get_limits() #to get the xlim of the axis
      xlimit2 <- max(- xlimits2)
      if (xlimit2 < xlimit1) {
        x_ventral <- x_ventral_1d_density_concatenate_by_titer(data_all, titer_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, color2, method_used, kernel, xlimit1) +
          annotation_custom(textGrob(as.character(name_M2), gp = gpar(col = as.character(color2), fontsize = 16)), 
                            xmin = -700, xmax = -200, ymin = 0.00, ymax = - xlimit1 * 1.2)
        x_dorsal <- x_dorsal +
          annotation_custom(textGrob(as.character(name_M1), gp = gpar(col = as.character(color1), fontsize = 16)), 
                            xmin = -700, xmax = -200, ymin = 0.00, ymax = xlimit1)
      }
      if (xlimit1 < xlimit2){
        x_dorsal <- x_dorsal_1d_density_concatenate_by_titer(data_all, titer_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, color1, method_used, kernel, xlimit2) +
          annotation_custom(textGrob(as.character(name_M1), gp = gpar(col = as.character(color1), fontsize = 16)), 
                            xmin = -700, xmax = -200, ymin = 0.00, ymax = xlimit2)
        x_ventral <- x_ventral +
          annotation_custom(textGrob(as.character(name_M2), gp = gpar(col = as.character(color2), fontsize = 16)), 
                            xmin = -700, xmax = -200, ymin = 0.00, ymax = - xlimit2 * 1.2)
        
      }
      y_ipsi <- y_ipsi_1d_density_concatenate_by_titer(data_all, titer_used, m1, m2, color_pal1, color_pal2,nb_samples_ext, nb_samples_flex, method_used, kernel)
      ylimits <- layer_scales(y_ipsi)$y$get_limits() #to get the ylim of the axis
      ylimit <-  max(ylimits)
      y_contra <- y_contra_1d_density_concatenate_by_titer(data_all,  titer_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, method_used, kernel, ylimit)
    }
  }
  
  if (concatenate == 0) {
    if (density == 0){
      p <- xy_dot_density_center(spinal, data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, Size) 
    }
    if (density == 1){
      p <- density_kernel_xy(spinal, data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, bin)
      
    } 
    x_dorsal <- x_dorsal_1d_density(data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, color1, color2, name_M1, name_M2, kernel, xlimit)
    x_ventral <- x_ventral_1d_density(data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, xlimit)
    xlimits1 <- layer_scales(x_dorsal)$y$get_limits() #to get the xlim of the axis
    xlimit1 <- max(xlimits1)
    xlimits2 <- layer_scales(x_ventral)$y$get_limits() #to get the xlim of the axis
    xlimit2 <- max(- xlimits2)
    if (xlimit2 < xlimit1) {
      x_ventral <- x_ventral_1d_density(data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, xlimit1)
      x_dorsal <- x_dorsal +
        annotation_custom(textGrob(as.character(name_M1), gp = gpar(col = as.character(color1), fontface = 'bold', fontsize = 20)), 
                          xmin = -700, xmax = -600, ymin = 0.00, ymax = xlimit1) +
        annotation_custom(textGrob(as.character(name_M2), gp = gpar(col = as.character(color2), fontface = 'bold', fontsize = 20)), 
                          xmin = -400, xmax = -300, ymin = 0.00, ymax = xlimit1)
    }
    if (xlimit1 < xlimit2){
      x_dorsal <- x_dorsal_1d_density(data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, color1, color2, name_M1, name_M2, kernel, xlimit2) +
        annotation_custom(textGrob(as.character(name_M1), gp = gpar(col = as.character(color1), fontface = 'bold', fontsize = 20)), 
                          xmin = -700, xmax = -600, ymin = 0.00, ymax = xlimit2) +
        annotation_custom(textGrob(as.character(name_M2), gp = gpar(col = as.character(color2), fontface = 'bold', fontsize = 20)), 
                          xmin = -400, xmax = -300, ymin = 0.00, ymax = xlimit2)
    }
    y_ipsi <- y_ipsi_1d_density(data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel)
    ylimits <- layer_scales(y_ipsi)$y$get_limits() #to get the ylim of the axis
    ylimit <-  max(ylimits)
    y_contra <- y_contra_1d_density(data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, ylimit)
  }
  graph <- xy_density_arrange_1d(x_dorsal, y_contra, p, y_ipsi, x_ventral, panel)
  graph
}

## Functions kernel 2d density xy ##
density_kernel_xy <- function (spinal, data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, bin) {
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 1
  indice_col_flex <- 1
  M1 <-  data_all %>% filter(animal == Animal[1] & muscle == m1 & identity == IN) 
  M2 <-  data_all %>% filter(animal == Animal[1] & muscle == m2 & identity == IN)
  
  
  p <- ggplot() +
    annotate("point", x = 0, y = 0, size = 4, colour = "black") +
    geom_path(data = spinal, aes(x = V1, y = V2), size = 1.5) + 
    geom_density_2d(data = M1, aes(x = x, y = y), bins = bin,  h=c(width.SJ(M1$x), width.SJ(M1$y)), colour = ext_colors[as.numeric(indice_col_ext)], show.legend = FALSE) +
    geom_density_2d(data = M2, aes(x = x, y = y), bins = bin,  h=c(width.SJ(M1$x), width.SJ(M1$y)), colour = flex_colors[as.numeric(indice_col_flex)], show.legend = FALSE) 
  
  if (2 <= length(Animal)) { 
    for (i in 2:length(Animal)) {
      M1 <-  data_all %>% filter(animal == Animal[i] & muscle == m1 & identity == IN) 
      M2 <-  data_all %>% filter(animal == Animal[i] & muscle == m2 & identity == IN)
      if (nrow(M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        p <- p +
          geom_density_2d(data = M1, aes(x = x, y = y), bins = bin,  h=c(width.SJ(M1$x), width.SJ(M1$y)), colour = ext_colors[as.numeric(indice_col_ext)], show.legend = FALSE) 
      } 
      if (nrow(M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        p <- p +
          geom_density_2d(data = M2, aes(x = x, y = y), bins = bin,  h=c(width.SJ(M1$x), width.SJ(M1$y)), colour = flex_colors[as.numeric(indice_col_flex)], show.legend = FALSE) 
      }
      
    }
  }    
  
  p <- p + 
    scale_y_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    ylab("") +  
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"))
  
  p
}
density_kernel_xy_by_method <- function (spinal, data_all, method_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, bin) {
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  M1 <-  data_all %>% filter(method == method_used[1] & muscle == m1 & identity == IN) 
  M2 <-  data_all %>% filter(method == method_used[1] & muscle == m2 & identity == IN)
  if (nrow(M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  
  p <- ggplot() +
    annotate("point", x = 0, y = 0, size = 4, colour = "black") +
    geom_path(data = spinal, aes(x = V1, y = V2), size = 1.5) + 
    geom_density_2d(data = M1, aes(x = x, y = y), bins = bin,  h=c(width.SJ(M1$x), width.SJ(M1$y)), colour = ext_colors[1], show.legend = FALSE) +
    geom_density_2d(data = M2, aes(x = x, y = y), bins = bin,  h=c(width.SJ(M1$x), width.SJ(M1$y)), colour = flex_colors[1], show.legend = FALSE) 
  
  if (2 <= length(method_used)) { 
    for (i in 2:length(method_used)) {
      M1 <-  data_all %>% filter(method == method_used[i] & muscle == m1 & identity == IN) 
      M2 <-  data_all %>% filter(method == method_used[i] & muscle == m2 & identity == IN)
      if (nrow(M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        p <- p +
          geom_density_2d(data = M1, aes(x = x, y = y), bins = bin,  h=c(width.SJ(M1$x), width.SJ(M1$y)), colour = ext_colors[as.numeric(indice_col_ext)], show.legend = FALSE) 
      } 
      if (nrow(M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        p <- p +
          geom_density_2d(data = M2, aes(x = x, y = y), bins = bin,  h=c(width.SJ(M1$x), width.SJ(M1$y)), colour = flex_colors[as.numeric(indice_col_flex)], show.legend = FALSE) 
      }
      
    }
  }    
  
  p <- p + 
    scale_y_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    ylab("") +  
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"))
  
  p
}
density_kernel_xy_concatenate <- function (spinal, data_all, Animal, m1, m2, color1, color2, IN, bin) {
  
  
  M1 <-  data_all %>% filter(animal == Animal[1] & muscle == m1 & identity == IN) 
  M2 <-  data_all %>% filter(animal == Animal[1] & muscle == m2 & identity == IN)
  
  if (2 <= length(Animal)) { 
    for (i in 2:length(Animal)) {
      df1 <-  data_all %>% filter(animal == Animal[i] & muscle == m1 & identity == IN) 
      df2 <-  data_all %>% filter(animal == Animal[i] & muscle == m2 & identity == IN)
      M1 <- rbind(M1, df1)
      M2 <- rbind(M2, df2)
    }
    
  }
  
  p <- ggplot() +
    annotate("point", x = 0, y = 0, size = 4, colour = "black") +
    geom_path(data = spinal, aes(x = V1, y = V2), size = 1.5) + 
    geom_density_2d(data = M1, aes(x = x, y = y), bins = bin,  h=c(width.SJ(M1$x), width.SJ(M1$y)), colour = as.character(color1), show.legend = FALSE) +
    geom_density_2d(data = M2, aes(x = x, y = y), bins = bin,  h=c(width.SJ(M1$x), width.SJ(M1$y)), colour = as.character(color2), show.legend = FALSE) + 
    scale_y_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    ylab("") +  
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"))
  
  p
}
density_kernel_xy_by_titer <- function (spinal, data_all, titer_used, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, bin, method_used) {
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  M1 <-  data_all %>% filter(titer == titer_used[1] & muscle == m1 & identity == IN & method == method_used) 
  M2 <-  data_all %>% filter(titer == titer_used[1] & muscle == m2 & identity == IN & method == method_used)
  if (nrow(M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  
  
  p <- ggplot() +
    annotate("point", x = 0, y = 0, size = 4, colour = "black") +
    geom_path(data = spinal, aes(x = V1, y = V2), size = 1.5) + 
    geom_density_2d(data = M1, aes(x = x, y = y), bins = bin,  h=c(width.SJ(M1$x), width.SJ(M1$y)), colour = ext_colors[1], show.legend = FALSE) +
    geom_density_2d(data = M2, aes(x = x, y = y), bins = bin,  h=c(width.SJ(M1$x), width.SJ(M1$y)), colour = flex_colors[1], show.legend = FALSE) 
  
  if (2 <= length(titer_used)) { 
    for (i in 2:length(titer_used)) {
      M1 <-  data_all %>% filter(titer == titer_used[i] & muscle == m1 & identity == IN & method == method_used) 
      M2 <-  data_all %>% filter(titer == titer_used[i] & muscle == m2 & identity == IN & method == method_used)
      if (nrow(M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        p <- p +
          geom_density_2d(data = M1, aes(x = x, y = y), bins = bin,  h=c(width.SJ(M1$x), width.SJ(M1$y)), colour = ext_colors[as.numeric(indice_col_ext)], show.legend = FALSE) 
      } 
      if (nrow(M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        p <- p +
          geom_density_2d(data = M2, aes(x = x, y = y), bins = bin,  h=c(width.SJ(M1$x), width.SJ(M1$y)), colour = flex_colors[as.numeric(indice_col_flex)], show.legend = FALSE) 
      }
      
    }
  }    
  
  p <- p + 
    scale_y_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    ylab("") +  
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"))
  
  p
}

## Functions to plot xz with interactive file ##
xz_dot_density_center <- function(spinal_rc, spinal_rc_contra, data, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, Size) {
  
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  
  M1 <-  data %>% filter(animal == Animal[1] & muscle == m1 & identity == IN | animal == Animal[1] & muscle == m1 & identity == MN) 
  M2 <-  data %>% filter(animal == Animal[1] & muscle == m2 & identity == IN | animal == Animal[1] & muscle == m2 & identity == MN)
  if (nrow(M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  p <- ggplot() +
    geom_path(data = spinal_rc, aes(x = V1, y = V2), size = 1.5) +
    geom_path(data = spinal_rc_contra, aes(x = V1, y = V2), size = 1.5) +
    geom_point(data = M1, aes(x = x, y = - z, shape = identity, size = identity, alpha = identity), colour = ext_colors[1], show.legend = FALSE) +
    geom_point(data = M2, aes(x = x, y = - z, shape = identity, size = identity, alpha = identity), colour = flex_colors[1], show.legend = FALSE) 
  
  if (2 <= length(Animal)) { 
    for (i in 2:length(Animal)) {
      M1 <-  data %>% filter(animal == Animal[i] & muscle == m1 & identity == IN | animal == Animal[i] & muscle == m1 & identity == MN) 
      M2 <-  data %>% filter(animal == Animal[i] & muscle == m2 & identity == IN | animal == Animal[i] & muscle == m2 & identity == MN)
      c <- c + nrow(M1) + nrow(M2)
      if (nrow(M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        p <- p +
          geom_point(data = M1, aes(x = x, y = -z, shape = identity, size = identity, alpha = identity), colour = ext_colors[as.numeric(indice_col_ext)], show.legend = FALSE) 
      } 
      if (nrow(M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        p <- p +
          geom_point(data = M2, aes(x = x, y = -z, shape = identity, size = identity, alpha = identity), colour = flex_colors[as.numeric(indice_col_flex)], show.legend = FALSE) 
      }
      
    }
  }
  
  p <- p +
    scale_size_manual(values = Size) +
    scale_alpha_manual(values = c(0.6, 1)) +
    scale_y_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    ylab("") +  
    xlab("M-L (μm)") +
    geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
    geom_segment(aes(x=0, xend= 0, y=-2500, yend = 4000), linetype="dashed", color = "black", size=0.5) +
    theme(axis.title.x = element_text(vjust=16), axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent")) + 
    annotation_custom(textGrob("L4", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = -600, xmax = -500, ymin = 200 , ymax = 400) +
    annotation_custom(textGrob("L5", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = -600, xmax = -500, ymin = -200, ymax = -400)
  
  
  p
}
xz_dot_density_center_concatenate <- function(spinal_rc, spinal_rc_contra, data, Animal, m1, m2, color1, color2, IN, MN, Size) {
  
  
  M1 <-  data %>% filter(animal == Animal[1] & muscle == m1 & identity == IN | animal == Animal[1] & muscle == m1 & identity == MN) 
  M2 <-  data %>% filter(animal == Animal[1] & muscle == m2 & identity == IN | animal == Animal[1] & muscle == m2 & identity == MN)
  
  p <- ggplot() +
    geom_path(data = spinal_rc, aes(x = V1, y = V2), size = 1.5) +
    geom_path(data = spinal_rc_contra, aes(x = V1, y = V2), size = 1.5) +
    geom_point(data = M1, aes(x = x, y = - z, shape = identity, size = identity, alpha = identity), colour = color1, show.legend = FALSE) +
    geom_point(data = M2, aes(x = x, y = - z, shape = identity, size = identity, alpha = identity), colour = color2, show.legend = FALSE) 
  
  if (2 <= length(Animal)) { 
    for (i in 2:length(Animal)) {
      M1 <-  data %>% filter(animal == Animal[i] & muscle == m1 & identity == IN | animal == Animal[i] & muscle == m1 & identity == MN) 
      M2 <-  data %>% filter(animal == Animal[i] & muscle == m2 & identity == IN | animal == Animal[i] & muscle == m2 & identity == MN)
      if (nrow(M1) >= 1) {
        p <- p +
          geom_point(data = M1, aes(x = x, y = - z, shape = identity, size = identity, alpha = identity), colour = color1, show.legend = FALSE) 
      } 
      if (nrow(M2) >= 1) {  
        p <- p +
          geom_point(data = M2, aes(x = x, y = - z, shape = identity, size = identity, alpha = identity), colour = color2, show.legend = FALSE) 
      }
      
    }
  }
  
  p <- p +
    scale_size_manual(values = Size) +
    scale_alpha_manual(values = c(0.6, 1)) +
    scale_y_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    ylab("") +  
    xlab("M-L (μm)") +
    geom_segment(aes(x=0, xend= 0, y=-2500, yend = 4000), linetype="dashed", color = "black", size=0.5) +
    geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
    theme(axis.title.x = element_text(vjust=16), axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent")) +
    annotation_custom(textGrob("L4", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = -600, xmax = -500, ymin = 200 , ymax = 400) +
    annotation_custom(textGrob("L5", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = -600, xmax = -500, ymin = -200, ymax = -400)
  
  p
}
xz_dot_density_center_concatenate_by_method <- function(spinal_rc, spinal_rc_contra, data, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, Size, method_used) {
  
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  
  M1 <-  data %>% filter(method == method_used[1] & muscle == m1 & identity == IN | method == method_used[1] & muscle == m1 & identity == MN) 
  M2 <-  data %>% filter(method == method_used[1] & muscle == m2 & identity == IN | method == method_used[1] & muscle == m2 & identity == MN)
  if (nrow(M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  p <- ggplot() +
    geom_path(data = spinal_rc, aes(x = V1, y = V2), size = 1.5) +
    geom_path(data = spinal_rc_contra, aes(x = V1, y = V2), size = 1.5) +
    geom_point(data = M1, aes(x = x, y = - z, shape = identity, size = identity, alpha = identity), colour = ext_colors[1], show.legend = FALSE) +
    geom_point(data = M2, aes(x = x, y = - z, shape = identity, size = identity, alpha = identity), colour = flex_colors[1], show.legend = FALSE) 
  
  if (2 <= length(method_used)) { 
    for (i in 2:length(method_used)) {
      M1 <-  data %>% filter(method == method_used[i] & muscle == m1 & identity == IN | method == method_used[i] & muscle == m1 & identity == MN) 
      M2 <-  data %>% filter(method == method_used[i] & muscle == m2 & identity == IN | method == method_used[i] & muscle == m2 & identity == MN)
      if (nrow(M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        p <- p +
          geom_point(data = M1, aes(x = x, y = -z, shape = identity, size = identity, alpha = identity), colour = ext_colors[as.numeric(indice_col_ext)], show.legend = FALSE) 
      } 
      if (nrow(M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        p <- p +
          geom_point(data = M2, aes(x = x, y = -z, shape = identity, size = identity, alpha = identity), colour = flex_colors[as.numeric(indice_col_flex)], show.legend = FALSE) 
      }
      
    }
  }
  
  p <- p +
    scale_size_manual(values = Size) +
    scale_alpha_manual(values = c(0.6, 1)) +
    scale_y_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    ylab("") +  
    xlab("M-L (μm)") +
    geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
    geom_segment(aes(x=0, xend= 0, y=-2500, yend = 4000), linetype="dashed", color = "black", size=0.5) +
    theme(axis.title.x = element_text(vjust=16), axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent")) + 
    annotation_custom(textGrob("L4", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = -600, xmax = -500, ymin = 200 , ymax = 400) +
    annotation_custom(textGrob("L5", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = -600, xmax = -500, ymin = -200, ymax = -400)
  
  
  p
}
xz_dot_density_center_concatenate_by_titer <- function(spinal_rc, spinal_rc_contra, data, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, Size, titer_used, method_used) {
  
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  
  M1 <-  data %>% filter(method == method_used & titer == titer_used[1] & muscle == m1 & identity == IN | method == method_used & titer == titer_used[1] & muscle == m1 & identity == MN) 
  M2 <-  data %>% filter(method == method_used & titer == titer_used[1] & muscle == m2 & identity == IN | method == method_used & titer == titer_used[1] & muscle == m2 & identity == MN)
  if (nrow(M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  p <- ggplot() +
    geom_path(data = spinal_rc, aes(x = V1, y = V2), size = 1.5) +
    geom_path(data = spinal_rc_contra, aes(x = V1, y = V2), size = 1.5) +
    geom_point(data = M1, aes(x = x, y = - z, shape = identity, size = identity, alpha = identity), colour = ext_colors[1], show.legend = FALSE) +
    geom_point(data = M2, aes(x = x, y = - z, shape = identity, size = identity, alpha = identity), colour = flex_colors[1], show.legend = FALSE) 
  
  if (2 <= length(titer_used)) { 
    for (i in 2:length(titer_used)) {
      M1 <-  data %>% filter(method == method_used & titer == titer_used[i] & muscle == m1 & identity == IN | method == method_used & titer == titer_used[i] & muscle == m1 & identity == MN) 
      M2 <-  data %>% filter(method == method_used & titer == titer_used[i] & muscle == m2 & identity == IN | method == method_used & titer == titer_used[i] & muscle == m2 & identity == MN)
      if (nrow(M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        p <- p +
          geom_point(data = M1, aes(x = x, y = -z, shape = identity, size = identity, alpha = identity), colour = ext_colors[as.numeric(indice_col_ext)], show.legend = FALSE) 
      } 
      if (nrow(M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        p <- p +
          geom_point(data = M2, aes(x = x, y = -z, shape = identity, size = identity, alpha = identity), colour = flex_colors[as.numeric(indice_col_flex)], show.legend = FALSE) 
      }
      
    }
  }
  
  p <- p +
    scale_size_manual(values = Size) +
    scale_alpha_manual(values = c(0.6, 1)) +
    scale_y_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    ylab("") +  
    xlab("M-L (μm)") +
    geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
    geom_segment(aes(x=0, xend= 0, y=-2500, yend = 4000), linetype="dashed", color = "black", size=0.5) +
    theme(axis.title.x = element_text(vjust=16), axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent")) + 
    annotation_custom(textGrob("L4", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = -600, xmax = -500, ymin = 200 , ymax = 400) +
    annotation_custom(textGrob("L5", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = -600, xmax = -500, ymin = -200, ymax = -400)
  
  
  p
}

z_ipsi_1d_density <- function(data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel){
  
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  data <- data_all %>% filter(animal == Animal[1] & identity == "IN")
  
  z_ipsi_M1 <- data %>% filter(x >= 0, muscle == m1)
  z_ipsi_M2 <- data %>% filter(x >= 0, muscle == m2)
  if (nrow(z_ipsi_M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(z_ipsi_M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_ipsi_M1 <- nrow(z_ipsi_M1)/nrow(l_M1)
  scale_ipsi_M2 <- nrow(z_ipsi_M2)/nrow(l_M2)
  bw_z_M1 <- sd(l_M1$z)/kernel
  bw_z_M2 <- sd(l_M2$z)/kernel
  
  z_ipsi_1d_density <- ggplot() +
    stat_density(data = z_ipsi_M1, aes(x = - z, y=stat(density)*scale_ipsi_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = ext_colors[1], position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = z_ipsi_M2, aes(x = - z, y=stat(density)*scale_ipsi_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = flex_colors[1], position = "identity", size = 1, show.legend = FALSE)
  
  if (2 <= length(Animal)) { 
    for (i in 2:length(Animal)) {
      data <- data_all %>% filter(animal == Animal[i] & identity == "IN")
      z_ipsi_M2 <- data %>% filter(x >= 0, muscle == m2)
      z_ipsi_M1 <- data %>% filter(x >= 0, muscle == m1)
      
      
      if (nrow(z_ipsi_M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        l_M1 <-  data %>% filter(muscle == m1)
        scale_ipsi_M1 <- nrow(z_ipsi_M1)/nrow(l_M1)
        bw_z_M1 <- sd(l_M1$z)/kernel
        z_ipsi_1d_density <- z_ipsi_1d_density +
          stat_density(data = z_ipsi_M1, aes(x = - z, y=stat(density)*scale_ipsi_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = ext_colors[indice_col_ext], position = "identity", size = 1, show.legend = FALSE) 
      } 
      if (nrow(z_ipsi_M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        l_M2 <-  data %>% filter(muscle == m2) 
        scale_ipsi_M2 <- nrow(z_ipsi_M2)/nrow(l_M2)
        bw_z_M2 <- sd(l_M2$z)/kernel
        z_ipsi_1d_density <- z_ipsi_1d_density +
          stat_density(data = z_ipsi_M2, aes(x = - z, y=stat(density)*scale_ipsi_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = flex_colors[indice_col_flex], position = "identity", size = 1, show.legend = FALSE)
      }
      
    }
  }
  
  z_ipsi_1d_density <- z_ipsi_1d_density +
    scale_x_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    scale_y_continuous(limits=c(0, NA), expand = c(0, 0, .2, 0))+
    theme_classic() +
    ylab("") +
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate()
  z_ipsi_1d_density
}
z_ipsi_1d_density_concatenate <- function(data_all, Animal, m1, m2, color1, color2, kernel){
  data <- data_all %>% filter(animal == Animal[1] & identity == "IN")
  if (2 <= length(Animal)) {
    for (i in 2:length(Animal)) {
      df <- data_all %>% filter(animal == Animal[i] & identity == "IN")
      data <- rbind(data, df)
    }
  }
  z_ipsi_M2 <- data %>% filter(x >= 0, muscle == m2)
  z_ipsi_M1 <- data %>% filter(x >= 0, muscle == m1)
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_ipsi_M2 <- nrow(z_ipsi_M2)/nrow(l_M2)
  scale_ipsi_M1 <- nrow(z_ipsi_M1)/nrow(l_M1)
  bw_z_M1 <- sd(l_M1$z)/kernel
  bw_z_M2 <- sd(l_M2$z)/kernel
  
  z_ipsi_1d_density <- ggplot() +
    stat_density(data = z_ipsi_M1, aes(x = -z, y=stat(density)*scale_ipsi_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = color1, position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = z_ipsi_M2, aes(x = -z, y=stat(density)*scale_ipsi_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = color2, position = "identity", size = 1, show.legend = FALSE) +
    scale_x_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    scale_y_continuous(limits=c(0, NA), expand = c(0, 0, .2, 0)) +
    theme_classic() +
    ylab("") +  
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate()
  
  z_ipsi_1d_density
}
z_ipsi_1d_density_concatenate_by_method <- function(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, method_used){
  
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  data <- data_all %>% filter(method == method_used[1] & identity == "IN")
  
  z_ipsi_M1 <- data %>% filter(x >= 0, muscle == m1)
  z_ipsi_M2 <- data %>% filter(x >= 0, muscle == m2)
  if (nrow(z_ipsi_M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(z_ipsi_M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_ipsi_M1 <- nrow(z_ipsi_M1)/nrow(l_M1)
  scale_ipsi_M2 <- nrow(z_ipsi_M2)/nrow(l_M2)
  bw_z_M1 <- sd(l_M1$z)/kernel
  bw_z_M2 <- sd(l_M2$z)/kernel
  
  z_ipsi_1d_density <- ggplot() +
    stat_density(data = z_ipsi_M1, aes(x = - z, y=stat(density)*scale_ipsi_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = ext_colors[1], position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = z_ipsi_M2, aes(x = - z, y=stat(density)*scale_ipsi_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = flex_colors[1], position = "identity", size = 1, show.legend = FALSE)
  
  if (2 <= length(method_used)) { 
    for (i in 2:length(method_used)) {
      data <- data_all %>% filter(method == method_used[i] & identity == "IN")
      z_ipsi_M2 <- data %>% filter(x >= 0, muscle == m2)
      z_ipsi_M1 <- data %>% filter(x >= 0, muscle == m1)
      
      
      if (nrow(z_ipsi_M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        l_M1 <-  data %>% filter(muscle == m1)
        scale_ipsi_M1 <- nrow(z_ipsi_M1)/nrow(l_M1)
        bw_z_M1 <- sd(l_M1$z)/kernel
        z_ipsi_1d_density <- z_ipsi_1d_density +
          stat_density(data = z_ipsi_M1, aes(x = - z, y=stat(density)*scale_ipsi_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = ext_colors[indice_col_ext], position = "identity", size = 1, show.legend = FALSE) 
      } 
      if (nrow(z_ipsi_M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        l_M2 <-  data %>% filter(muscle == m2) 
        scale_ipsi_M2 <- nrow(z_ipsi_M2)/nrow(l_M2)
        bw_z_M2 <- sd(l_M2$z)/kernel
        z_ipsi_1d_density <- z_ipsi_1d_density +
          stat_density(data = z_ipsi_M2, aes(x = - z, y=stat(density)*scale_ipsi_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = flex_colors[indice_col_flex], position = "identity", size = 1, show.legend = FALSE)
      }
      
    }
  }
  
  z_ipsi_1d_density <- z_ipsi_1d_density +
    scale_x_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    scale_y_continuous(limits=c(0, NA), expand = c(0, 0, .2, 0))+
    theme_classic() +
    ylab("") +
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate()
  z_ipsi_1d_density
}
z_ipsi_1d_density_concatenate_by_titer <- function(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, titer_used, method_used){
  
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  data <- data_all %>% filter(method == method_used & titer == titer_used[1] & identity == "IN")
  
  z_ipsi_M1 <- data %>% filter(x >= 0, muscle == m1)
  z_ipsi_M2 <- data %>% filter(x >= 0, muscle == m2)
  if (nrow(z_ipsi_M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(z_ipsi_M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_ipsi_M1 <- nrow(z_ipsi_M1)/nrow(l_M1)
  scale_ipsi_M2 <- nrow(z_ipsi_M2)/nrow(l_M2)
  bw_z_M1 <- sd(l_M1$z)/kernel
  bw_z_M2 <- sd(l_M2$z)/kernel
  
  z_ipsi_1d_density <- ggplot() +
    stat_density(data = z_ipsi_M1, aes(x = - z, y=stat(density)*scale_ipsi_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = ext_colors[1], position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = z_ipsi_M2, aes(x = - z, y=stat(density)*scale_ipsi_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = flex_colors[1], position = "identity", size = 1, show.legend = FALSE)
  
  if (2 <= length(titer_used)) { 
    for (i in 2:length(titer_used)) {
      data <- data_all %>% filter(method == method_used & titer == titer_used[i] & identity == "IN")
      z_ipsi_M2 <- data %>% filter(x >= 0, muscle == m2)
      z_ipsi_M1 <- data %>% filter(x >= 0, muscle == m1)
      
      
      if (nrow(z_ipsi_M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        l_M1 <-  data %>% filter(muscle == m1)
        scale_ipsi_M1 <- nrow(z_ipsi_M1)/nrow(l_M1)
        bw_z_M1 <- sd(l_M1$z)/kernel
        z_ipsi_1d_density <- z_ipsi_1d_density +
          stat_density(data = z_ipsi_M1, aes(x = - z, y=stat(density)*scale_ipsi_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = ext_colors[indice_col_ext], position = "identity", size = 1, show.legend = FALSE) 
      } 
      if (nrow(z_ipsi_M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        l_M2 <-  data %>% filter(muscle == m2) 
        scale_ipsi_M2 <- nrow(z_ipsi_M2)/nrow(l_M2)
        bw_z_M2 <- sd(l_M2$z)/kernel
        z_ipsi_1d_density <- z_ipsi_1d_density +
          stat_density(data = z_ipsi_M2, aes(x = - z, y=stat(density)*scale_ipsi_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = flex_colors[indice_col_flex], position = "identity", size = 1, show.legend = FALSE)
      }
      
    }
  }
  
  z_ipsi_1d_density <- z_ipsi_1d_density +
    scale_x_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    scale_y_continuous(limits=c(0, NA), expand = c(0, 0, .2, 0))+
    theme_classic() +
    ylab("") +
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate()
  z_ipsi_1d_density
}

z_contra_1d_density <- function(data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, ylimit){
  
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  data <- data_all %>% filter(animal == Animal[1] & identity == "IN")
  
  z_contra_M1 <- data %>% filter(x <= 0, muscle == m1)
  z_contra_M2 <- data %>% filter(x <= 0, muscle == m2)
  if (nrow(z_contra_M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(z_contra_M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_contra_M1 <- nrow(z_contra_M1)/nrow(l_M1)
  scale_contra_M2 <- nrow(z_contra_M2)/nrow(l_M2)
  bw_z_M1 <- sd(l_M1$z)/kernel
  bw_z_M2 <- sd(l_M2$z)/kernel
  
  z_contra_1d_density <- ggplot() +
    stat_density(data = z_contra_M1, aes(x = -z, y=stat(density)*scale_contra_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = ext_colors[1], position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = z_contra_M2, aes(x = -z, y=stat(density)*scale_contra_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = flex_colors[1], position = "identity", size = 1, show.legend = FALSE)
  
  if (2 <= length(Animal)) { 
    for (i in 2:length(Animal)) {
      data <- data_all %>% filter(animal == Animal[i] & identity == "IN")
      z_contra_M1 <- data %>% filter(x <= 0, muscle == m1)
      z_contra_M2 <- data %>% filter(x <= 0, muscle == m2)
      
      
      if (nrow(z_contra_M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        l_M1 <-  data %>% filter(muscle == m1)
        scale_contra_M1 <- nrow(z_contra_M1)/nrow(l_M1)
        bw_z_M1 <- sd(l_M1$z)/kernel
        z_contra_1d_density <- z_contra_1d_density +
          stat_density(data = z_contra_M1, aes(x = -z, y=stat(density)*scale_contra_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = ext_colors[indice_col_ext], position = "identity", size = 1, show.legend = FALSE) 
      } 
      if (nrow(z_contra_M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        l_M2 <-  data %>% filter(muscle == m2) 
        scale_contra_M2 <- nrow(z_contra_M2)/nrow(l_M2)
        bw_z_M2 <- sd(l_M2$z)/kernel
        z_contra_1d_density <- z_contra_1d_density +
          stat_density(data = z_contra_M2, aes(x = -z, y=stat(density)*scale_contra_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = flex_colors[indice_col_flex], position = "identity", size = 1, show.legend = FALSE)
      }
      
    }
  }
  
  z_contra_1d_density <- z_contra_1d_density +
    scale_x_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    scale_y_reverse(limits=c(ylimit, 0), expand = c(.2, 0, 0, 0))+
    theme_classic() +
    ylab("") +  
    xlab("C-R (μm)")+
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(), axis.title.y = element_text(vjust=-20, hjust = 0.42), panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate() 
  z_contra_1d_density
}
z_contra_1d_density_concatenate <- function(data_all, Animal, m1, m2, color1, color2, kernel, ylimit){
  data <- data_all %>% filter(animal == Animal[1] & identity == "IN")
  if (2 <= length(Animal)) {
    for (i in 2:length(Animal)) {
      df <- data_all %>% filter(animal == Animal[i] & identity == "IN")
      data <- rbind(data, df)
    }
  }
  z_contra_M2 <- data %>% filter(x <= 0, muscle == m2)
  z_contra_M1 <- data %>% filter(x <= 0, muscle == m1)
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_contra_M2 <- nrow(z_contra_M2)/nrow(l_M2)
  scale_contra_M1 <- nrow(z_contra_M1)/nrow(l_M1)
  bw_z_M1 <- sd(l_M1$z)/kernel
  bw_z_M2 <- sd(l_M2$z)/kernel
  
  z_contra_1d_density <- ggplot() +
    stat_density(data = z_contra_M1, aes(x = -z, y=stat(density)*scale_contra_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = color1, position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = z_contra_M2, aes(x = -z, y=stat(density)*scale_contra_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = color2, position = "identity", size = 1, show.legend = FALSE) +
    scale_x_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    scale_y_reverse(limits=c(ylimit, 0), expand = c(.2, 0, 0, 0))+
    theme_classic() +
    ylab("") +  
    xlab("C-R (μm)") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(), axis.title.y = element_text(vjust=-20, hjust = 0.42), panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate() 
  
  z_contra_1d_density
}
z_contra_1d_density_concatenate_by_method <- function(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, method_used, ylimit){
  
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  data <- data_all %>% filter(method == method_used[1] & identity == "IN")
  
  z_contra_M1 <- data %>% filter(x <= 0, muscle == m1)
  z_contra_M2 <- data %>% filter(x <= 0, muscle == m2)
  if (nrow(z_contra_M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(z_contra_M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_contra_M1 <- nrow(z_contra_M1)/nrow(l_M1)
  scale_contra_M2 <- nrow(z_contra_M2)/nrow(l_M2)
  bw_z_M1 <- sd(l_M1$z)/kernel
  bw_z_M2 <- sd(l_M2$z)/kernel
  
  z_contra_1d_density <- ggplot() +
    stat_density(data = z_contra_M1, aes(x = -z, y=stat(density)*scale_contra_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = ext_colors[1], position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = z_contra_M2, aes(x = -z, y=stat(density)*scale_contra_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = flex_colors[1], position = "identity", size = 1, show.legend = FALSE)
  
  if (2 <= length(method_used)) { 
    for (i in 2:length(method_used)) {
      data <- data_all %>% filter(method == method_used[i] & identity == "IN")
      z_contra_M1 <- data %>% filter(x <= 0, muscle == m1)
      z_contra_M2 <- data %>% filter(x <= 0, muscle == m2)
      
      
      if (nrow(z_contra_M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        l_M1 <-  data %>% filter(muscle == m1)
        scale_contra_M1 <- nrow(z_contra_M1)/nrow(l_M1)
        bw_z_M1 <- sd(l_M1$z)/kernel
        z_contra_1d_density <- z_contra_1d_density +
          stat_density(data = z_contra_M1, aes(x = -z, y=stat(density)*scale_contra_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = ext_colors[indice_col_ext], position = "identity", size = 1, show.legend = FALSE) 
      } 
      if (nrow(z_contra_M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        l_M2 <-  data %>% filter(muscle == m2) 
        scale_contra_M2 <- nrow(z_contra_M2)/nrow(l_M2)
        bw_z_M2 <- sd(l_M2$z)/kernel
        z_contra_1d_density <- z_contra_1d_density +
          stat_density(data = z_contra_M2, aes(x = -z, y=stat(density)*scale_contra_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = flex_colors[indice_col_flex], position = "identity", size = 1, show.legend = FALSE)
      }
      
    }
  }
  
  z_contra_1d_density <- z_contra_1d_density +
    scale_x_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    scale_y_reverse(limits=c(ylimit, 0), expand = c(.2, 0, 0, 0))+
    theme_classic() +
    ylab("") +  
    xlab("C-R (μm)")+
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(), axis.title.y = element_text(vjust=-20, hjust = 0.42), panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate() 
  z_contra_1d_density
}
z_contra_1d_density_concatenate_by_titer <- function(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, titer_used, method_used, ylimit){
  
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  data <- data_all %>% filter(method == method_used & titer == titer_used[1] & identity == "IN")
  
  z_contra_M1 <- data %>% filter(x <= 0, muscle == m1)
  z_contra_M2 <- data %>% filter(x <= 0, muscle == m2)
  if (nrow(z_contra_M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(z_contra_M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_contra_M1 <- nrow(z_contra_M1)/nrow(l_M1)
  scale_contra_M2 <- nrow(z_contra_M2)/nrow(l_M2)
  bw_z_M1 <- sd(l_M1$z)/kernel
  bw_z_M2 <- sd(l_M2$z)/kernel
  
  z_contra_1d_density <- ggplot() +
    stat_density(data = z_contra_M1, aes(x = -z, y=stat(density)*scale_contra_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = ext_colors[1], position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = z_contra_M2, aes(x = -z, y=stat(density)*scale_contra_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = flex_colors[1], position = "identity", size = 1, show.legend = FALSE)
  
  if (2 <= length(titer_used)) { 
    for (i in 2:length(titer_used)) {
      data <- data_all %>% filter(method == method_used & titer == titer_used[i] & identity == "IN")
      z_contra_M1 <- data %>% filter(x <= 0, muscle == m1)
      z_contra_M2 <- data %>% filter(x <= 0, muscle == m2)
      
      
      if (nrow(z_contra_M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        l_M1 <-  data %>% filter(muscle == m1)
        scale_contra_M1 <- nrow(z_contra_M1)/nrow(l_M1)
        bw_z_M1 <- sd(l_M1$z)/kernel
        z_contra_1d_density <- z_contra_1d_density +
          stat_density(data = z_contra_M1, aes(x = -z, y=stat(density)*scale_contra_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = ext_colors[indice_col_ext], position = "identity", size = 1, show.legend = FALSE) 
      } 
      if (nrow(z_contra_M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        l_M2 <-  data %>% filter(muscle == m2) 
        scale_contra_M2 <- nrow(z_contra_M2)/nrow(l_M2)
        bw_z_M2 <- sd(l_M2$z)/kernel
        z_contra_1d_density <- z_contra_1d_density +
          stat_density(data = z_contra_M2, aes(x = -z, y=stat(density)*scale_contra_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = flex_colors[indice_col_flex], position = "identity", size = 1, show.legend = FALSE)
      }
      
    }
  }
  
  z_contra_1d_density <- z_contra_1d_density +
    scale_x_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    scale_y_reverse(limits=c(ylimit, 0), expand = c(.2, 0, 0, 0))+
    theme_classic() +
    ylab("") +  
    xlab("C-R (μm)")+
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(), axis.title.y = element_text(vjust=-20, hjust = 0.42), panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate() 
  z_contra_1d_density
}

xz_density_arrange_1d <- function(z_contra, p, z_ipsi){
  
  p_arrange <- ggarrange(z_contra,NULL,p,NULL, z_ipsi,
                         ncol = 5, nrow = 1,  align = "hv",
                         widths = c(1.5,-0.7,3,-0.7,1.5), heights = 4,
                         common.legend = TRUE) 
  return(p_arrange)
}

complete_graph_xz_density <- function (spinal_rc, spinal_rc_contra, density, concatenate, data_all, Animal, m1, m2, color_pal1, color_pal2, color1, color2, nb_samples_ext, 
                                       nb_samples_flex, IN, MN, Size, kernel, bin, method_used, titer_used){
  
  if (concatenate == 1) {
    if (density == 0 & length(method_used) == 1 & length(titer_used) == 1){
      p <- xz_dot_density_center_concatenate(spinal_rc, spinal_rc_contra, data_all, Animal, m1, m2, color1, color2, IN, MN, Size)
      z_ipsi <- z_ipsi_1d_density_concatenate(data_all, Animal, m1, m2, color1, color2, kernel)
      ylimits <- layer_scales(z_ipsi)$y$get_limits() #to get the ylim of the axis
      ylimit <- max(ylimits)
      z_contra <- z_contra_1d_density_concatenate(data_all, Animal, m1, m2, color1, color2, kernel, ylimit)
    }
    if (density == 0 & length(method_used) > 1 & length(titer_used) == 1){
      z_ipsi <- z_ipsi_1d_density_concatenate_by_method(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, method_used)
      ylimits <- layer_scales(z_ipsi)$y$get_limits() #to get the ylim of the axis
      ylimit <- max(ylimits)
      z_contra <- z_contra_1d_density_concatenate_by_method(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, method_used, ylimit)
      p <- xz_dot_density_center_concatenate_by_method(spinal_rc, spinal_rc_contra, data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, Size, method_used)
    }
    if (density == 0 & length(method_used) == 1 & length(titer_used) > 1){
      p <- xz_dot_density_center_concatenate_by_titer(spinal_rc, spinal_rc_contra, data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, Size, titer_used, method_used)
      z_ipsi <- z_ipsi_1d_density_concatenate_by_titer(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, titer_used, method_used)
      ylimits <- layer_scales(z_ipsi)$y$get_limits() #to get the ylim of the axis
      ylimit <- max(ylimits)
      z_contra <- z_contra_1d_density_concatenate_by_titer(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, titer_used, method_used, ylimit)
    }
    if (density == 1 & length(method_used) == 1 & length(titer_used) == 1){
      p <- density_kernel_xz_concatenate(spinal_rc, spinal_rc_contra, data_all, Animal, m1, m2, color1, color2, IN, kernel, bin)
      z_ipsi <- z_ipsi_1d_density_concatenate(data_all, Animal, m1, m2, color1, color2, kernel)
      ylimits <- layer_scales(z_ipsi)$y$get_limits() #to get the ylim of the axis
      ylimit <- max(ylimits)
      z_contra <- z_contra_1d_density_concatenate(data_all, Animal, m1, m2, color1, color2, kernel, ylimit)
    }
    if (density == 1 & length(method_used) > 1 & length(titer_used) == 1){
      z_ipsi <- z_ipsi_1d_density_concatenate_by_method(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, method_used)
      ylimits <- layer_scales(z_ipsi)$y$get_limits() #to get the ylim of the axis
      ylimit <- max(ylimits)
      z_contra <- z_contra_1d_density_concatenate_by_method(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, method_used, ylimit)
      p <- density_kernel_xz_concatenate_by_method(spinal_rc, spinal_rc_contra, data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, kernel, bin, method_used)
    }
    if (density == 1 & length(method_used) == 1 & length(titer_used) > 1){
      z_ipsi <- z_ipsi_1d_density_concatenate_by_titer(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, titer_used, method_used)
      ylimits <- layer_scales(z_ipsi)$y$get_limits() #to get the ylim of the axis
      ylimit <- max(ylimits)
      z_contra <- z_contra_1d_density_concatenate_by_titer(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, titer_used, method_used, ylimit)
      p <- density_kernel_xz_concatenate_by_titer(spinal_rc, spinal_rc_contra, data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, kernel, bin, titer_used, method_used)
    }
  }
  if (concatenate == 0) {
    if (density == 0){
      p <- xz_dot_density_center(spinal_rc, spinal_rc_contra, data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, Size)
    }
    if (density == 1){
      p <- density_kernel_xz(spinal_rc, spinal_rc_contra, data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, kernel, bin)
      
    } 
    z_ipsi <- z_ipsi_1d_density(data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel)
    ylimits <- layer_scales(z_ipsi)$y$get_limits() #to get the ylim of the axis
    ylimit <- max(ylimits)
    z_contra <- z_contra_1d_density(data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, ylimit)
  }
  
  graph <- xz_density_arrange_1d(z_contra, p, z_ipsi)
  
  graph
}

## Functions kernel 2d density xz ##
density_kernel_xz <- function (spinal_rc, spinal_rc_contra, data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, kernel, bin) {
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  M1 <-  data_all %>% filter(animal == Animal[1] & muscle == m1 & identity == IN) 
  M2 <-  data_all %>% filter(animal == Animal[1] & muscle == m2 & identity == IN)
  if (nrow(M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  bw_x_M1 <- sd(M1$x)/kernel
  bw_z_M1 <- sd(M1$z)/kernel
  bw_x_M2 <- sd(M2$x)/kernel
  bw_z_M2 <- sd(M2$z)/kernel
  
  
  
  p <- ggplot() +
    geom_path(data = spinal_rc, aes(x = V1, y = V2), size = 1.5) +
    geom_path(data = spinal_rc_contra, aes(x = V1, y = V2), size = 1.5) +
    geom_density_2d(data = M1, aes(x = x, y = -z), bins = bin,  h=c(bw_x_M1, bw_z_M1), colour = ext_colors[1], show.legend = FALSE) +
    geom_density_2d(data = M2, aes(x = x, y = -z), bins = bin,  h=c(bw_x_M2, bw_z_M2), colour = flex_colors[1], show.legend = FALSE) 
  
  if (2 <= length(Animal)) { 
    for (i in 2:length(Animal)) {
      M1 <-  data_all %>% filter(animal == Animal[i] & muscle == m1 & identity == IN) 
      M2 <-  data_all %>% filter(animal == Animal[i] & muscle == m2 & identity == IN)
      if (nrow(M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        bw_x_M1 <- sd(M1$x)/kernel
        bw_z_M1 <- sd(M1$z)/kernel
        p <- p +
          geom_density_2d(data = M1, aes(x = x, y = -z), bins = bin,  h=c(bw_x_M1, bw_z_M1), colour = ext_colors[as.numeric(indice_col_ext)], show.legend = FALSE) 
      } 
      if (nrow(M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        bw_x_M2 <- sd(M2$x)/kernel
        bw_z_M2 <- sd(M2$z)/kernel
        p <- p +
          geom_density_2d(data = M2, aes(x = x, y = -z), bins = bin,  h=c(bw_x_M2, bw_z_M2), colour = flex_colors[as.numeric(indice_col_flex)], show.legend = FALSE) 
      }
      
    }
  }    
  
  p <- p + 
    scale_y_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    ylab("") +  
    xlab("M-L (μm)") +
    geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
    geom_segment(aes(x=0, xend= 0, y=-2500, yend = 4000), linetype="dashed", color = "black", size=0.5) +
    theme(axis.title.x = element_text(vjust=16), axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent")) + 
    annotation_custom(textGrob("L4", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = -600, xmax = -500, ymin = 200 , ymax = 400) +
    annotation_custom(textGrob("L5", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = -600, xmax = -500, ymin = -200, ymax = -400)
  
  
  p
}
density_kernel_xz_concatenate <- function (spinal_rc, spinal_rc_contra, data_all, Animal, m1, m2, color1, color2, IN, kernel, bin) {
  
  
  M1 <-  data_all %>% filter(animal == Animal[1] & muscle == m1 & identity == IN) 
  M2 <-  data_all %>% filter(animal == Animal[1] & muscle == m2 & identity == IN)
  
  if (2 <= length(Animal)) { 
    for (i in 2:length(Animal)) {
      df1 <-  data_all %>% filter(animal == Animal[i] & muscle == m1 & identity == IN) 
      df2 <-  data_all %>% filter(animal == Animal[i] & muscle == m2 & identity == IN)
      M1 <- rbind(M1, df1)
      M2 <- rbind(M2, df2)
    }
    
  }
  
  bw_x_M1 <- sd(M1$x)/kernel
  bw_z_M1 <- sd(M1$z)/kernel
  bw_x_M2 <- sd(M2$x)/kernel
  bw_z_M2 <- sd(M2$z)/kernel
  
  p <- ggplot() +
    geom_path(data = spinal_rc, aes(x = V1, y = V2), size = 1.5) +
    geom_path(data = spinal_rc_contra, aes(x = V1, y = V2), size = 1.5) +
    geom_density_2d(data = M1, aes(x = x, y = -z), bins = bin,  h=c(bw_x_M1, bw_z_M1), colour = as.character(color1), show.legend = FALSE) +
    geom_density_2d(data = M2, aes(x = x, y = -z), bins = bin,  h=c(bw_x_M2, bw_z_M2), colour = as.character(color2), show.legend = FALSE) + 
    scale_y_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    ylab("") +  
    xlab("M-L (μm)") +
    geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
    geom_segment(aes(x=0, xend= 0, y=-2500, yend = 4000), linetype="dashed", color = "black", size=0.5) +
    theme(axis.title.x = element_text(vjust=16), axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent")) + 
    annotation_custom(textGrob("L4", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = -600, xmax = -500, ymin = 200 , ymax = 400) +
    annotation_custom(textGrob("L5", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = -600, xmax = -500, ymin = -200, ymax = -400)
  
  p
}
density_kernel_xz_concatenate_by_method <- function (spinal_rc, spinal_rc_contra, data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, kernel, bin, method_used) {
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  M1 <-  data_all %>% filter(method == method_used[1] & muscle == m1 & identity == IN) 
  M2 <-  data_all %>% filter(method == method_used[1] & muscle == m2 & identity == IN)
  if (nrow(M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  bw_x_M1 <- sd(M1$x)/kernel
  bw_z_M1 <- sd(M1$z)/kernel
  bw_x_M2 <- sd(M2$x)/kernel
  bw_z_M2 <- sd(M2$z)/kernel
  
  p <- ggplot() +
    geom_path(data = spinal_rc, aes(x = V1, y = V2), size = 1.5) +
    geom_path(data = spinal_rc_contra, aes(x = V1, y = V2), size = 1.5) +
    geom_density_2d(data = M1, aes(x = x, y = -z), bins = bin,  h=c(bw_x_M1, bw_z_M1), colour = ext_colors[1], show.legend = FALSE) +
    geom_density_2d(data = M2, aes(x = x, y = -z), bins = bin,  h=c(bw_x_M2, bw_z_M2), colour = flex_colors[1], show.legend = FALSE) 
  
  if (2 <= length(method_used)) { 
    for (i in 2:length(method_used)) {
      M1 <-  data_all %>% filter(method == method_used[i] & muscle == m1 & identity == IN) 
      M2 <-  data_all %>% filter(method == method_used[i] & muscle == m2 & identity == IN)
      if (nrow(M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        bw_x_M1 <- sd(M1$x)/kernel
        bw_z_M1 <- sd(M1$z)/kernel
        p <- p +
          geom_density_2d(data = M1, aes(x = x, y = -z), bins = bin,  h=c(bw_x_M1, bw_z_M1), colour = ext_colors[as.numeric(indice_col_ext)], show.legend = FALSE) 
      } 
      if (nrow(M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        bw_x_M2 <- sd(M2$x)/kernel
        bw_z_M2 <- sd(M2$z)/kernel
        p <- p +
          geom_density_2d(data = M2, aes(x = x, y = -z), bins = bin,  h=c(bw_x_M2, bw_z_M2), colour = flex_colors[as.numeric(indice_col_flex)], show.legend = FALSE) 
      }
      
    }
  }    
  
  p <- p + 
    scale_y_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    ylab("") +  
    xlab("M-L (μm)") +
    geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
    geom_segment(aes(x=0, xend= 0, y=-2500, yend = 4000), linetype="dashed", color = "black", size=0.5) +
    theme(axis.title.x = element_text(vjust=16), axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent")) + 
    annotation_custom(textGrob("L4", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = -600, xmax = -500, ymin = 200 , ymax = 400) +
    annotation_custom(textGrob("L5", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = -600, xmax = -500, ymin = -200, ymax = -400)
  
  
  p
}
density_kernel_xz_concatenate_by_titer <- function (spinal_rc, spinal_rc_contra, data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, kernel, bin, titer_used, method_used) {
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  M1 <-  data_all %>% filter(method == method_used & titer == titer_used[1] & muscle == m1 & identity == IN) 
  M2 <-  data_all %>% filter(method == method_used & titer == titer_used[1] & muscle == m2 & identity == IN)
  if (nrow(M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  bw_x_M1 <- sd(M1$x)/kernel
  bw_z_M1 <- sd(M1$z)/kernel
  bw_x_M2 <- sd(M2$x)/kernel
  bw_z_M2 <- sd(M2$z)/kernel
  
  
  
  p <- ggplot() +
    geom_path(data = spinal_rc, aes(x = V1, y = V2), size = 1.5) +
    geom_path(data = spinal_rc_contra, aes(x = V1, y = V2), size = 1.5) +
    geom_density_2d(data = M1, aes(x = x, y = -z), bins = bin,  h=c(bw_x_M1, bw_z_M1), colour = ext_colors[1], show.legend = FALSE) +
    geom_density_2d(data = M2, aes(x = x, y = -z), bins = bin,  h=c(bw_x_M2, bw_z_M2), colour = flex_colors[1], show.legend = FALSE) 
  
  if (2 <= length(titer_used)) { 
    for (i in 2:length(titer_used)) {
      M1 <-  data_all %>% filter(method == method_used & titer == titer_used[i] & muscle == m1 & identity == IN) 
      M2 <-  data_all %>% filter(method == method_used & titer == titer_used[i] & muscle == m2 & identity == IN)
      if (nrow(M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        bw_x_M1 <- sd(M1$x)/kernel
        bw_z_M1 <- sd(M1$z)/kernel
        p <- p +
          geom_density_2d(data = M1, aes(x = x, y = -z), bins = bin,  h=c(bw_x_M1, bw_z_M1), colour = ext_colors[as.numeric(indice_col_ext)], show.legend = FALSE) 
      } 
      if (nrow(M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        bw_x_M2 <- sd(M2$x)/kernel
        bw_z_M2 <- sd(M2$z)/kernel
        p <- p +
          geom_density_2d(data = M2, aes(x = x, y = -z), bins = bin,  h=c(bw_x_M2, bw_z_M2), colour = flex_colors[as.numeric(indice_col_flex)], show.legend = FALSE) 
      }
      
    }
  }    
  
  p <- p + 
    scale_y_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    ylab("") +  
    xlab("M-L (μm)") +
    geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
    geom_segment(aes(x=0, xend= 0, y=-2500, yend = 4000), linetype="dashed", color = "black", size=0.5) +
    theme(axis.title.x = element_text(vjust=16), axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent")) + 
    annotation_custom(textGrob("L4", gp = gpar(col = "black", fontsize = 20)), 
                      xmin = -600, xmax = -500, ymin = 200 , ymax = 400) +
    annotation_custom(textGrob("L5", gp = gpar(col = "black", fontsize = 20)), 
                      xmin = -600, xmax = -500, ymin = -200, ymax = -400)
  
  
  p
}


## Functions to plot yz with interactive file ##
yz_dot_density_center <- function(spinal_rc_y, spinal_rc_y_contra, data, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, Size) {
  
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  
  M1 <-  data %>% filter(animal == Animal[1] & muscle == m1 & identity == IN | animal == Animal[1] & muscle == m1 & identity == MN) 
  M2 <-  data %>% filter(animal == Animal[1] & muscle == m2 & identity == IN | animal == Animal[1] & muscle == m2 & identity == MN)
  if (nrow(M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  p <- ggplot() +
    geom_path(data = spinal_rc_y, aes(x = V1, y = V2), size = 1.5) +
    geom_path(data = spinal_rc_y_contra, aes(x = V1, y = V2), size = 1.5) +
    geom_point(data = M1, aes(x = y, y = - z, shape = identity, size = identity, alpha = identity), colour = ext_colors[1], show.legend = FALSE) +
    geom_point(data = M2, aes(x = y, y = - z, shape = identity, size = identity, alpha = identity), colour = flex_colors[1], show.legend = FALSE) 
  
  if (2 <= length(Animal)) { 
    for (i in 2:length(Animal)) {
      M1 <-  data %>% filter(animal == Animal[i] & muscle == m1 & identity == IN | animal == Animal[i] & muscle == m1 & identity == MN) 
      M2 <-  data %>% filter(animal == Animal[i] & muscle == m2 & identity == IN | animal == Animal[i] & muscle == m2 & identity == MN)
      c <- c + nrow(M1) + nrow(M2)
      if (nrow(M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        p <- p +
          geom_point(data = M1, aes(x = y, y = -z, shape = identity, size = identity, alpha = identity), colour = ext_colors[as.numeric(indice_col_ext)], show.legend = FALSE) 
      } 
      if (nrow(M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        p <- p +
          geom_point(data = M2, aes(x = y, y = -z, shape = identity, size = identity, alpha = identity), colour = flex_colors[as.numeric(indice_col_flex)], show.legend = FALSE) 
      }
      
    }
  }
  
  p <- p +
    scale_size_manual(values = Size) +
    scale_alpha_manual(values = c(0.6, 1)) +
    scale_y_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    scale_x_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    ylab("") +  
    xlab("V-D (μm)") +
    geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
    geom_segment(aes(x=0, xend= 0, y=-2500, yend = 4000), linetype="dashed", color = "black", size=0.5) +
    theme(axis.title.x = element_text(vjust=16), axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent")) + 
    annotation_custom(textGrob("L4", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = 250, xmax = 350, ymin = 200 , ymax = 400) +
    annotation_custom(textGrob("L5", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = 250, xmax = 350, ymin = -200, ymax = -400)
  
  
  p
}
yz_dot_density_center_concatenate <- function(spinal_rc_y, spinal_rc_y_contra, data, Animal, m1, m2, color1, color2, IN, MN, Size) {
  
  
  M1 <-  data %>% filter(animal == Animal[1] & muscle == m1 & identity == IN | animal == Animal[1] & muscle == m1 & identity == MN) 
  M2 <-  data %>% filter(animal == Animal[1] & muscle == m2 & identity == IN | animal == Animal[1] & muscle == m2 & identity == MN)
  
  p <- ggplot() +
    geom_path(data = spinal_rc_y, aes(x = V1, y = V2), size = 1.5) +
    geom_path(data = spinal_rc_y_contra, aes(x = V1, y = V2), size = 1.5) +
    geom_point(data = M1, aes(x = y, y = - z, shape = identity, size = identity, alpha = identity), colour = color1, show.legend = FALSE) +
    geom_point(data = M2, aes(x = y, y = - z, shape = identity, size = identity, alpha = identity), colour = color2, show.legend = FALSE) 
  
  if (2 <= length(Animal)) { 
    for (i in 2:length(Animal)) {
      M1 <-  data %>% filter(animal == Animal[i] & muscle == m1 & identity == IN | animal == Animal[i] & muscle == m1 & identity == MN) 
      M2 <-  data %>% filter(animal == Animal[i] & muscle == m2 & identity == IN | animal == Animal[i] & muscle == m2 & identity == MN)
      if (nrow(M1) >= 1) {
        p <- p +
          geom_point(data = M1, aes(x = y, y = - z, shape = identity, size = identity, alpha = identity), colour = color1, show.legend = FALSE) 
      } 
      if (nrow(M2) >= 1) {  
        p <- p +
          geom_point(data = M2, aes(x = y, y = - z, shape = identity, size = identity, alpha = identity), colour = color2, show.legend = FALSE) 
      }
      
    }
  }
  
  p <- p +
    scale_size_manual(values = Size) +
    scale_alpha_manual(values = c(0.6, 1)) +
    scale_y_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    scale_x_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    ylab("") +  
    xlab("V-D (μm)") +
    geom_segment(aes(x=0, xend= 0, y=-2500, yend = 4000), linetype="dashed", color = "black", size=0.5) +
    geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
    theme(axis.title.x = element_text(vjust=16), axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent")) +
    annotation_custom(textGrob("L4", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = 250, xmax = 350, ymin = 200 , ymax = 400) +
    annotation_custom(textGrob("L5", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = 250, xmax = 350, ymin = -200, ymax = -400)
  
  p
}
yz_dot_density_center_concatenate_by_method <- function(spinal_rc_y, spinal_rc_y_contra, data, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, Size, method_used) {
  
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  
  M1 <-  data %>% filter(method == method_used[1] & muscle == m1 & identity == IN | method == method_used[1] & muscle == m1 & identity == MN) 
  M2 <-  data %>% filter(method == method_used[1] & muscle == m2 & identity == IN | method == method_used[1] & muscle == m2 & identity == MN)
  if (nrow(M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  p <- ggplot() +
    geom_path(data = spinal_rc_y, aes(x = V1, y = V2), size = 1.5) +
    geom_path(data = spinal_rc_y_contra, aes(x = V1, y = V2), size = 1.5) +
    geom_point(data = M1, aes(x = y, y = - z, shape = identity, size = identity, alpha = identity), colour = ext_colors[1], show.legend = FALSE) +
    geom_point(data = M2, aes(x = y, y = - z, shape = identity, size = identity, alpha = identity), colour = flex_colors[1], show.legend = FALSE) 
  
  if (2 <= length(method_used)) { 
    for (i in 2:length(method_used)) {
      M1 <-  data %>% filter(method == method_used[i] & muscle == m1 & identity == IN | method == method_used[i] & muscle == m1 & identity == MN) 
      M2 <-  data %>% filter(method == method_used[i] & muscle == m2 & identity == IN | method == method_used[i] & muscle == m2 & identity == MN)
      c <- c + nrow(M1) + nrow(M2)
      if (nrow(M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        p <- p +
          geom_point(data = M1, aes(x = y, y = -z, shape = identity, size = identity, alpha = identity), colour = ext_colors[as.numeric(indice_col_ext)], show.legend = FALSE) 
      } 
      if (nrow(M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        p <- p +
          geom_point(data = M2, aes(x = y, y = -z, shape = identity, size = identity, alpha = identity), colour = flex_colors[as.numeric(indice_col_flex)], show.legend = FALSE) 
      }
      
    }
  }
  
  p <- p +
    scale_size_manual(values = Size) +
    scale_alpha_manual(values = c(0.6, 1)) +
    scale_y_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    scale_x_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    ylab("") +  
    xlab("V-D (μm)") +
    geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
    geom_segment(aes(x=0, xend= 0, y=-2500, yend = 4000), linetype="dashed", color = "black", size=0.5) +
    theme(axis.title.x = element_text(vjust=16), axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent")) + 
    annotation_custom(textGrob("L4", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = 250, xmax = 350, ymin = 200 , ymax = 400) +
    annotation_custom(textGrob("L5", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = 250, xmax = 350, ymin = -200, ymax = -400)
  
  
  p
}
yz_dot_density_center_concatenate_by_titer <- function(spinal_rc_y, spinal_rc_y_contra, data, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, Size, titer_used, method_used) {
  
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  
  M1 <-  data %>% filter(method == method_used & titer == titer_used[1] & muscle == m1 & identity == IN | method == method_used & titer == titer_used[1] & muscle == m1 & identity == MN) 
  M2 <-  data %>% filter(method == method_used & titer == titer_used[1] & muscle == m2 & identity == IN | method == method_used & titer == titer_used[1] & muscle == m2 & identity == MN)
  if (nrow(M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  p <- ggplot() +
    geom_path(data = spinal_rc_y, aes(x = V1, y = V2), size = 1.5) +
    geom_path(data = spinal_rc_y_contra, aes(x = V1, y = V2), size = 1.5) +
    geom_point(data = M1, aes(x = y, y = - z, shape = identity, size = identity, alpha = identity), colour = ext_colors[1], show.legend = FALSE) +
    geom_point(data = M2, aes(x = y, y = - z, shape = identity, size = identity, alpha = identity), colour = flex_colors[1], show.legend = FALSE) 
  
  if (2 <= length(titer_used)) { 
    for (i in 2:length(titer_used)) {
      M1 <-  data %>% filter(method == method_used & titer == titer_used[i] & muscle == m1 & identity == IN | method == method_used & titer == titer_used[i] & muscle == m1 & identity == MN) 
      M2 <-  data %>% filter(method == method_used & titer == titer_used[i] & muscle == m2 & identity == IN | method == method_used & titer == titer_used[i] & muscle == m2 & identity == MN)
      if (nrow(M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        p <- p +
          geom_point(data = M1, aes(x = y, y = -z, shape = identity, size = identity, alpha = identity), colour = ext_colors[as.numeric(indice_col_ext)], show.legend = FALSE) 
      } 
      if (nrow(M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        p <- p +
          geom_point(data = M2, aes(x = y, y = -z, shape = identity, size = identity, alpha = identity), colour = flex_colors[as.numeric(indice_col_flex)], show.legend = FALSE) 
      }
      
    }
  }
  
  p <- p +
    scale_size_manual(values = Size) +
    scale_alpha_manual(values = c(0.6, 1)) +
    scale_y_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    scale_x_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    ylab("") +  
    xlab("V-D (μm)") +
    geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
    geom_segment(aes(x=0, xend= 0, y=-2500, yend = 4000), linetype="dashed", color = "black", size=0.5) +
    theme(axis.title.x = element_text(vjust=16), axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent")) + 
    annotation_custom(textGrob("L4", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = 250, xmax = 350, ymin = 200 , ymax = 400) +
    annotation_custom(textGrob("L5", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = 250, xmax = 350, ymin = -200, ymax = -400)
  
  
  p
}

z_ventral_1d_density <- function(data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, ylimit){
  
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  data <- data_all %>% filter(animal == Animal[1] & identity == "IN")
  
  z_ventral_M1 <- data %>% filter(y <= 0, muscle == m1)
  z_ventral_M2 <- data %>% filter(y <= 0, muscle == m2)
  if (nrow(z_ventral_M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(z_ventral_M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_ventral_M1 <- nrow(z_ventral_M1)/nrow(l_M1)
  scale_ventral_M2 <- nrow(z_ventral_M2)/nrow(l_M2)
  bw_z_M1 <- sd(l_M1$z)/kernel
  bw_z_M2 <- sd(l_M2$z)/kernel
  
  z_ventral_1d_density <- ggplot() +
    stat_density(data = z_ventral_M1, aes(x = - z, y=stat(density)*scale_ventral_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = ext_colors[1], position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = z_ventral_M2, aes(x = - z, y=stat(density)*scale_ventral_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = flex_colors[1], position = "identity", size = 1, show.legend = FALSE)
  
  if (2 <= length(Animal)) { 
    for (i in 2:length(Animal)) {
      data <- data_all %>% filter(animal == Animal[i] & identity == "IN")
      z_ventral_M2 <- data %>% filter(y <= 0, muscle == m2)
      z_ventral_M1 <- data %>% filter(y <= 0, muscle == m1)
      
      
      if (nrow(z_ventral_M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        l_M1 <-  data %>% filter(muscle == m1)
        scale_ventral_M1 <- nrow(z_ventral_M1)/nrow(l_M1)
        bw_z_M1 <- sd(l_M1$z)/kernel
        z_ventral_1d_density <- z_ventral_1d_density +
          stat_density(data = z_ventral_M1, aes(x = - z, y=stat(density)*scale_ventral_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = ext_colors[indice_col_ext], position = "identity", size = 1, show.legend = FALSE) 
      } 
      if (nrow(z_ventral_M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        l_M2 <-  data %>% filter(muscle == m2) 
        scale_ventral_M2 <- nrow(z_ventral_M2)/nrow(l_M2)
        bw_z_M2 <- sd(l_M2$z)/kernel
        z_ventral_1d_density <- z_ventral_1d_density +
          stat_density(data = z_ventral_M2, aes(x = - z, y=stat(density)*scale_ventral_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = flex_colors[indice_col_flex], position = "identity", size = 1, show.legend = FALSE)
      }
      
    }
  }
  if (ylimit == 0) {
    z_ventral_1d_density <- z_ventral_1d_density +
      scale_y_reverse(limits=c(NA, 0), expand = c(.2, 0, 0, 0))
  }
  if (ylimit != 0) {
    z_ventral_1d_density <- z_ventral_1d_density +
      scale_y_reverse(limits=c(ylimit, 0), expand = c(.2, 0, 0, 0)) 
  }
  z_ventral_1d_density <- z_ventral_1d_density +
    scale_x_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    theme_classic() +
    ylab("") +  
    xlab("")+
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(), 
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(), #axis.title.y = element_text(vjust=-20, hjust = 0.42), panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate() 
  z_ventral_1d_density
}
z_ventral_1d_density_concatenate <- function(data_all, Animal, m1, m2, color1, color2, kernel, ylimit){
  data <- data_all %>% filter(animal == Animal[1] & identity == "IN")
  if (2 <= length(Animal)) {
    for (i in 2:length(Animal)) {
      df <- data_all %>% filter(animal == Animal[i] & identity == "IN")
      data <- rbind(data, df)
    }
  }
  z_ventral_M2 <- data %>% filter(y <= 0, muscle == m2)
  z_ventral_M1 <- data %>% filter(y <= 0, muscle == m1)
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_ventral_M2 <- nrow(z_ventral_M2)/nrow(l_M2)
  scale_ventral_M1 <- nrow(z_ventral_M1)/nrow(l_M1)
  bw_z_M1 <- sd(l_M1$z)/kernel
  bw_z_M2 <- sd(l_M2$z)/kernel
  
  z_ventral_1d_density <- ggplot() +
    stat_density(data = z_ventral_M1, aes(x = -z, y=stat(density)*scale_ventral_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = color1, position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = z_ventral_M2, aes(x = -z, y=stat(density)*scale_ventral_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = color2, position = "identity", size = 1, show.legend = FALSE) 
  if (ylimit == 0) {
    z_ventral_1d_density <- z_ventral_1d_density +
      scale_y_reverse(limits=c(NA, 0), expand = c(.2, 0, 0, 0))
  }
  if (ylimit != 0) {
    z_ventral_1d_density <- z_ventral_1d_density +
      scale_y_reverse(limits=c(ylimit, 0), expand = c(.2, 0, 0, 0)) 
  }
  z_ventral_1d_density <- z_ventral_1d_density +
    scale_x_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    theme_classic() +
    ylab("") +  
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(), 
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(), #axis.title.y = element_text(vjust=-20, hjust = 0.42), panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate() 
  z_ventral_1d_density
}
z_ventral_1d_density_concatenate_by_method <- function(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, method_used, ylimit){
  
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  data <- data_all %>% filter(method == method_used[1] & identity == "IN")
  
  z_ventral_M1 <- data %>% filter(y <= 0, muscle == m1)
  z_ventral_M2 <- data %>% filter(y <= 0, muscle == m2)
  if (nrow(z_ventral_M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(z_ventral_M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_ventral_M1 <- nrow(z_ventral_M1)/nrow(l_M1)
  scale_ventral_M2 <- nrow(z_ventral_M2)/nrow(l_M2)
  bw_z_M1 <- sd(l_M1$z)/kernel
  bw_z_M2 <- sd(l_M2$z)/kernel
  
  z_ventral_1d_density <- ggplot() +
    stat_density(data = z_ventral_M1, aes(x = - z, y=stat(density)*scale_ventral_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = ext_colors[1], position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = z_ventral_M2, aes(x = - z, y=stat(density)*scale_ventral_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = flex_colors[1], position = "identity", size = 1, show.legend = FALSE)
  
  if (2 <= length(method_used)) { 
    for (i in 2:length(method_used)) {
      data <- data_all %>% filter(method == method_used[i] & identity == "IN")
      z_ventral_M2 <- data %>% filter(y <= 0, muscle == m2)
      z_ventral_M1 <- data %>% filter(y <= 0, muscle == m1)
      
      
      if (nrow(z_ventral_M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        l_M1 <-  data %>% filter(muscle == m1)
        scale_ventral_M1 <- nrow(z_ventral_M1)/nrow(l_M1)
        bw_z_M1 <- sd(l_M1$z)/kernel
        z_ventral_1d_density <- z_ventral_1d_density +
          stat_density(data = z_ventral_M1, aes(x = - z, y=stat(density)*scale_ventral_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = ext_colors[indice_col_ext], position = "identity", size = 1, show.legend = FALSE) 
      } 
      if (nrow(z_ventral_M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        l_M2 <-  data %>% filter(muscle == m2) 
        scale_ventral_M2 <- nrow(z_ventral_M2)/nrow(l_M2)
        bw_z_M2 <- sd(l_M2$z)/kernel
        z_ventral_1d_density <- z_ventral_1d_density +
          stat_density(data = z_ventral_M2, aes(x = - z, y=stat(density)*scale_ventral_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = flex_colors[indice_col_flex], position = "identity", size = 1, show.legend = FALSE)
      }
      
    }
  }
  if (ylimit == 0) {
    z_ventral_1d_density <- z_ventral_1d_density +
      scale_y_reverse(limits=c(NA, 0), expand = c(.2, 0, 0, 0))
  }
  if (ylimit != 0) {
    z_ventral_1d_density <- z_ventral_1d_density +
      scale_y_reverse(limits=c(ylimit, 0), expand = c(.2, 0, 0, 0)) 
  }
  z_ventral_1d_density <- z_ventral_1d_density +
    scale_x_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    theme_classic() +
    ylab("") +  
    xlab("")+
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(), 
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(), #axis.title.y = element_text(vjust=-20, hjust = 0.42), panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate() 
  z_ventral_1d_density
}
z_ventral_1d_density_concatenate_by_titer <- function(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, titer_used, method_used, ylimit){
  
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  data <- data_all %>% filter(method == method_used & titer == titer_used[1] & identity == "IN")
  
  z_ventral_M1 <- data %>% filter(y <= 0, muscle == m1)
  z_ventral_M2 <- data %>% filter(y <= 0, muscle == m2)
  if (nrow(z_ventral_M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(z_ventral_M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_ventral_M1 <- nrow(z_ventral_M1)/nrow(l_M1)
  scale_ventral_M2 <- nrow(z_ventral_M2)/nrow(l_M2)
  bw_z_M1 <- sd(l_M1$z)/kernel
  bw_z_M2 <- sd(l_M2$z)/kernel
  
  z_ventral_1d_density <- ggplot() +
    stat_density(data = z_ventral_M1, aes(x = - z, y=stat(density)*scale_ventral_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = ext_colors[1], position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = z_ventral_M2, aes(x = - z, y=stat(density)*scale_ventral_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = flex_colors[1], position = "identity", size = 1, show.legend = FALSE)
  
  if (2 <= length(titer_used)) { 
    for (i in 2:length(titer_used)) {
      data <- data_all %>% filter(method == method_used & titer == titer_used[i] & identity == "IN")
      z_ventral_M2 <- data %>% filter(y <= 0, muscle == m2)
      z_ventral_M1 <- data %>% filter(y <= 0, muscle == m1)
      
      
      if (nrow(z_ventral_M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        l_M1 <-  data %>% filter(muscle == m1)
        scale_ventral_M1 <- nrow(z_ventral_M1)/nrow(l_M1)
        bw_z_M1 <- sd(l_M1$z)/kernel
        z_ventral_1d_density <- z_ventral_1d_density +
          stat_density(data = z_ventral_M1, aes(x = - z, y=stat(density)*scale_ventral_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = ext_colors[indice_col_ext], position = "identity", size = 1, show.legend = FALSE) 
      } 
      if (nrow(z_ventral_M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        l_M2 <-  data %>% filter(muscle == m2) 
        scale_ventral_M2 <- nrow(z_ventral_M2)/nrow(l_M2)
        bw_z_M2 <- sd(l_M2$z)/kernel
        z_ventral_1d_density <- z_ventral_1d_density +
          stat_density(data = z_ventral_M2, aes(x = - z, y=stat(density)*scale_ventral_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = flex_colors[indice_col_flex], position = "identity", size = 1, show.legend = FALSE)
      }
      
    }
  }
  if (ylimit == 0) {
    z_ventral_1d_density <- z_ventral_1d_density +
      scale_y_reverse(limits=c(NA, 0), expand = c(.2, 0, 0, 0))
  }
  if (ylimit != 0) {
    z_ventral_1d_density <- z_ventral_1d_density +
      scale_y_reverse(limits=c(ylimit, 0), expand = c(.2, 0, 0, 0)) 
  }
  z_ventral_1d_density <- z_ventral_1d_density +
    scale_x_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    theme_classic() +
    ylab("") +  
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(), 
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate() 
  z_ventral_1d_density
}


z_dorsal_1d_density <- function(data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, ylimit){
  
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  data <- data_all %>% filter(animal == Animal[1] & identity == "IN")
  
  z_dorsal_M1 <- data %>% filter(y >= 0, muscle == m1)
  z_dorsal_M2 <- data %>% filter(y >= 0, muscle == m2)
  if (nrow(z_dorsal_M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(z_dorsal_M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_dorsal_M1 <- nrow(z_dorsal_M1)/nrow(l_M1)
  scale_dorsal_M2 <- nrow(z_dorsal_M2)/nrow(l_M2)
  bw_z_M1 <- sd(l_M1$z)/kernel
  bw_z_M2 <- sd(l_M2$z)/kernel
  
  z_dorsal_1d_density <- ggplot() +
    stat_density(data = z_dorsal_M1, aes(x = -z, y=stat(density)*scale_dorsal_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = ext_colors[1], position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = z_dorsal_M2, aes(x = -z, y=stat(density)*scale_dorsal_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = flex_colors[1], position = "identity", size = 1, show.legend = FALSE)
  
  if (2 <= length(Animal)) { 
    for (i in 2:length(Animal)) {
      data <- data_all %>% filter(animal == Animal[i] & identity == "IN")
      z_dorsal_M1 <- data %>% filter(y >= 0, muscle == m1)
      z_dorsal_M2 <- data %>% filter(y >= 0, muscle == m2)
      
      
      if (nrow(z_dorsal_M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        l_M1 <-  data %>% filter(muscle == m1)
        scale_dorsal_M1 <- nrow(z_dorsal_M1)/nrow(l_M1)
        bw_z_M1 <- sd(l_M1$z)/kernel
        z_dorsal_1d_density <- z_dorsal_1d_density +
          stat_density(data = z_dorsal_M1, aes(x = -z, y=stat(density)*scale_dorsal_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = ext_colors[indice_col_ext], position = "identity", size = 1, show.legend = FALSE) 
      } 
      if (nrow(z_dorsal_M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        l_M2 <-  data %>% filter(muscle == m2) 
        scale_dorsal_M2 <- nrow(z_dorsal_M2)/nrow(l_M2)
        bw_z_M2 <- sd(l_M2$z)/kernel
        z_dorsal_1d_density <- z_dorsal_1d_density +
          stat_density(data = z_dorsal_M2, aes(x = -z, y=stat(density)*scale_dorsal_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = flex_colors[indice_col_flex], position = "identity", size = 1, show.legend = FALSE)
      }
      
    }
  }
  if (ylimit == 0) {
    z_dorsal_1d_density <- z_dorsal_1d_density +
      scale_y_continuous(limits=c(0, NA), expand = c(0, 0, .2, 0))
  }
  if (ylimit != 0) {
    z_dorsal_1d_density <- z_dorsal_1d_density +
      scale_y_continuous(limits=c(0, ylimit), expand = c(0, 0, 0.2, 0)) 
  }
  z_dorsal_1d_density <- z_dorsal_1d_density +
    scale_x_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    theme_classic() +
    ylab("") +
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate()
  z_dorsal_1d_density
}
z_dorsal_1d_density_concatenate <- function(data_all, Animal, m1, m2, color1, color2, kernel, ylimit){
  data <- data_all %>% filter(animal == Animal[1] & identity == "IN")
  if (2 <= length(Animal)) {
    for (i in 2:length(Animal)) {
      df <- data_all %>% filter(animal == Animal[i] & identity == "IN")
      data <- rbind(data, df)
    }
  }
  z_dorsal_M2 <- data %>% filter(y >= 0, muscle == m2)
  z_dorsal_M1 <- data %>% filter(y >= 0, muscle == m1)
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_dorsal_M2 <- nrow(z_dorsal_M2)/nrow(l_M2)
  scale_dorsal_M1 <- nrow(z_dorsal_M1)/nrow(l_M1)
  bw_z_M1 <- sd(l_M1$z)/kernel
  bw_z_M2 <- sd(l_M2$z)/kernel
  
  z_dorsal_1d_density <- ggplot() +
    stat_density(data = z_dorsal_M1, aes(x = -z, y=stat(density)*scale_dorsal_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = color1, position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = z_dorsal_M2, aes(x = -z, y=stat(density)*scale_dorsal_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = color2, position = "identity", size = 1, show.legend = FALSE)
  if (ylimit == 0) {
    z_dorsal_1d_density <- z_dorsal_1d_density +
      scale_y_continuous(limits=c(0, NA), expand = c(0, 0, .2, 0))
  }
  if (ylimit != 0) {
    z_dorsal_1d_density <- z_dorsal_1d_density +
      scale_y_continuous(limits=c(0, ylimit), expand = c(0, 0, 0.2, 0)) 
  }
  z_dorsal_1d_density <- z_dorsal_1d_density +
    scale_x_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    theme_classic() +
    ylab("") +  
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate()
  
  z_dorsal_1d_density
}
z_dorsal_1d_density_concatenate_by_method <- function(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, method_used, ylimit){
  
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  data <- data_all %>% filter(method == method_used[1] & identity == "IN")
  
  z_dorsal_M1 <- data %>% filter(y >= 0, muscle == m1)
  z_dorsal_M2 <- data %>% filter(y >= 0, muscle == m2)
  if (nrow(z_dorsal_M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(z_dorsal_M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_dorsal_M1 <- nrow(z_dorsal_M1)/nrow(l_M1)
  scale_dorsal_M2 <- nrow(z_dorsal_M2)/nrow(l_M2)
  bw_z_M1 <- sd(l_M1$z)/kernel
  bw_z_M2 <- sd(l_M2$z)/kernel
  
  z_dorsal_1d_density <- ggplot() +
    stat_density(data = z_dorsal_M1, aes(x = -z, y=stat(density)*scale_dorsal_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = ext_colors[1], position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = z_dorsal_M2, aes(x = -z, y=stat(density)*scale_dorsal_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = flex_colors[1], position = "identity", size = 1, show.legend = FALSE)
  
  if (2 <= length(method_used)) { 
    for (i in 2:length(method_used)) {
      data <- data_all %>% filter(method == method_used[i] & identity == "IN")
      z_dorsal_M1 <- data %>% filter(y >= 0, muscle == m1)
      z_dorsal_M2 <- data %>% filter(y >= 0, muscle == m2)
      
      
      if (nrow(z_dorsal_M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        l_M1 <-  data %>% filter(muscle == m1)
        scale_dorsal_M1 <- nrow(z_dorsal_M1)/nrow(l_M1)
        bw_z_M1 <- sd(l_M1$z)/kernel
        z_dorsal_1d_density <- z_dorsal_1d_density +
          stat_density(data = z_dorsal_M1, aes(x = -z, y=stat(density)*scale_dorsal_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = ext_colors[indice_col_ext], position = "identity", size = 1, show.legend = FALSE) 
      } 
      if (nrow(z_dorsal_M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        l_M2 <-  data %>% filter(muscle == m2) 
        scale_dorsal_M2 <- nrow(z_dorsal_M2)/nrow(l_M2)
        bw_z_M2 <- sd(l_M2$z)/kernel
        z_dorsal_1d_density <- z_dorsal_1d_density +
          stat_density(data = z_dorsal_M2, aes(x = -z, y=stat(density)*scale_dorsal_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = flex_colors[indice_col_flex], position = "identity", size = 1, show.legend = FALSE)
      }
      
    }
  }
  if (ylimit == 0) {
    z_dorsal_1d_density <- z_dorsal_1d_density +
      scale_y_continuous(limits=c(0, NA), expand = c(0, 0, 0.2, 0))
  }
  if (ylimit != 0) {
    z_dorsal_1d_density <- z_dorsal_1d_density +
      scale_y_continuous(limits=c(0, ylimit), expand = c(0, 0, 0.2, 0)) 
  }
  z_dorsal_1d_density <- z_dorsal_1d_density +
    scale_x_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    theme_classic() +
    ylab("") +
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate()
  z_dorsal_1d_density
}
z_dorsal_1d_density_concatenate_by_titer <- function(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, titer_used, method_used, ylimit){
  
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  data <- data_all %>% filter(method == method_used & titer == titer_used[1] & identity == "IN")
  
  z_dorsal_M1 <- data %>% filter(y >= 0, muscle == m1)
  z_dorsal_M2 <- data %>% filter(y >= 0, muscle == m2)
  if (nrow(z_dorsal_M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(z_dorsal_M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  l_M1 <-  data %>% filter(muscle == m1) 
  l_M2 <-  data %>% filter(muscle == m2) 
  scale_dorsal_M1 <- nrow(z_dorsal_M1)/nrow(l_M1)
  scale_dorsal_M2 <- nrow(z_dorsal_M2)/nrow(l_M2)
  bw_z_M1 <- sd(l_M1$z)/kernel
  bw_z_M2 <- sd(l_M2$z)/kernel
  
  z_dorsal_1d_density <- ggplot() +
    stat_density(data = z_dorsal_M1, aes(x = -z, y=stat(density)*scale_dorsal_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = ext_colors[1], position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = z_dorsal_M2, aes(x = -z, y=stat(density)*scale_dorsal_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = flex_colors[1], position = "identity", size = 1, show.legend = FALSE)
  
  if (2 <= length(titer_used)) { 
    for (i in 2:length(titer_used)) {
      data <- data_all %>% filter(method == method_used & titer == titer_used[i] & identity == "IN")
      z_dorsal_M1 <- data %>% filter(y >= 0, muscle == m1)
      z_dorsal_M2 <- data %>% filter(y >= 0, muscle == m2)
      
      
      if (nrow(z_dorsal_M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        l_M1 <-  data %>% filter(muscle == m1)
        scale_dorsal_M1 <- nrow(z_dorsal_M1)/nrow(l_M1)
        bw_z_M1 <- sd(l_M1$z)/kernel
        z_dorsal_1d_density <- z_dorsal_1d_density +
          stat_density(data = z_dorsal_M1, aes(x = -z, y=stat(density)*scale_dorsal_M1), bw = bw_z_M1, kernel = "gaussian", geom = "line", colour = ext_colors[indice_col_ext], position = "identity", size = 1, show.legend = FALSE) 
      } 
      if (nrow(z_dorsal_M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        l_M2 <-  data %>% filter(muscle == m2) 
        scale_dorsal_M2 <- nrow(z_dorsal_M2)/nrow(l_M2)
        bw_z_M2 <- sd(l_M2$z)/kernel
        z_dorsal_1d_density <- z_dorsal_1d_density +
          stat_density(data = z_dorsal_M2, aes(x = -z, y=stat(density)*scale_dorsal_M2), bw = bw_z_M2, kernel = "gaussian", geom = "line", colour = flex_colors[indice_col_flex], position = "identity", size = 1, show.legend = FALSE)
      }
      
    }
  }
  if (ylimit == 0) {
    z_dorsal_1d_density <- z_dorsal_1d_density +
      scale_y_continuous(limits=c(0, NA), expand = c(0, 0, 0.2, 0))
  }
  if (ylimit != 0) {
    z_dorsal_1d_density <- z_dorsal_1d_density +
      scale_y_continuous(limits=c(0, ylimit), expand = c(0, 0, 0.2, 0)) 
  }
  z_dorsal_1d_density <- z_dorsal_1d_density +
    scale_x_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    theme_classic() +
    ylab("") +
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate()
  z_dorsal_1d_density
}

yz_density_arrange_1d <- function(z_ventral, p, z_dorsal){
  
  p_arrange <- ggarrange(z_ventral,NULL,p,NULL, z_dorsal,
                         ncol = 5, nrow = 1,  align = "h",
                         widths = c(1.5,-0.3,2.7,-0.3,1.5), heights = 4,
                         common.legend = TRUE) 
  return(p_arrange)
}

complete_graph_yz_density <- function (spinal_rc_y, spinal_rc_y_contra, density, concatenate, data_all, Animal, m1, m2, color_pal1, color_pal2, color1, color2, nb_samples_ext, 
                                       nb_samples_flex, IN, MN, Size, kernel, bin, titer_used, method_used){
  
  ylimit <- 0
  if (concatenate == 1) {
    if (density == 0 & length(method_used) == 1 & length(titer_used) == 1){
      p <- yz_dot_density_center_concatenate(spinal_rc_y, spinal_rc_y_contra, data_all, Animal, m1, m2, color1, color2, IN, MN, Size)
      z_ventral <- z_ventral_1d_density_concatenate(data_all, Animal, m1, m2, color1, color2, kernel, ylimit)
      z_dorsal <- z_dorsal_1d_density_concatenate(data_all, Animal, m1, m2, color1, color2, kernel, ylimit)
      ylimits1 <- layer_scales(z_dorsal)$y$get_limits() #to get the xlim of the axis
      ylimit1 <- max(ylimits1)
      ylimits2 <- layer_scales(z_ventral)$y$get_limits() #to get the xlim of the axis
      ylimit2 <- max(- ylimits2)
      if (ylimit2 < ylimit1) {
        z_ventral <- z_ventral_1d_density_concatenate(data_all, Animal, m1, m2, color1, color2, kernel, ylimit1)
      }
      if (ylimit1 < ylimit2){
        z_dorsal <- z_dorsal_1d_density_concatenate(data_all, Animal, m1, m2, color1, color2, kernel, ylimit2)     
      }
    }
    if (density == 0 & length(method_used) > 1 & length(titer_used) == 1){
      p <- yz_dot_density_center_concatenate_by_method(spinal_rc_y, spinal_rc_y_contra, data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, Size, method_used)
      z_ventral <- z_ventral_1d_density_concatenate_by_method(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, method_used, ylimit)
      z_dorsal <- z_dorsal_1d_density_concatenate_by_method(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, method_used, ylimit)
      ylimits1 <- layer_scales(z_dorsal)$y$get_limits() #to get the xlim of the axis
      ylimit1 <- max(ylimits1)
      ylimits2 <- layer_scales(z_ventral)$y$get_limits() #to get the xlim of the axis
      ylimit2 <- max(- ylimits2)
      if (ylimit2 < ylimit1) {
        z_ventral <- z_ventral_1d_density_concatenate_by_method(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, method_used, ylimit1)
      }
      if (ylimit1 < ylimit2){
        z_dorsal <- z_dorsal_1d_density_concatenate_by_method(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, method_used, ylimit2)     
      }
    }
    if (density == 0 & length(method_used) == 1 & length(titer_used) > 1){
      p <- yz_dot_density_center_concatenate_by_titer(spinal_rc_y, spinal_rc_y_contra, data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, Size, titer_used, method_used)
      z_ventral <- z_ventral_1d_density_concatenate_by_titer(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, titer_used, method_used, ylimit)
      z_dorsal <- z_dorsal_1d_density_concatenate_by_titer(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, titer_used, method_used, ylimit)
      ylimits1 <- layer_scales(z_dorsal)$y$get_limits() #to get the xlim of the axis
      ylimit1 <- max(ylimits1)
      ylimits2 <- layer_scales(z_ventral)$y$get_limits() #to get the xlim of the axis
      ylimit2 <- max(- ylimits2)
      if (ylimit2 < ylimit1) {
        z_ventral <- z_ventral_1d_density_concatenate_by_titer(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, titer_used, method_used, ylimit1)
      }
      if (ylimit1 < ylimit2){
        z_dorsal <- z_dorsal_1d_density_concatenate_by_titer(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, titer_used, method_used, ylimit2)     
      }
    }
    if (density == 1 & length(method_used) == 1 & length(titer_used) == 1){
      p <- density_kernel_yz_concatenate(spinal_rc_y, spinal_rc_y_contra, data_all, Animal, m1, m2, color1, color2, IN, kernel, bin)
      z_ventral <- z_ventral_1d_density_concatenate(data_all, Animal, m1, m2, color1, color2, kernel, ylimit)
      z_dorsal <- z_dorsal_1d_density_concatenate(data_all, Animal, m1, m2, color1, color2, kernel, ylimit)
      ylimits1 <- layer_scales(z_dorsal)$y$get_limits() #to get the xlim of the axis
      ylimit1 <- max(ylimits1)
      ylimits2 <- layer_scales(z_ventral)$y$get_limits() #to get the xlim of the axis
      ylimit2 <- max(- ylimits2)
      if (ylimit2 < ylimit1) {
        z_ventral <- z_ventral_1d_density_concatenate(data_all, Animal, m1, m2, color1, color2, kernel, ylimit1)
      }
      if (ylimit1 < ylimit2){
        z_dorsal <- z_dorsal_1d_density_concatenate(data_all, Animal, m1, m2, color1, color2, kernel, ylimit2)     
      }
    }
    if (density == 1 & length(method_used) > 1 & length(titer_used) == 1){
      p <- density_kernel_yz_concatenate_by_method(spinal_rc_y, spinal_rc_y_contra, data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, kernel, bin, method_used)
      z_ventral <- z_ventral_1d_density_concatenate_by_method(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, method_used, ylimit)
      z_dorsal <- z_dorsal_1d_density_concatenate_by_method(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, method_used, ylimit)
      ylimits1 <- layer_scales(z_dorsal)$y$get_limits() #to get the xlim of the axis
      ylimit1 <- max(ylimits1)
      ylimits2 <- layer_scales(z_ventral)$y$get_limits() #to get the xlim of the axis
      ylimit2 <- max(- ylimits2)
      if (ylimit2 < ylimit1) {
        z_ventral <- z_ventral_1d_density_concatenate_by_method(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, method_used, ylimit1)
      }
      if (ylimit1 < ylimit2){
        z_dorsal <- z_dorsal_1d_density_concatenate_by_method(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, method_used, ylimit2)     
      }
    }
    if (density == 1 & length(method_used) == 1 & length(titer_used) > 1){
      p <- density_kernel_yz_concatenate_by_titer(spinal_rc_y, spinal_rc_y_contra, data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, kernel, bin, titer_used, method_used)
      z_ventral <- z_ventral_1d_density_concatenate_by_titer(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, titer_used, method_used, ylimit)
      z_dorsal <- z_dorsal_1d_density_concatenate_by_titer(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, titer_used, method_used, ylimit)
      ylimits1 <- layer_scales(z_dorsal)$y$get_limits() #to get the xlim of the axis
      ylimit1 <- max(ylimits1)
      ylimits2 <- layer_scales(z_ventral)$y$get_limits() #to get the xlim of the axis
      ylimit2 <- max(- ylimits2)
      if (ylimit2 < ylimit1) {
        z_ventral <- z_ventral_1d_density_concatenate_by_titer(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, titer_used, method_used, ylimit1)
      }
      if (ylimit1 < ylimit2){
        z_dorsal <- z_dorsal_1d_density_concatenate_by_titer(data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, titer_used, method_used, ylimit2)     
      }
    }
  }
  if (concatenate == 0) {
    if (density == 0){
      p <- yz_dot_density_center(spinal_rc_y, spinal_rc_y_contra, data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, Size)
    }
    if (density == 1){
      p <- density_kernel_yz(spinal_rc_y, spinal_rc_y_contra, data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, kernel, bin)
      
    } 
    z_ventral <- z_ventral_1d_density(data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, ylimit)
    z_dorsal <- z_dorsal_1d_density(data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, ylimit)
    ylimits1 <- layer_scales(z_dorsal)$y$get_limits() #to get the xlim of the axis
    ylimit1 <- max(ylimits1)
    ylimits2 <- layer_scales(z_ventral)$y$get_limits() #to get the xlim of the axis
    ylimit2 <- max(- ylimits2)
    if (ylimit2 < ylimit1) {
      z_ventral <- z_ventral_1d_density(data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, ylimit1)
    }
    if (ylimit1 < ylimit2){
      z_dorsal <- z_dorsal_1d_density(data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, kernel, ylimit2)     
    }
    
  }
  
  graph <- yz_density_arrange_1d(z_ventral, p, z_dorsal)
  
  graph
}

## Functions kernel 2d density yz ##
density_kernel_yz <- function (spinal_rc_y, spinal_rc_y_contra, data_all, Animal, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, kernel, bin) {
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  M1 <-  data_all %>% filter(animal == Animal[1] & muscle == m1 & identity == IN) 
  M2 <-  data_all %>% filter(animal == Animal[1] & muscle == m2 & identity == IN)
  if (nrow(M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  bw_y_M1 <- sd(M1$y)/kernel
  bw_z_M1 <- sd(M1$z)/kernel
  bw_y_M2 <- sd(M2$y)/kernel
  bw_z_M2 <- sd(M2$z)/kernel
  
  
  
  p <- ggplot() +
    geom_path(data = spinal_rc_y, aes(x = V1, y = V2), size = 1.5) +
    geom_path(data = spinal_rc_y_contra, aes(x = V1, y = V2), size = 1.5) +
    geom_density_2d(data = M1, aes(x = y, y = -z), bins = bin,  h=c(bw_y_M1, bw_z_M1), colour = ext_colors[1], show.legend = FALSE) +
    geom_density_2d(data = M2, aes(x = y, y = -z), bins = bin,  h=c(bw_y_M2, bw_z_M2), colour = flex_colors[1], show.legend = FALSE) 
  
  if (2 <= length(Animal)) { 
    for (i in 2:length(Animal)) {
      M1 <-  data_all %>% filter(animal == Animal[i] & muscle == m1 & identity == IN) 
      M2 <-  data_all %>% filter(animal == Animal[i] & muscle == m2 & identity == IN)
      if (nrow(M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        bw_y_M1 <- sd(M1$y)/kernel
        bw_z_M1 <- sd(M1$z)/kernel
        p <- p +
          geom_density_2d(data = M1, aes(x = y, y = -z), bins = bin,  h=c(bw_y_M1, bw_z_M1), colour = ext_colors[as.numeric(indice_col_ext)], show.legend = FALSE) 
      } 
      if (nrow(M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        bw_y_M2 <- sd(M2$y)/kernel
        bw_z_M2 <- sd(M2$z)/kernel
        p <- p +
          geom_density_2d(data = M2, aes(x = y, y = -z), bins = bin,  h=c(bw_y_M2, bw_z_M2), colour = flex_colors[as.numeric(indice_col_flex)], show.legend = FALSE) 
      }
      
    }
  }    
  
  p <- p + 
    scale_y_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    scale_x_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    ylab("") +  
    xlab("V-D (μm)") +
    geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
    geom_segment(aes(x=0, xend= 0, y=-2500, yend = 4000), linetype="dashed", color = "black", size=0.5) +
    theme(axis.title.x = element_text(vjust=16), axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent")) + 
    annotation_custom(textGrob("L4", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = 250, xmax = 350, ymin = 200 , ymax = 400) +
    annotation_custom(textGrob("L5", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = 250, xmax = 350, ymin = -200, ymax = -400)
  
  
  p
}
density_kernel_yz_concatenate <- function (spinal_rc_y, spinal_rc_y_contra, data_all, Animal, m1, m2, color1, color2, IN, kernel, bin) {
  
  
  M1 <-  data_all %>% filter(animal == Animal[1] & muscle == m1 & identity == IN) 
  M2 <-  data_all %>% filter(animal == Animal[1] & muscle == m2 & identity == IN)
  
  if (2 <= length(Animal)) { 
    for (i in 2:length(Animal)) {
      df1 <-  data_all %>% filter(animal == Animal[i] & muscle == m1 & identity == IN) 
      df2 <-  data_all %>% filter(animal == Animal[i] & muscle == m2 & identity == IN)
      M1 <- rbind(M1, df1)
      M2 <- rbind(M2, df2)
    }
    
  }
  
  bw_y_M1 <- sd(M1$y)/kernel
  bw_z_M1 <- sd(M1$z)/kernel
  bw_y_M2 <- sd(M2$y)/kernel
  bw_z_M2 <- sd(M2$z)/kernel
  
  p <- ggplot() +
    geom_path(data = spinal_rc_y, aes(x = V1, y = V2), size = 1.5) +
    geom_path(data = spinal_rc_y_contra, aes(x = V1, y = V2), size = 1.5) +
    geom_density_2d(data = M1, aes(x = y, y = -z), bins = bin,  h=c(bw_y_M1, bw_z_M1), colour = as.character(color1), show.legend = FALSE) +
    geom_density_2d(data = M2, aes(x = y, y = -z), bins = bin,  h=c(bw_y_M2, bw_z_M2), colour = as.character(color2), show.legend = FALSE) + 
    scale_y_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    scale_x_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    ylab("") +  
    xlab("V-D (μm)") +
    geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
    geom_segment(aes(x=0, xend= 0, y=-2500, yend = 4000), linetype="dashed", color = "black", size=0.5) +
    theme(axis.title.x = element_text(vjust=16), axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent")) + 
    annotation_custom(textGrob("L4", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = 250, xmax = 350, ymin = 200 , ymax = 400) +
    annotation_custom(textGrob("L5", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = 250, xmax = 350, ymin = -200, ymax = -400)
  
  p
}
density_kernel_yz_concatenate_by_method <- function (spinal_rc_y, spinal_rc_y_contra, data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, kernel, bin, method_used) {
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  M1 <-  data_all %>% filter(method == method_used[1] & muscle == m1 & identity == IN) 
  M2 <-  data_all %>% filter(method == method_used[1] & muscle == m2 & identity == IN)
  if (nrow(M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  bw_y_M1 <- sd(M1$y)/kernel
  bw_z_M1 <- sd(M1$z)/kernel
  bw_y_M2 <- sd(M2$y)/kernel
  bw_z_M2 <- sd(M2$z)/kernel
  
  
  
  p <- ggplot() +
    geom_path(data = spinal_rc_y, aes(x = V1, y = V2), size = 1.5) +
    geom_path(data = spinal_rc_y_contra, aes(x = V1, y = V2), size = 1.5) +
    geom_density_2d(data = M1, aes(x = y, y = -z), bins = bin,  h=c(bw_y_M1, bw_z_M1), colour = ext_colors[1], show.legend = FALSE) +
    geom_density_2d(data = M2, aes(x = y, y = -z), bins = bin,  h=c(bw_y_M2, bw_z_M2), colour = flex_colors[1], show.legend = FALSE) 
  
  if (2 <= length(method_used)) { 
    for (i in 2:length(method_used)) {
      M1 <-  data_all %>% filter(method == method_used[i] & muscle == m1 & identity == IN) 
      M2 <-  data_all %>% filter(method == method_used[i] & muscle == m2 & identity == IN)
      if (nrow(M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        bw_y_M1 <- sd(M1$y)/kernel
        bw_z_M1 <- sd(M1$z)/kernel
        p <- p +
          geom_density_2d(data = M1, aes(x = y, y = -z), bins = bin,  h=c(bw_y_M1, bw_z_M1), colour = ext_colors[as.numeric(indice_col_ext)], show.legend = FALSE) 
      } 
      if (nrow(M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        bw_y_M2 <- sd(M2$y)/kernel
        bw_z_M2 <- sd(M2$z)/kernel
        p <- p +
          geom_density_2d(data = M2, aes(x = y, y = -z), bins = bin,  h=c(bw_y_M2, bw_z_M2), colour = flex_colors[as.numeric(indice_col_flex)], show.legend = FALSE) 
      }
      
    }
  }    
  
  p <- p + 
    scale_y_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    scale_x_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    ylab("") +  
    xlab("V-D (μm)") +
    geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
    geom_segment(aes(x=0, xend= 0, y=-2500, yend = 4000), linetype="dashed", color = "black", size=0.5) +
    theme(axis.title.x = element_text(vjust=16), axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent")) + 
    annotation_custom(textGrob("L4", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = 250, xmax = 350, ymin = 200 , ymax = 400) +
    annotation_custom(textGrob("L5", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = 250, xmax = 350, ymin = -200, ymax = -400)
  
  
  p
}
density_kernel_yz_concatenate_by_titer <- function (spinal_rc_y, spinal_rc_y_contra, data_all, m1, m2, color_pal1, color_pal2, nb_samples_ext, nb_samples_flex, IN, MN, kernel, bin, titer_used, method_used) {
  if (nb_samples_ext >= 1) {
    ext_colors <- color_pal1(nb_samples_ext)
  } else {
    ext_colors <- "transparent"
  }
  if (nb_samples_flex >= 1) {
    flex_colors <- color_pal2(nb_samples_flex)
  } else {
    flex_colors <- "transparent"
  }
  indice_col_ext <- 0
  indice_col_flex <- 0
  M1 <-  data_all %>% filter(method == method_used & titer == titer_used[1] & muscle == m1 & identity == IN) 
  M2 <-  data_all %>% filter(method == method_used & titer == titer_used[1] & muscle == m2 & identity == IN)
  if (nrow(M1) >= 1) {
    indice_col_ext <- indice_col_ext + 1 }
  if (nrow(M2) >= 1) {
    indice_col_flex <- indice_col_flex + 1 }
  bw_y_M1 <- sd(M1$y)/kernel
  bw_z_M1 <- sd(M1$z)/kernel
  bw_y_M2 <- sd(M2$y)/kernel
  bw_z_M2 <- sd(M2$z)/kernel
  
  
  
  p <- ggplot() +
    geom_path(data = spinal_rc_y, aes(x = V1, y = V2), size = 1.5) +
    geom_path(data = spinal_rc_y_contra, aes(x = V1, y = V2), size = 1.5) +
    geom_density_2d(data = M1, aes(x = y, y = -z), bins = bin,  h=c(bw_y_M1, bw_z_M1), colour = ext_colors[1], show.legend = FALSE) +
    geom_density_2d(data = M2, aes(x = y, y = -z), bins = bin,  h=c(bw_y_M2, bw_z_M2), colour = flex_colors[1], show.legend = FALSE) 
  
  if (2 <= length(titer_used)) { 
    for (i in 2:length(titer_used)) {
      M1 <-  data_all %>% filter(method == method_used & titer == titer_used[i] & muscle == m1 & identity == IN) 
      M2 <-  data_all %>% filter(method == method_used & titer == titer_used[i] & muscle == m2 & identity == IN)
      if (nrow(M1) >= 1) {
        indice_col_ext <- indice_col_ext + 1
        bw_y_M1 <- sd(M1$y)/kernel
        bw_z_M1 <- sd(M1$z)/kernel
        p <- p +
          geom_density_2d(data = M1, aes(x = y, y = -z), bins = bin,  h=c(bw_y_M1, bw_z_M1), colour = ext_colors[as.numeric(indice_col_ext)], show.legend = FALSE) 
      } 
      if (nrow(M2) >= 1) {  
        indice_col_flex <- indice_col_flex + 1
        bw_y_M2 <- sd(M2$y)/kernel
        bw_z_M2 <- sd(M2$z)/kernel
        p <- p +
          geom_density_2d(data = M2, aes(x = y, y = -z), bins = bin,  h=c(bw_y_M2, bw_z_M2), colour = flex_colors[as.numeric(indice_col_flex)], show.legend = FALSE) 
      }
      
    }
  }    
  
  p <- p + 
    scale_y_continuous(limits=c(-3000, 4000), breaks=c(-2500, 0, 3000), expand = c(0, 0)) +
    scale_x_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    ylab("") +  
    xlab("V-D (μm)") +
    geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.5) +
    geom_segment(aes(x=0, xend= 0, y=-2500, yend = 4000), linetype="dashed", color = "black", size=0.5) +
    theme(axis.title.x = element_text(vjust=16), axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent")) + 
    annotation_custom(textGrob("L4", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = 250, xmax = 350, ymin = 200 , ymax = 400) +
    annotation_custom(textGrob("L5", gp = gpar(col = "black", fontsize = 18)), 
                      xmin = 250, xmax = 350, ymin = -200, ymax = -400)
  
  
  p
}


## Functions correlation/Hedges'G ##
get_Corr_HedgesG <- function(data_all, m1, m2, method_used, titer_used, N, lim) {
  corr <- data.frame(sample_1 = c(), sample_2 = c(), CorrCoef= c(), Hedges = c())
  data_corr <- data_all %>% filter(muscle == m1 & identity == IN | muscle == m2 & identity == IN)
  data_corr$animal <- str_c(data_corr$muscle, "_", data_corr$animal)
  remove <- c("_n_djm","_n_rmb", "_n_nz", "_n_slp")
  data_corr$animal <- str_remove_all(data_corr$animal, paste(remove, collapse = "|"))
  if (length(titer_used) == 1){
    if (titer_used != 0) {
    df <- data_corr %>% filter(method == as.character(method_used), titer == as.character(titer_used))
    }
    if (titer_used == 0){
      df <- data_corr %>% filter(method == as.character(method_used))
    } 
  }
  if (length(titer_used) == 2){
    df <- data_corr %>% filter(method == as.character(method_used) & titer == as.character(titer_used[1]) |method == as.character(method_used) & titer == as.character(titer_used[2]) )
  }
  animals <- distinct(data.frame(df$animal))
  Animal <- as.character(animals$df.animal)
  for (k in 1:length(Animal)) {
    d1 <-  data_corr %>% filter(animal == Animal[k]) 
    M1 <- kde2d(d1$x,d1$y, n = N, lims = lim) # for other type of bandwidth computing h = c(width.SJ(d2$x),width.SJ(d2$y))
    med1 <- median(M1$z)
    d1h <- d1 %>% filter(x >= 0, y >= 0)
    MeanX1 <- mean(d1h$x)
    for (l in k:length(Animal)) {
      d2 <-  data_corr %>% filter(animal == Animal[l])
      d2h <- d2 %>% filter(x >=  0, y >=  0)
      MeanX2 <- mean(d2h$x)
      M2 <- kde2d(d2$x,d2$y, n = N, lims = lim) # for other type of bandwidth computing h = c(width.SJ(d2$x),width.SJ(d2$y))
      med2 <- median(M2$z)
      Sn <- 0
      Sd1 <- 0
      Sd2 <- 0
      for (i in 1:N[1]){
        for (j in 1:N[2]){
          n <- (M1$z[i,j] - med1)*(M2$z[i,j] - med2)
          Sn <- Sn + n
          d1 <- (M1$z[i,j] - med1)^2
          Sd1 <- Sd1 + d1
          d2 <- (M2$z[i,j] - med2)^2
          Sd2 <- Sd2 + d2
        }
      }
      S <- sqrt(Sd1*Sd2)
      Rnm <- Sn/S # correlation coef
      M <- MeanX1 - MeanX2
      SS <- sqrt( (((nrow(d1h)-1)*sd(d1h$x)^2) + ((nrow(d2h)-1)*sd(d2h$x)^2))/(nrow(d1h)+nrow(d2h)-2))
      HedgesG <- M/SS
      R <- c(Animal[k], Animal[l], Rnm, HedgesG)
      corr <- rbind(corr, R, stringsAsFactors = FALSE)
    }
  }
  colnames(corr) <- c("sample_1", "sample_2", "CorrCoef", "HedgesG")
  corr
}
organise_Corr_hedges <- function(coor_hedges_df){
  samples <- distinct(data.frame(coor_hedges_df$sample_1))
  Sample <- as.character(samples$coor_hedges_df.sample_1)
  M_fig <- matrix(0, length(Sample), length(Sample))
  c <- 0
  for (i in 1: length(Sample)){
    for (j in i:length(Sample)){
      c <- c + 1
      M_fig[j,i] <- abs(as.numeric(coor_hedges_df[c,4]))
      M_fig[i,j] <- as.numeric(coor_hedges_df[c,3])
    }
  }
  rownames(M_fig) <- Sample
  colnames(M_fig) <- Sample
  M_fig
} 
Corr_HedgesG <- function(data_all, m1, m2, method_used, titer_used, N, lim, panel, color1, color2, nb_samples_1, nb_samples_2, numbers) {
  
  corr_hedges_df <- get_Corr_HedgesG(data_all, m1, m2, method_used, titer_used, N, lim)
  M_corr_hedges <- organise_Corr_hedges(corr_hedges_df)
  M_corr_hedges_melt <- melt(M_corr_hedges[(as.numeric(nb_samples_1)+as.numeric(nb_samples_2)):1,(as.numeric(nb_samples_1)+as.numeric(nb_samples_2)):1]) #it has to be a matrix so that the melt function takes Var1 = rownames and Var2= colnames
  l <- distinct(data.frame(M_corr_hedges_melt$Var1))
  l <- unlist(l, use.names = FALSE)
  M_corr_hedges_melt$Var1 <- factor(M_corr_hedges_melt$Var1, levels = l, ordered = TRUE)
  l <- rev(l)
  M_corr_hedges_melt$Var2 <- factor(M_corr_hedges_melt$Var2, levels = l, ordered = TRUE)
  
  Figure <- ggplot(M_corr_hedges_melt, aes(x= Var1, y =Var2, fill = value)) + 
    geom_tile() +
    scale_fill_gradient(low = 'black', high = 'white', limit =c(-0.000001, 1), breaks = c(0.0, 1.0)) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_discrete(expand = c(0, 0)) +
    ylab("") +  
    xlab("") +
    labs(fill="") +
    guides(fill = guide_colourbar(barheight = 12)) + 
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1, colour = c(rep(as.character(color2),nb_samples_2),rep(as.character(color1), nb_samples_1))),
          axis.text.y = element_text(colour = c(rep(as.character(color1),nb_samples_1),rep(as.character(color2), nb_samples_2))),
          legend.title = element_text(angle = - 90, hjust = 1),
          legend.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.box.background = element_rect(fill='transparent', colour = NA_character_),
          legend.key = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          panel.background = element_rect(fill = "transparent", colour = NA_character_)) +
    annotate("text", x = (nb_samples_2 + nb_samples_1/2 + 0.5), y = (nb_samples_1 + nb_samples_2/2 + 0.5), size = 9, colour = 'black', label = "C") +
    annotate("text", x = (nb_samples_2/2 + 0.5), y = (nb_samples_1/2 + 0.5), size = 9, colour = 'red', label = "H") +
    annotate("segment", x = (nb_samples_2 +0.5), xend = (nb_samples_2 +0.5), y = 0, yend = (nb_samples_1 +0.5), colour = "white", size=2) + 
    annotate("segment", x = 0, xend = (nb_samples_2 +0.5), y = (nb_samples_1 +0.5), yend = (nb_samples_1 +0.5), colour = "white", size=2) +
    annotate("segment", x = (nb_samples_2 +0.5), xend = (nb_samples_2 +0.5), y = (nb_samples_1 +0.5), yend = (nb_samples_1 + nb_samples_2 +0.5), colour = "black", size=2) + 
    annotate("segment", x = (nb_samples_2 +0.5), xend = (nb_samples_1 + nb_samples_2 +0.5), y = (nb_samples_1 +0.5), yend = (nb_samples_1 +0.5), colour = "black", size=2)
  
  if (numbers == 1){
    Figure <- Figure +
      geom_text(data = M_corr_hedges_melt, aes(label = round(value, 2), colour = value), show.legend = FALSE) + 
      #scale_colour_manual(values = c('red', 'black'))
      scale_colour_gradient2(low = 'red', mid = 'red', high = 'black')
  }
  Figure  <- ggarrange(NULL, Figure, nrow = 1, widths = c(0.2, 4), labels = as.character(panel), font.label = list(size = 26, color = "black"))
  
  Figure
}

boot_method_muscle <- function(data_all, Animal, R, m1){
  
  C <- 1 
  for (i in 1:length(Animal)){ #Select the Animal in the random distribution with the highest number of observations
    df <- data_all %>% filter(animal == Animal[i] & muscle == m1 & identity == IN, x>0, y>0)
    if (nrow(df)>C){
      C <- nrow(df)
    }
  }
  
  set.seed(4)
  stat_mean <- c()
  stat_sd <- c()
  stat_counter <- c()
  for (j in 1:R){
    Samples <- sample(1:length(Animal), length(Animal), replace=TRUE)
    bootstrap_stat_mean <- c()
    Count_n <- 0
    for (i in 1:length(Animal)){ #Select a distribution of C values of x in each sample that verify x>0, y>0
      A <- Samples[i] 
      df <- data_all %>% filter(animal == Animal[A] & muscle == m1 & identity == IN, x>0, y>0)
      P <- sample(1:nrow(df), C, replace=TRUE)
      #bootstrap_stat_median <- c()
      for (k in 1:C){
        Count_n <- Count_n+1
        bootstrap_stat_mean[Count_n] <- df$x[P[k]] 
      }
    }
    stat_counter[j] <- C*length(Animal) 
    stat_mean[j] <- mean(bootstrap_stat_mean)
    stat_sd[j] <- sd(bootstrap_stat_mean)
  }
  sum_stats <- cbind(stat_counter, stat_mean, stat_sd) #data frame that contains for each replica nb_trial*nb_animal, median and sd
  colnames(sum_stats) <- c("count", "mean", "SD")
  sum_stats
}
Calcul_HedgesG <- function(boot_m1_M1, boot_m2_M2){ #this function takes the outputs of bootstrap and calculate the Hedges for each replicate
  if (nrow(boot_m1_M1) != nrow(boot_m2_M2)){
    print("error in data frame dimensions, nrow df1 should match nrow df2")
  }
  if (nrow(boot_m1_M1) == nrow(boot_m2_M2)){
    Hedges <- c()
    for (i in 1:nrow(boot_m1_M1)){
      M <- boot_m1_M1$mean[i] - boot_m2_M2$mean[i]
      D <- sqrt(((boot_m1_M1$count[i] - 1)*boot_m1_M1$SD[i]^2 + (boot_m2_M2$count[i] - 1)*boot_m2_M2$SD[i]^2)/(boot_m1_M1$count[i]+boot_m2_M2$count[i]-2))
      Hedges[i] <- M/D
    }
  }
  Hedges
}
get_Corr_concatenate <- function(data_all, titer_used, N, lim) { #takes a data frame with the methods we want to compare 
  #and calculate correlations. If titer_used = 1 it splits the low and high titer conditions. 
  corr <- data.frame(group_1 = c(), group_2 = c(), CorrCoef= c())
  data_corr <- data_all %>% filter(identity == IN)
  
  
  if (titer_used == 1){
    data_corr <- data_corr %>% mutate(method = case_when(as.character(data_corr$method) == "G_Rab_ChAT_RGT" ~ str_c(data_corr$method, "_", data_corr$titer), 
                                                         as.character(data_corr$method) != "G_Rab_ChAT_RGT" ~ str_c(data_corr$method)))
  }
  data_corr$method <- str_c(data_corr$muscle, "_", data_corr$method)
  methods <- distinct(data.frame(data_corr$method))
  Method <- as.character(methods$data_corr.method) #list of the methods
  for (k in 1:length(Method)) {
    d1 <-  data_corr %>% filter(method == Method[k]) 
    M1 <- kde2d(d1$x,d1$y, n = N, lims = lim) # for other type of bandwidth computing h = c(width.SJ(d2$x),width.SJ(d2$y))
    med1 <- median(M1$z)
    for (l in k:length(Method)) {
      d2 <-  data_corr %>% filter(method == Method[l])
      M2 <- kde2d(d2$x,d2$y, n = N, lims = lim) # for other type of bandwidth computing h = c(width.SJ(d2$x),width.SJ(d2$y))
      med2 <- median(M2$z)
      Sn <- 0
      Sd1 <- 0
      Sd2 <- 0
      for (i in 1:N[1]){
        for (j in 1:N[2]){
          n <- (M1$z[i,j] - med1)*(M2$z[i,j] - med2)
          Sn <- Sn + n
          d1 <- (M1$z[i,j] - med1)^2
          Sd1 <- Sd1 + d1
          d2 <- (M2$z[i,j] - med2)^2
          Sd2 <- Sd2 + d2
        }
      }
      S <- sqrt(Sd1*Sd2)
      Rnm <- Sn/S # correlation coef
      R <- c(Method[k], Method[l], Rnm)
      corr <- rbind(corr, R, stringsAsFactors = FALSE)
    }
  }
  colnames(corr) <- c("method_1", "method_2", "CorrCoef")
  corr
}
get_Hedges_boot <- function(titer_used, data_1, R){#takes a data frame with the methods we want to compare 
  #and calculate Hedges. If titer_used = 1 it splits the low and high titer conditions.
  list_boot_m_M <- list()
  HedgesG <- data.frame(method_1 = c(), method_2 = c(), HedgesGcoef = c())
  df <- data_1 %>% filter(identity == IN)
  if (titer_used == 1){
    df <- df %>% mutate(method = case_when(as.character(df$method) == "G_Rab_ChAT_RGT" ~ str_c(df$method, "_", df$titer), 
                                           as.character(df$method) != "G_Rab_ChAT_RGT" ~ str_c(df$method)))
    #df$method <- str_c(df$method, "_", df$titer)
  }
  df$method <- str_c(df$muscle, "_", df$method)
  methods <- distinct(data.frame(df$method))
  Method <- as.character(methods$df.method) #list of the methods
  for (k in 1:length(Method)) {
    data_H <- df %>% filter(method == as.character(Method[k]))
    muscles <- distinct(data.frame(data_H$muscle))
    Muscle <- as.character(muscles$data_H.muscle)
    data_H <- data_H %>% filter(muscle == as.character(Muscle))
    animals <- distinct(data.frame(data_H$animal))
    Animal <- as.character(animals$data_H.animal)
    boot_m_M <- boot_method_muscle(data_1, Animal, R, Muscle)
    ldf <- list()
    Name <- paste(as.character(Method[k])) 
    ldf[[Name]] <- list(boot_m_M)
    list_boot_m_M <- c(list_boot_m_M, ldf) 
  }
  for (i in 1:length(list_boot_m_M)){
    boot_m_M1 <- data.frame(list_boot_m_M[[i]])
    for (j in i:length(list_boot_m_M)){
      boot_m_M2 <- data.frame(list_boot_m_M[[j]])
      H <- Calcul_HedgesG(boot_m_M1, boot_m_M2)
      median_Hedges <- median(H)
      method_1 <- names(list_boot_m_M)[i]
      method_2 <- names(list_boot_m_M)[j]
      R <- c(method_1, method_2, median_Hedges)
      HedgesG <- rbind(HedgesG, R, stringsAsFactors = FALSE)
    }
  }
  colnames(HedgesG) <- c("method_1", "method_2", "HedgesGcoef")
  HedgesG
}
Corr_HedgesG_methods <- function(df_corr_Hedges, panel, color1, color2, nb_samples_1, nb_samples_2, numbers){
  
  df_corr_Hedges_melt <- melt(df_corr_Hedges[(as.numeric(nb_samples_1)+as.numeric(nb_samples_2)):1,])
  #it has to be a matrix so that the melt function takes Var1 = rownames and Var2= colnames
  l <- distinct(data.frame(df_corr_Hedges_melt$Var1))
  l <- unlist(l, use.names = FALSE)
  df_corr_Hedges_melt$Var1 <- factor(df_corr_Hedges_melt$Var1, levels = l, ordered = TRUE) 
  l <- rev(l)
  df_corr_Hedges_melt$Var2 <- factor(df_corr_Hedges_melt$Var2, levels = l, ordered = TRUE)
  
  Figure <- ggplot(df_corr_Hedges_melt, aes(x= Var1, y =Var2, fill = value)) + 
    geom_tile() +
    scale_fill_gradient(low = 'black', high = 'white', limit =c(-0.000001, 1), breaks = c(0.0, 1.0)) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_discrete(expand = c(0, 0)) +
    ylab("") +  
    xlab("") +
    labs(fill="") +
    guides(fill = guide_colourbar(barheight = 12)) + 
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1, colour = c(rev(color1),rev(color2))),
          axis.text.y = element_text(colour = c(color2,color1)),
          legend.title = element_text(angle = - 90, hjust = 1),
          legend.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.box.background = element_rect(fill='transparent', colour = NA_character_),
          legend.key = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          panel.background = element_rect(fill = "transparent", colour = NA_character_)) 
  
  if (numbers == 1){
    Figure <- Figure +
      geom_text(data = df_corr_Hedges_melt, aes(label = round(value, 2), colour = value), show.legend = FALSE) + 
      scale_colour_gradient2(low = 'red', mid = 'red', high = 'black')
  }
  Figure  <- ggarrange(NULL, Figure, nrow = 1, widths = c(0.2, 4), labels = as.character(panel), font.label = list(size = 26, color = "black"))
  
  Figure
}

## Functions boxplot applied to individual samples ##
boxplot_per_sample_interactive <- function(data, method_used, titer_used, m1, m2, color1, color2, ymin, ymax){
  
  remove <- c("_n_djm","_n_rmb", "_n_nz", "_n_slp")
  data$animal <- str_remove_all(data$animal, paste(remove, collapse = "|"))
  if (titer_used != 0){
    x_dorsal_ipsi_m1 <- data %>% filter(y >= 0, x >= 0, method == method_used, identity == "IN", muscle == m1, titer == titer_used)
    x_dorsal_ipsi_m2 <- data %>% filter(y >= 0, x >= 0, method == method_used, identity == "IN", muscle == m2,  titer == titer_used)
  }
  if (titer_used == 0){
    x_dorsal_ipsi_m1 <- data %>% filter(y >= 0, x >= 0, method == method_used, identity == "IN", muscle == m1)
    x_dorsal_ipsi_m2 <- data %>% filter(y >= 0, x >= 0, method == method_used, identity == "IN", muscle == m2)
  }
  x_dorsal_ipsi_m1$muscle <- factor(x_dorsal_ipsi_m1$muscle, levels = c(as.character(m1), as.character(m2)))
  x_dorsal_ipsi_m2$muscle <- factor(x_dorsal_ipsi_m2$muscle, levels = c(as.character(m1), as.character(m2)))
  
  x_dorsal_ipsi_m1$animal <- paste(as.character(m1), "_",  x_dorsal_ipsi_m1$animal, sep="")
  x_dorsal_ipsi_m2$animal <- paste(as.character(m2), "_",  x_dorsal_ipsi_m2$animal, sep="")
  nb_sample_m1 <- nrow(distinct(data.frame(x_dorsal_ipsi_m1$animal)))
  nb_sample_m2 <- nrow(distinct(data.frame(x_dorsal_ipsi_m2$animal)))
  c1 <- distinct(data.frame(x_dorsal_ipsi_m1$animal))
  c2 <- distinct(data.frame(x_dorsal_ipsi_m2$animal))
  l <- c(c1, c2)
  l <- unlist(l, use.names = FALSE)
  x_dorsal_ipsi <- rbind(x_dorsal_ipsi_m1, x_dorsal_ipsi_m2)
  x_dorsal_ipsi$animal <- factor(x_dorsal_ipsi$animal, levels = l)
  
  graph <- ggplot(data = x_dorsal_ipsi,aes(x = animal, y = x, colour = muscle)) +
    geom_boxplot( show.legend = FALSE, width= 0.4, size = 1, outlier.shape = 3) +
    scale_color_manual(values = c(as.character(color1), as.character(color2))) +
    scale_y_continuous(limits=c(ymin, ymax), breaks=seq(from = ymin, to = 850, by = 50), expand = c(0, 0)) +
    ylab("M-L (μm)") +  
    xlab("") +
    theme(axis.text=element_text(size=12, colour = "black")) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust=1)) +
    theme(axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"))
  
  font = list(family = "Arial" , size = 14, color = "white")
  label = list(bordercolor = "transparent", font = font)
  
  graph_interactive = ggplotly(graph, height = 450, width=800) %>%
    style(hoverlabel = label) %>%
    layout(font = font)
  
  graph_interactive$x$data <- lapply(graph_interactive$x$data, FUN = function(x){
    x$marker$outliercolor = x$line$color 
    x$marker$color = x$line$color 
    x$marker$line = x$line$color
    return(x)
  })
  
  graph_interactive
}

## Function building a data frame with the median of each sample ##

df_individual <- function(data_all, method_used, titer_used, m1, m2, M1, M2){
  
  if (titer_used != 0){
    data_m1 <- data_all %>% filter(y > 0, x > 0, method == method_used, identity == "IN", muscle == m1, titer == titer_used)
    data_m2 <- data_all %>% filter(y > 0, x > 0, method == method_used, identity == "IN", muscle == m2, titer == titer_used)
  }
  if (titer_used == 0){
    data_m1 <- data_all %>% filter(y > 0, x > 0, method == method_used, identity == "IN", muscle == m1)
    data_m2 <- data_all %>% filter(y > 0, x > 0, method == method_used, identity == "IN", muscle == m2)
  }
  medio_lateral <- c()
  sample <- c()
  l_m1 <- distinct(data.frame(data_m1$animal))
  l_m1 <- unlist(l_m1, use.names = FALSE)
  l_m2 <- distinct(data.frame(data_m2$animal))
  l_m2 <- unlist(l_m2, use.names = FALSE)
  group <- rep(c(as.character(M1),as.character(M2)),times=c(length(l_m1), length(l_m2)))
  
  for (i in 1:length(l_m1)){
    df <- data_m1 %>% filter(animal == as.character(l_m1[i]))
    med <- median(df$x)
    sample  <- c(sample, as.character(l_m1[i]))
    medio_lateral <- c(medio_lateral, med)
  }
  for (i in 1:length(l_m2)){
    df <- data_m2 %>% filter(animal == as.character(l_m2[i]))
    med <- median(df$x)
    sample  <- c(sample, as.character(l_m2[i]))
    medio_lateral <- c(medio_lateral, med)
  }
  t = (as.numeric(length(l_m1)) + as.numeric(length(l_m2)))
  method <- c(rep(as.character(method_used),times = as.numeric(t)))
  dat<- data.frame(group, medio_lateral, sample, method)
  
  dat
}

## Functions boxplot and individual experiment side by side ##
Boxplot_individual <- function(data, color1, color2, ymin, ymax) { # Here I forced all the data to be included in whiskers
  graph <- ggplot(data = data, aes(y = medio_lateral)) +
    stat_boxplot(aes(x = group, colour = group), coef = 4, geom = "errorbar", width = 0.1, show.legend = FALSE, size = 1) +
    geom_boxplot(aes(x = group, colour = group), coef= 4, show.legend = FALSE, width= 0.2, size = 1) +
    scale_color_manual(values = c(as.character(color1), as.character(color2))) +
    scale_y_continuous(limits=c(ymin, ymax), breaks=seq(from = ymin, to = 850, by = 850), expand = c(0, 0)) +
    geom_jitter(aes(x = as.numeric(group) + 0.25, y = medio_lateral, colour = group), size = 3.5, show.legend = FALSE, fill = NA, alpha = 0.4,  width = 0.1) +
    ylab("M-L (μm)") +  
    xlab("") +
    theme(axis.text=element_text(size=12, colour = "black")) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 16, colour = c(as.character(color1), as.character(color2)))) +
    theme(axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"))
  
  graph
}
Boxplot_individual_interactive <- function(data, color1, color2, ymin, ymax) {
  
  graph <- ggplot(data = data, aes(y = medio_lateral, text = paste0("Medio-Lateral: ", round(medio_lateral, digits = 2)))) +
    geom_boxplot(aes(x = group, colour = group), show.legend = FALSE, width= 0.2, size = 1) +
    scale_color_manual(values = c(as.character(color1), as.character(color2))) +
    scale_y_continuous(limits=c(ymin, ymax), breaks=seq(from = ymin, to = 850, by = 50), expand = c(0, 0)) + #,labels = c(ymin, rep("", 3), 850)
    geom_jitter(aes(x = as.numeric(group) + 0.4, y = medio_lateral, colour = group), size = 3, show.legend = FALSE, fill = NA, alpha = 0.4,  width = 0.12) +
    ylab("M-L (μm)") +  
    xlab("") +
    theme(axis.text=element_text(size=12, colour = "black")) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 16)) +
    theme(axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"))
  
  
  font = list(family = "Arial" , size = 14, color = "white")
  label = list(bordercolor = "transparent", font = font)
  
  graph_interactive = ggplotly(graph, height = 450, width=200, tooltip = c("text")) %>%
    style(hoverlabel = label) %>%
    layout(font = font)
  
  return (graph_interactive)
}

## Functions boxplot and individual experiment, methods side by side ##

Boxplot_methods_interactive <- function(data, colors_vector, ymin, ymax) {
  graph <- ggplot(data = data, aes(x = group, y = medio_lateral, text = paste0("Medio-Lateral: ", round(medio_lateral, digits = 2)))) +
    geom_boxplot(aes(colour = group), show.legend = FALSE, width= 0.2, size = 1) +
    scale_color_manual(values = colors_vector) +
    scale_y_continuous(limits=c(ymin, ymax), breaks=seq(from = ymin, to = 850, by = 50), expand = c(0, 0)) +
    geom_jitter(aes(x = as.numeric(group) + 0.45, y = medio_lateral, colour = group), size = 3, show.legend = FALSE, fill = NA, alpha = 0.4,  width = 0.12) +
    ylab("M-L (μm)") +  
    xlab("") +
    theme(axis.text=element_text(size=12, colour = "black")) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 12, angle = 45, hjust=1)) +
    theme(axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"))
  
  
  font = list(family = "Arial" , size = 14, color = "white")
  label = list(bordercolor = "transparent", font = font)
  
  graph_interactive = ggplotly(graph, height = 450, width=600, tooltip = c("text")) %>%
    style(hoverlabel = label) %>%
    layout(font = font)
  
  return (graph_interactive)
}

## -- function to collect density for fig3 supp 1 -- ##
xy_dot_density_center_concatenate_fig3_figSupp <- function (spinal, L, m1, m2, color1, color2, kernel, Size, ymin, ymax, xmin, xmax) {
  
  M1 <-  L %>% filter(muscle == m1 & identity == IN) 
  M2 <-  L %>% filter(muscle == m2 & identity == IN)
  
  p <- ggplot() +
    annotate("point", x = 0, y = 0, size = 4, colour = "black") +
    geom_path(data = spinal, aes(x = V1, y = V2), size = 1.5) + 
    geom_point(data = M1, aes(x = x, y = y, shape = identity, size = identity), colour = color1, show.legend = FALSE) +
    geom_point(data = M2, aes(x = x, y = y, shape = identity, size = identity), colour = color2, show.legend = FALSE) 
  
  p <- p +
    scale_y_continuous(limits=c(ymin, ymax), breaks=seq(from = ymin, to = ymax, by = ymax), expand = c(0, 0)) +
    scale_x_continuous(limits=c(xmin, xmax), breaks=seq(from = xmin, to = xmax, by = xmax), expand = c(0, 0)) +
    scale_size_manual(values = Size) +
    ylab("") +  
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"))
  
  p
}
x_ventral_1d_density_concatenate_fig3_figSupp <- function(L, m1, m2, color1, color2, LX, kernel, xmin, xmax, ylimit){
  
  x_ventral_M1 <- L %>% filter(y <= 0 & muscle == m1 & identity == "IN")
  x_ventral_M2 <- L %>% filter(y <= 0 & muscle == m2 & identity == "IN")
  l_M1 <-  L %>% filter(muscle == m1 & identity == "IN") 
  l_M2 <-  L %>% filter(muscle == m2 & identity == "IN")
  
  scale_ventral_M2 <- nrow(x_ventral_M2)/nrow(l_M2)
  scale_ventral_M1 <- nrow(x_ventral_M1)/nrow(l_M1)
  bw_x_M1 <- sd(l_M1$x)/kernel
  bw_x_M2 <- sd(l_M2$x)/kernel
  
  x_ventral_1d_density <- ggplot() +
    stat_density(data = x_ventral_M1, aes(x = x, y=stat(density)*scale_ventral_M1), bw = bw_x_M1, kernel = "gaussian", geom = "line", colour = color1, position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = x_ventral_M2, aes(x = x, y=stat(density)*scale_ventral_M2), bw = bw_x_M2, kernel = "gaussian", geom = "line", colour = color2, position = "identity", size = 1, show.legend = FALSE) +
    scale_x_continuous(limits=c(xmin, xmax), breaks=seq(from = xmin, to = xmax, by = xmax), expand = c(0, 0)) +
    scale_y_reverse(limits=c(ylimit, 0), expand = c(.2, 0, 0, 0))+
    theme_classic() +
    ylab("") +
    xlab("M-L (μm)") +
    theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(), axis.title.x = element_text(vjust=13),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    annotate("text", x = xmin + 150, y = ylimit/2, size = 6.5, colour = "black", label = as.character(LX))
  
  
  x_ventral_1d_density
}
x_dorsal_1d_density_concatenate_fig3_figSupp <- function(L, m1, m2, color1, color2, LX, kernel, M1, M2, xmin, xmax){
  
  x_dorsal_M1 <- L %>% filter(y >= 0 & muscle == m1 & identity == "IN")
  x_dorsal_M2 <- L %>% filter(y >= 0 & muscle == m2 & identity == "IN")
  l_M1 <-  L %>% filter(muscle == m1 & identity == "IN") 
  l_M2 <-  L %>% filter(muscle == m2 & identity == "IN")
  
  scale_dorsal_M2 <- nrow(x_dorsal_M2)/nrow(l_M2)
  scale_dorsal_M1 <- nrow(x_dorsal_M1)/nrow(l_M1)
  bw_x_M1 <- sd(l_M1$x)/kernel
  bw_x_M2 <- sd(l_M2$x)/kernel
  
  x_dorsal_1d_density <- ggplot() +
    stat_density(data = x_dorsal_M1, aes(x = x, y=stat(density)*scale_dorsal_M1), bw = bw_x_M1, kernel = "gaussian", geom = "line", colour = color1, position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = x_dorsal_M2, aes(x = x, y=stat(density)*scale_dorsal_M2), bw = bw_x_M2, kernel = "gaussian", geom = "line", colour = color2, position = "identity", size = 1, show.legend = FALSE) +
    scale_x_continuous(limits=c(xmin, xmax), breaks=seq(from = xmin, to = xmax, by = xmax), expand = c(0, 0)) +
    scale_y_continuous(limits=c(0, NA), expand = c(0, 0, .2, 0))+
    theme_classic() +
    ylab("") +  
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_), # necessary to avoid drawing plot outline
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    annotation_custom(textGrob(as.character(M1), gp = gpar(col = as.character(color1), fontface = 'bold', fontsize = 20)), 
                      xmin = -700, xmax = -600, ymin = 0.001, ymax = 0.002) +
    annotation_custom(textGrob(as.character(M2), gp = gpar(col = as.character(color2), fontface = 'bold', fontsize = 20)), 
                      xmin = -400, xmax = -300, ymin = 0.001, ymax = 0.002)
  
  x_dorsal_1d_density
}
y_contra_1d_density_concatenate_fig3_figSupp <- function(L, m1, m2, color1, color2, kernel, ymin, ymax, ylimit){
  
  y_contra_M1 <- L %>% filter(x <= 0 & muscle == m1 & identity == "IN")
  y_contra_M2 <- L %>% filter(x <= 0 & muscle == m2 & identity == "IN")
  l_M1 <-  L %>% filter(muscle == m1 & identity == "IN") 
  l_M2 <-  L %>% filter(muscle == m2 & identity == "IN") 
  
  scale_contra_M2 <- nrow(y_contra_M2)/nrow(l_M2)
  scale_contra_M1 <- nrow(y_contra_M1)/nrow(l_M1)
  bw_y_M1 <- sd(l_M1$y)/kernel
  bw_y_M2 <- sd(l_M2$y)/kernel
  
  y_contra_1d_density <- ggplot() +
    stat_density(data = y_contra_M1, aes(x = y, y=stat(density)*scale_contra_M1), bw = bw_y_M1, kernel = "gaussian", geom = "line", colour = color1, position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = y_contra_M2, aes(x = y, y=stat(density)*scale_contra_M2), bw = bw_y_M2, kernel = "gaussian", geom = "line", colour = color2, position = "identity", size = 1, show.legend = FALSE) +
    scale_x_continuous(limits=c(ymin, ymax), breaks=seq(from = ymin, to = ymax, by = ymax), expand = c(0, 0)) +
    scale_y_reverse(limits=c(ylimit, 0), expand = c(.2, 0, 0, 0))+
    theme_classic() +
    ylab("") +  
    xlab("V-D (μm)") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(), axis.title.y = element_text(vjust=-16), panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate() 
  
  y_contra_1d_density
}
y_ipsi_1d_density_concatenate_fig3_figSupp <- function(L, m1, m2, color1, color2, kernel, ymin, ymax){
  
  y_ipsi_M1 <- L %>% filter(x >= 0 & muscle == m1 & identity == "IN")
  y_ipsi_M2 <- L %>% filter(x >= 0& muscle == m2 & identity == "IN")
  l_M1 <-  L %>% filter(muscle == m1 & identity == "IN") 
  l_M2 <-  L %>% filter(muscle == m2 & identity == "IN") 
  
  scale_ipsi_M2 <- nrow(y_ipsi_M2)/nrow(l_M2)
  scale_ipsi_M1 <- nrow(y_ipsi_M1)/nrow(l_M1)
  bw_y_M1 <- sd(l_M1$y)/kernel
  bw_y_M2 <- sd(l_M2$y)/kernel
  
  y_ipsi_1d_density <- ggplot() +
    stat_density(data = y_ipsi_M1, aes(x = y, y=stat(density)*scale_ipsi_M1), bw = bw_y_M1, kernel = "gaussian", geom = "line", colour = color1, position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = y_ipsi_M2, aes(x = y, y=stat(density)*scale_ipsi_M2), bw = bw_y_M2, kernel = "gaussian", geom = "line", colour = color2, position = "identity", size = 1, show.legend = FALSE) +
    scale_x_continuous(limits=c(ymin, ymax), breaks=seq(from = ymin, to = ymax, by = ymax), expand = c(0, 0)) +
    scale_y_continuous(limits=c(0, NA), expand = c(0, 0, 0.2, 0)) +
    theme_classic() +
    ylab("") +  
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate()
  
  y_ipsi_1d_density
}
complete_graph_xy_density_fig3_figSupp <- function(spinal, L, m1, m2, color1, color2, LX, kernel, M1, M2, Size, ymin, ymax, xmin, xmax, panel){
  
  x_dorsal <- x_dorsal_1d_density_concatenate_fig3_figSupp(L, m1, m2, color1, color2, LX, kernel, M1, M2, xmin, xmax)
  ylimits <- layer_scales(x_dorsal)$y$get_limits() #to get the ylim of the axis
  ylimit <- max(ylimits)
  x_ventral <- x_ventral_1d_density_concatenate_fig3_figSupp(L, m1, m2, color1, color2, LX, kernel, xmin, xmax, ylimit)
  y_ipsi <- y_ipsi_1d_density_concatenate_fig3_figSupp(L, m1, m2, color1, color2, kernel, ymin, ymax)
  ylimits <- layer_scales(y_ipsi)$y$get_limits() #to get the ylim of the axis
  ylimit <- max(ylimits)
  y_contra <- y_contra_1d_density_concatenate_fig3_figSupp(L, m1, m2, color1, color2, kernel, ymin, ymax, ylimit)
  p <- xy_dot_density_center_concatenate_fig3_figSupp(spinal, L, m1, m2, color1, color2, kernel, Size, ymin, ymax, xmin, xmax)
  graph <- xy_density_arrange_1d(x_dorsal, y_contra, p, y_ipsi, x_ventral, panel)
  
  graph
}

## -- function to collect density for fig3 supp 2 -- ##

spinal_transform <- function(spinal_raw, segment_norm) {
  spinal_raw <- spinal_raw %>% mutate(V2 = ifelse(V2 > 0, V2 * 1600, V2 * 1300))
  spinal_raw$V1 <- spinal_raw$V1 * 2 * segment_norm
  spinal_contra <- spinal_raw[order(nrow(spinal_raw):1),]
  spinal_contra$V1[sapply(spinal_contra$V1 , is.numeric)] <- spinal_contra$V1 [sapply(spinal_contra$V1, is.numeric)] * -1
  spinal_L <- rbind(spinal_raw, spinal_contra)
  spinal_L
}

xy_dot_density_center_concatenate_fig3_figSupp2 <- function(spinal_L, L_lg, L_ta, color1, color2, segment, kernel, LG, TA, ymin, ymax, xmin, xmax){
  p <- ggplot() +
    annotate("point", x = 0, y = 0, size = 4, colour = "black") +
    geom_path(data = spinal_L, aes(x = V1, y = V2), size = 1.5) + 
    geom_point(data = L_lg, aes(x = x, y = y), size = 0.05, colour = color1, show.legend = FALSE, alpha = 0.6) +
    geom_point(data = L_ta, aes(x = x, y = y), size = 0.05, colour = color2, show.legend = FALSE, alpha = 0.6) + 
    scale_y_continuous(limits=c(ymin, ymax), breaks=c(ymin, 0, ymax), expand = c(0, 0)) +
    scale_x_continuous(limits=c(xmin, xmax), breaks=seq(from = xmin, to = xmax, by = xmax), expand = c(0, 0)) +
    ylab("") +  
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"))
  p
}
x_ventral_1d_density_concatenate_fig3_figSupp2 <- function(L_lg, L_ta, color1, color2, LX, kernel, xmin, xmax, ylimit){
  
  x_ventral_M1 <- L_lg %>% filter(y <= 0)
  x_ventral_M2 <- L_ta %>% filter(y <= 0)
  
  scale_ventral_M1 <- nrow(x_ventral_M1)/nrow(L_lg)
  scale_ventral_M2 <- nrow(x_ventral_M2)/nrow(L_ta)
  bw_x_M1 <- sd(L_lg$x)/kernel
  bw_x_M2 <- sd(L_ta$x)/kernel
  
  x_ventral_1d_density <- ggplot() +
    stat_density(data = x_ventral_M1, aes(x = x, y=stat(density)*scale_ventral_M1), bw = bw_x_M1, kernel = "gaussian", geom = "line", colour = color1, position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = x_ventral_M2, aes(x = x, y=stat(density)*scale_ventral_M2), bw = bw_x_M2, kernel = "gaussian", geom = "line", colour = color2, position = "identity", size = 1, show.legend = FALSE) +
    scale_x_continuous(limits=c(xmin, xmax), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    scale_y_reverse(limits=c(ylimit, 0), expand = c(.2, 0, 0, 0))+
    theme_classic() +
    ylab("") +
    xlab("M-L (μm)") +
    theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(), axis.title.x = element_text(vjust=13),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    annotate("text", x = xmin + 150, y = 0.0015, size = 6.5, colour = "black", label = as.character(LX))
  
  
  x_ventral_1d_density
}
x_dorsal_1d_density_concatenate_fig3_figSupp2 <- function(L_lg, L_ta, color1, color2, LX, kernel, M1, M2, xmin, xmax){
  
  x_dorsal_M1 <- L_lg %>% filter(y >= 0)
  x_dorsal_M2 <- L_ta %>% filter(y >= 0)
  
  scale_dorsal_M1 <- nrow(x_dorsal_M1)/nrow(L_lg)
  scale_dorsal_M2 <- nrow(x_dorsal_M2)/nrow(L_ta)
  bw_x_M1 <- sd(L_lg$x)/kernel
  bw_x_M2 <- sd(L_ta$x)/kernel
  
  x_dorsal_1d_density <- ggplot() +
    stat_density(data = x_dorsal_M1, aes(x = x, y=stat(density)*scale_dorsal_M1), bw = bw_x_M1, kernel = "gaussian", geom = "line", colour = color1, position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = x_dorsal_M2, aes(x = x, y=stat(density)*scale_dorsal_M2), bw = bw_x_M2, kernel = "gaussian", geom = "line", colour = color2, position = "identity", size = 1, show.legend = FALSE) +
    scale_x_continuous(limits=c(xmin, xmax), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    scale_y_continuous(limits=c(0, NA), expand = c(0, 0, .2, 0))+
    theme_classic() +
    ylab("") +  
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_), # necessary to avoid drawing plot outline
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    annotation_custom(textGrob(as.character(M1), gp = gpar(col = as.character(color1), fontface = 'bold', fontsize = 20)), 
                      xmin = -850, xmax = -750, ymin = 0.001, ymax = 0.002) +
    annotation_custom(textGrob(as.character(M2), gp = gpar(col = as.character(color2), fontface = 'bold', fontsize = 20)), 
                      xmin = -550, xmax = -450, ymin = 0.001, ymax = 0.002)
  
  x_dorsal_1d_density
}
y_contra_1d_density_concatenate_fig3_figSupp2 <- function(L_lg, L_ta, color1, color2, kernel, ymin, ymax, ylimit){
  
  y_contra_M1 <- L_lg %>% filter(x <= 0)
  y_contra_M2 <- L_ta %>% filter(x <= 0)
  
  scale_contra_M1 <- nrow(y_contra_M1)/nrow(L_lg)
  scale_contra_M2 <- nrow(y_contra_M2)/nrow(L_ta)
  bw_y_M1 <- sd(L_lg$y)/kernel
  bw_y_M2 <- sd(L_ta$y)/kernel
  
  y_contra_1d_density <- ggplot() +
    stat_density(data = y_contra_M1, aes(x = y, y=stat(density)*scale_contra_M1), bw = bw_y_M1, kernel = "gaussian", geom = "line", colour = color1, position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = y_contra_M2, aes(x = y, y=stat(density)*scale_contra_M2), bw = bw_y_M2, kernel = "gaussian", geom = "line", colour = color2, position = "identity", size = 1, show.legend = FALSE) +
    scale_x_continuous(limits=c(ymin, ymax), breaks=c(ymin,0, ymax), expand = c(0, 0)) +
    scale_y_reverse(limits=c(ylimit, 0), expand = c(.2, 0, 0, 0))+
    theme_classic() +
    ylab("") +  
    xlab("V-D (μm)") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(), axis.title.y = element_text(vjust=-16), panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate() 
  
  y_contra_1d_density
}
y_ipsi_1d_density_concatenate_fig3_figSupp2 <- function(L_lg, L_ta, color1, color2, kernel, ymin, ymax){
  
  y_ipsi_M1 <- L_lg %>% filter(x >= 0)
  y_ipsi_M2 <- L_ta %>% filter(x >= 0)
  
  scale_ipsi_M1 <- nrow(y_ipsi_M1)/nrow(L_lg)
  scale_ipsi_M2 <- nrow(y_ipsi_M2)/nrow(L_ta)
  bw_y_M1 <- sd(L_lg$y)/kernel
  bw_y_M2 <- sd(L_ta$y)/kernel
  
  y_ipsi_1d_density <- ggplot() +
    stat_density(data = y_ipsi_M1, aes(x = y, y=stat(density)*scale_ipsi_M1), bw = bw_y_M1, kernel = "gaussian", geom = "line", colour = color1, position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = y_ipsi_M2, aes(x = y, y=stat(density)*scale_ipsi_M2), bw = bw_y_M2, kernel = "gaussian", geom = "line", colour = color2, position = "identity", size = 1, show.legend = FALSE) +
    scale_x_continuous(limits=c(ymin, ymax), breaks=seq(from = ymin, to = ymax, by = ymax), expand = c(0, 0)) +
    scale_y_continuous(limits=c(0, NA), expand = c(0, 0, .2, 0)) +
    theme_classic() +
    ylab("") +  
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate()
  
  y_ipsi_1d_density
}
complete_graph_xy_density_fig3_figSupp2 <- function(spinal_L, L_lg, L_ta, color1, color2, LX, kernel, M1, M2, ymin, ymax, xmin, xmax, panel){
  
  x_dorsal <- x_dorsal_1d_density_concatenate_fig3_figSupp2(L_lg, L_ta, color1, color2, LX, kernel, M1, M2, xmin, xmax)
  ylimits <- layer_scales(x_dorsal)$y$get_limits() #to get the ylim of the axis
  ylimit <- max(ylimits)
  x_ventral <- x_ventral_1d_density_concatenate_fig3_figSupp2(L_lg, L_ta, color1, color2, LX, kernel, xmin, xmax, ylimit)
  y_ipsi <- y_ipsi_1d_density_concatenate_fig3_figSupp2(L_lg, L_ta, color1, color2, kernel, ymin, ymax)
  ylimits <- layer_scales(y_ipsi)$y$get_limits() #to get the ylim of the axis
  ylimit <- max(ylimits)
  y_contra <- y_contra_1d_density_concatenate_fig3_figSupp2(L_lg, L_ta, color1, color2, kernel, ymin, ymax, ylimit)
  p <- xy_dot_density_center_concatenate_fig3_figSupp2(spinal_L, L_lg, L_ta, color1, color2, segment, kernel, LG, TA, ymin, ymax, xmin, xmax)
  graph <- xy_density_arrange_1d(x_dorsal, y_contra, p, y_ipsi, x_ventral, panel)
  
  graph
}



## -- Funciton for figure 4 figure supplement 1 -- ##
boxplot_per_sample_titer_interactive <- function(data, method_used, titer_used, m1, color1, color2, ymin, ymax){
  
  remove <- c("_n_djm","_n_rmb", "_n_nz", "_n_slp")
  data$animal <- str_remove_all(data$animal, paste(remove, collapse = "|"))
  
  x_dorsal_ipsi_t1 <- data %>% filter(y >= 0, x >= 0, method == method_used, identity == "IN", muscle == m1, titer == titer_used[1])
  x_dorsal_ipsi_t2 <- data %>% filter(y >= 0, x >= 0, method == method_used, identity == "IN", muscle == m1,  titer == titer_used[2])
  
  x_dorsal_ipsi_t1$animal <- paste(as.character(m1), "_",  x_dorsal_ipsi_t1$animal, sep="")
  x_dorsal_ipsi_t2$animal <- paste(as.character(m1), "_",  x_dorsal_ipsi_t2$animal, sep="")
  nb_sample_t1 <- nrow(distinct(data.frame(x_dorsal_ipsi_t1$animal)))
  nb_sample_t2 <- nrow(distinct(data.frame(x_dorsal_ipsi_t2$animal)))
  c1 <- distinct(data.frame(x_dorsal_ipsi_t1$animal))
  c2 <- distinct(data.frame(x_dorsal_ipsi_t2$animal))
  l <- c(c1, c2)
  l <- unlist(l, use.names = FALSE)
  x_dorsal_ipsi <- rbind(x_dorsal_ipsi_t1, x_dorsal_ipsi_t2)
  x_dorsal_ipsi$animal <- factor(x_dorsal_ipsi$animal, levels = l, ordered = TRUE)
  x_dorsal_ipsi$muscle <- factor(x_dorsal_ipsi$muscle, levels = c(titer_used[1], titer_used[2]), ordered = TRUE)
  graph <- ggplot(data = x_dorsal_ipsi,aes(x = animal, y = x, colour = titer)) +
    geom_boxplot(show.legend = FALSE, width= 0.4, size = 1,  outlier.shape = 3) +
    scale_color_manual(values = c(as.character(color1), as.character(color2))) +
    scale_y_continuous(limits=c(ymin, ymax), breaks=seq(from = ymin, to = 850, by = 850), expand = c(0, 0)) +
    ylab("M-L (μm)") +  
    xlab("") +
    theme(axis.text=element_text(size=12, colour = "black")) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust=1)) +
    theme(axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"))
  
  font = list(family = "Arial" , size = 14, color = "white")
  label = list(bordercolor = "transparent", font = font)
  
  graph_interactive = ggplotly(graph, height = 450, width=800) %>%
    style(hoverlabel = label) %>%
    layout(font = font)
  
  graph_interactive$x$data <- lapply(graph_interactive$x$data, FUN = function(x){
    x$marker$outliercolor = x$line$color 
    x$marker$color = x$line$color 
    x$marker$line = x$line$color
    return(x)
  })
  
  graph_interactive
}


## -- function to collect density for fig 6 -- ##
density_kernel_xy_concatenate_fig6 <- function (spinal, data_all, m_nt1, m_nt2, color1, color2, bin, nt_or_muscle) {
  
  if (grepl("GlyT2", nt_or_muscle)) {
    M1 <-  data_all %>% filter(GlyT2 == nt_or_muscle & muscle == m_nt1) 
    M2 <-  data_all %>% filter(GlyT2 == nt_or_muscle & muscle == m_nt2)
  }
  if (grepl("GlyT2", m_nt1)) {
    M1 <-  data_all %>% filter(GlyT2 == m_nt1 & muscle == nt_or_muscle) 
    M2 <-  data_all %>% filter(GlyT2 == m_nt2 & muscle == nt_or_muscle)
  }
  
  
  p <- ggplot() +
    annotate("point", x = 0, y = 0, size = 4, colour = "black") +
    geom_path(data = spinal, aes(x = V1, y = V2), size = 1.5) + 
    geom_density_2d(data = M1, aes(x = x, y = y), bins = bin,  h=c(width.SJ(M1$x), width.SJ(M1$y)), colour = as.character(color1), show.legend = FALSE) +
    geom_density_2d(data = M2, aes(x = x, y = y), bins = bin,  h=c(width.SJ(M1$x), width.SJ(M1$y)), colour = as.character(color2), show.legend = FALSE) + 
    scale_y_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    ylab("") +  
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"))
  
  p
}
x_ventral_1d_density_concatenate_fig6 <- function(data_all, m_nt1, m_nt2, color1, color2, nt_or_muscle, kernel){
  
  if (grepl("GlyT2", nt_or_muscle)) {
    data <- data_all %>% filter(GlyT2 == nt_or_muscle & identity == "IN")
    x_ventral_M1 <- data %>% filter(y <= 0, muscle == m_nt1)
    x_ventral_M2 <- data %>% filter(y <= 0, muscle == m_nt2)
    l_M1 <-  data %>% filter(muscle == m_nt1) 
    l_M2 <-  data %>% filter(muscle == m_nt2)
  }
  if (grepl("GlyT2", m_nt1)) {
    data <- data_all %>% filter(muscle == nt_or_muscle & identity == "IN")
    x_ventral_M1 <- data %>% filter(y <= 0, GlyT2 == m_nt1)
    x_ventral_M2 <- data %>% filter(y <= 0, GlyT2 == m_nt2)
    l_M1 <-  data %>% filter(GlyT2 == m_nt1) 
    l_M2 <-  data %>% filter(GlyT2 == m_nt2)
  }
  
  scale_ventral_M2 <- nrow(x_ventral_M2)/nrow(l_M2)
  scale_ventral_M1 <- nrow(x_ventral_M1)/nrow(l_M1)
  bw_x_M1 <- sd(l_M1$x)/kernel
  bw_x_M2 <- sd(l_M2$x)/kernel
  
  x_ventral_1d_density <- ggplot() +
    stat_density(data = x_ventral_M1, aes(x = x, y=stat(density)*scale_ventral_M1), bw = bw_x_M1, kernel = "gaussian", geom = "line", colour = color1, position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = x_ventral_M2, aes(x = x, y=stat(density)*scale_ventral_M2), bw = bw_x_M2, kernel = "gaussian", geom = "line", colour = color2, position = "identity", size = 1, show.legend = FALSE) +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    scale_y_reverse(limits=c(0.0025, 0), expand = c(0, 0))+
    theme_classic() +
    ylab("") +
    xlab("M-L (μm)") +
    theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(), axis.title.x = element_text(vjust=13),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent"))
  if (grepl("on", nt_or_muscle)) {
    x_ventral_1d_density <- x_ventral_1d_density +
      annotate("text", x = -380, y = 0.0015, size = 5.5, colour = "black", label = expression("GlyT2"^"ON"))
  }
  if (grepl("off", nt_or_muscle)) {
    x_ventral_1d_density <- x_ventral_1d_density +
      annotate("text", x = -380, y = 0.0015, size = 5.5, colour = "black", label = expression("GlyT2"^"OFF"))
  }
  if (grepl("GlyT2", m_nt1)) {
    if (grepl("lg", nt_or_muscle)) {
      x_ventral_1d_density <- x_ventral_1d_density +
        annotate("text", x = -300, y = 0.0015, size = 5.5, colour = color2, label = expression("LG-GlyT2"^"OFF"))
    }
    if (grepl("ta", nt_or_muscle)) {
      x_ventral_1d_density <- x_ventral_1d_density +
        annotate("text", x = -300, y = 0.0015, size = 5.5, colour = color2, label = expression("TA-GlyT2"^"OFF"))
    }
  }
  x_ventral_1d_density
}
x_dorsal_1d_density_concatenate_fig6 <- function(data_all, m_nt1, m_nt2, color1, color2, nt_or_muscle, kernel){
  if (grepl("GlyT2", nt_or_muscle)) {
    data <- data_all %>% filter(GlyT2 == nt_or_muscle & identity == "IN")
    x_dorsal_M1 <- data %>% filter(y >= 0, muscle == m_nt1)
    x_dorsal_M2 <- data %>% filter(y >= 0, muscle == m_nt2)
    l_M1 <-  data %>% filter(muscle == m_nt1) 
    l_M2 <-  data %>% filter(muscle == m_nt2)
  }
  if (grepl("GlyT2", m_nt1)) {
    data <- data_all %>% filter(muscle == nt_or_muscle & identity == "IN")
    x_dorsal_M1 <- data %>% filter(y >= 0, GlyT2 == m_nt1)
    x_dorsal_M2 <- data %>% filter(y >= 0, GlyT2 == m_nt2)
    l_M1 <-  data %>% filter(GlyT2 == m_nt1) 
    l_M2 <-  data %>% filter(GlyT2 == m_nt2)
  }
  scale_dorsal_M2 <- nrow(x_dorsal_M2)/nrow(l_M2)
  scale_dorsal_M1 <- nrow(x_dorsal_M1)/nrow(l_M1)
  bw_x_M1 <- sd(l_M1$x)/kernel
  bw_x_M2 <- sd(l_M2$x)/kernel
  
  x_dorsal_1d_density <- ggplot() +
    stat_density(data = x_dorsal_M1, aes(x = x, y=stat(density)*scale_dorsal_M1), bw = bw_x_M1, kernel = "gaussian", geom = "line", colour = color1, position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = x_dorsal_M2, aes(x = x, y=stat(density)*scale_dorsal_M2), bw = bw_x_M2, kernel = "gaussian", geom = "line", colour = color2, position = "identity", size = 1, show.legend = FALSE) +
    scale_x_continuous(limits=c(-850, 850), breaks=seq(from = -850, to = 850, by = 850), expand = c(0, 0)) +
    scale_y_continuous(limits=c(0, 0.0025), expand = c(0, 0))+
    theme_classic() +
    ylab("") +  
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_), # necessary to avoid drawing plot outline
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) 
  if (grepl("GlyT2", m_nt1)) {
    if (grepl("lg", nt_or_muscle)) {
      x_dorsal_1d_density <- x_dorsal_1d_density +
        annotate("text", x = -300, y = 0.0015, size = 5.5, colour = color1, label = expression("LG-GlyT2"^"ON"))
      
    }
    if (grepl("ta", nt_or_muscle)) {
      x_dorsal_1d_density <- x_dorsal_1d_density +
        annotate("text", x = -300, y = 0.0015, size = 5.5, colour = color1, label = expression("TA-GlyT2"^"ON"))
    }
  }
  if (grepl("GlyT2", nt_or_muscle)) {
    x_dorsal_1d_density <- x_dorsal_1d_density +
      annotation_custom(textGrob("LG", gp = gpar(col = as.character(color1), fontface = 'bold', fontsize = 20)), 
                        xmin = -700, xmax = -600, ymin = 0.001, ymax = 0.002) +
      annotation_custom(textGrob("TA", gp = gpar(col = as.character(color2), fontface = 'bold', fontsize = 20)), 
                        xmin = -400, xmax = -300, ymin = 0.001, ymax = 0.002)
  }
  x_dorsal_1d_density
}
y_contra_1d_density_concatenate_fig6 <- function(data_all, m_nt1, m_nt2, color1, color2, nt_or_muscle, kernel){
  
  if (grepl("GlyT2", nt_or_muscle)) {
    data <- data_all %>% filter(GlyT2 == nt_or_muscle & identity == "IN")
    y_contra_M1 <- data %>% filter(x <= 0, muscle == m_nt1)
    y_contra_M2 <- data %>% filter(x <= 0, muscle == m_nt2)
    l_M1 <-  data %>% filter(muscle == m_nt1) 
    l_M2 <-  data %>% filter(muscle == m_nt2) 
  }
  if (grepl("GlyT2", m_nt1)) {
    data <- data_all %>% filter(muscle == nt_or_muscle & identity == "IN")
    y_contra_M1 <- data %>% filter(x <= 0, GlyT2 == m_nt1)
    y_contra_M2 <- data %>% filter(x <= 0, GlyT2 == m_nt2)
    l_M1 <-  data %>% filter(GlyT2 == m_nt1) 
    l_M2 <-  data %>% filter(GlyT2 == m_nt2)
  }
  
  scale_contra_M2 <- nrow(y_contra_M2)/nrow(l_M2)
  scale_contra_M1 <- nrow(y_contra_M1)/nrow(l_M1)
  bw_y_M1 <- sd(l_M1$y)/kernel
  bw_y_M2 <- sd(l_M2$y)/kernel
  
  y_contra_1d_density <- ggplot() +
    stat_density(data = y_contra_M1, aes(x = y, y=stat(density)*scale_contra_M1), bw = bw_y_M1, kernel = "gaussian", geom = "line", colour = color1, position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = y_contra_M2, aes(x = y, y=stat(density)*scale_contra_M2), bw = bw_y_M2, kernel = "gaussian", geom = "line", colour = color2, position = "identity", size = 1, show.legend = FALSE) +
    scale_x_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    scale_y_reverse(limits=c(0.0055, 0), expand = c(0, 0))+
    theme_classic() +
    ylab("") +  
    xlab("V-D (μm)") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(), axis.title.y = element_text(vjust=-16), panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate() 
  
  y_contra_1d_density
}
y_ipsi_1d_density_concatenate_fig6 <- function(data_all, m_nt1, m_nt2, color1, color2, nt_or_muscle, kernel){
  if (grepl("GlyT2", nt_or_muscle)) {
    data <- data_all %>% filter(GlyT2 == nt_or_muscle & identity == "IN")
    y_ipsi_M1 <- data %>% filter(x >= 0, muscle == m_nt1)
    y_ipsi_M2 <- data %>% filter(x >= 0, muscle == m_nt2)
    l_M1 <-  data %>% filter(muscle == m_nt1) 
    l_M2 <-  data %>% filter(muscle == m_nt2) 
  }
  if (grepl("GlyT2", m_nt1)) {
    data <- data_all %>% filter(muscle == nt_or_muscle & identity == "IN")
    y_ipsi_M1 <- data %>% filter(x >= 0, GlyT2 == m_nt1)
    y_ipsi_M2 <- data %>% filter(x >= 0, GlyT2 == m_nt2)
    l_M1 <-  data %>% filter(GlyT2 == m_nt1) 
    l_M2 <-  data %>% filter(GlyT2 == m_nt2)
  }
  
  scale_ipsi_M2 <- nrow(y_ipsi_M2)/nrow(l_M2)
  scale_ipsi_M1 <- nrow(y_ipsi_M1)/nrow(l_M1)
  bw_y_M1 <- sd(l_M1$y)/kernel
  bw_y_M2 <- sd(l_M2$y)/kernel
  
  y_ipsi_1d_density <- ggplot() +
    stat_density(data = y_ipsi_M1, aes(x = y, y=stat(density)*scale_ipsi_M1), bw = bw_y_M1, kernel = "gaussian", geom = "line", colour = color1, position = "identity", size = 1, show.legend = FALSE) +
    stat_density(data = y_ipsi_M2, aes(x = y, y=stat(density)*scale_ipsi_M2), bw = bw_y_M2, kernel = "gaussian", geom = "line", colour = color2, position = "identity", size = 1, show.legend = FALSE) +
    scale_x_continuous(limits=c(-450, 450), breaks=seq(from = -450, to = 450, by = 450), expand = c(0, 0)) +
    scale_y_continuous(limits=c(0, 0.0055), expand = c(0, 0)) +
    theme_classic() +
    ylab("") +  
    xlab("") +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA_character_),
          plot.background = element_rect(fill = "transparent", colour = NA_character_),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent"),
          panel.border = element_rect(color = "black", fill = "transparent")) +
    rotate()
  
  y_ipsi_1d_density
}
complete_graph_xy_density_fig6 <- function(spinal, data_all, m_nt1, m_nt2, color1, color2, kernel, bin, nt_or_muscle, panel){
  
  x_ventral <- x_ventral_1d_density_concatenate_fig6(data_all, m_nt1, m_nt2, color1, color2, nt_or_muscle, kernel)
  x_dorsal <- x_dorsal_1d_density_concatenate_fig6(data_all, m_nt1, m_nt2, color1, color2, nt_or_muscle, kernel)
  y_ipsi <- y_ipsi_1d_density_concatenate_fig6(data_all, m_nt1, m_nt2, color1, color2, nt_or_muscle, kernel)
  y_contra <- y_contra_1d_density_concatenate_fig6(data_all, m_nt1, m_nt2, color1, color2, nt_or_muscle, kernel)
  p <- density_kernel_xy_concatenate_fig6(spinal, data_all, m_nt1, m_nt2, color1, color2, bin, nt_or_muscle) 
  graph <- xy_density_arrange_1d(x_dorsal, y_contra, p, y_ipsi, x_ventral, panel)
  
  graph
}

## -- function to draw half violin plot for fig 6 -- ##
# From: https://gist.github.com/dgrtwo/eb7750e74997891d7c20
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}
geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}
GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom, setup_data = function(data, params) {
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)
    
    # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
    data %>%
      group_by(group) %>%
      mutate(ymin = min(y),
             ymax = max(y),
             xmin = x,
             xmax = x + width / 2)
    
  },
  draw_group = function(data, panel_scales, coord) {
    # Find the points for the line to go all the way around
    data <- transform(data, xminv = x,
                      xmaxv = x + violinwidth * (xmax - x))
    
    # Make sure it's sorted properly to draw the outline
    newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                     plyr::arrange(transform(data, x = xmaxv), -y))
    
    # Close the polygon: set first and last point the same
    # Needed for coord_polar and such
    newdata <- rbind(newdata, newdata[1,])
    
    ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
  },
  draw_key = draw_key_polygon,
  default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                    alpha = NA, linetype = "solid"), required_aes = c("x", "y"))

## -- functions to collect median for fig 6-- ##
df_individual_fig6 <- function(data, method_used, transmitter, hemicord, m1, m2, M1, M2){
  
  if (hemicord == "dorsal"){
    data_m1 <- data %>% filter(y >= 0, GlyT2 == transmitter, identity == "IN", muscle == m1)
    data_m2 <- data %>% filter(y >= 0, GlyT2 == transmitter, identity == "IN", muscle == m2)
  }
  if (hemicord == "ventral"){
    data_m1 <- data %>% filter(y <= 0, GlyT2 == transmitter, identity == "IN", muscle == m1)
    data_m2 <- data %>% filter(y <= 0, GlyT2 == transmitter, identity == "IN", muscle == m2)
  }
  medio_lateral <- c()
  sample <- c()
  l_m1 <- distinct(data.frame(data_m1$animal))
  l_m1 <- unlist(l_m1, use.names = FALSE)
  l_m2 <- distinct(data.frame(data_m2$animal))
  l_m2 <- unlist(l_m2, use.names = FALSE)
  group <- rep(c(as.character(M1),as.character(M2)),times=c(length(l_m1), length(l_m2)))
  
  for (i in 1:length(l_m1)){
    df <- data_m1 %>% filter(animal == as.character(l_m1[i]))
    med <- median(df$x)
    sample  <- c(sample, as.character(l_m1[i]))
    medio_lateral <- c(medio_lateral, med)
  }
  for (i in 1:length(l_m2)){
    df <- data_m2 %>% filter(animal == as.character(l_m2[i]))
    med <- median(df$x)
    sample  <- c(sample, as.character(l_m2[i]))
    medio_lateral <- c(medio_lateral, med)
  }
  t = (as.numeric(length(l_m1)) + as.numeric(length(l_m2)))
  method <- c(rep(as.character(method_used),times = as.numeric(t)))
  dat<- data.frame(group, medio_lateral, sample, method)
  
  dat
}
boxplot_per_sample_fig6 <- function(data, m1, m2, color1, color2, ymin, ymax, transmitter, hemicord){
  
  remove <- c("_n_djm","_n_rmb", "_n_nz", "_n_slp")
  data$animal <- str_remove_all(data$animal, paste(remove, collapse = "|"))
  
  if (hemicord == "dorsal"){
    x_m1 <- data %>% filter(y >= 0, GlyT2 == transmitter, identity == "IN", muscle == m1)
    x_m2 <- data %>% filter(y >= 0, GlyT2 == transmitter, identity == "IN", muscle == m2)
  }
  if (hemicord == "ventral"){
    x_m1 <- data %>% filter(y <= 0, GlyT2 == transmitter, identity == "IN", muscle == m1)
    x_m2 <- data %>% filter(y <= 0, GlyT2 == transmitter, identity == "IN", muscle == m2)
  }
  x_m1$animal <- paste(as.character(m1), "_",  x_m1$animal, sep="")
  x_m2$animal <- paste(as.character(m2), "_",  x_m2$animal, sep="")
  nb_sample_m1 <- nrow(distinct(data.frame(x_m1$animal)))
  nb_sample_m2 <- nrow(distinct(data.frame(x_m2$animal)))
  c1 <- distinct(data.frame(x_m1$animal))
  c2 <- distinct(data.frame(x_m2$animal))
  l <- c(c1, c2)
  l <- unlist(l, use.names = FALSE)
  x_distribution <- rbind(x_m1, x_m2)
  x_distribution$animal <- factor(x_distribution$animal, levels = l, ordered = TRUE)
  x_distribution$muscle <- factor(x_distribution$muscle, levels = c(m1, m2), ordered = TRUE)
  
  graph <- ggplot(data = x_distribution,aes(y = x, colour = muscle)) +
    stat_boxplot(aes(x = animal), geom = "errorbar", width = 0.2, show.legend = FALSE, size = 1, linetype = 5) +
    geom_boxplot(aes(x = animal), show.legend = FALSE, width= 0.4, size = 1,  outlier.shape = 3, outlier.stroke = 1.3, linetype = 0) +
    geom_boxplot(aes(x = animal), show.legend = FALSE, width= 0.4, size = 1, outlier.shape = NA, coef = 0) +
    geom_flat_violin(aes(fill = muscle, x = animal), kernel = "gaussian", bw = "ucv", adjust = 1, position = position_nudge(x = 0.3), show.legend = FALSE, trim=TRUE) +
    scale_color_manual(values = c(as.character(color1), as.character(color2))) +
    scale_fill_manual(values = c(as.character(color1), as.character(color2))) +
    scale_y_continuous(limits=c(ymin, ymax), breaks=seq(from = ymin, to = 850, by = 850), expand = c(0, 0)) +
    ylab("M-L (μm)") +  
    xlab("") +
    theme(axis.text=element_text(size=12, colour = "black")) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust=1, colour = c(rep(as.character(color1),nb_sample_m1),rep(as.character(color2), nb_sample_m2)))) +
    theme(axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"))
  
  graph
}

# -- functions to get data for Figure 6I and 6L --#
Selection_glyT2 <- function(dat_6C, dat_6F, M) {
  dat_OFF <- dat_6C %>% filter(group == as.character(M))
  group <- rep(as.character("GlyT2<sup>OFF</sup>"),times=nrow(dat_OFF))
  colnames(dat_OFF) <- c("muscle", "medio_lateral")
  dat_OFF <- cbind(dat_OFF, group)
  
  dat_ON <- dat_6F %>% filter(group == as.character(M))
  group <- rep(as.character("GlyT2<sup>ON</sup>"),times=nrow(dat_ON))
  colnames(dat_ON) <- c("muscle", "medio_lateral")
  dat_ON <- cbind(dat_ON, group)
  data_glyT2 <- rbind(dat_ON, dat_OFF)
  
  data_glyT2
}
make_labels <- function(labels) {
  result <- str_split(labels, "\\.")
  unlist(lapply(result, function(x) x[1]))
}
boxplot_per_sample_fig6_glyT2 <- function(data, m1, color1, color2, ymin, ymax, hemicord){
  
  remove <- c("_n_djm","_n_rmb", "_n_nz", "_n_slp")
  data$animal <- str_remove_all(data$animal, paste(remove, collapse = "|"))
  
  if (hemicord == "dorsal"){
    x_on <- data %>% filter(y >= 0, GlyT2 == "GlyT2-on", identity == "IN", muscle == m1)
    x_off <- data %>% filter(y >= 0, GlyT2 == "GlyT2-off", identity == "IN", muscle == m1)
  }
  if (hemicord == "ventral"){
    x_on <- data %>% filter(y <= 0, GlyT2 == "GlyT2-on", identity == "IN", muscle == m1)
    x_off <- data %>% filter(y <= 0, GlyT2 == "GlyT2-off", identity == "IN", muscle == m1)
  }
  x_on$animal <- paste(as.character(m1), "_",  x_on$animal, sep="")
  x_off$animal <- paste(as.character(m1), "_",  x_off$animal, sep="")
  nb_sample_on <- nrow(distinct(data.frame(x_on$animal)))
  nb_sample_off <- nrow(distinct(data.frame(x_off$animal)))
  x_distribution <- rbind(x_off, x_on)
  x_distribution <- x_distribution %>%
    arrange(GlyT2, animal) %>%
    mutate(
      category = interaction(animal, GlyT2),
      category = forcats::fct_inorder(category)) 
  l <- distinct(data.frame(x_distribution$category))
  l <- unlist(l, use.names = FALSE)
  l <- rev(l)
  x_distribution$category <- factor(x_distribution$category, levels = l, ordered = TRUE)
  x_distribution$GlyT2 <- factor(x_distribution$GlyT2, levels = c("GlyT2-on", "GlyT2-off"), ordered = TRUE)
  #x_distribution$category <- factor(x_distribution$category, levels = category, ordered = TRUE)
  
  
  
  graph <- ggplot(data = x_distribution, aes(y = x, colour = GlyT2)) +
    stat_boxplot(aes(x = category), geom = "errorbar", width = 0.2, show.legend = FALSE, size = 1, linetype = 5) +
    geom_boxplot(aes(x = category), show.legend = FALSE, width= 0.4, size = 1,  outlier.shape = 3, outlier.stroke = 1.3, linetype = 0) +
    geom_boxplot(aes(x = category), show.legend = FALSE, width= 0.4, size = 1, outlier.shape = NA, coef = 0) +
    geom_flat_violin(aes(fill = GlyT2, x = category), kernel = "triangular", bw = "ucv", adjust = 1, position = position_nudge(x = 0.3), show.legend = FALSE, trim=TRUE) +
    scale_color_manual(values = c(as.character(color1), as.character(color2))) +
    scale_fill_manual(values = c(as.character(color1), as.character(color2))) +
    scale_y_continuous(limits=c(ymin, ymax), breaks=seq(from = ymin, to = 850, by = 850), expand = c(0, 0)) +
    scale_x_discrete(labels = make_labels) +
    ylab("M-L (μm)") +  
    xlab("") +
    theme(axis.text=element_text(size=12, colour = "black")) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 10, angle = 45, hjust=1, colour = c(rep(as.character(color1),nb_sample_on),rep(as.character(color2), nb_sample_off)))) +
    theme(axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"))
  
  graph
}

