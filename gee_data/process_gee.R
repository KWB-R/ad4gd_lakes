library(dplyr)

files <- dir("D:/06_git/ad4gd_lakes/gee_data")
files <- grep(pattern = ".csv$", x = files, value = TRUE)
lakes <- 
  unique(sapply(
    strsplit(x = files, split = "_"), 
    function(x){
      l <- length(x)
      x <- x[-c(l-1, l)]
      paste(x, collapse = "_")
    })
  )


if(FALSE){
  for(lake in lakes){
    print(lake)
    df <- load_lake_point(
      path = "D:/06_git/ad4gd_lakes/gee_data", 
      lake = lake)
    df <- remove_cloudy_pixels(df_in = df)
    df <- add_normed_bands(df_in = df, bands = 1:6)
    df <- add_weighted_wavelength(df_in = df, bands = 1:6)
    
    
    df$months <- format(df$dateTime, "%m")
    df$year <- format(df$dateTime, "%Y")
    df_plot <- df %>% group_by(year, months, MGRS_TILE, lake) %>% 
      summarise("mean_wwl" = mean(wwl),
                "min_wwl" = min(wwl),
                "max_wwl" = max(wwl),
                "n" = n())
    
    # only march to november (sampling period for deriving the tropic index)
    df_plot <- df_plot[as.numeric(df_plot$months) %in% 3:11,]
    
    write.table(
      x = df_plot, 
      file = file.path("D:/06_git/ad4gd_lakes/gee_data/output" , 
                       paste0(lake,".csv")), 
      sep = ";", dec = ";", row.names = FALSE
    )
    
    tiles <- names(summary(as.factor(df_plot$MGRS_TILE)))
    
    for(tile in tiles){
      png(filename = file.path("D:/06_git/ad4gd_lakes/gee_data/output" , 
                               paste0(lake, "_", tile, ".png")), 
          width = 8, height = 7, units = "in", res = 300)
      plot_average_wavelength(df_plot = df_plot, tile_name = tile)
      dev.off()
    }
    
    {
      png(filename = file.path("D:/06_git/ad4gd_lakes/gee_data/output" , 
                               paste0(lake, "_moving_average", ".png")), 
          width = 8, height = 7, units = "in", res = 300)
      plot_moving_wavelength(df_in = df, tile = tiles[1], n_surrounding = 4)
      dev.off()
    }
    
  }
}


load_lake_point <- function(path, lake){
  all_files <- dir(path)
  selected_files <- all_files[grep(pattern = lake, x = all_files)]
  band_files <- selected_files[grep(pattern = "_bands_", x = selected_files)]
  meta_files <- selected_files[grep(pattern = "_meta_", x = selected_files)]
  
  file_list <- lapply(band_files, function(file){
    read.table(file = file.path(path, file), header = TRUE, sep = ",")
  })
  df_band <- inner_list_rbind(x = file_list, required = paste0("B", 1:6))
  
  file_list <- lapply(meta_files, function(file){
    read.table(file = file.path(path, file), header = TRUE, sep = ",")
  })
  df_meta <- inner_list_rbind(x = file_list)
  
  merge(x = df_band, y = df_meta, by.x = "id", by.y = "system.index")
}

inner_list_rbind <- function(x, required = NULL){ # list of data frames
  cols_available <- summary(as.factor(unlist(lapply(x, colnames))))
  select_columns <- unique(c(
    names(cols_available)[cols_available == length(x)],
    required))
  x <- lapply(x, function(df){
    if(all(select_columns %in% colnames(df))){
      df[,select_columns]
    } else {
      warning("Not all required columns available in all years")
      NULL
    }
   
  })
  do.call(rbind, x)
}

remove_cloudy_pixels <- function(df_in){
  # Q60 -> indicates cloud mask. More information here:
  # https://sentinel.esa.int/web/sentinel/technical-guides/sentinel-2-msi/level-1c/cloud-masks
  # Cloud free pixel is Q60 = 0
  df <- df[df$QA60 == 0,]
  df$dateTime <- as.POSIXct(df$system.time_start/1000, origin = "1970-01-01")
  df  
}


add_normed_bands <- function(
  df_in, 
  bands = 1:6
){
  df_pro <- df_in[,paste0("B", bands)]
  ranges <- apply(X = df_pro, MARGIN = 1, FUN = range)
  
  df_out <- do.call(rbind, lapply(1:nrow(df_pro), function(i){
    (df_pro[i,] - ranges[1,i]) / diff(ranges)[i]
  }))
  
  colnames(df_out) <- paste0(colnames(df_out), "_normed")
  cbind(df_in, df_out)
}


add_weighted_wavelength <- function(
  df_in, 
  bands = 1:6, 
  band_wl = c(443, 490, 560, 665, 705, 740)
){
  df_pro <- df_in[,paste0("B", bands, "_normed")]
  r_sums <- rowSums(df_pro)
  keep_data <- !is.na(r_sums)
  df_in <- df_in[keep_data,]
  wwl <- as.matrix(df_pro[keep_data,]) %*% matrix(band_wl, ncol = 1) / 
    r_sums[keep_data]
  df_in$wwl <- wwl
  df_in
}

plot_average_wavelength <- function(df_plot, tile_name = NULL, years = NULL){
  tiles <- summary(as.factor(df_plot$MGRS_TILE))
  tile_list <- lapply(seq_along(tiles), function(i){
    df_plot[df_plot$MGRS_TILE == names(tiles)[i],]
  })
  names(tile_list) <- names(tiles)
  if(is.null(tile_name)){
    tile_name <- names(tiles)[1]
  }
  years_to_plot <- if(is.null(years)){
    levels(as.factor(df_plot$year))
  } else {
    years
  }
  
  df_plot <- tile_list[[which(names(tile_list) == tile_name)]]
  colors <- c(paste0("deepskyblue", 1:4), paste0("deeppink", 1:4),
              paste0("orange", 1:4), paste0("gray", c(70,50,30,0)))
  
  plot(x = 0, y = 0, type = "n", ylim = c(443, 770), xlim = c(1,12), 
       xaxs = "i", yaxs = "i", 
       ylab = "Mean reflected wavelength", xlab = "Month", 
       main = paste("Lake:", df_plot$lake[1], " - Tile:", tile_name))
  for(i in seq_along(years_to_plot)){
    df_year <- df_plot[df_plot$year == years_to_plot[i],]
    line_color <- colors[i]
    
    lines(x = as.numeric(df_year$months), y = df_year$mean_wwl, 
          col = line_color, lwd = 2)
    
  }
  df_summary <- df_plot %>% 
    group_by(year) %>% 
    summarise("yearly_mean" = round(mean(mean_wwl),0)) 
  legend(
    "topright", 
    legend = c(paste0(df_summary$year, ": ", df_summary$yearly_mean, "nm")), 
    cex = 0.7, title = "Yearly Average", 
    lwd = 2, col = colors[seq_along(years_to_plot)])
  
}

moving_average <- function(v, n_surrounding = 2){
  i <- (1 + n_surrounding):(length(v) - n_surrounding)
  
  c(rep(NA, n_surrounding), 
    sapply(i, function(j){
      mean(v[(j - n_surrounding):(j + n_surrounding)])
    }),
    rep(NA, n_surrounding))
}

plot_moving_wavelength <- function(df_in, tile, n_surrounding = 4){
  
  df_plot <- df_in[df_in$MGRS_TILE == tile,]
  
  split_end <- c(
    which(diff(as.numeric(df_plot$dateTime)) / 60 / 60 / 24 > 30), 
    nrow(df_plot))
  split_start <- c(
    1, 
    split_end[-length(split_end)] + 1)
  
  plot_list <- lapply(seq_along(split_start), function(i){
    df_plot[split_start[i]:split_end[i],]
  })
  
  plot_list <- lapply(plot_list, function(x){
    if(nrow(x) >= (1 + 2 * n_surrounding)){
      x$ma <- moving_average(v = c(x$wwl), n_surrounding = n_surrounding)
      x
    } else {
      NULL
    }
  })
 
  plot(x = df_plot$dateTime, y = df_plot$wwl, 
       type = "n", lwd = 2, col = "deepskyblue3", 
       xlab = "Years", ylab = "Mean reflected Wavelength", main = df_in$lake[1])
  for(x_part in plot_list){
    lines(x = x_part$dateTime, y = x_part$ma, 
          lwd = 2, col = "deepskyblue3")
  }
  
}
# 
# 
# z <- as.matrix(df[,-1])
# 
# # Die Farbe kann in abh?ngigkeit der z-Werts ebenfalls als Matrix angegeben werden
# col_mat <- matrix(
#   data = cut(z, breaks = c(-0.1, 0.2, 0.4, 0.6, 0.8, 1.1), 
#              labels = c("forestgreen", "green", "yellow", "orange", "firebrick")), 
#   nrow = nrow(z), ncol = ncol(z))
# 
# a <- persp(x = df$date_time, 
#            z = z, y = c(443, 490, 560, 665, 705, 740),
#            theta = 40, # im Kreis drehen
#            phi = 10, # nach oben schwenken
#            r = sqrt(3),
#            d = 1,
#            col = col_mat, 
#            expand = 0.5, # Stauchung (L?nge der Z-Achse) 
#            border = NULL,
#            axes = T, 
#            box = TRUE, 
#            nticks = 3, 
#            ticktype = "detailed", xlab = "Time", ylab = "Wavelength in nm", 
#            zlab = "Normalised Reflection")
# 
# z[87,]
