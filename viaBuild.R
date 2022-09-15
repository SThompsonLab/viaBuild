library(tidyverse)
library(openxlsx)
library(ggpubr)

viaBuild <- function(wd = "./",
                         # Add a time list in case things go wrong
                         time_list = F,
                         # Make a wide or long dataset, with wide being more user friendly for excel *eyeroll*
                         long_or_wide = "long",
                         #Extinction coefficient for 600
                         nm_600_E = 117216,
                         #Extinction coefficient for 570
                         nm_570_E = 80586,
                         # Which concentration to normalize by
                         norm_conc = "DMSO",
                         #Which concentration has no cells in it
                         no_cell = "No_cells",
                         # How much time add to the zero mark
                         add_time = 0,
                         # Graph the data or not
                         graph_it = T,
                         #Which variable to be on the X axis ("Time" or "Read_number")
                         graphed_time = "Time",
                         # Graph by "relative" or "absolute" reduction
                         graph_type = "relative", 
                         #Graph the individual plates
                         plate_graph = F,
                         # Correct for edge effects
                         edge_effect = T){
  setwd(wd)
  plate_list <- list.files(pattern = "xlsx")
  if (is.double(time_list)){
    monkey = 1
    read_id <- time_list[monkey]
  } else {
    read_id <- 1
  }
  
  temPlate <- read_csv("plate.csv")

  
  for (plate_read in plate_list){
    interim <- read.xlsx(plate_read)
    interim <- as_tibble(interim)
    read_date <- strsplit(plate_read, " ")[[1]][5]
    read_time <- strsplit(strsplit(plate_read, " ")[[1]][6], ".xlsx")[[1]][1]
    if(!exists("starting_point")){
      starting_point <- read_time
      starting_time <- 0+add_time
    }
    
    interim <- interim[5:22, 1:13]
    names(interim) <- c("ROW", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
    if(grepl("570 nm", interim[9,1])){
      df_600 <- interim[1:8,]
      df_570 <- interim[11:18,]
    } else {
      df_570 <- interim[1:8,]
      df_600 <- interim[11:18,]
    }
    if (!exists("all_data")){
      all_data <- data.frame("Well_number" = c(1:96),
                             "Read_time" = paste0("Day: ", read_date, ", Time: ", read_time),
                             "Read_number" = read_id,
                             "Time" = starting_time,
                             "Position" = "UNK",
                             "Concentration" = "UNK",
                             "nm_570" = 0,
                             "nm_600" = 0)
      starting_horse <- 1
      for (i in c(1:8)){
        for (j in c(2:13)){
          all_data$Position[starting_horse] <- paste0(df_570$ROW[i], as.character(j-1))
          all_data$nm_570[starting_horse] <- unlist(df_570[i,j])
          all_data$nm_600[starting_horse] <- unlist(df_600[i,j])
          all_data$Concentration[starting_horse] <- unlist(temPlate[i,j])
          
          starting_horse <- starting_horse+1
        }
      }
      all_data$Reduction <- (nm_600_E*all_data$nm_570)-(nm_570_E*all_data$nm_600)
      if (edge_effect){
        all_data$row <- substr(all_data$Position, 1, 1)
        all_data$column_num <- substr(all_data$Position, 2, nchar(all_data$Position))
        all_data$column_num <- as.numeric(all_data$column_num)
        
        all_data[all_data$row == "H",]$row <- "1"
        all_data[all_data$row == "G",]$row <- "2"
        all_data[all_data$row == "F",]$row <- "3"
        all_data[all_data$row == "E",]$row <- "4"
        all_data[all_data$row == "D",]$row <- "5"
        all_data[all_data$row == "C",]$row <- "6"
        all_data[all_data$row == "B",]$row <- "7"
        all_data[all_data$row == "A",]$row <- "8"
        all_data$row <- as.numeric(all_data$row)
        all_data$distance_to_edge <- 0
        for (well in 1:nrow(all_data)){
          x_dist_1 <- all_data[well,]$column_num-min(all_data$column_num)
          x_dist_2 <- max(all_data$column_num)-all_data[well,]$column_num
          y_dist_1 <- all_data[well,]$row-min(all_data$row)
          y_dist_2 <- max(all_data$row)-all_data[well,]$row
          all_data[well,]$distance_to_edge <- sqrt(min(x_dist_1, x_dist_2)+min(y_dist_1, y_dist_2))
        }
        
        draft <- ggbarplot(data = all_data, x = "column_num", y = "Reduction", position = position_dodge(), add="mean_sd")
        draft
        draft <- ggbarplot(data = all_data, x = "row", y = "Reduction", position = position_dodge(), add="mean_sd")
        draft
        
        linEQ <- lm(Reduction~distance_to_edge, data = all_data[all_data$Concentration != no_cell,])
        edge_intercept <- as.numeric(linEQ$coefficients[1])
        edge_slope <- as.numeric(linEQ$coefficients[2])
        edge_r <- as.numeric(summary(linEQ)$r.squared)
        
        draft <- ggplot(data = all_data[all_data$Concentration != no_cell,], aes(x = distance_to_edge, y = Reduction))+
          geom_point()+
          theme_classic2()+
          geom_smooth(method = "lm")+
          geom_point(aes(color = Concentration))+
          geom_text(x=median(all_data$distance_to_edge), y=max(all_data$Reduction),label = paste0("Slope: ", round(edge_slope, 2), 
                                                                                                    "\nY-intercept: ", round(edge_intercept, 0),
                                                                                                    "\nR-squared: ", round(edge_r, 3)))
        
        ggsave(paste0("Distance_effect_", unique(all_data$Read_number),".png"), dpi = 300)
        all_data$Old_Reduction <- all_data$Reduction
        all_data[all_data$Concentration != no_cell,]$Reduction <- all_data[all_data$Concentration != no_cell,]$Old_Reduction-(all_data[all_data$Concentration!= no_cell,]$distance_to_edge*edge_slope)
      }
      all_data$Relative_reduction <- round(all_data$Reduction/mean(all_data[all_data$Concentration == norm_conc,]$Reduction)*100, 2)
      
      
    }else {
      if(as.numeric(strsplit(starting_point, split = "-")[[1]][1]) > as.numeric(strsplit(read_time, split = "-")[[1]][1])){
        time_since <- abs((24-as.numeric(strsplit(starting_point, split = "-")[[1]][1]))-as.numeric(strsplit(read_time, split = "-")[[1]][1]))
      }else {
        time_since <- abs(as.numeric(strsplit(starting_point, split = "-")[[1]][1])-as.numeric(strsplit(read_time, split = "-")[[1]][1]))
      }
      starting_point <- read_time
      starting_time <- starting_time+time_since
      
      some_data <- data.frame("Well_number" = c(1:96),
                              "Read_time" = paste0("Day: ", read_date, ", Time: ", read_time),
                              "Read_number" = read_id,
                              "Time" = starting_time,
                              "Position" = "UNK",
                              "Concentration" = "UNK",
                              "nm_570" = 0,
                              "nm_600" = 0)
      starting_horse <- 1
      for (i in c(1:8)){
        for (j in c(2:13)){
          some_data$Position[starting_horse] <- paste0(df_570$ROW[i], as.character(j-1))
          some_data$nm_570[starting_horse] <- unlist(df_570[i,j])
          some_data$nm_600[starting_horse] <- unlist(df_600[i,j])
          some_data$Concentration[starting_horse] <- unlist(temPlate[i,j])
          
          starting_horse <- starting_horse+1
        }
      }
      
      some_data$Reduction <- (nm_600_E*some_data$nm_570)-(nm_570_E*some_data$nm_600)
      if (edge_effect){
        some_data$row <- substr(some_data$Position, 1, 1)
        some_data$column_num <- substr(some_data$Position, 2, nchar(some_data$Position))
        some_data$column_num <- as.numeric(some_data$column_num)
        
        some_data[some_data$row == "H",]$row <- "1"
        some_data[some_data$row == "G",]$row <- "2"
        some_data[some_data$row == "F",]$row <- "3"
        some_data[some_data$row == "E",]$row <- "4"
        some_data[some_data$row == "D",]$row <- "5"
        some_data[some_data$row == "C",]$row <- "6"
        some_data[some_data$row == "B",]$row <- "7"
        some_data[some_data$row == "A",]$row <- "8"
        some_data$row <- as.numeric(some_data$row)
        some_data$distance_to_edge <- 0
        for (well in 1:nrow(some_data)){
          x_dist_1 <- some_data[well,]$column_num-min(some_data$column_num)
          x_dist_2 <- max(some_data$column_num)-some_data[well,]$column_num
          y_dist_1 <- some_data[well,]$row-min(some_data$row)
          y_dist_2 <- max(some_data$row)-some_data[well,]$row
          some_data[well,]$distance_to_edge <- sqrt(min(x_dist_1, x_dist_2)+min(y_dist_1, y_dist_2))
        }
        
        draft <- ggbarplot(data = some_data, x = "column_num", y = "Reduction", position = position_dodge(), add="mean_sd")
        draft
        draft <- ggbarplot(data = some_data, x = "row", y = "Reduction", position = position_dodge(), add="mean_sd")
        draft
        
        linEQ <- lm(Reduction~distance_to_edge, data = some_data[some_data$Concentration != no_cell,])
        edge_intercept <- as.numeric(linEQ$coefficients[1])
        edge_slope <- as.numeric(linEQ$coefficients[2])
        edge_r <- as.numeric(summary(linEQ)$r.squared)
        
        draft <- ggplot(data = some_data[some_data$Concentration != no_cell,], aes(x = distance_to_edge, y = Reduction))+
          geom_point()+
          theme_classic2()+
          geom_smooth(method = "lm")+
          geom_point(aes(color = Concentration))+
          geom_text(x=median(some_data$distance_to_edge), y=max(some_data$Reduction),label = paste0("Slope: ", round(edge_slope, 2), 
                                                                                                    "\nY-intercept: ", round(edge_intercept, 0),
                                                                                                    "\nR-squared: ", round(edge_r, 3)))
        
        ggsave(paste0("Distance_effect_", unique(some_data$Read_number),".png"), dpi = 300)
        some_data$Old_Reduction <- some_data$Reduction
        some_data[some_data$Concentration != no_cell,]$Reduction <- some_data[some_data$Concentration != no_cell,]$Old_Reduction-(some_data[some_data$Concentration != no_cell,]$distance_to_edge*edge_slope)
      }
      some_data$Relative_reduction <- round(some_data$Reduction/mean(some_data[some_data$Concentration == norm_conc,]$Reduction)*100, 2)
      
      if (long_or_wide == "wide"){
        names(some_data) <- paste0("Read ", read_id, " - ", names(some_data))
        all_data <-cbind(all_data, some_data[,c(2,3,5,6)])
      } else {
        all_data <- rbind(all_data, some_data)
      }
    }
    if (is.double(time_list)){
      monkey <- monkey+1
      read_id <- time_list[monkey]
    } else {
      read_id <- read_id+1
    }
  }
  
  write.csv(all_data, "all_datapoints.csv", row.names = F)
  if(graph_it == T){
    if (graph_type == "relative"){
      draft <- ggplot(data = all_data[!grepl(pattern = "_BLANK", all_data$Concentration),], 
                      aes(x=unlist(all_data[!grepl(pattern = "_BLANK", all_data$Concentration),][graphed_time]), 
                          y=Relative_reduction, color = Concentration))+
        geom_point()+
        geom_smooth()+
        ylab(paste0("Reduction (Relative to ", norm_conc, ")"))+
        xlab("Read time (hours)")+
        theme_classic2()
      print(draft)
      ggsave("all_data_line.svg")
      
      all_data$Time <- as.factor(all_data$Time)
      all_data$Read_number <- as.factor(all_data$Read_number)
      draft <- ggbarplot(data = all_data[!grepl(pattern = "_BLANK", all_data$Concentration),],
                         x="Concentration", 
                         y = "Relative_reduction", 
                         fill = graphed_time, 
                         add = "mean_sd", 
                         position = position_dodge(0.8))+
        ylab(paste0("Reduction (Relative to ", norm_conc, ")"))+
        xlab("Read time (hours)")+
        geom_hline(yintercept = 100, linetype = 2, size = 2, color = "red")+
        theme_classic2()
    } else {
      draft <- ggplot(data = all_data[!grepl(pattern = "_BLANK", all_data$Concentration),], 
                      aes(x=unlist(all_data[!grepl(pattern = "_BLANK", all_data$Concentration),][graphed_time]), 
                          y=Reduction, color = Concentration))+
        geom_point()+
        geom_smooth()+
        ylab(paste0("Absolute reduction"))+
        xlab("Read time (hours)")+
        theme_classic2()
      print(draft)
      ggsave("all_data_line.svg")
      
      all_data$Time <- as.factor(all_data$Time)
      all_data$Read_number <- as.factor(all_data$Read_number)
      draft <- ggbarplot(data = all_data[!grepl(pattern = "_BLANK", all_data$Concentration),],
                         x="Concentration", 
                         y = "Reduction", 
                         fill = graphed_time, 
                         add = "mean_sd", 
                         position = position_dodge(0.8))+
        ylab(paste0("Absolute reduction"))+
        xlab("Read time (hours)")+
        theme_classic2()
    }
    
    print(draft)
    ggsave("all_data_bar.svg")
  }
  if (plate_graph==T){
    all_data <- read_csv("all_datapoints.csv")
    for (timer in unique(all_data$Read_number)){
      interim <- all_data[all_data$Read_number == timer,]
      if (!exists("startingValue")){
        startingValue <- median(interim$Reduction)
      }
      interim$row <- substr(interim$Position, 1, 1)
      interim$column <- substr(interim$Position, 2, nchar(interim$Position))
      interim$column <- ordered(interim$column, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
      #interim$row <- ordered(interim$row, levels = c("H", "G", "F", "E", "D", "C", "B", "A"))
      draft <- ggplot(data = interim,
                      aes(y = rev(row),
                          x=column,
                          fill=Reduction))+
        geom_point(size = 20, shape = 21)+
        scale_fill_gradient2(low = "blue", high = "pink", mid = "#8203fdff", midpoint = startingValue)+
        theme_classic2()+
        xlab("")+
        ylab("")+
        theme(legend.position = "none")+
        scale_y_discrete(labels=c("H", "G", "F", "E", "D", "C", "B", "A"))
      print(draft)
      ggsave(paste0(unique(interim$Time), "_plate.png"), dpi=300)
    }
  }
}
