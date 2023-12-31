# moreFuns

jsave <- function(filename, base_folder, 
                  plot = last_plot(), 
                  square = T, 
                  square_size = 5000, 
                  scale = 1,
                  ratio = c(6,9),
                  dpi = 1000){
  if(square){
    ggsave(filename = filename,
           plot = plot,
           path = base_folder,
           dpi = dpi,
           width = square_size,
           height = square_size,
           scale = scale,
           units = "px")
  }else{
    total = square_size^2
    a <- sqrt((total*ratio[1])/ratio[2])
    b <- (ratio[2]*a)/ratio[1]
    ggsave(filename = filename,
           plot = plot, 
           path = base_folder,
           dpi = dpi,
           width = round(b),
           height = round(a),
           scale = scale,
           units = "px")
  }
}

make_numeric_decimal <- function(.data){
  df <- .data
  cols_to_format <- unlist(lapply(df, is.numeric))
  df[,cols_to_format] <- bind_cols(lapply(df[,cols_to_format], sprintf, fmt = '%#.2f'))
  return(df)
}

addBoxLabel <- function(i, color = "white", size = 0.5){
  list(
    annotate("rect", 
             xmin = lims$xmin[i], xmax = lims$xmax[i],
             ymin = lims$ymin[i], ymax = lims$ymax[i],
             color = color, fill = NA, size = size)
  )
}

#' @name createTimeMap
#' @param map_temp sf dataset
#' @param col_type character for the color scale used by viridis
#' @param cut_prob (defaults to 0.02) cuts the lower and upper
#' quantiles 
createTimeMap <- function(map_temp, col_type, cut_prob = 0.02){
  
  # range of posterior medians
  lower_cut_prob = cut_prob/2
  col_range <- unname(quantile(map_temp$point, probs = c(lower_cut_prob, 1-lower_cut_prob)))
  
	for(t in 1:length(seq_years)){
		  
		# base map - no legend
		base <- map_temp %>% 
		   filter(year == seq_years[t]) %>% 
		   ggplot(aes(fill = point))+
		   theme_void()+
		   geom_sf(col = "grey", size = 0.1)+
		   geom_sf(data = wa_border, aes(geometry = geometry), 
				   colour = "black", fill = NA, size = 0.2)+
		   scale_fill_viridis_c(begin = 0, end = 1, 
								direction = -1,
								option = col_type, limits = col_range,
								oob = squish)+
		   theme(legend.position = "none",
				text = element_text(size = 8),
				plot.title = element_text(margin = margin(0,0,2,0)),
				plot.margin = unit(c(1,1,1,1), "mm"))

		# base map_with legend
		temp <- base +
		  labs(fill = metric)+
		  guides(fill = guide_colourbar(barwidth = 15, 
										title.position = "top",
										title.hjust = 0.5))+
		  theme(legend.position = "bottom")
		base_legend <- get_legend(temp)

		# Base map with boxes
		#base_boxes <- base_legend + addBoxLabel(1, color = "green", size = 0.2)
		base_boxes <- base + addBoxLabel(1, color = "black", size = 0.2)

		# Create list of insets
		perth_inset <- base +
		  xlim(lims$xmin[1], lims$xmax[1]) +
		  ylim(lims$ymin[1], lims$ymax[1]) +
		  theme(panel.border = element_rect(colour = "black", size=0.2, fill=NA),
				plot.title = element_text(margin = margin(0,0,2,0)),
				plot.margin = unit(c(1,1,1,1), "mm"))

		# create full map
		lay <- rbind(c(2,1),
					 c(1,1))
		year_plt_list[[t]] = arrangeGrob(grobs = list(base_boxes, perth_inset), 
										 layout_matrix  = lay,
										 top = textGrob(seq_years[t],gp=gpar(fontsize=8)))

		# Progress 
		message("---- Year: ", seq_years[t])

	}

# Final plot
lay <- rbind(c(1,2,3),
             c(4,5,6),
             c(7,7,7))
return(arrangeGrob(grobs = c(year_plt_list, list(base_legend)), layout_matrix  = lay))
}

## -----------------------------------------------------------------------------
#' @name createCaterpillarPlot
#' @param map_temp sf dataset
#' @param col_type character for the color scale used by viridis
#' @param cut_prob (defaults to 0.02) cuts the lower and upper
#' quantiles 
createCaterpillarPlot <- function(map_temp, 
                           col_type, 
                           cut_prob = 0.02){
  
  # range of posterior medians
  lower_cut_prob = cut_prob/2
  col_range <- unname(quantile(map_temp$point, 
                               probs = c(lower_cut_prob, 1-lower_cut_prob)))
  
  # create plot
  map_temp %>% 
    mutate(year = as.character(year)) %>% 
    group_by(year) %>% 
    mutate(ord_id = order(order(point))) %>% 
    ungroup() %>% 
    ggplot(aes(y = point, ymin = lower, ymax = upper,
               x = ord_id,
               col = point))+
    theme_bw()+
    geom_errorbar()+
    geom_point(col = "black", size = 0.3)+
    facet_wrap(.~year) +
    scale_color_viridis_c(begin = 0, end = 1, 
                          direction = -1,
                          option = col_type, 
                          limits = col_range,
                          oob = squish)+
    labs(y = metric,
         x = "Ranked LGAs",
         color = "")+
    guides(fill = guide_colourbar(barwidth = 15, 
                                  title.position = "top",
                                  title.hjust = 0.5))+
    theme(legend.position = "none",
          text = element_text(size = 8))
  
}

## -----------------------------------------------------------------------------
#' @param draws iterations by observations
#' @param model model column content (as single character)
#' @param metric metric column content (as single character)
#' @param conf_level numeric for the confidence size of the interval (defaults to 0.95)
#' @param other_data data.frame with other data to be column binded to result 
# (NOTE: must be same dimensions and same order!)
#' @param prefix character vector added to point, upper, lower, se and RSE columns (defaults to "")
#' @param addDPP logical (defaults to false)
#' @returns Dataset with posterior summaries including: 
# median point estimates, credible intervals (HDI)
# standard deviations and RSE
# More arguments can be passed to the getDPP() function using `...` (e.g. null_value)
getResultsData <- function(draws, 
                           model = NULL, metric = NULL,
                           prefix = "",
                           conf_level = 0.95,
                           other_data = NULL, addDPP = FALSE, ...){
  
  # sum_func <- function(x){
  #   c(point = median(x, na.rm = T),
  #     lower = unname(HDInterval::hdi(x, credMass = conf_level)[1]),
  #     upper = unname(HDInterval::hdi(x, credMass = conf_level)[2]),
  #     se = sd(x, na.rm = T))
  # }
  # bind_rows(pblapply(asplit(draws, 2), sum_func))
  
  if(!is.null(other_data)){
    message(paste0("NOTE (not an error): Please check that the row order of ", deparse(substitute(other_data)), " matches that of the column order of ", deparse(substitute(draws))))
    if(nrow(other_data) != ncol(draws))stop(paste0("The number of columns of ", deparse(substitute(draws)), " does NOT match the number of rows of ", deparse(substitute(other_data)), ". They must match!"))
  }
  
  if(is.null(dim(draws))){
    r <- data.frame(point = draws,
                    lower = NA,
                    upper = NA,
                    se = NA) %>%
      mutate(RSE = NA) %>%
      setNames(paste0(prefix, names(.)))
    r <- bind_cols(r, other_data)
  }else{
    # Get objects
    message("Progress ... -> Point estimates...")
    point_in = pbapply::pbapply(draws, 2, median, na.rm = T)
    message("Progress ... -> Standard errors...")
    se_in = pbapply::pbapply(draws, 2, sd, na.rm = T)
    message("Progress ... -> Highest density intervals...")
    hd_ints = pbapply::pbapply(draws, 2, HDInterval::hdi, credMass = conf_level)
    if(addDPP){
      DPP <- jf$getDPP(draws, ...)
      r <- data.frame(point = point_in,
                      #lower = apply(draws, 2, quantile, prob = 0.025, na.rm = T),
                      lower = hd_ints[1,],
                      #upper = apply(draws, 2, quantile, prob = 0.975, na.rm = T),
                      upper = hd_ints[2,],
                      se = se_in,
                      EP = DPP$EP,
                      compared_to_null = DPP$compared_to_null,
                      DPP = DPP$DPP,
                      DPPsig = DPP$DPP_sig) %>%
        mutate(RSE = 100 * (se/point)) %>%
        setNames(paste0(prefix, names(.)))
      r <- bind_cols(r, other_data)
    }else{
      r <- data.frame(point = point_in,
                      #lower = apply(draws, 2, quantile, prob = 0.025, na.rm = T),
                      lower = hd_ints[1,],
                      #upper = apply(draws, 2, quantile, prob = 0.975, na.rm = T),
                      upper = hd_ints[2,],
                      se = se_in) %>%
        mutate(RSE = 100 * (se/point)) %>%
        setNames(paste0(prefix, names(.)))
      r <- bind_cols(r, other_data)
    }
  }
  
  # Add columns if given
  if(!is.null(model) & !is.null(metric)){
    r <- r %>% 
      mutate(model = model,
             metric = metric) %>% 
      relocate(model, metric)
  }
  if(!is.null(model) & is.null(metric)){
    r <- r %>% 
      mutate(model = model) %>% 
      relocate(model)
  }
  if(is.null(model) & !is.null(metric)){
    r <- r %>% 
      mutate(metric = metric) %>% 
      relocate(metric)
  }
  
  # return objects
  rownames(r) <- NULL
  return(r)
}
