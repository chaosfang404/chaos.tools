wcpa_plot = function(
				.data,
				name,
				res = "2500000",
				norm = "KR",
				chr = c(1:22,"X","Y")
){
	wcpa_matrix(
				.data,
				name,
				res,
				norm,
				chr
			) %>%
		pheatmap( 
				cluster_rows = F, 
				cluster_cols = F,
				legend = T, 
				main = name,
				display_numbers = F, 
				fontsize = 6, 
				fontsize_row = 10, 
				fontsize_col = 10, 
				number_color = "black",
				number_format = "%.1f",
				angle_col ="0",
				color = colorRampPalette(colors = c("#3c5488","white","#e64b35"))(1000),
				scale = "none",
				border_color = "white",
				border = T
				#在指定了border_color之后，border=F会被忽略掉，当不需要border时，必须注释掉border_color
			)
}
