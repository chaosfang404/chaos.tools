wcpa_diff = function(
				.data,
				name1,
				name2,
				res = "2.5Mb",
				norm = "KR",
				chr = c(1:22,"X","Y")
){
	min <- 0.8
	step <- 0.001
	mid <- 1
	# In this project, 1 means there is no difference between 2 samples.
	# since 1 is the special mark, we need to define the color of it specifically
	max <- 2*mid - min
	#此时，max - mid = mid - min，即两段长度相同
	#也可以指定max值，使得两段长度不同
    
    data_1 <- wcpa_matrix(.data,name1,res,norm,chr)
	data_2 <- wcpa_matrix(.data,name2,res,norm,chr)
	
	(data_1/data_2) %>%
			pheatmap( 
				cluster_rows = F, 
				cluster_cols = F,
				legend = T, 
				main = paste(name1,name2,sep = "/"),
				display_numbers = F, 
				fontsize = 6, 
				fontsize_row = 10, 
				fontsize_col = 10, 
				number_color = "black",
				number_format = "%.1f",
				angle_col ="0",
				color = c(
							colorRampPalette(colors = c("#3c5488","white"))(length(seq(min,mid,step))),
							colorRampPalette(colors = c("white","#e64b35"))(length(seq(mid,max,step)))
				),
				#如果两段颜色长度不同，须要分段设置颜色，长度相同时可以合并为一个，分开写更为通用
				legened_breaks = seq(min,max,(max-min)/4),
				#4 means 5 numbers from min to max
				breaks=seq(min,max,step),
				scale = "none",
				border_color = "white",
				border = T
				#在指定了border_color之后，border=F会被忽略掉，当不需要border时，必须注释掉border_color
			)
}
