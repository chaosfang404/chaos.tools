juicer_tool <- function(
					cmd = "eigenvector",
					hic_file,
					chr,
					resolution = 1e6,
					norm = "KR",
					juicer_tool_path ="~/local/juicer/common/juicer_tools.jar",
					matrix_type = 1
){
	res <- format(resolution,scientific = F,trim = T)

	result <- paste(
					"java -jar -Xmx120g",juicer_tool_path, cmd, "-p",norm, hic_file, chr,"BP",res,sep = " "
				) %>%
				system(inter = T)

	if(cmd == "eigenvector")
	{
		as.data.table(
			result
		)[
			,.(eigen = result)
		][
			is.na(eigen),
			eigen := 0
		][]
	}else if(cmd == "pearsons")
	{
		matrix <- as.data.table(result[-1]) %>% 
					separate_col("V1",sep = " ") %>%
					setnames(paste0("bin_",1:ncol(.)))

		if(matrix_type == 1)
		{
			matrix
		}else if(matrix_type == 2)
		{
			matrix[
				,x := paste0("bin_",1:.N)
			] %>%
			melt("x", paste0("bin_",1:nrow(.)),variable.name = "y", value.name = "pearsons")
		}
	}
}
