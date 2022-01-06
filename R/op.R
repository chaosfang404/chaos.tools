op <- function(
		file
){
	
	ext <- strsplit(file,"[.]") %>% 
			unlist() %>% 
			rev() %>% 
			head(1)

	if(ext == "pdf")
	{
		program <- "/mnt/c/Users/Chao/AppData/Local/SumatraPDF/SumatraPDF.exe"
	} else if (ext == "png" | ext == "jpeg" | ext == "bmp")
	{
		program <- "/mnt/c/Program Files/Honeyview/Honeyview.exe"
	} else
	{
		program <- "/mnt/c/Program Files/Sublime Text/subl.exe"
	}

	cmd <- paste(shQuote(program),file,sep = " ")
	system(cmd,inter = F)
}

##  else if (ext == "ppt" | ext == "pptx")
## {
## 	program <- "/mnt/c/Program\ Files/Microsoft\ Office/root/Office16/POWERPNT.EXE"
## } else if(ext == "xls" | ext == "xlsx" | ext == "csv")
## {
## 	program <- "/mnt/c/Program\ Files/Microsoft\ Office/root/Office16/EXCEL.EXE"
## }
