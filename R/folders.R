scratch <- function()
{
	for (i in dir("/scratch"))
	{
		tmp_path <- file.path("/scratch",i,"chaos")
		if (dir.exists(tmp_path))
		{
			return(tmp_path)
		}
	}
}

desktop <- function()
{
	if(Sys.info()[1] == "Windwos")
	{
		return("c:/Users/Chao/Desktop")
	}else if(Sys.info()[1] == "Linux")
	{
		return("~/chao/Desktop")
	}
}