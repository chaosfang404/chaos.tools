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

desktop <- function(
			username = "chao"
){
	if(is.na(username))
	{
		username <- Sys.info()[7]
	}

	platform <- Sys.info()[1]

	if(platform == "Windows")
	{
		file.path("C:/Users",username,"Desktop")
	}else if(platform == "Linux")
	{
		if(!grepl(Sys.info()[2],pattern = "WSL"))
		{
			file.path("~",username, "Desktop")
		}else
		{
			file.path("/mnt/c/Users",username,"Desktop")
		}
	}
}

data_dir <- function(
){
	platform <- Sys.info()[1]

	if(platform == "Linux")
	{
		if(grepl(Sys.info()[2],pattern = "WSL"))
		{
			file.path("/mnt/d/work")
		}else
		{
			scratch()
		}
	}else if(platform == "Windows")
	{
		file.path("D:/work")
	}
}

integration_dir <- function(
						integration_name,
						setwd = FALSE
){
	tmp <- paste0(data_dir(), "/integration/",integration_name)
	if(dir.exists(tmp))
	{
		if(isTRUE(setwd)){setwd(tmp)}
		tmp
	}else
	{
		print("Dir doesn't exist")
	}
}
