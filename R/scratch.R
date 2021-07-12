scratch = function()
{
	for (i in dir("/scratch"))
	{
		tmp_path=file.path("/scratch",i,"chaos")
		if (dir.exists(tmp_path))
		{
			work_dir <- tmp_path
			print(paste0("change work dir to ", work_dir))
			setwd(work_dir)
		}
	}
}
