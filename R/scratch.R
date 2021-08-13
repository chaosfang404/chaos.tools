scratch = function()
{
	for (i in dir("/scratch"))
	{
		tmp_path=file.path("/scratch",i,"chaos")
		if (dir.exists(tmp_path))
		{
			work_dir <- tmp_path
			print(work_dir)
		}
	}
}
