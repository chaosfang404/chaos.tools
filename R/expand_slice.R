seq_dt <- function(
			start,
			end,
			slice_number,
			trim = "none"
){
	tmp1 <- seq(
				start,
				end,
				length.out = slice_number +1
			)

	if(trim == "floor")
	{
		tmp2 <- floor(tmp1)
	}else if(trim == "ceiling")
	{
		tmp2 <- ceiling(tmp1)
	}else if(trim == "round")
	{
		tmp2 <- round(tmp1)
	}else
	{
		tmp2 <- tmp1
	}

	data.table(
		tmp2[1:slice_number],
		tmp2[2:(slice_number + 1)]
	)
}


expand_slice <- function(
					.data,
					expand = 10000,
					flank_slice_number = 25,
					body_slice_number = 50,
					trim = "ceiling"
){
	dt <- as.data.table(test)

	restname <- colnames(dt)[4:ncol(dt)]

	tmp <- function(x)
	{
		chr <- x[1]
		start <- as.numeric(x[2])
		end <- as.numeric(x[3])
		rest <- x[4:length(x)]
	
		upstream_dt <- seq_dt(
							start - expand,
							start,
							flank_slice_number,
							trim = trim
						)[
							,block := 1:.N
						][
							V1 >=0
						]
	
		body_dt <- seq_dt(
						start,
						end,
						body_slice_number,
						trim = trim
					)[
						,block := (1:.N) + flank_slice_number
					]
	
		downstream_dt <- seq_dt(
							end,
							end + expand,
							flank_slice_number,
							trim = trim
						)[
							,block := (1:.N) + flank_slice_number + body_slice_number
						]
	
		dt1 <- rbind(
					upstream_dt,
					body_dt,
					downstream_dt
				)[
					,chr := chr
				][
					,.(chr,start = V1,end = V2,block)
				]
		dt2 <- x[4:length(x)] %>% 
				rep(each = nrow(dt1)) %>% 
				matrix(nrow = nrow(dt1)) %>% 
				as.data.table() %>%
				setnames(restname)

		cbind(dt1,dt2)
	}
	apply(dt,1,tmp) %>%
	rbindlist()
}
