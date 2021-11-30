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


## original version
# expand_slice <- function(
# 					.data,
# 					expand = 10000,
# 					flank_slice_number = 25,
# 					body_slice_number = 50,
# 					trim = "ceiling"
# ){
# 	dt <- as.data.table(.data)
# 
# 	restname <- colnames(dt)[4:ncol(dt)]
# 
# 	tmp <- function(x)
# 	{
# 		chr <- x[1]
# 		start <- as.numeric(x[2])
# 		end <- as.numeric(x[3])
# 		rest <- x[4:length(x)]
# 
# 		upstream_dt <- seq_dt(
# 							start - expand,
# 							start,
# 							flank_slice_number,
# 							trim = trim
# 						)[
# 							,block := 1:.N
# 						][
# 							V1 >=0
# 						]
# 
# 		body_dt <- seq_dt(
# 						start,
# 						end,
# 						body_slice_number,
# 						trim = trim
# 					)[
# 						,block := (1:.N) + flank_slice_number
# 					]
# 
# 		downstream_dt <- seq_dt(
# 							end,
# 							end + expand,
# 							flank_slice_number,
# 							trim = trim
# 						)[
# 							,block := (1:.N) + flank_slice_number + body_slice_number
# 						]
# 
# 		dt1 <- rbind(
# 					upstream_dt,
# 					body_dt,
# 					downstream_dt
# 				)[
# 					,chr := chr
# 				][
# 					,.(chr,start = V1,end = V2,block)
# 				]
# 		dt2 <- x[4:length(x)] %>% 
# 				rep(each = nrow(dt1)) %>% 
# 				matrix(nrow = nrow(dt1)) %>% 
# 				as.data.table() %>%
# 				setnames(restname)
# 
# 		cbind(dt1,dt2)
# 	}
# 	apply(dt,1,tmp) %>%
# 	rbindlist()
# }

## another version, actually it's faster than formal one, but the code is too long
# expand_slice2 <- function(
# 					.data,
# 					expand = 10000,
# 					flank_slice_number = 25,
# 					body_slice_number = 50,
# 					trim = "ceiling",
# 					direction_col = NA
# ){
# 	if(!is.na(direction_col))
# 	{
# 		dt_raw <- as.data.table(.data) %>%
# 					setnames(
# 						old = direction_col,
# 						new = "hope_you_will_never_find_out"
# 					)
# 
# 		dt <- dt_raw[hope_you_will_never_find_out == "+"]
# 
# 		dt_reward <- dt_raw[hope_you_will_never_find_out == "-"]
# 	}else
# 	{
# 		dt <- as.data.table(.data)
# 	}
# 
# 	restname <- colnames(dt)[4:ncol(dt)]
# 
# 	forward_slice <- function(x)
# 	{
# 		chr <- x[1]
# 		start <- as.numeric(x[2])
# 		end <- as.numeric(x[3])
# 		rest <- x[4:length(x)]
# 
# 		upstream_dt <- seq_dt(
# 							start - expand,
# 							start,
# 							flank_slice_number,
# 							trim = trim
# 						)[
# 							,block := 1:.N
# 						][
# 							V1 >=0
# 						]
# 
# 		body_dt <- seq_dt(
# 						start,
# 						end,
# 						body_slice_number,
# 						trim = trim
# 					)[
# 						,block := (1:.N) + flank_slice_number
# 					]
# 
# 		downstream_dt <- seq_dt(
# 							end,
# 							end + expand,
# 							flank_slice_number,
# 							trim = trim
# 						)[
# 							,block := (1:.N) + flank_slice_number + body_slice_number
# 						]
# 
# 		dt1 <- rbind(
# 					upstream_dt,
# 					body_dt,
# 					downstream_dt
# 				)[
# 					,chr := chr
# 				][
# 					,.(chr,start = V1,end = V2,block)
# 				]
# 
# 		dt2 <- x[4:length(x)] %>% 
# 				rep(each = nrow(dt1)) %>% 
# 				matrix(nrow = nrow(dt1)) %>% 
# 				as.data.table() %>%
# 				setnames(restname)
# 
# 		cbind(dt1,dt2)
# 	}
# 	forward <- apply(dt,1,forward_slice) %>%
# 				rbindlist()
# 
# 	if(!is.na(direction_col))
# 	{
# 		reward_slice <- function(x)
# 		{
# 			chr <- x[1]
# 			start <- as.numeric(x[3])
# 			end <- as.numeric(x[2])
# 			rest <- x[4:length(x)]
# 
# 			upstream_dt <- seq_dt(
# 								start + expand,
# 								start,
# 								flank_slice_number,
# 								trim = trim
# 							)[
# 								,block := 1:.N
# 							]
# 
# 			body_dt <- seq_dt(
# 							start,
# 							end,
# 							body_slice_number,
# 							trim = trim
# 						)[
# 							,block := (1:.N) + flank_slice_number
# 						]
# 
# 			downstream_dt <- seq_dt(
# 								end,
# 								end - expand,
# 								flank_slice_number,
# 								trim = trim
# 							)[
# 								,block := (1:.N) + flank_slice_number + body_slice_number
# 							][
# 								V1 >= 0
# 							]
# 
# 			dt1 <- rbind(
# 						upstream_dt,
# 						body_dt,
# 						downstream_dt
# 					)[
# 						,chr := chr
# 					][
# 						,.(chr,start = V2,end = V1,block)
# 					]
# 
# 			dt2 <- x[4:length(x)] %>% 
# 					rep(each = nrow(dt1)) %>% 
# 					matrix(nrow = nrow(dt1)) %>% 
# 					as.data.table() %>%
# 					setnames(restname)
# 
# 			cbind(dt1,dt2)
# 		}
# 		reward <- apply(dt_reward,1,reward_slice) %>%
# 					rbindlist()
# 	}else
# 	{
# 		reward <- NULL
# 	}
# 
# 	final <- rbind(forward,reward)
# 
# 	if(!is.na(direction_col))
# 	{
# 		setnames(
# 			final,
# 			old = "hope_you_will_never_find_out",
# 			new = direction_col
# 		)
# 	}
# 	final
# }

expand_slice <- function(
					.data,
					expand = 10000,
					flank_slice_number = 25,
					body_slice_number = 50,
					trim = "ceiling",
					direction_col_number = NA
){
	dt <- as.data.table(.data)

	if(ncol(dt) < 4)
	{
		dt[,V4 := paste0("ref_ID_",1:.N)]
	}

	restname <- colnames(dt)[4:ncol(dt)]

	tmp <- function(x)
	{
		chr <- x[1]
		if(!is.na(direction_col_number))
		{
			if(x[direction_col_number] == "-")
			{
				start <- as.numeric(x[3])
				end <- as.numeric(x[2])
				expand <- expand * -1
			}else if(x[direction_col_number] == "+")
			{
				start <- as.numeric(x[2])
				end <- as.numeric(x[3])
			}
		}else
		{
			start <- as.numeric(x[2])
			end <- as.numeric(x[3])
		}

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
						][
							V2 >= 0
						]

		dt1 <- rbind(
					upstream_dt,
					body_dt,
					downstream_dt
				)[
					,chr := chr
				]

		if(!is.na(direction_col_number))
		{
			if(x[direction_col_number] == "-")
			{
				dt1 <- dt1[,.(chr,start = V2,end = V1,block)]
			}else if(x[direction_col_number] == "+")
			{
				dt1 <- dt1[,.(chr,start = V1,end = V2,block)]
			}
		}else
		{
			dt1 <- dt1[,.(chr,start = V1,end = V2,block)]
		}


		dt2 <- x[4:length(x)] %>% 
				rep(each = nrow(dt1)) %>% 
				matrix(nrow = nrow(dt1)) %>% 
				as.data.table() %>%
				setnames(restname)

		cbind(dt1,dt2)
	}
	apply(dt,1,tmp) %>%
	rbindlist() %>%
	.[]
}
