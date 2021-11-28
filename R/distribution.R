chr_omit <- function(
				.data
){
	.data %>%
	gsub(pattern = "chr",replacement = "")
}

distribution <- function(
					reference,
					reference_name = NA,
					align,
					align_name = NA,
					expand = 10000,
					flank_slice_number = 25,
					body_slice_number = 50,
					trim = "ceiling",
					direction_col_number = NA,
					random_times = 3,
					align_ref = "hg19",
					reference_key = NA,
					align_key = NA,
					plot = FALSE
){
	if(length(reference_name) == 1)
	{
		if(is.na(reference_name))
		{
			reference_name <- base_name(reference)
		}
	}

	if(length(align_name) == 1)
	{
		if(is.na(align_name))
		{
			align_name <- base_name(align)
		}
	}

	reference_dt <- data.table(
						rep(reference,each = length(expand)),
						rep(reference_name,each = length(expand)),
						rep(expand,each = length(reference))
					)

	align_dt <- data.table(
						align,
						align_name) 

	reference_expand_slice <- function(
							x
	){
		fread(
			x[1]
		)[
			,V1 := chr_omit(V1)
		] %>%
		expand_slice(
			expand = as.numeric(x[3]),
			flank_slice_number = flank_slice_number,
			body_slice_number = body_slice_number,
			direction_col_number = direction_col_number,
			trim = trim
		) %>%
		.[
			,reference := x[2]
		] %>%
		.[
			,expand := x[3]
		]
	}

	reference_info <- reference_dt %>%
						apply(1,reference_expand_slice) %>%
						rbindlist() %>%
						setkey(chr,start,end)


	read_align <- function(x)
	{
		fread(
			x[1]
		)[
			,V1 := chr_omit(V1)
		][
			,align := x[2]
		]
	}
	
	align_real <- align_dt %>%
				apply(1,read_align) %>%
				rbindlist() %>%
				.[,exp := "real"]

	if(random_times > 0)
	{
		align_random_fun <- function(
								x
		){
			shuffle(
				align_real,
				ref = align_ref
			)[
				,exp := paste0("random_",x[1])
			]
		}

		align_random <- data.table(
							1:random_times
						) %>%
						apply(1,align_random_fun) %>%
						rbindlist()
		random_col <- paste0("random_",1:random_times)
	}else
	{
		align_random <- NULL
		random_col <- NULL
	}

	if(length(reference_key) == 1)
	{
		if(!is.na(reference_key))
		{
			reference_key <- c("block","reference","expand",reference_key)
		}else
		{
			reference_key <- c("block","reference","expand")
		}
	}else
	{
		reference_key <- c("block","reference","expand",reference_key)
	}

	if(length(align_key) == 1)
	{
		if(!is.na(align_key))
		{
			align_key <- c("align",align_key)
		}else
		{
			align_key <- c("align")
		}
	}else
	{
		align_key <- c("align",align_key)
	}

	final_col <- c(
					reference_key,
					align_key,
					"relative",
					"real",
					random_col
				)


	overlap <- rbind(
					align_real,
					align_random
				) %>%
				setnames(
					old = c("V1","V2","V3"),
					new = c("chr","start","end")
				) %>%
				setkey(chr,start,end) %>%
				foverlaps(
					reference_info,
					nomatch = NULL
				) %>%
				.[
					,.N,
					c(reference_key,align_key,"exp")
				] %>%
				wider_dt(
					name = "exp",
					value = "N",
					fill = 0
				)

	if(random_times > 0)
	{
		mean_random <- apply(overlap[,..random_col],1,mean)
		dt <- overlap[,relative := real - mean_random]
	}else
	{
		dt <- overlap[,relative := real]
	}
	
	dt
}
