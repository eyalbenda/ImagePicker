library(tidyverse)
library(magick)
library(patchwork)
image_parse = function(dr,exts)
{
  ext_str = paste(exts,collapse="|")
  fls = list.files(path = dr,recursive=T,full.names = F,pattern=str_glue("*.({ext_str})$")) 
  fls_list = strsplit(fls,"/")
  df_cols =  max(unique(sapply(fls_list,length)))
  df_rows = length(fls)
  df_file = data.frame(matrix(NA,nrow=df_rows,ncol=df_cols),stringsAsFactors = F)
  for(i in 1:df_rows)
  {
    cur_len = length(fls_list[[i]])
    for(j in 1:cur_len)
    {
      df_file[i,df_cols-j+1] = fls_list[[i]][cur_len-j+1]
    }
  }
  return(df_file)
}

image_batch = function(df_file,level=1)
{
  max_level = ncol(df_file)-1
  if(level>max_level)
  {
    level = max_level
    print(paste0("Can't set level to higher than ",max_level))
  }
  out_concat = out_concat_full = "/"
  for(i in 1:(ncol(df_file)-level))
  {
    out_concat = file.path(out_concat,df_file[,i])
  }
  for(i in 1:ncol(df_file))
  {
    out_concat_full = file.path(out_concat_full,df_file[,i])
  }
  out_concat_full = sub("/","",out_concat_full)
  out_paths = list()
  for(i in unique(out_concat))
  {
    out_paths[i] = list(out_concat_full[out_concat==i])
  }
  return (out_paths)
}

img_plot = function(imgs,pl_name)
{
  patch = image_ggplot(image_read(imgs[1])) + ggtitle(pl_name)
  for(i in 2:length(imgs))
    patch = patch + image_ggplot(image_read(imgs[i]))
  return(patch)
}