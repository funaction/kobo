######################################################################
#
#                       KOBOTOOL
#                    
# description: utiliy functions to prepare the FUNACTION CSV 
#              Kobotoolbox data
#
# author:       Daniel Romero Mujalli
# email:        daniel.romero@supsi.ch
#
# last update:  20240619
#
######################################################################
###############################################################
#' prepare_data
#'
#' DESCRIPTION
#' the function reads funaction kobotoolbox csv file, removes <not 
#' approved> data and photo data columns, simplifies variable names,
#' and returns the resulting dataframe object
#'
#' PARAMETERS
#' @param kobodata name of the funaction-kobo data file
#'
#' @param filename name of the output file
#'
#' @param outdir directory path where to store the file
#'
#' OUTPUT
#' @return a prepared data frame (also writes data on disk to outdir)
#'
#' @export

######################################################################
prepare_data <- function(kobodata
                        ,filename = "fundata.csv"
                        ,outdir = getwd()
                        )
{
    # check that kobodata file exists
    if(!file.exists(kobodata))
        return (print("file not found"))

    # read kobodata
    df <- read.csv(file = kobodata, header = TRUE, sep = ";")

    # remove not-approved records
    df <- df[df$X_validation_status != "Not Approved",]

    # simplify the very long names used for leaf litter
    # 1. find the root string
    root <- names(df)[46]
    # replace root by "" in selected names(df)
    names(df) <- unlist(sapply(X = names(df)
                              ,FUN = sub
                              ,pattern = root
                              ,replacement = ""
                              )
                       )
    # add name to missing names (i.e., names(df) == ""; col 46)
    names(df)[46] <- "leafsp"
    
    # simplify the name for geographic coordinates
    geocord <- c(26:29)
    names(df)[geocord] <- unlist(sapply(X = names(df)[geocord]
                    ,FUN = sub, pattern = "X_Capture.your.location_"
                    ,replacement = ""
                    ))

    # simplify names for type of protection
    root <- paste0(names(df)[18],".")
    names(df)[18] <- "type.protection"
    names(df) <- unlist(sapply(X = names(df)
                    ,FUN = sub, pattern = root, replacement = "")
                    )
    
    # remove "X_", "..", "__" tags from names
    names(df) <- sub(pattern = "X_", replacement = "", x = names(df))
    names(df) <- gsub(pattern = "_*", replacement = "", x = names(df))
    # remove dots repeated more than once
    names(df) <- gsub(pattern = "[.]{2,}", replacement = "", x = names(df))

    # substitute dots at first / last position by ""
    for(name in colnames(df))
    {
        v <- unlist(strsplit(name, split = ""))
        if(v[1] == ".")
            v <- v[-1]
        if(v[length(v)] == ".")
            v <- v[-length(v)]
        # concatenate character vector v and substitute the original name
        newname <- paste(v, collapse = "")
        colnames(df)[colnames(df) == name] <- newname
    }

    # manually simplify other names
    names(df)[6] <- "siteID"
    names(df)[14:17] <- c("W1stvex","W2stvex","status","landuse")
    names(df)[34:37] <- c("SPC","Cond","O2%","dO2")

    # remove boolean columns
    root <- names(df)[7] # "Sample.types.taken"
    selection <- names(df)[grepl(pattern = root, x = names(df))]
    df <- df[, !names(df) %in% selection]

    # remove Photo column names
    selection <- names(df)[grepl(pattern = "Photo", x = names(df))]
    df <- df[, !names(df) %in% selection]

    # write prepared data to outdir/filename
    write.table(x = df
               ,file = paste0(outdir,filename)
               ,quote = FALSE, row.names = FALSE
               ,sep = ";"
               )

    # return prepared data
    return(df)
}
