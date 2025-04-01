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
# last update:  20241015
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
#' @param outfname name of the output file
#'
#' @param projectname filter by project name  (e.g., funaction)
#'                    Use "all" to select all projects
#'
#' @param rm_photo_data remove data related to photos. Default TRUE
#'
#' @param rm_data_check_columns remove boolean data check variables.
#'                    Default TRUE
#'
#' OUTPUT
#' @return a prepared data frame (also writes data on disk to outdir)
#'
#' @export
#'
######################################################################
prepare_data <- function(kobodata
                        ,outfname = "fundata.csv"
                        ,projectname = "funaction"
                        ,rm_photo_data = TRUE
                        ,rm_data_check_columns = TRUE
                        )
{
    # ensure lower case
    projectname <- tolower(projectname)

    # check that kobodata file exists
    if(!file.exists(kobodata))
        return (print("file not found"))

    # read kobodata
    df <- read.csv(file = kobodata, header = TRUE, sep = ";")

    # select data for desired project
    if(sum(projectname != "all") > 0)
        df <- df[tolower(df$Project) %in% projectname, ]

    # Simplify country names
    #df$Country <- sub(pattern = "funaction_"
    #                 ,replacement = ""
    #                 ,x = df$Country
    #                 )

    # remove not-approved records
    selection <- grep(pattern = "not approved|on hold"
                     ,x = tolower(df$X_validation_status)
                     )
    df <- df[-selection,]

    # simplify names by removing redundant roots
    root <- "Sample.types.taken"
    names(df)[names(df) %in% root] <- "samples_taken"
    names(df) <- sub(pattern = root
                     ,replacement = ""
                     ,x = names(df)
                     )
    root <- "..label.as.USID.|..label.as.UISD."
    names(df) <- sub(pattern = root
                     ,replacement = "_"
                     ,x = names(df)
                     )

    root <- "Unique.Site.ID.."
    names(df) <- sub(pattern = root
                     ,replacement = ""
                     ,x = names(df)
                     )

    root <- ".Water.for.chemical.analysis_"
    names(df) <- sub(pattern = root
                     ,replacement = ""
                     ,x = names(df)
                     )
    root <- "......DO.NOT.filter|......Filter.this.sample.through.0.45.um"
    names(df) <- sub(pattern = root
                     ,replacement = ""
                     ,x = names(df)
                     )
    
    root <- "Volume.filtered.for.Sterivex.filter..ml..."
    names(df) <- sub(pattern = root
                     ,replacement = "volume_"
                     ,x = names(df)
                     )
    
    root <- "X_Capture.your.location_"
    names(df) <- sub(pattern = root
                     ,replacement = ""
                     ,x = names(df)
                     )
    # simplify names used for leaf species
    root <- "leaf_species."
    names(df) <- sub(pattern = root
                     ,replacement = ""
                     ,x = names(df)
                     )
    
    # simplify names for sterivex volume filtered
    root <- "Volume_filtered_for_ivex_filter_ml_"
    names(df) <- sub(pattern = root
                     ,replacement = "volume"
                     ,x = names(df)
                     )
    
    root <- "Plant.taxa.from.which.leaves.were.collected.in.the.Litter.sample..if.sampled.at.the.site."
    names(df)[names(df) %in% root] <- "plant_taxa"
    names(df) <- sub(pattern = root
                     ,replacement = ""
                     ,x = names(df)
                     )

    # simplify names used for PA_type
    root <- "Type.s..of.protections"
    names(df)[names(df) %in% root] <- "PA_type"
    names(df) <- sub(pattern = root
                     ,replacement = ""
                     ,x = names(df)
                     )

    # substitute any use of character ";" by "," to avoid
    # conflict when writing a csv file using sep = ";"
    # 1: identify which columns make use of the char ";"
    selection <- lapply(X = df, FUN = grep, pattern = ";")
    selection <- unlist(lapply(X = selection
                       ,FUN = function(x){
                                            length(x) > 0
                                         }
                              )
                       )
    selection <- names(selection)[selection == TRUE]
    df[, selection] <- gsub(pattern = ";", replacement = ","
                           ,x = df[, selection]
                           )

    # manually simplify other names
    selection <- c("Sampling.Time"
                  ,"Sampling.Date"
                  ,"Water.Temperature...C."
                  ,"Electric.Conductivity.SPC..µS.cm."
                  ,"Electric.Conductivity.C..µS.cm."
                  ,"Oxygen.saturation...."
                  ,"Dissolved.Oxygen..mg.L."
                  )
    names(df)[names(df) %in% selection] <- c(
                                            "time"
                                            ,"date"
                                            ,"waterTemp"
                                            ,"ECspc"
                                            ,"EC"
                                            ,"O2"
                                            ,"DO"
                                            )

    # remove photo data
    if(rm_photo_data)
    {
        selection <- grep(pattern = "photo", x = tolower(names(df)))
        df <- df[, -selection]
    }

    # remove data check boolean variables
    if(rm_data_check_columns)
    {
        selection <- grep(pattern = "sample|X_", x = names(df))
        df <- df[, -selection]
    }

    # write data to outfname
    write.table(x = df
               ,file = outfname
               ,quote = FALSE, row.names = FALSE
               ,sep = ";"
               )

    # return prepared data
    return(df)
}
