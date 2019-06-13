#' Get the actual name of the object for the base name object. e.g. stock
#' object may have a suffix and be stockpc <- new('Stock') or similar
#' Assumes the first instance of stock* <- contains the object name
get_obj_name <- function(rmd, obj_base_name){
  paste0(obj_base_name,
         regmatches(rmd,
                    regexpr(paste0("(?<=", obj_base_name,")[\\w-]+(?= *\\<-.*)"),
                            rmd,
                            perl = TRUE)))[1]
}

#' Change the chunk and tag suffixes for a .Rmd file
#'
#' @param file_name Filename/path of the .rmd file to create/modify. If it does not exist,
#'  an error will be given
#' @param chunk_suffix A string to be appended to the chunk names and tags with a preceding dash.
#'  If a name has already been appended this new suffix will replace it. If this is the empty
#'  string, any previous suffixes will be removed.
#'
#' @return Nothing
#' @export
#'
#' @examples
#' testing_path <- tempdir()
#' dir.create(testing_path, showWarnings = FALSE)
#' create_default_rmd(file.path(testing_path, "test.Rmd"))
#' change_chunk_suffix(file.path(testing_path, "test.Rmd"), "test-suffix")
change_chunk_suffix <- function(file_name,
                                chunk_suffix = ""){
  if (!file.exists(file_name)){
    stop("Error - file '", file_name, "' does not exist. Run create_default_rmd(file_name) ",
         "to create it.", call. = FALSE)
  }
  if(length(chunk_suffix) > 1){
    stop("Error - chunk_suffix must be a single string, not a vector.",
         call. = FALSE)
  }
  if(gfutilities::has_specials(chunk_suffix, white = TRUE)){
    stop("Error - chunk_suffix can only contain letters, numbers, dashes (-), ",
         "or underscores (_).", call. = FALSE)
  }
  ## Regex will find both tags and code chunk names (| part in lookbehind)
  chunk_name_regex <- "(?<=desc-)[\\w-]+(?=\\}| *,)"
  rmd <- readLines(file_name)
  val <- grep(chunk_name_regex, rmd, perl = TRUE)
  lapply(val, function(x){
    k <- stringr::str_split(regmatches(rmd[x], regexpr(chunk_name_regex, rmd[x], perl = TRUE)), "-")[[1]]
    if(length(k) >= 3){
      ## Remove old suffix if it exists
      k <- k[c(1,2)]
    }
    ## Must check the second part to see if it is a legal slot name. This is for tags for base
    ## types such as 'stock' and 'fleet' which may have a previously added suffix
    if(length(k) == 2){
      if(k[1] == "stock"){
        if(!any(k[2] == tolower(DLMtool::StockDescription$Slot))){
          k <- k[-2]
        }
      }else if(k[1] == "fleet"){
        if(!any(k[2] == tolower(DLMtool::FleetDescription$Slot))){
          k <- k[-2]
        }
      }else if(k[1] == "obs"){
        if(!any(k[2] == tolower(DLMtool::ObsDescription$Slot))){
          k <- k[-2]
        }
      }else if(k[1] == "imp"){
        if(!any(k[2] == tolower(DLMtool::ImpDescription$Slot))){
          k <- k[-2]
        }
      }else{
        k <- k[-2]
      }
    }
    if(chunk_suffix != ""){
      k[length(k) + 1] <- chunk_suffix
    }
    rmd[x] <<- sub(chunk_name_regex, paste(k, collapse = "-"), rmd[x], perl = TRUE)
  })

  ## Now change all instances of stock, fleet, obs, and impl objects in code chunks to have suffix
  purrr::map(c("stock", "fleet", "obs", "imp"), function(x){
    rmd <<- gsub(get_obj_name(rmd, x), paste0(x, chunk_suffix), rmd)
  })

  conn <- file(file_name)
  write(rmd, conn)
  close(conn)

}

#' Create .Rmd file describing DLMtool Objects and Slots and inject custom descriptions into it
#'
#' @param file_name Filename/path of the .rmd file to create/modify. If it does not exist,
#'  it will be created using create_default_rmd()
#' @param cust_desc_file_name Filename/path of the .csv file containing the custom descriptions.
#'  Use generate_default_custom_descriptions_file() in scratch.R to auto-generate it.
#'
#' @return Nothing
#' @export
#'
#' @examples
#' testing_path <- tempdir()
#' dir.create(testing_path, showWarnings = FALSE)
#' create_rmd(file.path(testing_path, "test.Rmd"))
create_rmd <- function(file_name,
                       cust_desc_file_name = system.file("alt-slot-descriptions.csv", package = "pbs2dlm"),
                       slot_type_order_file_name = system.file("slot-type-order.csv", package = "pbs2dlm"),
                       ...){

  if (!file.exists(file_name)){
    create_default_rmd(file_name, ...)
  }

  cust_desc <- readr::read_csv(cust_desc_file_name)
  cust_desc[,c(1,2)] <- apply(cust_desc[,c(1,2)], c(1,2), tolower)

  slot_type_order <- readr::read_csv(slot_type_order_file_name)

  rmd <- readLines(file_name)
  beg <- grep("<!-- slot-chunk-begin -->", rmd)
  end <- grep("<!-- slot-chunk-end -->", rmd)
  if(length(beg) != length(end)){
    stop("Error - mismatch between number of slot begin tags (", length(beg), ") and ",
         "end tags (", length(end), ").\nLine numbers for begin tags are:\n", paste(beg, collapse = " "),
         "\nLine numbers for end tags are:\n", paste(end, collapse = " "), "\n",
         call. = FALSE)
  }
  pre <- rmd[1:(beg[1] - 1)]
  ## Remove Initial slot type header and code from pre
  hash <- grep("##", pre)
  hash <- hash[length(hash)]
  pre <- pre[1:(hash - 1)]

  if(end[length(end)] == length(rmd)){
    post <- ""
  }else{
    post <- rmd[(end[length(end)] + 1):length(rmd)]
  }
  ## Create list of slot chunks and between slot chunks
  slots <- lapply(seq_along(beg), function(x){
    rmd[beg[x]:end[x]]
  })

  ## Add custom definitions to slots which are set to have them, and check if they are to
  ## be shown in the document. List elements will be slot chunks that are set to be shown,
  ## or NULLs for those which are not
  slots <- lapply(slots, function(x){
    k <- stringr::str_split(regmatches(x, regexpr("(?<=desc-)[\\w-]+(?=\\})", x, perl = TRUE)), "-")[[1]]
    kk <- cust_desc %>%
      dplyr::filter(slot_type == k[1]) %>%
      dplyr::filter(slot == k[2])
    if(nrow(kk) != 1){
      return(NULL)
    }
    if(kk$use_custom_description){
      val <- grep("^\\*.*\\*$", x)
      if(!length(val)){
        stop("Error trying to find the description inside autogen chunk. Note it needs to start and end with an asterisk:\n",
             paste0(j, collapse = "\n"),
             call. = FALSE)
      }
      if(length(val) > 1){
        stop("Error - more than one line matches as a description inside autogen chunk:\n",
             paste0(j, collapse = "\n"),
             call. = FALSE)
      }
      x[val] <- paste0("*", kk$custom_description, "*")
    }
    if(!kk$show_slot){
      return(NULL)
    }
    x
  })

  ## Remove any list elements that became NULL do to non-inclusion ini the above lapply
  slots[sapply(slots, is.null)] <- NULL

  if(length(unique(slot_type_order$order)) != nrow(slot_type_order)){
    stop("Error - all values in the order column in '", slot_type_order_file_name, "' must be unique.",
         call. = FALSE)
  }
  ## Reorder the slot list by slot_type first as found in slot_type_order_file_name,
  ## then order as found in cust_desc_file_name.
  jj <- slot_type_order %>%
    dplyr::arrange(order) %>%
    dplyr::pull(slot_type) %>%
    tolower()

  xxx <- do.call(rbind, lapply(jj, function(x){
    kk <- cust_desc %>%
      dplyr::filter(slot_type == x) %>%
      dplyr::arrange(slot_order)

    if(length(unique(kk$slot_order)) != nrow(kk)){
      stop("Error - all values in the slot_order column in '", cust_desc_file_name,
           "' for slot_type '", x, "' must be unique.",
           call. = FALSE)
    }
    kk
  })) %>%
     dplyr::transmute(slot_type, slot, order = row_number())

  last_slot_type <- "none"
  ## Check every row in the ordered description table and if it is in the slots list,
  ## insert it in order
  s_in_list <- function(nm){
    # Get logical vector of where the slot name nm is in the slots list
    nm_str <- paste(nm$slot_type, nm$slot, sep = "-")
    whr <- sapply(slots, function(x){
      k <- stringr::str_split(regmatches(x, regexpr("(?<=desc-)[\\w-]+(?=\\})", x, perl = TRUE)), "-")[[1]]
      g <- paste(k[1], k[2], sep = "-")
      if(g == nm_str){
        return(TRUE)
      }
      FALSE
    })

    if(any(whr)){
      if(sum(whr) > 1){
        stop("Error - More than one chunk matches the name '", nm_str, "'.",
             call. = FALSE)
      }
      if(last_slot_type != nm$slot_type){
        if(nm$slot_type == "stock"){
          stock_obj_name <- get_obj_name(rmd, "stock")
          slots[whr][[1]] <- c("",
                               "## STOCK SLOT DESCRIPTIONS {#app:desc-stock}",
                               "",
                               "```{r warnings = FALSE}",
                               paste0(stock_obj_name, " <- methods::new('Stock')"),
                               "```",
                               "",
                               slots[whr][[1]])
        }else if(nm$slot_type == "fleet"){
          fleet_obj_name <- get_obj_name(rmd, "Fleet")
          slots[whr][[1]] <- c("",
                               "## FLEET SLOT DESCRIPTIONS {#app:desc-fleet}",
                               "",
                               "```{r warnings = FALSE}",
                               paste0(fleet_obj_name, " <- DLMtool::Generic_Fleet # TODO: temporary"),
                               "```",
                               "",
                               slots[whr][[1]])
        }else if(nm$slot_type == "obs"){
          obs_obj_name <- get_obj_name(rmd, "obs")
          slots[whr][[1]] <- c("",
                               "## OBS SLOT DESCRIPTIONS {#app:desc-obs}",
                               "",
                               "```{r warnings = FALSE}",
                               paste0(obs_obj_name, " <- methods::new('Obs')"),
                               "```",
                               "",
                               slots[whr][[1]])
        }else if(nm$slot_type == "imp"){
          imp_obj_name <- get_obj_name(rmd, "imp")
          slots[whr][[1]] <- c("",
                               "## IMP SLOT DESCRIPTIONS {#app:desc-imp}",
                               "",
                               "```{r warnings = FALSE}",
                               paste0(imp_obj_name, " <- methods::new('Imp')"),
                               "```",
                               "",
                               slots[whr][[1]])
        }else{
          slots[whr][[1]] <- c("", slots[whr][[1]])
        }
        last_slot_type <<- nm$slot_type
      }
      return(slots[whr])
    }
    NULL
  }

  ordered_slots <- list()
  for(i in seq(1, nrow(xxx))){
    new_slot <- s_in_list(xxx[i,])
    ordered_slots <- c(ordered_slots, new_slot)
  }

  new_rmd <- unlist(c(pre, ordered_slots, post))

  conn <- file(file_name)
  write(new_rmd, conn)
  close(conn)
}

#' Create template Rmd file describing DLMtool Objects and Slots
#'
#' @param file_name Filename/path of where to save the .Rmd file.
#' @param overwrite Overwrite?
#' @param knitr_results Show knitr results?
#' @param knitr_echo Echo knitr code?
#'
#' @return Nothing
#' @export
#'
#' @examples
#' testing_path <- tempdir()
#' dir.create(testing_path, showWarnings = FALSE)
#' create_default_rmd(file.path(testing_path, "test.Rmd"))
create_default_rmd <- function(file_name, overwrite = FALSE,
  knitr_results = TRUE, knitr_echo = TRUE) {
  if (file.exists(file_name) && !overwrite)
    stop("File '", file_name, "' already exists. ",
      "Set `overwrite = TRUE` if you want to overwrite it.",
      call. = FALSE)

  rmd <- c(
    "```{r message = FALSE}\nlibrary(DLMtool)",
    paste0("knitr_results <- ", as.character(eval(knitr_results))),
    paste0("knitr_echo <- ", as.character(eval(knitr_echo))),
    "```\n",
    # format_desc(DLMtool::DataDescription, "Data"),
    format_desc(DLMtool::StockDescription, "Stock"),
    format_desc(DLMtool::FleetDescription, "Fleet"),
    format_desc(DLMtool::ObsDescription, "Obs"),
    format_desc(DLMtool::ImpDescription, "Imp")
    # format_desc(DLMtool::OMDescription, "OM")
  )

  conn <- file(file_name)
  write(rmd, conn)
  close(conn)
}

#' Format a DLMtool description dataframe into Rmarkdown format and produce a
#'   string combining all of them together with a section name
#'
#' @param df DLMtool Description data frame
#' @param obj_name Name to use for the section
#' @param inst_obj_name The name to use for the instance of the object
#'
#' @return The Rmd - formatted vector of strings
#' @noRd
#'
#' @examples
#' format_desc(DLMtool::DataDescription, "Data")
format_desc <- function(df,
  obj_name = "Data",
  inst_obj_name = tolower(obj_name)) {

  df$chunk_name <- tolower(paste0("desc-", inst_obj_name, "-", df$Slot))

  df <- df %>%
    dplyr::mutate(
      code = paste0(
        "```{r ",
        chunk_name,
        ", results = knitr_results, echo = knitr_echo}\n",
        inst_obj_name, "@", Slot,
        "\n```\n",
        "<!-- slot-chunk-end -->\n"
      ),
      Slot = paste0(
        "<!-- slot-chunk-begin -->\n",
        "### ",
        Slot,
        paste0(" {#app:", chunk_name, "}")
      ),
      Description = paste0(
        "*",
        Description,
        "*\n"
      )
    )

  df <- df[!grepl("NO LONGER USED", toupper(df$Description)), , drop = FALSE]
  df$chunk_name <- NULL
  df$Description <- gsub("([A-Za-z0-9]+)\\*$", "\\1.*", df$Description)
  df$Description <- gsub("([A-Za-z0-9]+)@([A-Za-z0-9]+)", "`\\1@\\2`",
    df$Description)

  c(
    paste0(
      toupper(paste0("## ", obj_name, " slot descriptions ")),
      tolower(paste0("{#app:desc-", obj_name, "}\n"))
    ),
    paste0(
      "```{r warnings = FALSE}\n",
      inst_obj_name, " <- methods::new('", obj_name, "')\n```\n"
    ),
    apply(df, 1, function(x) paste0(x, collapse = "\n\n"))
  )
}
