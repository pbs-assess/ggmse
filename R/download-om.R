#' Download an operating model from the DLMtool website
#'
#' @param file The name of the file. If there is no .rds extension one will be appended
#' @param web_file Like this: "Pacific_Ocean_Perch_QC_BC_DFO/OM.rdata" See webpage for listing
#' @param base_url The URL to find the web_file on
#' @param method Download method. It seems that on "wget" works on MS Windows
#' @param overwrite TRUE or FALSE to overwrite file
#'
#' @importFrom stringi stri_sub
#' @export
#'
#' @examples
#' download_om("pop", "Pacific_Ocean_Perch_QC_BC_DFO/OM.rdata")
download_om <- function(file = "default-download-om.rds",
                        web_file,
                        base_url = "http://www.datalimitedtoolkit.org/Case_Studies_Table/",
                        method = "wget",
                        overwrite = FALSE){
  nc <- nchar(file)
  if(tolower(stringi::stri_sub(file, from = nc - 3, to = nc)) != ".rds"){
    file <- paste0(file, ".rds")
  }

  if(overwrite | !file.exists(file)){
    download.file(paste0(base_url, web_file),
                  file,
                  method = method)
  }
}
