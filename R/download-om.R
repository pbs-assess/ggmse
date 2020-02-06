#' Download an operating model from the DLMtool website
#'
#' @param file The name of the file. If there is no .rds extension one will be appended
#' @param web_file Like this: "Pacific_Ocean_Perch_QC_BC_DFO/OM.rdata" See webpage for listing
#' @param base_url The URL to find the web_file on
#' @param method Download method. It seems that on "wget" works on MS Windows
#' @param overwrite TRUE or FALSE to overwrite file
#'
#' @importFrom gfutilities file_addext
#' @importFrom utils download.file
#' @export
#'
#' @examples
#' \donttest{
#' library(gfdlm)
#' download_om("pop", "Pacific_Ocean_Perch_QC_BC_DFO/OM.rdata")
#' }
download_om <- function(file = "default-download-om.rds",
                        web_file,
                        base_url = "http://www.datalimitedtoolkit.org/Case_Studies_Table/",
                        method = "wget",
                        overwrite = FALSE) {

  file <- gfutilities::file_addext(file, "rds")

  if (overwrite || !file.exists(file)) {
    download.file(paste0(base_url, web_file),
      file,
      method = method
    )
  }
}
