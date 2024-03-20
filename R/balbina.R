#' balbina
#'
#' Soundscape data for 5 sites on two islands in the Balbina Hydroelectric Reservoir in
#' Brazilian Amazonia. At each site, we collected 1,440 sound files using a 1-min/5-min
#' sampling regime for a period of 5 days. These recordings were processed in the
#' soundscapeR package using the `ss_find_files`, `ss_assess_files`, `ss_index_calc` and
#' `ss_create` functions. The result is a list of five S4 objects of the class 'soundscape'.
#'
#' @docType data
#'
#' @usage data(balbina)
#'
#' @format A list of S4 objects of class \code{"soundscape"}; see \code{\link[soundscapeR]{ss_create}}.
#'
#' @keywords datasets
#'
#' @references Luypaert et al. (2022) Methods in Ecology and Evolution 13(10):2262-2274
#' (\href{https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13924}{MEE})
#'
#' @source \href{https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13924}{MEE}
#'
#' @examples
#' data(balbina)
#' soundscape_pcoa <- ss_pcoa(soundscape_list = balbina)
"balbina"
