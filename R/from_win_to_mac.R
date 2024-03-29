#' Title
#'
#' @param windows_path Windows path to be converted to Mac path
#' @param mounted_drive_name Mounted drive to be included in path
#'
#' @return Converted Mac path
#'
#' @examples
#' \dontrun{
#' # Convert a Windows path to a Mac path with a mounted drive name of "mydrive"
#' from_win_to_mac("C:\\Users\\myname\\Documents\\myfile.txt", "mydrive")
#'
#' # Convert a Windows path to a Mac path with a mounted drive name of "Documents"
#' from_win_to_mac("C:\\Users\\myname\\Documents\\myfile.txt", "Documents")
#' }
#' @importFrom stringr str_replace_all
#' @export
from_win_to_mac <- function(windows_path,
                            mounted_drive_name) {

  # remove the drive letter and colon
  generalizable_path <-
    stringr::str_replace_all(windows_path, "^.*?(/.*)$", "\\1")
  # replace backslashes with forward slashes
  generalizable_path <-
    stringr::str_replace_all(generalizable_path, "\\\\", "/")
  # escape spaces with a backslash
  generalizable_path <-
    stringr::str_replace_all(generalizable_path, " ", "\\ ")
  # escape forward slashes with a backslash
  generalizable_path <-
    stringr::str_replace_all(generalizable_path, "/", "\\/")
  created_mac_path <-
    paste0("/Volumes/", mounted_drive_name, generalizable_path)


  return(created_mac_path)
}

