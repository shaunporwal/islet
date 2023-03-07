test_that("from_win_to_mac works", {


  windows_path <- 'L:/asdf/qwer/zxcv/uiop/hjkl.sql'

  mounted_drive_name <- 'test_mounted_drive'

  from_win_to_mac(windows_path = windows_path,
                  mounted_drive_name = mounted_drive_name)

  expect_equal("/Volumes/test_mounted_drive/asdf/qwer/zxcv/uiop/hjkl.sql",
               from_win_to_mac(windows_path = windows_path,
                               mounted_drive_name = mounted_drive_name))

  # file.exists(from_win_to_mac(windows_path = windows_path,
  #                             mounted_drive_name = mounted_drive_name))

})
