## Create temporary directory in file system
testing_path <- paste0(tempdir(), "/testing_directory")
dir.create(testing_path, showWarnings = FALSE)
if(getwd() != testing_path){
  setwd(testing_path)
}

## ------------------------------------------------------------------------------------------------
context("Check that the package files are present")

test_that("Custom .csvfiles are present in installed package", {
  expect_true(file.exists(file.path(system.file(package = "pbs2dlm"), "alt-slot-descriptions.csv")))
  expect_true(file.exists(file.path(system.file(package = "pbs2dlm"), "slot-type-order.csv")))
})

## ------------------------------------------------------------------------------------------------
context("Create a default .rmd file")

if(file.exists("test-desc.rmd")) unlink("test-desc.rmd")
create_default_rmd("test-desc.rmd")

test_that("New .rmd description file is present", {
  expect_true(file.exists("test-desc.rmd"))
})

## ------------------------------------------------------------------------------------------------
context("Check if an slot-chunk tag is manually or accidentally removed from the file")

if(file.exists("test-desc.rmd")) unlink("test-desc.rmd")
create_default_rmd("test-desc.rmd")
rmd <- readLines("test-desc.rmd")
## Remove last autogen end tag from file
rmd <- rmd[-969]
unlink("test-desc.rmd")
conn <- file("test-desc.rmd")
write(rmd, conn)
close(conn)

test_that("Removal of an autogen tag results in error", {
  expect_error(create_rmd("test-desc.rmd", "test-slot-descriptions.csv"))
})

## ------------------------------------------------------------------------------------------------
context("Check if a description header was manually or accidentally removed from the file")
if(file.exists("test-desc.rmd")) unlink("test-desc.rmd")
create_default_rmd("test-desc.rmd")
rmd <- readLines("test-desc.rmd")
## Remove last description from file
rmd <- rmd[-974]
unlink("test-desc.rmd")
conn <- file("test-desc.rmd")
write(rmd, conn)
close(conn)

test_that("Removal of a description results in error", {
  expect_error(create_rmd("test-desc.rmd", "test-slot-descriptions.csv"))
})

## ------------------------------------------------------------------------------------------------
context("Check if more than one description header in an autogen chunk")
if(file.exists("test-desc.rmd")) unlink("test-desc.rmd")
create_default_rmd("test-desc.rmd")
rmd <- readLines("test-desc.rmd")
## Add duplicate description to file
rmd <- append(rmd, rmd[974], after = 974)
unlink("test-desc.rmd")
conn <- file("test-desc.rmd")
write(rmd, conn)
close(conn)

test_that("Duplicate description results in error", {
  expect_error(create_rmd("test-desc.rmd", "test-slot-descriptions.csv"))
})

## ------------------------------------------------------------------------------------------------
context("Check that adding a suffix to the chunk names works")
if(file.exists("test-desc.rmd")) unlink("test-desc.rmd")
create_default_rmd("test-desc.rmd")
change_chunk_suffix("test-desc.rmd", "testme")
rmd <- readLines("test-desc.rmd")
chunk_name_regex <- "(?<=desc-)[\\w-]+(?=\\}| *,)"
val <- grep(chunk_name_regex, rmd, perl = TRUE)
mtch <- grep("testme", rmd[val])

test_that("All lines that should have had sufixes added do", {
  expect_equal(length(val), length(mtch))
})

## ------------------------------------------------------------------------------------------------
context("Check that adding a suffix with special characters or a vector of suffixes gives an error")
if(file.exists("test-desc.rmd")) unlink("test-desc.rmd")
create_default_rmd("test-desc.rmd")

test_that("All lines that should have had sufixes added do", {
  expect_error(change_chunk_suffix("test-desc.rmd", "hello?"))
  expect_error(change_chunk_suffix("test-desc.rmd", "world!"))
  expect_error(change_chunk_suffix("test-desc.rmd", "foo."))
  expect_error(change_chunk_suffix("test-desc.rmd", "."))
  expect_error(change_chunk_suffix("test-desc.rmd", "0<1"))
  expect_error(change_chunk_suffix("test-desc.rmd", "1>0"))
  expect_error(change_chunk_suffix("test-desc.rmd", "\\"))
  expect_error(change_chunk_suffix("test-desc.rmd", "\\\\"))
  expect_error(change_chunk_suffix("test-desc.rmd", "\\\\\\\\"))
  expect_error(change_chunk_suffix("test-desc.rmd", "\\\\\\\\\\"))
  expect_error(change_chunk_suffix("test-desc.rmd", "\\\\\\\\\\\\"))
  expect_error(change_chunk_suffix("test-desc.rmd", "1|2"))
  expect_error(change_chunk_suffix("test-desc.rmd", "3&4"))
  expect_error(change_chunk_suffix("test-desc.rmd", "5:6"))
  expect_error(change_chunk_suffix("test-desc.rmd", "7^8"))
  expect_error(change_chunk_suffix("test-desc.rmd", "hello@worldcom"))
  expect_error(change_chunk_suffix("test-desc.rmd", "5;6"))
  expect_error(change_chunk_suffix("test-desc.rmd", "a~b"))
  expect_error(change_chunk_suffix("test-desc.rmd", "5(6"))
  expect_error(change_chunk_suffix("test-desc.rmd", "5)6"))
  expect_error(change_chunk_suffix("test-desc.rmd", "5[6"))
  expect_error(change_chunk_suffix("test-desc.rmd", "5]6"))
  expect_error(change_chunk_suffix("test-desc.rmd", "5{6"))
  expect_error(change_chunk_suffix("test-desc.rmd", "5}6"))
  expect_error(change_chunk_suffix("test-desc.rmd", "hello`"))
})

## ------------------------------------------------------------------------------------------------
context("Check that ordering and showing/blocking of slots is working")

if(file.exists("test-desc.rmd")) unlink("test-desc.rmd")
if(file.exists("new-test-desc.rmd")) unlink("new-test-desc.rmd")
if(file.exists("test-desc.csv")) unlink("test-desc.csv")
if(file.exists("test-slot-type-order.csv")) unlink("test-slot-type-order.csv")
d1 <- tibble::tribble(~slot_type, ~order,
                         "Stock",      1,
                         "Fleet",      2,
                           "Obs",      3,
                           "Imp",      4)
readr::write_csv(d1, "test-slot-type-order.csv")

d2 <- tibble::tribble(~slot_type, ~slot, ~slot_order, ~show_slot, ~use_custom_description, ~custom_description,
                      "Stock",   "Name",           2,       TRUE,                    TRUE,            "CUSTOM",
                      "Stock",   "Common_Name",    1,       TRUE,                    TRUE,            "CUSTOM",
                      "Fleet",   "Name",           9,       TRUE,                    TRUE,            "CUSTOM",
                        "Obs",   "Name",           1,       TRUE,                   FALSE,            "CUSTOM")
readr::write_csv(d2, "test-desc.csv")

create_default_rmd("test-desc.rmd")
create_rmd("test-desc.rmd", "test-desc.csv", "test-slot-type-order.csv")
rmd <- readLines("new-test-desc.rmd")

test_that("File structure and orders are correct", {
  expect_equal(rmd[8], "## STOCK SLOT DESCRIPTIONS {#app:desc-stock}")
  expect_equal(rmd[15], "### Common_Name {#app:desc-stock-common_name}")
  expect_equal(rmd[17], "*CUSTOM*")
  expect_equal(rmd[25], "### Name {#app:desc-stock-name}")
  expect_equal(rmd[27], "*CUSTOM*")
  expect_equal(rmd[35], "## FLEET SLOT DESCRIPTIONS {#app:desc-fleet}")
  expect_equal(rmd[42], "### Name {#app:desc-fleet-name}")
  expect_equal(rmd[44], "*CUSTOM*")
  expect_equal(rmd[52], "## OBS SLOT DESCRIPTIONS {#app:desc-obs}")
  expect_equal(rmd[59], "### Name {#app:desc-obs-name}")
  expect_equal(rmd[61], "*The name of the observation model object. Single value. Character string.*")
  expect_equal(rmd[68], "")
})

d1$order <- c(4, 3, 2, 1)
d2$slot_order <- c(1, 2, 9, 9)

if(file.exists("test-desc.rmd")) unlink("test-desc.rmd")
if(file.exists("new-test-desc.rmd")) unlink("new-test-desc.rmd")
if(file.exists("test-desc.csv")) unlink("test-desc.csv")
if(file.exists("test-slot-type-order.csv")) unlink("test-slot-type-order.csv")
readr::write_csv(d1, "test-slot-type-order.csv")
readr::write_csv(d2, "test-desc.csv")
create_default_rmd("test-desc.rmd")
create_rmd("test-desc.rmd", "test-desc.csv", "test-slot-type-order.csv")
rmd <- readLines("new-test-desc.rmd")

test_that("Re-ordering and when a slot type (Imp) is missing but comes before other slot types it still works", {
  expect_equal(rmd[8], "## OBS SLOT DESCRIPTIONS {#app:desc-obs}")
  expect_equal(rmd[15], "### Name {#app:desc-obs-name}")
  expect_equal(rmd[17], "*The name of the observation model object. Single value. Character string.*")
  expect_equal(rmd[25], "## FLEET SLOT DESCRIPTIONS {#app:desc-fleet}")
  expect_equal(rmd[32], "### Name {#app:desc-fleet-name}")
  expect_equal(rmd[34], "*CUSTOM*")
  expect_equal(rmd[42], "## STOCK SLOT DESCRIPTIONS {#app:desc-stock}")
  expect_equal(rmd[49], "### Name {#app:desc-stock-name}")
  expect_equal(rmd[51], "*CUSTOM*")
  expect_equal(rmd[59], "### Common_Name {#app:desc-stock-common_name}")
  expect_equal(rmd[61], "*CUSTOM*")
  expect_equal(rmd[68], "")
})

d1$order <- c(1, 3, 2, 1)
readr::write_csv(d1, "test-slot-type-order.csv")

test_that("Duplicate ordering values in slot_type file produces error", {
  expect_error(create_rmd("test-desc.rmd", "test-desc.csv", "test-slot-type-order.csv"))
})

d2$slot_order <- c(1, 1, 9, 9)
readr::write_csv(d2, "test-desc.csv")

test_that("Duplicate ordering values in custom description file produces error", {
  expect_error(create_rmd("test-desc.rmd", "test-desc.csv", "test-slot-type-order.csv"))
})
