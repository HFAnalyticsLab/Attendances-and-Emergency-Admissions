test_that("version from filename works", {
    expect_equal(parse_month("Timeseries-February-ae-by-provider-12341.xls"), "february")
    expect_equal(parse_month("Timeseries-March-ae-by-provider-afeb.xls"), "march") # check not matching the afeb
    expect_equal(parse_month("Timeseries-Quarterly-ae-by-provider-afeb.xls"), NA)

})

test_that("date from filename works", {
    expect_equal(parse_year("Timeseries-February-2020-ae-by-provider-12341.xls"), 2020)
    expect_equal(parse_year("Timeseries-February-2020-ae-by-provider-1234.xls"), 2020) # find first 'datey' thing
    expect_equal(parse_year("Timeseries-February-ae-by-provider-12341.xls"), NA)
})

test_that("release_frequency from filename works", {
    expect_equal(parse_release_frequency("Monthly-Timeseries-February-2020-ae-by-provider-12341.xls"), "monthly")
    expect_equal(parse_release_frequency("Quarterly-Timeseries-February-2020-ae-by-provider-12341.xls"), "quarterly")
    expect_equal(parse_release_frequency("Timeseries-February-2020-ae-by-provider-12341.xls"), NA)
})

test_that("adjusted flag from filename works", {
    expect_true(parse_adjusted("Monthly-Timeseries-February-2020-adjusted-ae-by-provider-12341.xls"))
    expect_false(parse_adjusted("Quaterly-Timeseries-February-2020-unadjusted-ae-by-provider-12341.xls"), )
    expect_equal(parse_adjusted("Quaterly-Timeseries-February-2020-ae-by-provider-12341.xls"), NA)

})
