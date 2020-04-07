context('RStudio Conf 2020 Success')

#' @title T1.1
#' @section Last Updated By:
#' Not Ellis Hughes
#' @section Last Update Date:
#' 2020/01/29

test_that('T1.1',{

  joke_result <- joke('What do you call a fake noodle?','An Impasta')
  expect_true(is_dad_joke(joke_result))
  expect_true(caused_laugher(joke_result))
  expect_true(embarrased_significant_other(joke_result))

})
