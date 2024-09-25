
test_that("`is_whole_number()` returns correct values", {
  is_whole_number(1)   %>% expect_true()
  is_whole_number(985) %>% expect_true()
  is_whole_number(37)  %>% expect_true()

  is_whole_number(0.2)    %>% expect_false()
  is_whole_number(25.05)  %>% expect_false()
  is_whole_number(75.489) %>% expect_false()
})


test_that("`an_a()` returns correct values", {
  an_a("start") %>% expect_equal("a")
  an_a("end")   %>% expect_equal("an")
})


test_that("`an_a_type()` returns correct values", {
  an_a_type("bla") %>% expect_equal("a string")
  an_a_type(4)     %>% expect_equal("a double (numeric value)")
})
