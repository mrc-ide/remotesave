context("core")

test_that("keys", {
  k <- remote_save_keys("root", "user", "session")
  expect_equal(k$latest, "root:user:session:latest")
  expect_equal(k$pattern, "root:user:*:latest")
  expect_equal(k$fmt, "root:user:%s:latest")
  expect_equal(k$re, "^root:user:([^:]+):latest$")

  expect_true(grepl(k$re, k$latest))
  expect_equal(sub(k$re, "\\1", k$latest), "session")
  expect_equal(sprintf(k$fmt, "session"), k$latest)
})


test_that("empty object", {
  user <- ids::random_id()
  remote <- remote_save(TEST_ROOT, user)
  expect_equal(
    remote$list(),
    data_frame(key = character(0),
               session = character(0),
               current = logical(0),
               label = character(0),
               time = character(0)))
  expect_identical(remote$last(), NA_character_)
})


test_that("save and retrieve", {
  user <- ids::random_id(bytes = 4)
  session <- ids::random_id(bytes = 4)
  remote <- remote_save(TEST_ROOT, user, session = session)
  remote$save(mtcars, "label")
  expect_equal(remote$fetch(session), mtcars)
  last <- remote$last()
  expect_is(last, "character")
  expect_equal(last, remote$list()$time)
})


test_that("save - 2 sessions", {
  user <- ids::random_id(bytes = 4)
  session1 <- ids::random_id(bytes = 4)
  session2 <- ids::random_id(bytes = 4)
  remote1 <- remote_save(TEST_ROOT, user, session = session1)
  remote2 <- remote_save(TEST_ROOT, user, session = session2)
  remote1$save(mtcars, "label1")
  remote2$save(iris, "label2")

  d1 <- remote1$list()
  expect_equal(d1$session, c(session1, session2))
  expect_equal(d1$label, c("label1", "label2"))
  expect_equal(d1$current, c(TRUE, FALSE))
  expect_is(d1$time, "character")
  expect_true(d1$time[[1]] < d1$time[[2]])

  d2 <- remote2$list()
  expect_equal(d2$current, !d1$current)
  d2$current <- d1$current
  expect_equal(d1, d2)

  remote1$delete()

  expect_equal(remote1$list(), d1[2, ], check.attributes = FALSE)
  remote1$delete_user()
  expect_equal(remote1$list(), d1[integer(0), ])
})


test_that("save works with empty label", {
  user <- ids::random_id(bytes = 4)
  session <- ids::random_id(bytes = 4)
  remote <- remote_save(TEST_ROOT, user, session = session)

  cmp <- data_frame(session = session, label = "")

  remote$save(mtcars, "")
  expect_equal(remote$list()[c("session", "label")], cmp)

  remote$save(mtcars, NA)
  expect_equal(remote$list()[c("session", "label")], cmp)

  remote$save(mtcars, NULL)
  expect_equal(remote$list()[c("session", "label")], cmp)
})


test_that("info", {
  user <- ids::random_id(bytes = 4)
  session <- ids::random_id(bytes = 4)
  remote <- remote_save(TEST_ROOT, user, session = session)
  expect_equal(
    remote$info(),
    list(host = "127.0.0.1", root = TEST_ROOT, user = user, session = session))
})


test_that("delete sessions", {
  user <- ids::random_id(bytes = 4)
  session1 <- ids::random_id(bytes = 4)
  session2 <- ids::random_id(bytes = 4)
  remote1 <- remote_save(TEST_ROOT, user, session = session1)
  remote2 <- remote_save(TEST_ROOT, user, session = session2)
  remote1$save(mtcars, "label1")
  remote2$save(iris, "label2")
  sessions <- c(session1, session2)
  remote1$delete_sessions(sessions)
  expect_equal(nrow(remote1$list()), 0)
})
