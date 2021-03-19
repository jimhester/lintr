org_travis_repo_slug <- Sys.getenv("TRAVIS_REPO_SLUG")
org_jenkins_url <- Sys.getenv("JENKINS_URL")
org_git_url <- Sys.getenv("GIT_URL")
org_git_url_1 <- Sys.getenv("GIT_URL_1")
org_change_id <- Sys.getenv("CHANGE_ID")
org_git_commit <- Sys.getenv("GIT_COMMIT")

setup({
  Sys.unsetenv(c("JENKINS_URL", "GIT_URL", "GIT_URL_1", "CHANGE_ID", "GIT_COMMIT"))
})

teardown({
  Sys.setenv(
    TRAVIS_REPO_SLUG = org_travis_repo_slug,
    JENKINS_URL = org_jenkins_url,
    GIT_URL = org_git_url,
    GIT_URL_1 = org_git_url_1,
    CHANGE_ID = org_change_id,
    GIT_COMMIT = org_git_commit
  )
})

test_that("it detects CI environments", {
  Sys.setenv(TRAVIS_REPO_SLUG = "foo/bar")
  expect_true(in_ci())
  Sys.setenv(TRAVIS_REPO_SLUG = "")
  expect_false(in_ci())
})

test_that("it returns NULL if GIT_URL is not on github", {
  Sys.setenv(
    JENKINS_URL = "https://jenkins.example.org/",
    GIT_URL = "https://example.com/user/repo.git",
    CHANGE_ID = "123"
  )
  expect_false(in_ci())
  Sys.unsetenv(c("JENKINS_URL", "GIT_URL", "CHANGE_ID"))
})

test_that("it determines Jenkins PR build info", {
  Sys.setenv(
    JENKINS_URL = "https://jenkins.example.org/",
    GIT_URL = "https://github.com/user/repo.git",
    CHANGE_ID = "123"
  )
  expect_true(in_ci())

  expect_equal(ci_build_info(), list(
    user = "user",
    repo = "repo",
    pull = "123",
    commit = NULL
  ))

  Sys.unsetenv(c("JENKINS_URL", "GIT_URL", "CHANGE_ID"))
  expect_false(in_ci())
})

test_that("it determines Jenkins commit build info", {
  Sys.setenv(
    JENKINS_URL = "https://jenkins.example.org/",
    GIT_URL_1 = "https://github.com/user/repo.git",
    GIT_COMMIT = "abcde"
  )

  expect_true(in_ci())
  expect_equal(ci_build_info(), list(
    user = "user",
    repo = "repo",
    pull = NULL,
    commit = "abcde"
  ))

  Sys.unsetenv(c("JENKINS_URL", "GIT_URL_1", "GIT_COMMIT"))
})
