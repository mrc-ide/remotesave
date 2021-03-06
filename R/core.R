##' Remotely save state
##' @title Remotely save state
##'
##' @param root The root within the redis server to save state at
##'
##' @param user The username (will be appended to the root)
##'
##' @param url The url for the redis server - optional, and using the
##'   \code{REDIS_URL} environment variable may be preferable
##'
##' @param session An optional session id as a string.  If missing
##'   then a random string will be used.
##'
##' @export
remote_save <- function(root, user, url = NULL, session = NULL) {
  R6_remote_save$new(root, user, url, session)
}


R6_remote_save <- R6::R6Class(
  "remote_save",
  cloneable = FALSE,

  private = list(
    con = NULL,
    root = NULL,
    user = NULL,
    session = NULL,
    keys = NULL
  ),

  public = list(
    initialize = function(root, user, url, session) {
      con <- if (is.null(url)) redux::hiredis() else redux::hiredis(url = url)
      private$con <- con
      private$root <- root
      private$user <- user
      self$new_session(session)
    },

    info = function() {
      list(host = private$con$config()$host,
           root = private$root,
           user = private$user,
           session = private$session)
    },

    ## TODO: support 'parent' here
    new_session = function(id = NULL) {
      private$session <- id %||% ids::random_id()
      private$keys <-
        remote_save_keys(private$root, private$user, private$session)
    },

    save = function(value, label = NULL) {
      time <- format(Sys.time(), "%Y-%m-%d %H:%M:%OS6")
      data <- serialize(value, NULL, xdr = FALSE, version = 3)
      value <- list(time = time, label = safe_label(label), value = data)
      invisible(private$con$HMSET(private$keys$latest, names(value), value))
    },

    list = function(oldest_first = FALSE, include_current = FALSE) {
      found <- redux::scan_find(private$con, private$keys$pattern)
      time <- vcapply(found, function(x) private$con$HGET(x, "time"))
      label <- vcapply(found, function(x)
        private$con$HGET(x, "label") %||% NA_character_)
      session <- sub(private$keys$re, "\\1", found)
      current <- session == private$session
      i <- order(vnapply(time, function(x) as.numeric(as.POSIXct(x))),
                 decreasing = TRUE)
      if (oldest_first) {
        i <- rev(i)
      }
      if (!include_current) {
        i <- i[!current[i]]
      }
      ret <- data_frame(key = found[i],
                        session = session[i],
                        current = current[i],
                        label = label[i],
                        time = time[i])
      rownames(ret) <- NULL
      ret
    },

    fetch = function(session) {
      key <- remote_save_keys(private$root, private$user, session)$latest
      unserialize(private$con$HGET(key, "value"))
    },

    delete = function() {
      invisible(private$con$DEL(private$keys$latest) > 0)
    },

    delete_user = function() {
      invisible(redux::scan_del(private$con, private$keys$pattern))
    },

    delete_sessions = function(sessions) {
      if (length(sessions) > 0L) {
        keys <- sprintf(private$keys$fmt, sessions)
        invisible(private$con$DEL(keys))
      }
    },

    last = function() {
      private$con$HGET(private$keys$latest, "time") %||% NA_character_
    }
  )
)


remote_save_keys <- function(root, user, session) {
  list(latest = sprintf("%s:%s:%s:latest", root, user, session),
       pattern = sprintf("%s:%s:%s:latest", root, user, "*"),
       fmt = sprintf("%s:%s:%s:latest", root, user, "%s"),
       re = sprintf("^%s:%s:%s:latest$", root, user, "([^:]+)"))
}


safe_label <- function(label) {
  if (is.null(label) || is.na(label)) {
    label <- ""
  }
  label
}
