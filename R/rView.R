#' Simplify viewing of R objects on remote machine via RStudio, exploiting rmote server
#'
#' This function is designed to be used on both the remote RStudio session and
#' also on the local RStudio session.  It does different things in each session.
#'
#' @param obj The R object to view.
#' @param port The number of the port to access on the local machine to download objects.
#' Note that you can give the command \code{rView(port = XXXX)} without any reference
#' to \code{obj}, because \code{obj} is not used within the function before exiting it
#' when simply updating the port.
#'
#' @details
#' On the remote session, which is identified by having \code{getOption("rmote_server_dir")} being
#' nonNULL, \code{rView(x)} saves the object x to an rds file called x.rds in a directory
#' called \code{rview_objs} in the \code{rmote_server_dir}.  If it is called by piping an
#' output into \code{rView()},
#' then the object gets written to \code{__dot.rds}.
#'
#' On the local session, calling \code{rView(x)} downloads \code{x.rds} from the
#' forwarded port that is hosting files from the remote machine at \code{rmote_server_dir},
#' then it sends it to the \code{View()} command, to view in an RStudio data browser
#' window, that it names "r:x".
#' Piping output into \code{rView} or calling \code{rView(.)} downloads the object in
#' \code{__dot.rds} and views it in an RStudio data browser window (named "r:.").
#'
#' In the local session, if \code{getOption("rview_port")} is not defined (as it likely
#' will not be in a new session) then you can set it to what it should be using the
#' port argument.  For example, if the remote machine is forwarding rmote traffic
#' to port 4321, you would use \code{rView(port = 4321)}.
#'
#' See the vignette for information about setting up port forwarding, and how to use
#' this function in a high-performance computer cluster environment.
#' @export
rView <- function(obj, port = NULL) {

  # if indicated, just pass the argument through as the return value
  rpt <- getOption("rview_pass_through")
  if (!is.null(rpt) && rpt == TRUE) {
    pass_through <- TRUE
  } else {
    pass_through <- FALSE
  }
  if (pass_through == TRUE) {
    return(obj)
  }


  # if port is not null, then it simply writes that to the options(rview_port = XXXX)
  # and then exits
  if (!is.null(port)) {
    options(rview_port = port)
    message("rView port (for local machine) set to ", port)
    return(invisible())
  }
  # if port is NULL, then, if getOption("rmote_port") is not NULL
  # it knows it must be on the remote machine and that is fine.  There is nothing else to be done.
  # If getOption("rmote_port") is NULL, it assumes we are on the local machine and looks for an
  # rview_port option.  If it doesn't find it, it throws an error with an informative message.
  rmport <- getOption("rmote_server_port")
  if (!is.null(rmport)) {
    ;
  } else {
    rview_port = getOption("rview_port")
    if (is.null(rview_port)) {
      stop("rview_port not given:\nIf on remote machine, be sure to start rmote server with:\n\trmote::start_rmote(server_dir = \"your  desired path\", port = XXXX)\nIf on local laptop set rview port to XXXX (whatever port is being used by rmote on the remote server) with:\n\trView(port = XXXX)\nIf rmote server is started on your remote server, find its port with getOption(\"rmote_server_port\") in your remote R session.")
    }
  }


  # get the name of the object sent in
  obj_name <- deparse(substitute(obj))
  if (obj_name == ".") {
    obj_name <- "__dot"
  }

  # now, determine if we are running on the remote or the local session
  server_dir <- getOption("rmote_server_dir")
  if (is.null(server_dir)) {
    onRemote <- FALSE
    # in this case we are on the local R session
    prefix <- paste0("http://localhost:", rview_port)
  } else {
    onRemote <- TRUE
    if (!file.exists(file.path(server_dir, "rview_objs"))) {
      dir.create(file.path(server_dir, "rview_objs"), recursive = TRUE, showWarnings = FALSE)
    }
    prefix <- server_dir
  }

  # get the path of this object inside the server_dir/prefix
  obj_path <- file.path(prefix, "rview_objs", paste0(obj_name, ".rds"))

  if (onRemote == TRUE) {
    message("Saving on remote to: ", obj_path)
    saveRDS(object = obj, file = obj_path)
  } else {
    message("Downloading from forwarded port: ", obj_path)
    file <- tempfile()
    download.file(obj_path, destfile = file)
    if (obj_name == "__dot") {
      obj_name <- "."
    }
    show_name <- paste0("r:", obj_name)
    obj_down <- readRDS(file)
    View(obj_down, title = show_name)
  }
}

