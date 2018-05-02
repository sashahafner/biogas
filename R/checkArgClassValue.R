
checkArgClassValue <- function(
  object, 
  expected.class = NULL, 
  expected.values = NULL, 
  expected.range = NULL, 
  case.sens = TRUE,
  warn.only = FALSE
  ) {

  if(any(expected.class == 'character') & !case.sens) {
    object <- tolower(object)
    expected.values <- tolower(expected.values)
  }

  # call. set to FALSE so user does not see reference to unknown checkArgClassValue() function.
  ###if(!is.null(expected.class) && !any(class(object) %in% expected.class))  {
  if(!is.null(expected.class) && !class(object) %in% expected.class)  {
    if(!warn.only) {
      stop('Expect class \"', paste(expected.class, collapse = ', '), '\" for argument ', deparse(substitute(object)), ' but got \"', paste(class(object), collapse = ', '), '\".', call. = FALSE)
    } else {
      warning('Expect class \"', paste(expected.class, collapse = ', '), '\" for argument ', deparse(substitute(object)), ' but got \"', paste(class(object), collapse = ', '), '\".', call. = FALSE)
    }
  } 

  if(!is.null(object) && !is.null(expected.values) && !object  %in% expected.values) {
    if(!warn.only) {
      stop('Expect one of the following values \"', paste(expected.values, collapse = ', '), '\" for argument ', deparse(substitute(object)), ' but got \"', object, '\".', call. = FALSE)
    } else {
      warning('Expect one of the following values \"', paste(expected.values, collapse = ', '), '\" for argument ', deparse(substitute(object)), ' but got \"', object, '\".', call. = FALSE)
    }
  }

  if(!is.null(object) && !is.null(expected.range) && any(na.omit(object)  < min(expected.range) | na.omit(object) > max(expected.range))) {
    if(!warn.only) {
      stop('Expect values within the range \"', paste(expected.range, collapse = ', '), '\" for argument ', deparse(substitute(object)), ' but got \"', paste(range(object), collapse = ', '), '\".', call. = FALSE)
    } else {
      warning('Expect values within the range \"', paste(expected.range, collapse = ', '), '\" for argument ', deparse(substitute(object)), ' but got \"', paste(range(object), collapse = ', '), '\".', call. = FALSE)
    }
  }

  return(invisible(TRUE)) # May be useful
}
