ref_class_object <- setRefClass("ref_class_object",
  fields = list(
    env = "environment",
    lst = "list"
  ),

  methods = list(
    set_env = function(x, val) { env[[x]] <<- val },
    get_env = function(x) { env[[x]] },
    set_lst = function(x, val) { lst[[x]] <<- val },
    get_lst = function(x) { lst[[x]] }
  )
)
