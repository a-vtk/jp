-type task_name() :: binary().
-type task_command() :: binary().

-record(task, {
    name :: task_name(),
    command :: task_command(),
    deps = [] :: [task_name()]
}).
-type task() :: #task{}.
