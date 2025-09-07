# PPX Deriver for AOC

## Goal

This deriver will generate a function that pattern matches against command line input arguments.
It should generate most of the entry point of the AOC code from my repository. There is a lot of
boilerplate code in the entry point.

```ocaml
let command =
  Command.basic
    ~summary:"Run a task for a specific day"
    (let%map_open.Command day = flag "-day" (required int) ~doc:"INT Day number"
     and task =
       flag
         "-task"
         (optional int)
         ~doc:"INT Task number (optional, runs both if omitted)"
     in
     fun () ->
       (match task with
        | Some t ->
          Printf.printf
            "=== Welcome to AOC ===\nThis is task %d of day %d:\n"
            t
            day
        | None ->
          Printf.printf
            "=== Welcome to AOC ===\nRunning all tasks for day %d:\n"
            day);
       match day, task with
       (* Day 1 *)
       | 1, Some 1 -> Advent.print_res (Day1.task_one ())
       | 1, Some 2 -> Advent.print_res (Day1.task_two ())
       | 1, None ->
         Printf.printf "\n--- Day 1, Task 1 ---\n";
         Advent.print_res (Day1.task_one ());
         Printf.printf "\n--- Day 1, Task 2 ---\n";
         Advent.print_res (Day1.task_two ())
       (* Day 2 *)
       | 2, Some 1 -> Advent.print_res Day2.task_one
       | 2, Some 2 -> Advent.print_res Day2.task_two
       | 2, None ->
         Printf.printf "\n--- Day 2, Task 1 ---\n";
         Advent.print_res Day2.task_one;
         Printf.printf "\n--- Day 2, Task 2 ---\n";
         Advent.print_res Day2.task_two
```

The command is being created this will be the same for every advent of code, I don't want to copy the
code. Then we match against the day and the task if no task is given both tasks for that day should
run. There is always the same print and always the same structure. I want to only provide the tasks
and never the whole repeating matching logic.

```ocaml
(* structure for aoc setup *)
type aoc_func = unit -> int
type aoc = {
  summary: string;
  days: {
    day1: aoc_func * aoc_func;
    day2: aoc_func * aoc_func;
    day3: aoc_func * aoc_func;
  };
}
[@@ deriving aoccli]
```
