type aoc_record =
  { day1 : (unit -> unit) * (unit -> unit)
  ; day2 : (unit -> unit) * (unit -> unit)
  }
[@@deriving aocmatch]

let func number = fun () -> print_endline ("hello" ^ string_of_int number)
let aoc_record = { day1 = func 11, func 12; day2 = func 21, func 22 }

let () =
  let day = 1 in
  let task = None in
  run_aoc aoc_record day task
;;
