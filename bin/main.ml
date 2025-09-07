type aoc_days =
  { name : string
  ; age : int
  }
[@@deriving aocmatch]

let () =
  let ad = { name = "tom"; age = 10 } in
  Printf.printf "%s\n" (name ad)
;;
