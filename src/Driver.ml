let _ =
  match Syntax.parse Sys.argv.(1) with
  | `Ok   _ -> Printf.printf "Parsed\n"
  | `Fail s -> Printf.printf "Failed: %s\n" s
