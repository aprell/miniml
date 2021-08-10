open Core

let repl ?(prompt = "miniml> ") () =
  let rec loop () =
    print_string prompt;
    try
      let v, t = interpret (read_line ()) in
      Printf.printf "- : %s = %s\n"
        (Type.string_of_type t)
        (Value.string_of_value v);
      loop ()
    with
    | End_of_file -> print_endline "Bye"
    | _ -> loop ()
  in
  loop ()
