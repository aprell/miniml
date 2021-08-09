open Core

let repl ?(prompt = "miniml> ") () =
  let rec loop () =
    print_string prompt;
    try
      read_line () |> interpret |> Value.print;
      loop ()
    with
    | End_of_file -> print_endline "Bye"
    | _ -> loop ()
  in
  loop ()
