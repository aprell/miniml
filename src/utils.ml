let read_file name =
  let file = open_in name in
  let len = in_channel_length file in
  let inp = really_input_string file len in
  close_in file;
  inp

let printf ?(indent = "") =
  print_string indent;
  Printf.printf
