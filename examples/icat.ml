let icat filename =
  let img = Result.get_ok (Stb_image.load ~channels:4 filename) in
  let img_s = Kittyimg.string_of_bytes_ba img.Stb_image.data in
  print_newline ();
  Kittyimg.send_image
    ~w:img.Stb_image.width ~h:img.Stb_image.height
    ~format:`RGBA ~mode:(`Display (Kittyimg.display_opts ()))
    img_s;
  print_newline ()

let () =
  match Array.to_list Sys.argv |> List.tl with
  | [ filename ] -> icat filename
  | _ ->
    Printf.eprintf "usage: %s <imagefile>\n" Sys.argv.(0); exit 1
