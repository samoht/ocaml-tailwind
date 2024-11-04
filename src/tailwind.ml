type key = Key of string * string option
type value = Value of string

type entry =
  | Entry of key list * value
  | Media of string * entry list
  | Font_face of string

let key str tail =
  String.fold_right
    (fun c acc -> match c with '\\' -> acc | c -> c :: acc)
    str []
  |> List.to_seq
  |> String.of_seq
  |> fun str -> Key (str, tail)

let parse input =
  let n = String.length input in
  let rec read_comment i k =
    if i + 1 >= n then k i
    else
      match (input.[i], input.[i + 1]) with
      | '/', '*' -> skip_comment (i + 2) k
      | _ -> k i
  and skip_comment i k =
    if i + 1 >= n then failwith "skip_comment: invalid CSS";
    match (input.[i], input.[i + 1]) with
    | '*', '/' -> k (i + 2)
    | _ -> skip_comment (i + 1) k
  and read_entries i k =
    if i >= n then k i []
    else
      match input.[i] with
      | '}' -> k (i + 1) []
      | _ ->
          read_entry i @@ fun i entry ->
          read_entries i @@ fun i entries -> k i (entry :: entries)
  and read_entry i k =
    match input.[i] with
    | '@' -> (
        if i + 1 >= n then failwith "read_entry: malformed CSSS";
        match input.[i + 1] with
        | 'm' -> read_media i i k
        | 'f' -> read_font_face i i k
        | _ -> failwith "read_entry: invalid @ statement")
    | _ ->
        read_keys i i @@ fun i keys ->
        read_value i i @@ fun i value -> k i (Entry (keys, value))
  and read_keys i j k =
    if j >= n then failwith "read_keys: malformed CSS";
    match input.[j] with
    | ' ' | '>' ->
        let head = String.sub input i (j - i) in
        read_key_tail j j @@ fun j tail ->
        let key = key head tail in
        read_keys j j @@ fun j keys -> k j (key :: keys)
    | ',' ->
        let head = String.sub input i (j - i) in
        let key = key head None in
        read_keys (j + 1) (j + 1) @@ fun j keys -> k j (key :: keys)
    | '(' -> read_close_par (j + 1) @@ fun j -> read_keys i j k
    | '[' -> read_close_bracket (j + 1) @@ fun j -> read_keys i j k
    | '{' ->
        if i = j then k (j + 1) []
        else
          let head = String.sub input i (j - i) in
          let key = key head None in
          k (j + 1) [ key ]
    | _ -> read_keys i (j + 1) k
  and read_key_tail i j k =
    if j >= n then failwith "read_key_tail: malformed CSS";
    match input.[j] with
    | ',' ->
        let tail = String.sub input i (j - i) in
        k (j + 1) (Some tail)
    | '{' ->
        let tail = String.sub input i (j - i) in
        k j (Some tail)
    | '(' -> read_close_par (j + 1) @@ fun j -> read_key_tail i j k
    | '[' -> read_close_bracket (j + 1) @@ fun j -> read_key_tail i j k
    | _ -> read_key_tail i (j + 1) k
  and read_close_par j k =
    if j >= n then failwith "read_close_par: malformed CSS";
    match input.[j] with ')' -> k (j + 1) | _ -> read_close_par (j + 1) k
  and read_close_bracket j k =
    if j >= n then failwith "read_close_bracket: malformed CSS";
    match input.[j] with ']' -> k (j + 1) | _ -> read_close_bracket (j + 1) k
  and read_close_curly j k =
    if j >= n then failwith "read_close_curly: malformed CSS";
    match input.[j] with '}' -> k (j + 1) | _ -> read_close_curly (j + 1) k
  and read_media i j k =
    if j >= n then failwith "read_media: malformed CSS";
    match input.[j] with
    | '{' ->
        let media = String.sub input i (j - i) in
        read_entries (j + 1) @@ fun j entries -> k j (Media (media, entries))
    | _ -> read_media i (j + 1) k
  and read_font_face i j k =
    if j >= n then failwith "read_font_face: malformed CSS";
    match input.[j] with
    | '{' ->
        let font_face = String.sub input i (j - i) in
        assert (font_face = "@font-face");
        read_close_curly j @@ fun r ->
        let contents = String.sub input j (r - j) in
        k r (Font_face contents)
    | _ -> read_font_face i (j + 1) k
  and read_value i j k =
    if j >= n then failwith "read_value: malformed CSS";
    match input.[j] with
    | '}' ->
        let value = String.sub input i (j - i) in
        k (j + 1) (Value value)
    | _ -> read_value i (j + 1) k
  in
  read_comment 0 @@ fun i ->
  read_entries i @@ fun _ entries -> entries

let rec print_entries prefix entries =
  List.iter
    (function
      | Entry (ks, _) ->
          List.iter (function Key (k, _) -> print_endline (prefix ^ k)) ks
      | Media (m, entries) ->
          print_endline m;
          print_entries "  " entries
      | Font_face _ -> ())
    entries

let () =
  let ic = open_in Sys.argv.(1) in
  let input = really_input_string ic (in_channel_length ic) in
  let entries = parse input in
  print_entries "" entries
