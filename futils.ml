let with_i in_chan f = 
    try
        let ans = f in_chan in
        close_in in_chan; Some ans
    with exc ->
        close_in in_chan; None;;


let with_o out_chan f =
    try
        let ans = f out_chan in
        close_out out_chan; Some ans
    with exc -> 
        close_out out_chan; None;;


let in_int in_chan =
    Scanf.fscanf in_chan " %d" (fun x -> x);;


let in_float in_chan =
    Scanf.fscanf in_chan " %f" (fun x -> x);;


let in_string in_chan =
    Scanf.fscanf in_chan " %s" (fun x -> x);;


let in_array unit_f in_chan n =
    Array.init n (fun x -> unit_f in_chan);;


let into_array unit_f in_chan a =
    Array.iteri (fun pos v -> a.(pos) <- unit_f in_chan) a;;


let in_array_int = in_array in_int;;
let in_array_float = in_array in_float;;
let in_array_string = in_array in_string;;

let into_array_int = into_array in_int;;
let into_array_float = into_array in_float;;
let into_array_string = into_array in_string;;


let new_line out_chan =
    Printf.fprintf out_chan "\n";;

let out_int out_chan x = Printf.fprintf out_chan "%d " x;; 

let out_float out_chan x =
    Printf.fprintf out_chan "%f " x;;


let out_string out_chan x =
    Printf.fprintf out_chan "%s " x;;

let out_array unit_f out_chan a =
    Array.iter (unit_f out_chan) a;;


let out_array_int = out_array out_int;;
let out_array_float = out_array out_float;;
let out_array_string = out_array out_string;;

let drop x = ();;

let test () =
    let x = Array.init 10 (fun i -> i) in
    drop (with_o (open_out "t_o") (fun oc -> out_array_int oc x));
    let ans = with_i (open_in "t_o") (fun ic -> in_array_int ic 10) in
    out_array_int stdout ((fun x -> match x with Some c -> c | None -> Array.make 0 0) ans);;


let () = print_int 3;;