[%%shared
open Eliom_lib
open Eliom_content
open Html.D
open Js_of_ocaml
open Js
open Lwt
]

module H42n42_app =
  Eliom_registration.App (
  struct
    let application_name = "h42n42"
    let global_data_path = None
  end)

let%shared width = 1050
let%shared height = 800
let%shared refresh_rate = 0.01
let%shared direction_length = 200

let%shared cut_array a i =
  Array.append (Array.sub a 0 i) (Array.sub a (i + 1) (Array.length a - (i + 1)))

let%client standard_creet_tuple =
  (((138,43,226), (width/2, height/2), height/30), direction_length, (Random.self_init (); Random.int 4), (-1, 0))

(* Draws a line between two given points in a canvas *)
let%client draw ctx ((r, g, b), size, (x1, y1), (x2, y2)) =
  let color = CSS.Color.string_of_t (CSS.Color.rgb r g b) in
  ctx##.strokeStyle := (Js.string color);
  ctx##.lineWidth := float_of_int size;
  ctx##beginPath;
  ctx##(moveTo (float_of_int x1) (float_of_int y1));
  ctx##(lineTo (float_of_int x2) (float_of_int y2));
  ctx##stroke

let%client draw_creet ctx ((r, g, b), (x, y), radius) =
  draw ctx ((r, g, b), 3, (x - radius, y + radius), (x + radius, y + radius));
  draw ctx ((r, g, b), 3, (x - radius, y - radius), (x + radius, y - radius));
  draw ctx ((r, g, b), 3, (x - radius, y + radius), (x - radius, y - radius));
  draw ctx ((r, g, b), 3, (x + radius, y + radius), (x + radius, y - radius))

(* Draw the static map (river, land, hospital) *)
let%client init_map ctx =
  draw ctx ((0, 154, 23), 8, (0, 0), (0, height));
  draw ctx ((0, 154, 23), 8, (width, 0), (width, height));
  draw ctx ((0, 154, 23), 8, (0, height), (width, height));
  draw ctx ((134, 180, 188), 8, (0, 0), (width, 0));
  draw ctx ((134, 180, 188), 4, (0, height/10), (width, height/10));
  draw ctx ((134, 180, 188), 8, (0, 0), (0, height/10));
  draw ctx ((134, 180, 188), 8, (width, 0), (width, height/10));
  draw ctx ((232, 0, 0), 4, (0, height - height/10), (width, height - height/10));
  draw ctx ((232, 0, 0), 8, (0, height - height/10), (0, height));
  draw ctx ((232, 0, 0), 8, (width, height - height/10), (width, height));
  draw ctx ((232, 0, 0), 8, (0, height), (width, height))

let%client creet_radius infection radius =
  match (snd infection) with
  | x when x < 7 -> radius
  | 7 -> radius + (if (fst infection) mod 10 = 0 then 1 else 0)
  | _ -> failwith "Invalid value in creet_radius"

let%client creet_color ((r, g, b), y, radius) =
  if y - radius < height/10 then (217,230,80) else (r, g, b)

let%client creet_infected infection y radius =
  if (fst infection) = -1 && y - radius < height/10 then (0, (Random.self_init (); Random.int 8))
  else if (fst infection) != -1 then ((fst infection) + 1, (snd infection))
  else (-1, (snd infection))

let%client rec wall_rebound direction x y radius steps =
  if steps = 0 then wall_rebound (Random.self_init (); Random.int 4) x y radius 1
  else if x-radius < 0 then 1
  else if x+radius > width then 0
  else if y-radius < 0 then 2
  else if y+radius > height then 3
  else if direction = 0 && x-radius <= 0 then wall_rebound (Random.self_init (); Random.int 4) x y radius steps
  else if direction = 1 && x+radius >= width then wall_rebound (Random.self_init (); Random.int 4) x y radius steps
  else if direction = 2 && y+radius >= height then wall_rebound (Random.self_init (); Random.int 4) x y radius steps
  else if direction = 3 && y-radius <= 0 then wall_rebound (Random.self_init (); Random.int 4) x y radius steps
  else direction

let%client creet_move direction x y radius =
  match direction with
  | 0 -> if x-radius <= 0 then (x + 1, y) else (x - 1, y)
  | 1 -> if x+radius >= width then (x - 1, y) else (x + 1, y)
  | 2 -> if y+radius >= height then (x, y - 1) else (x, y + 1)
  | 3 -> if y-radius <= 0 then (x, y + 1) else (x, y - 1)
  | _ -> failwith "Invalid value in creet_move"

let%client creet ctx (((r, g, b), (x, y), radius), steps, direction, infection) =
  draw_creet ctx ((r,g,b), (x, y), radius);
  let new_radius = (creet_radius infection radius) in
  (((creet_color ((r,g,b), y, new_radius)),
        (creet_move direction x y new_radius),
        new_radius),
        (if steps = 0 then direction_length else steps - 1),
        (wall_rebound direction x y new_radius steps),
        (creet_infected infection y new_radius))

let%client is_dead (((r, g, b), (x, y), radius), steps, direction, infection) creets_array i =
  if (fst infection) >= int_of_float (1.0 /. refresh_rate *. 4.5) then (* Keep infected creet alive for 7 seconds *)
    (cut_array creets_array i, i)
  else
    (creets_array, i + 1)

let%client rec loop_creets ctx creets_array i =
  creets_array.(i) <- creet ctx creets_array.(i);
  let new_array, new_i = is_dead creets_array.(i) creets_array i in
  if new_i < (Array.length new_array) then loop_creets ctx new_array new_i else new_array

let%client spawn_creet creets_array i =
  if i >= int_of_float (1.0 /. refresh_rate *. 7.) then (* Keep infected creet alive for 10 seconds *)
    (Array.append [| standard_creet_tuple |] creets_array, 0)
  else
    (creets_array, i + 1)

let%client rec update_frontend ctx creets_array i =
  ctx##clearRect 0. 0. (float_of_int width) (float_of_int height);
  init_map ctx;
  let new_creet_array, new_i = spawn_creet creets_array i in
  let new_creets_array = loop_creets ctx new_creet_array 0 in
  Js_of_ocaml_lwt__.Lwt_js.sleep refresh_rate >>= fun () -> update_frontend ctx new_creets_array new_i (* >>= symbol is necessary to wait for the promise to resolve, it is like 'await' in javascript *)

let canvas_display =
  canvas ~a:[a_width width; a_height height]
    [txt "your browser doesn't support canvas"]

let%client init_client () =
  let canvas = Eliom_content.Html.To_dom.of_canvas ~%canvas_display in
  let ctx = canvas##(getContext (Dom_html._2d_)) in
  ctx##.lineCap := Js.string "rectangular";
  init_map ctx;
  let creets_array = [| standard_creet_tuple |] in
  ignore(update_frontend ctx creets_array 0)

let page () =
  (html
    (head (title (txt "h42n42")) [])
    (body [h1 [txt "h42n42"];
          canvas_display]))

let main_service =
  Eliom_service.create
    ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

let () =
  H42n42_app.register
    ~service:main_service
    (fun () () ->
      let _ = [%client (init_client () : unit) ] in
      Lwt.return (page ()))
