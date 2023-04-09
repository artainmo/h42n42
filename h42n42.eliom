[%%shared
open Eliom_lib
open Eliom_content
open Html.D
open Js_of_ocaml
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

type creet_type = {
	mutable color : int * int * int;
	mutable position_x : int;
  mutable position_y : int;
	mutable radius : int;
}

(* Draws a line between two given points in a canvas *)
let%client draw ctx ((r, g, b), size, (x1, y1), (x2, y2)) =
  let color = CSS.Color.string_of_t (CSS.Color.rgb r g b) in
  ctx##.strokeStyle := (Js.string color);
  ctx##.lineWidth := float size;
  ctx##beginPath;
  ctx##(moveTo (float x1) (float y1));
  ctx##(lineTo (float x2) (float y2));
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

let%client creet_move direction x y radius =
  match direction with
  | 0 -> if x - radius = 0 then (x + 1, y) else (x - 1, y)
  | 1 -> if x + radius = width then (x - 1, y) else (x + 1, y)
  | 2 -> if y + radius = height then (x, y - 1) else (x, y + 1)
  | 3 -> if y - radius = 0 then (x, y + 1) else (x, y - 1)
  | _ -> failwith "Invalid value in creet_move_y"

let%client rec creet ctx ((r, g, b), (x, y), radius) steps direction =
  ctx##clearRect 0. 0. (float_of_int width) (float_of_int height);
  init_map ctx;
  draw_creet ctx ((r,g,b), (x, y), radius);
  Js_of_ocaml_lwt__.Lwt_js.sleep 0.01 >>= fun () -> creet ctx
        ((r,g,b), (creet_move direction x y radius), radius)
        (if steps = 0 then 100 else steps - 1)
        (if steps = 0 then (Random.self_init (); Random.int 4) else direction)

let%client rec update_frontend ctx =
  ignore (creet ctx ((138,43,226), (width/2, height/2), height/30) 100 (Random.self_init (); Random.int 4))
  (* Js_of_ocaml_lwt__.Lwt_js.sleep 10. >>= fun () -> update_frontend ctx *)
  (* >>= symbol is necessary to wait for the promise to resolve, it is like 'await' in javascript *)

let canvas_display =
  canvas ~a:[a_width width; a_height height]
    [txt "your browser doesn't support canvas"]

let%client init_client () =
  let canvas = Eliom_content.Html.To_dom.of_canvas ~%canvas_display in
  let ctx = canvas##(getContext (Dom_html._2d_)) in
  ctx##.lineCap := Js.string "rectangular";
  init_map ctx;
  ignore (update_frontend ctx)

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
