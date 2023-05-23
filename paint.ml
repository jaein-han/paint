(** The main paint application *)

;; open Gctx
;; open Widget

(******************************************)
(**    SHAPES, MODES, and PROGRAM STATE   *)
(******************************************)

(** A location in the paint_canvas widget *)
type point = position  (* from Gctx *)

(** The shapes that are visible in the paint canvas -- these make up the
    picture that the user has drawn, as well as any other "visible" elements
    that must show up in the canvas area (e.g. a "selection rectangle"). At
    the start of the homework, the only available shape is a line.  *)
(* TODO: You will modify this definition in Tasks 3, 4, 5 and maybe 6. *)
type shape = 
  | Line of {color: color; p1: point; p2: point; thickness: thickness}
  | Points of {color: Gctx.color; points: point list}
  | Ellipse of {color: Gctx.color; center: position;
      rx: int; ry: int; thickness: thickness}

(** These are the possible interaction modes that the paint program might be
    in. Some interactions require two modes. For example, the GUI might
    recognize the first mouse click as starting a line and a second mouse
    click as finishing the line.

    To start out, there are only two modes:

      - LineStartMode means the paint program is waiting for the user to make
        the first click to start a line.

      - LineEndMode means that the paint program is waiting for the user's
        second click. The point associated with this mode stores the location
        of the user's first mouse click.  *)
(* TODO: You will need to modify this type in Tasks 3 and 4, and maybe 6. *)
type mode = 
  | LineStartMode
  | LineEndMode of point
  | PointMode
  | EllipseStartMode
  | EllipseEndMode of position

(** The state of the paint program. *)
type state = {
  (** The sequence of all shapes drawn by the user, in order from
      least recent (the head) to most recent (the tail). *)
  shapes : shape Deque.deque;

  (** The input mode the Paint program is in. *)
  mutable mode : mode;

  (** The currently selected pen color. *)
  mutable color : color;

  (* TODO: You will need to add new state for Tasks 2, 5, and *)
  (* possibly 6 *) 
  mutable preview: shape option;
  mutable thickness: thickness
}

(** Initial values of the program state. *)
let paint : state = {
  shapes = Deque.create ();
  mode = LineStartMode;
  color = black;
  (* TODO: You will need to add new state for Tasks 2, 5, and maybe 6 *)
  preview = None;
  thickness = default
}


(** This function creates a graphics context with the appropriate
    pen color. *)
(* TODO: Your will need to modify this function in Task 5 *)
let with_params (g: gctx) (c: color) (t: thickness) : gctx =
  let g = with_color g c in
  let g1 = with_thickness g t in
  g1

(*********************************)
(**    MAIN CANVAS REPAINTING    *)
(*********************************)

(** The paint_canvas repaint function.

    This function iterates through all the drawn shapes (in order of least
    recent to most recent so that they are layered on top of one another
    correctly) and uses the Gctx.draw_xyz functions to display them on the
    canvas.  *)

(* TODO: You will need to modify this repaint function in Tasks 2, 3,
   4, and possibly 5 or 6. For example, if the user is performing some
   operation that provides "preview" (see Task 2) the repaint function
   must also show the preview. *)
let repaint (g: gctx) : unit =
  let draw_shape (s: shape) : unit =
    begin match s with
      | Line l -> draw_line (with_params g l.color l.thickness) l.p1 l.p2
      | Points ps -> Gctx.draw_points (with_params g ps.color default) ps.points
      | Ellipse el -> 
          draw_ellipse (with_params g el.color el.thickness) 
            el.center el.rx el.ry
    end in
  begin match paint.preview with
  | None -> ()
  | Some l ->
    draw_shape l
  end;
  Deque.iterate draw_shape paint.shapes

(** Create the actual paint_canvas widget and its associated
    notifier_controller . *)
let ((paint_canvas : widget), (paint_canvas_controller : notifier_controller)) =
  canvas (600, 350) repaint


(************************************)
(**  PAINT CANVAS EVENT HANDLER     *)
(************************************)

(** The paint_action function processes all events that occur
    in the canvas region. *)
(* TODO: Tasks 2, 3, 4, 5, and 6 involve changes to paint_action. *)
let paint_action (gc:gctx) (event:event) : unit =
  let p  = event_pos event gc in  (* mouse position *)
  let (x', y') = p in
  begin match (event_type event) with
    | MouseDown ->
      begin match paint.mode with
      | LineStartMode -> paint.mode <- LineEndMode p
      | PointMode -> 
          paint.preview <- Some (Points {color=paint.color; points =[p]})
      | EllipseStartMode ->
          paint.mode <- EllipseEndMode p
      | _ -> ()
      end
    | MouseDrag ->
      (* In this case, the mouse has been clicked, and it's being dragged
         with the button down. Initially there is nothing to do, but you'll
         need to update this part for Task 2, 3, 4 and maybe 6. *)
      begin match paint.mode with
      | LineEndMode old_p ->
        paint.preview <- Some 
          (Line {color=paint.color; p1=old_p; p2=p; thickness=paint.thickness})
      | PointMode ->
        let points_list =
          begin match paint.preview with
          | Some (Points ps) -> ps.points
          | _ -> []
          end in
        paint.preview <- 
          Some (Points {color=paint.color; points = p::(points_list)})
      | EllipseEndMode old_p ->
        let (x, y) = old_p in
        let el_rx = abs ((x'-x)/2) in
        let el_ry = abs ((y'-y)/2) in
          paint.preview <-
            Some (Ellipse
              {color=paint.color; center=((min x x')+el_rx, (min y y')+el_ry);
                rx=el_rx; ry=el_ry; thickness=paint.thickness})
      | _ -> ()
      end
    | MouseUp ->
      (* In this case there was a mouse button release event. TODO: Tasks 2, 
         3, 4, and possibly 6 need to do something different here. *)
      begin match paint.mode with
      | LineEndMode old_p ->
        Deque.insert_tail
          (Line {color=paint.color; p1=old_p; p2=p; 
            thickness=paint.thickness}) paint.shapes;
        paint.mode <- LineStartMode;
        paint.preview <- None
      | PointMode ->
        let points_list =
          begin match paint.preview with
          | Some (Points ps) -> ps.points
          | _ -> []
          end in
        Deque.insert_tail 
          (Points {color=paint.color; points=points_list}) paint.shapes;
        paint.preview <- None
      | EllipseEndMode old_p ->
        let (x, y) = old_p in
        let el_rx = abs ((x'-x)/2) in
        let el_ry = abs ((y'-y)/2) in
        Deque.insert_tail
          (Ellipse 
            {color=paint.color; center=((min x x')+el_rx, (min y y')+el_ry);
              rx=el_rx; ry=el_ry; thickness=paint.thickness})
            paint.shapes;
        paint.mode <- EllipseStartMode;
        paint.preview <- None
      | _ -> ()
      end
    | _ -> ()
    (* This catches the MouseMove event (where the user moved the mouse over
       the canvas without pushing any buttons) and the KeyPress event (where
       the user typed a key when the mouse was over the canvas). *)
  end

(** Add the paint_action function as a listener to the paint_canvas *)
;; paint_canvas_controller.add_event_listener paint_action

(**************************************)
(** TOOLBARS AND PAINT PROGRAM LAYOUT *)
(**************************************)

(** This part of the program creates the other widgets for the paint
    program -- the buttons, color selectors, etc., and lays them out
    in the top - level window. *)
(* TODO: Tasks 1, 4, 5, and 6 involve adding new buttons or changing
   the layout of the Paint GUI. Initially the layout is ugly because
   we use only the hpair widget demonstrated in Lecture. Task 1 is to
   make improvements to make the layout more appealing. You may choose
   to arrange the buttons and other GUI elements of the paint program
   however you like (so long as it is easily apparent how to use the
   interface).  The sample screenshot of our solution shows one
   possible design.  Also, feel free to improve the visual components
   of the GUI; for example, our solution puts borders around the
   buttons and uses a custom "color button" that changes its
   appearance based on whether or not the color is currently
   selected. *)

(** Create the Undo button *)
let (w_undo, lc_undo, nc_undo) = button "Undo"

(** Other buttons *)
let (w_ellipse, lc_ellipse, nc_ellipse) = button "Ellipse"
let (w_point, lc_point, nc_point) = button "Point"
let (w_line, lc_line, nc_line) = button "Line"

(** This function runs when the Undo button is clicked.
    It simply removes the last shape from the shapes deque. *)
(* TODO: You need to modify this in Task 3 and 4, and potentially 2
   (depending on your implementation). *)

(** Checkbox functions *)
let (w_checkbox, vc_checkbox) = checkbox false "Thick Lines"

;; vc_checkbox.add_change_listener (fun vc_checkbox -> 
    if vc_checkbox then paint.thickness <- thick
      else paint.thickness <- default)

(** Slider functions *)
let (w_slider, vc_slider) = slider 1

;; vc_slider.add_change_listener (fun vc_slider ->
  if vc_checkbox.get_value () then
  paint.thickness <- (vc_slider/5)
  else ())

(** Ellipse button functions *)
let ellipse_button () : unit =
  paint.mode <- EllipseStartMode
;; nc_ellipse.add_event_listener (mouseclick_listener ellipse_button)

(** Point button functions *)
let point_button () : unit =
  paint.mode <- PointMode
;; nc_point.add_event_listener (mouseclick_listener point_button)

(** Line button functions *)
let line_button () : unit =
  paint.mode <- LineStartMode
;; nc_line.add_event_listener (mouseclick_listener line_button)

(** Undo button functions *)
let undo () : unit =
  if Deque.is_empty paint.shapes then () else
    ignore (Deque.remove_tail paint.shapes)
;; nc_undo.add_event_listener (mouseclick_listener undo)

(** A spacer widget *)
let spacer : widget = space (10,10)

(** The mode toolbar, initially containing just the Undo button.
    TODO: you will need to modify this widget to add more buttons 
    to the toolbar in Tasks 5, and possibly 6. *)
let mode_toolbar : widget = 
  hlist [border w_undo; spacer; border w_point; spacer; 
    border w_line; spacer; border w_ellipse; spacer;
    w_checkbox; spacer; w_slider]

(* The color selection toolbar. *)
(* This toolbar contains an indicator for the currently selected color
   and some buttons for changing it. Both the indicator and the buttons
   are small square widgets built from this higher-order function. *)
(** Create a widget that displays itself as colored square with the given
    width and color specified by the [get_color] function. *)
let colored_square (width:int) (get_color: unit -> color)
  : widget * notifier_controller =
  let repaint_square (gc:gctx) =
    let c = get_color () in
    fill_rect (with_color gc c) (0, 0) (width-1, width-1) in
  canvas (width,width) repaint_square

(** The color_indicator repaints itself with the currently selected
    color of the paint application. *)
let color_indicator =
  let indicator,_ = colored_square 24 (fun () -> paint.color) in
  let lab, _ = label "Current Color" in
  border (hpair lab indicator)

(** color_buttons repaint themselves with whatever color they were created
    with. They are also installed with a mouseclick listener
    that changes the selected color of the paint app to their color. *)
let color_button (c: color) : widget =
  let w,nc = colored_square 10 (fun () -> c) in
  nc.add_event_listener (mouseclick_listener (fun () ->
      paint.color <- c ));
  w

(** The color selection toolbar. Contains the color indicator and
    buttons for several different colors. *)
(* TODO: Task 1 - This code contains a great deal of boilerplate.  You
     should come up with a better, more elegant, more concise solution... *)  
   let color_toolbar : widget =
    let color_button_list = [
      color_indicator; spacer;
      color_button black; spacer;
      color_button white; spacer;
      color_button red; spacer;
      color_button green; spacer;
      color_button blue; spacer;
      color_button yellow; spacer;
      color_button cyan; spacer;
      color_button magenta
      ] in
    hlist color_button_list

(** The top-level paint program widget: a combination of the
    mode_toolbar, the color_toolbar and the paint_canvas widgets. *)
(* TODO: Task 1 (and others) involve modifing this layout to add new
   buttons and make the layout more aesthetically appealing. *)
let paint_widget =
  let layout_list = [paint_canvas; spacer; mode_toolbar; spacer;
    color_toolbar] in
  vlist layout_list

(**************************************)
(**      Start the application        *)
(**************************************)

(** Run the event loop to process user events. *)
;; Eventloop.run paint_widget