(* Random.self_init (); *)

type direction = [= `n | `ne | `e | `se | `s | `sw | `w | `nw ];

value directions:array direction = [| `n; `ne; `e; `se; `s; `sw; `w; `nw |];
value directionsNum = Array.length directions;
value directionToStr d =
	match d with
	[ `n -> "n"
	| `nw -> "nw"
	| `w -> "w"
	| `sw -> "sw"
	| `s -> "s"
	| `se -> "se"
	| `e -> "e"
	| `ne -> "ne"
	];
value offsets = [| (0.25, ~-.0.5); (0., 0.); (~-.0.5, 0.); (~-.1., 0.); (~-.1.5, ~-.0.5); (~-.1., ~-.1.); (~-.0.5, ~-.1.); (0., ~-.1.) |];

type compassState = [= `released | `pushed ];

class compass () =
	object(self)
		inherit Sprite.c as super;

		value img = Image.load "img/compass.png";
		value directionIndx = Random.int 1000 mod directionsNum;
		value roratable = Sprite.create ();
		value mutable state = `released;

		initializer
			(
				let labelPos = Shape.circle 0x000000 1. 2. in
				let (w, h) = (img#width, img#height) in
				let arrowAngleRand = Random.int 1000 mod 8 in
				let arrowAngle = float arrowAngleRand *. LightCommon.pi /. 4. in
				let dotRand = (if Random.int 1000 mod 2 = 0 then 1 else 5) + (Random.int 1000 mod 3) in
				let dotAngle = float dotRand *. LightCommon.pi /. 4. in
				let directionAngle = float directionIndx *. LightCommon.pi /. 4. in
				let direction = directions.(directionIndx) in
					(
						roratable#addChild img;
						roratable#addChild labelPos;


						img#setPos (~-.1. *. img#width /. 2.) (~-.1. *. img#height /. 2.);


							(
								labelPos#setPos (w /. 2. *. (cos (dotAngle))) (h /. 2. *. (sin dotAngle));
								self#addChild roratable;
								roratable#setRotation arrowAngle;

								let makeLabel i = 
									let ((tw, th), tlf) = TLF.create (TLF.p ~color:0x000000 ~fontSize:16 [ `text (directionToStr directions.((directionIndx + i) mod directionsNum)) ]) in
									let angle = float ((i + arrowAngleRand) mod directionsNum) *. LightCommon.pi /. 4. in
									let x = (w /. 2. *. (cos angle)) +. tw *. (fst offsets.((i + arrowAngleRand) mod directionsNum)) in
									let y = (h /. 2. *. (sin angle)) +. th *. (snd offsets.((i + arrowAngleRand) mod directionsNum)) in
										(
											tlf#setPos x y;
											self#addChild tlf;
										)
								in
									(
(* 														for i = 0 to directionsNum - 1 do {
											makeLabel i;
										}; *)
										(* makeLabel 0; *)
										makeLabel dotRand;
									);
									
								(* }; *)
							);
					);
			);

		method select () = img#setFilters [ Filters.glow 0xffff00 ];
		method deselect () = img#setFilters [];
		method correct () = ( self#setTouchable False; img#setFilters [ Filters.glow 0x00ff00 ]; );
		method wrong () = ( self#setTouchable False; img#setFilters [ Filters.glow 0xff0000 ]; );

		method! width = img#width;
		method! height = img#height;
		method direction:direction = directions.(directionIndx);
	end;

let stage w h =
	object(self)
		inherit Stage.c w h as super;

		value bgColor = 0xcccccc;
		value mutable selectedCompass = None;

		initializer
			(
				BitmapFont.register "fonts/Arial.fnt";

				let margin = 60. in
				let rows = 5 in
				let cols = 10 in
				let bottom = ref 0. in
					(
						for row = 1 to rows do {
							for col = 1 to cols do {
								let compass = new compass () in
								let (x, y) = (margin +. (compass#width +. margin) *. (float col -. 1.), margin +. (compass#height +. margin) *. (float row -. 1.)) in
									(
										if y +. compass#height > !bottom
										then bottom.val := y +. compass#height
										else ();

										compass#setPos x y;
										self#addChild compass;

										ignore(compass#addEventListener Stage.ev_TOUCH (fun ev _ _ ->
											match Stage.touches_of_data ev.Ev.data with
											[ Some [ t :: _ ] when t.Touch.phase = Touch.TouchPhaseEnded ->
												match selectedCompass with
												[ None -> 
													(
														selectedCompass := Some compass;
														compass#select ();
													)
												| Some sc ->
													(
														sc#deselect ();
														if sc <> compass
														then
															(
																compass#select ();
																selectedCompass := Some compass;
															)
														else selectedCompass := None
													)
												]
											| _ -> ()
											]
										));
									);
							};
						};

						let x = ref 0. in
							Array.iter (fun d ->
								let dstr = directionToStr d in
								let ((w, h), tlf) = TLF.create (TLF.p ~fontSize:30 [ `text dstr ]) in
									(
										tlf#setPos !x (!bottom +. 50.);
										x.val := !x +. w +. 30.;
										self#addChild tlf;

										ignore(tlf#addEventListener Stage.ev_TOUCH (fun ev _ _ ->
											match Stage.touches_of_data ev.Ev.data with
											[ Some [ t :: _ ] when t.Touch.phase = Touch.TouchPhaseEnded ->
												match selectedCompass with
												[ Some sc ->
													(
														if sc#direction = d
														then sc#correct ()
														else sc#wrong ();

														selectedCompass := None;
													)
												| _ -> ()
												]
											| _ -> ()
											]
										));
									)
							) directions;						
					);

(* 				let keyMap = String.concat ", " (Array.to_list (Array.mapi (fun i d -> Printf.sprintf "%s - %d" (directionToStr d) i) directions)) in
				let (_, tlf) = TLF.create (TLF.p ~fontSize:20 [ `text keyMap ]) in
					self#addChild tlf; *)
			);
	end
in
	Lightning.init stage;