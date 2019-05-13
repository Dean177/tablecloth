open Tablecloth

module Ref = struct
  type 'a t = 'a ref

  let swap = Base.Ref.swap
end

module Matrix = struct
  type 'a t = 'a array array

  let isValidIndex t (x, y) = 
    Int.inRange x ~lower:0 ~upper:(Array.length t) && 
    Int.inRange y ~lower:0 ~upper:(Array.length (t.(x)))

  let create ~rows ~columns ~f = 
  	Array.initialize ~length:columns ~f:(fun y -> 
  	  Array.initialize ~length:rows ~f:(fun y ->
  	  	f x y
  	  )   	
  	) 

  let column m i = m.(i)

  let row m i = 
    let r = Array.initialize ~length:(Array.length m.(0)) ~f:(fun r -> m.(i).(r))

  let clone m = 
  	let columns = Array.length m in
  	let rows = Base.(Array.length (m.(0))) in
  	create ~rows ~columns ~f:(fun x y -> Base.(m.(x).(y)))

  let rotate m = 
  	let width = Array.length m in
  	let height = Base.(Array.length (m.(0))) in
  	create ~rows:height ~columns:width ~f:(fun x y -> Base.(m.(y).(x)))

  let fold (m : 'a t) ~(initial : 'b) ~(f: 'b -> 'a -> 'b) : 'b = 
  	Array.foldLeft m ~initial ~f:(fun (column : 'a array) (acc : 'b)  : 'b ->
	  Array.foldLeft column ~initial:acc ~f:(fun (element : 'a) (accRow : 'b) : 'b->
  		f accRow element
  	  )
  	)

  let foldWithIndex m ~initial ~f = 
  	Array.foldWithIndex m ~initial ~f:(fun x acc column ->
	  Array.foldWithIndex column ~initial:acc ~f:(fun y accRow element ->
  		f accRow element x y
  	  )
  	)

  let any m ~f = 
  	Array.any m ~f:(Array.any ~f)

  let forEach m ~f =
  	Array.forEach m ~f:(Array.forEach ~f)

  let forEachI m ~f =
    Array.forEachI m ~f:(fun x column -> 
	  Array.forEachI column ~f:(fun y element -> 
	  	f x y element
  	  )
	)

  let toString m ~f = 
    Array.map m ~f:(Array.toString ~f)
  	|> String.concatenateArray ~sep:"\n" 
end

module Permutations : sig 
  type 'a t

  val fromArray : 'a array -> 'a t

  val next : 'a t -> 'a array option 

  (* val ofList : 'a list -> 'a list list *)

  (* val ofArray : 'a array -> 'a array array *)
end = struct
  type direction = Left | Right

  let flip = function 
    | Left-> Right 
    | Right -> Left

  type 'a t = (int * direction * 'a) array

  let fromArray a = 
    Array.mapWithIndex a ~f:(fun tag element -> (tag, Left, element))

  let isMovable t index =  
    let (tag, direction, _) = t.(index) in
    match direction with
    | Left ->       
      tag > 0 && (
        let (previousTag, _, _) = t.(index - 1) in
        tag > previousTag
      )
    | Right -> tag < (Array.length t - 1) && tag > (Tuple3.first t.(index + 1))

  let move t index =  
    let (_, direction, _) = t.(index) in
    match direction with
    | Left -> Array.swap t index (index - 1)
    | Right -> Array.swap t index (index + 1)

  let findIndexOfLargestMovableTag t = 
    Array.foldWithIndex t ~initial:None ~f:(fun index largestIndex (tag, _, _) ->
      if not (isMovable t index) then 
        largestIndex
      else (       
        match largestIndex with
        | None -> (Some index)
        | Some j -> 
          let newLargest = if tag < Tuple3.first(t.(j)) then j else index in
          (Some newLargest)
      )
    )    

  let flipLargerTags t ~tag =  
    Array.forEachI t ~f:(fun index (elementTag, direction, element) -> 
      if elementTag > tag then t.(index) <- (index, flip direction, element)
    )
    
  let next t =     
    match findIndexOfLargestMovableTag t with
    | None -> None
    | Some index -> (
      let (tag, _, _) = t.(index) in
      move t index;
      flipLargerTags t ~tag;
      Some (Array.map t ~f:Tuple3.third)
    )
end

module Square = struct
  open Base
  module T = struct 
	  type t =		
		| Empty
		| Tile of int 
		[@@deriving compare]

      let isEmpty = function | Empty -> true | _ -> false

	  let sexp_of_t _ : Sexp.t =
	  	Atom "fail"

	  let toString = function
	    | Empty -> " "
	  	| Tile n -> Int.to_string n	

	  let score = function
        | Empty -> 0
	    | Tile n -> n
  end
  include T
  include Comparator.Make(T)
end

module Piece = struct
  type t = Square.t Matrix.t  

  let rotations tile = 
  	let t2 = Matrix.rotate tile in
  	let t3 = Matrix.rotate t2 in
  	let t4 = Matrix.rotate t3 in
  	[tile; t2; t3; t4]

  let zero = [|
    [|Tile 0; Tile 0; Tile 0; Tile 0;|];
    [|Tile 0; Empty;  Empty;  Tile 0;|];
    [|Tile 0; Tile 0; Tile 0; Tile 0;|];
  |]

  let one = [|
    [|Tile 1; Tile 1; Tile 1; Tile 1;|];
    [|Tile 1; Empty;  Empty;  Empty; |];
  |]

  let two = [|
    [|Tile 2; Tile 2; Empty; Empty|];
    [|Tile 2; Tile 2; Tile 2; Tile 2;|];
    [|Tile 2; Empty;  Tile 2; Tile 2;|];
  |]

  let three = [|
    [|Tile 3; Tile 3; Tile 3; Tile 3;|];
    [|Tile 3; Empty;  Tile 3; Tile 3;|];
    [|Tile 3; Empty;  Empty;  Tile 3;|];
  |]

  let four = [|   
    [|Tile 4; Empty;  Tile 4; Tile 4;|];
    [|Tile 4; Tile 4; Tile 4; Tile 4;|];
    [|Empty;  Empty;  Tile 4; Empty; |];
  |]

  let five = [|   
    [|Tile 5; Tile 5; Tile 5; Tile 5;|];
    [|Tile 5; Tile 5; Empty;  Tile 5;|];
    [|Tile 5; Tile 5; Empty;  Tile 5;|];
  |]

  let six = [|    
    [|Tile 6; Tile 6; Tile 6; Tile 6;|];
    [|Tile 6; Tile 6; Empty;  Tile 6;|];
    [|Tile 6; Tile 6; Empty;  Empty; |];
  |]

  let seven = [|
    [|Tile 7; Tile 7; Empty;  Tile 7;|];
    [|Empty;  Tile 7; Tile 7; Tile 7;|];
    [|Empty;  Empty;  Empty;  Tile 7;|];    
  |]

  let eight = [|
    [|Tile 8; Tile 8; Empty;  Empty  |];
    [|Tile 8; Tile 8; Tile 8; Tile 8;|];
    [|Empty;  Empty;  Tile 8; Tile 8;|];    
  |]

  let nine = [|
    [|Tile 9; Tile 9; Tile 9; Tile 9;|];
    [|Tile 9; Tile 9; Tile 9; Tile 9;|];
    [|Empty;  Empty;  Tile 9; Tile 9;|];    
  |]

  let all = [zero; one; two; three; four; five; six; seven; eight; nine]
end

module Seed = struct
  type t = int list

  let tileNumbers = [0;1;2;3;4;5;6;7;8;9]

  (* let all = tileNumbers |> Array.fromList |> Permutations.fromArray *)

  let toString = String.concat
end

module Coordinates = struct
  type t = int * int

  let surrounding (x, y) = 
    [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)]
end

module Layer = struct
  type t = (Square.t Matrix.t)

  let place layer piece (x, y) = 
  	let newLayer = Matrix.clone layer in
    Matrix.forEachI piece ~f:Base.(fun tx ty tile -> 
      newLayer.(x + tx).(y + ty) <- tile;
    );
  	newLayer

  let toString l = Matrix.toString l ~f:Square.toString
end

module Board = struct 
  type t =  Layer.t array 

  let createLayer tile = 
  	Matrix.create ~rows:9 ~columns:9 ~f:(fun _ _ -> tile)

  let createInitialBoard () = Square.([|    
  	createLayer Empty;
  	createLayer Empty;
  	createLayer Empty;
  	createLayer Empty;	
  |])

  let canPlace board piece (x, y, z) : bool =
    let isOverlapping = 
      Matrix.foldWithIndex piece ~initial:false ~f:(fun overlapped square px py ->
        overlapped || Square.isEmpty(square) || Square.isEmpty(board.(z - 1).(x + px).(y + py))
      )            	  
  	in
  	if (z = 0) then (
      let hasNeighbour = Matrix.foldWithIndex piece ~initial:false ~f:(fun foundNeighbour square px py -> 
        foundNeighbour || Square.isEmpty(pieceSquare) || (
          Coordinates.surrounding (x, y) 
          |> List.filter ~f:(Matrix.isValidIndex (board.(z)))
          |> List.any ~f:(fun (nx, ny) -> Square.isEmpty(board.(z).(x).(y)))
        )
      )
      not isOverlapping (** && TODO check it is 'touching at least one other tile' *)


  	) else (
      let distinctFoundationTiles = 
    		Matrix.foldWithIndex piece ~initial:[] ~f:(fun acc square px py ->
  	   	  match square with 
          | Empty -> acc
  	  	  | _ -> board.(z - 1).(x + px).(y + py) :: acc
  	  	)
  	  	|> Base.Set.of_list (module Square)
        |> Base.Set.length
  	  in
      (not isOverlapping) && (distinctFoundationTiles > 1)
  	)

  let place board piece (x, y, z) =
    Array.initialize ~length:(Array.length board) ~f:(fun layer -> 
      if z <> layer then board.(z) else (
        Layer.place board.(z) piece (x, y)
      )
    )

  let score board =
    Array.foldWithIndex board ~initial:0 ~f:(fun layerNumber total layer -> 
      Matrix.fold layer ~initial:(Base.Set.empty (module Square)) ~f:Base.Set.add 
  	  |> Base.Set.to_array
      |> Array.map ~f:Square.score
      |> Array.sum
	  |> Int.multiply layerNumber
	  |> Int.add total
    ) 

  let toString b = 
    Array.toString b ~sep:"\n\n" ~f:Layer.toString
end
