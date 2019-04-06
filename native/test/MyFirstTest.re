open Tablecloth;
open TestFramework;

describe("List", ({test}) => {
  test()
});

  AT.check (AT.list AT.int) "reverse empty list" (List.reverse []) [];
  AT.check (AT.list AT.int) "reverse one element" (List.reverse [0]) [0];
  AT.check (AT.list AT.int) "reverse two elements" (List.reverse [0;1]) [1;0];

  AT.check (AT.list AT.int) "map2 empty lists" (List.map2 ~f:(+) [] []) [];
  AT.check (AT.list AT.int) "map2 one element" (List.map2 ~f:(+) [1] [1]) [2];
  AT.check (AT.list AT.int) "map2 two elements" (List.map2 ~f:(+) [1;2] [1;2]) [2;4];

  AT.check (AT.list AT.int) "indexedMap empty list" (List.indexedMap ~f:(fun i _ -> i) []) [];
  AT.check (AT.list AT.int) "indexedMap one element" (List.indexedMap ~f:(fun i _ -> i) ['a']) [0];
  AT.check (AT.list AT.int) "indexedMap two elements" (List.indexedMap ~f:(fun i _ -> i) ['a';'b']) [0;1];

  AT.check (AT.list AT.int) "indexedMap empty list" (List.indexedMap ~f:(fun _ n -> n + 1) []) [];
  AT.check (AT.list AT.int) "indexedMap one element" (List.indexedMap ~f:(fun _ n -> n + 1) [-1]) [0];
  AT.check (AT.list AT.int) "indexedMap two elements" (List.indexedMap ~f:(fun _ n -> n + 1) [-1; 0]) [0;1];


  AT.check (AT.pair (AT.list AT.int) (AT.list AT.int)) "partition empty list" (List.partition ~f:(fun x -> x mod 2 = 0) []) ([], []);
  AT.check (AT.pair (AT.list AT.int) (AT.list AT.int)) "partition one element" (List.partition ~f:(fun x -> x mod 2 = 0) [1]) ([], [1]);
  AT.check (AT.pair (AT.list AT.int) (AT.list AT.int)) "partition four elements" (List.partition ~f:(fun x -> x mod 2 = 0) [1;2;3;4]) ([2;4], [1;3]);

  ()
