(* There are two players, called Human and Comput. *)
type player = Human | Comput

(* next Human is Comput 
 * next Comput is Human *)
val next: player -> player

(* to_string *)
val player2s: player -> string

(*** Helper functions on matrices. *)

type 'a matrix = 'a array array

(* Create a matrix of n line and m columns with all elements equal to val*)
val create_matrix: int -> int -> 'a -> 'a matrix

val clean : int matrix -> int -> int -> int matrix

val clone_matrix: 'a matrix -> 'a matrix

val count_ones : int array -> int

val countall : int matrix -> int -> int

(* Finds a cell which satisfies the predicate (p). 
 * Returns Some (row, col) if the cell at (row, col) satisfies p.
 * Returns None if no cell satisfies p. *)
val find_cell: 'a matrix -> ('a -> bool) -> (int * int) option

(* matrix2s mat v2s : maps a matrix to a printable string.
 *                    v2s maps a single value to a string (of fixed length, e.g. always 2 characters). *)
val matrix2s: 'a matrix -> ('a -> string) -> string

