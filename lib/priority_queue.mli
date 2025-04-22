type 'a t = { mutable items : 'a list; priority_func : 'a -> int; }
val create : ('a -> int) -> 'a t
val is_empty : 'a t -> bool
val add : 'a t -> 'a -> unit
val take : 'a t -> 'a
val peek : 'a t -> 'a
val peek_opt : 'a t -> 'a option
val size : 'a t -> int
val exists : 'a t -> ('a -> bool) -> bool
