type 'a t = { mutable items : 'a list; priority_func : 'a -> int; }
val create : ('a -> int) -> 'a t
val is_empty : 'a t -> bool
val add : 'a t -> 'a -> unit
val take : 'a t -> 'a
val peek : 'a t -> 'a
val size : 'a t -> int
