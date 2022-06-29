open Dynamic

structure HashTblTy =
struct
  type 'v bucket = (word * string * 'v) list

  datatype hashtbl = HASH of 
    {hash: string -> word,
      eq: string * string -> bool,
      buckets: (Dynamic.void Dynamic.dyn bucket) Array.array ref,
      size: int ref}
end