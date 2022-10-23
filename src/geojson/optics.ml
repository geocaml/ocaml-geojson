let undefined _ =
  let exception Undefined in
  raise Undefined

module Either = struct
  type ('a, 'b) t = Left of 'a | Right of 'b

  let left a = Left a
  let right b = Right b
end

module Lens = struct
  type ('s, 'a) t = V : ('s -> 'a * 'r) * ('a * 'r -> 's) -> ('s, 'a) t

  let v (type a b r) (f : a -> b * r) (g : b * r -> a) = V (f, g)
  let get (type a b) (V (f, _) : (a, b) t) (v : a) : b = fst @@ f v

  let set (type a b) (V (f, g) : (a, b) t) (t : a) (v : b) =
    let _, r = f t in
    g (v, r)

  let id x = x

  let fst : ('a * 'b, 'a) t = V (id, id)
  let snd : ('a * 'b, 'b) t = V ((fun (a, b) -> (b, a)), fun (b, a) -> (a, b))

  let head : ('a list, 'a) t =
    V ((fun lst -> (List.hd lst, List.tl lst)), fun (hd, tl) -> hd :: tl)

  let splice_out lst n =
    let rec aux ((before, after) as acc) n = function
      | [] -> (List.rev before, List.rev after)
      | x :: xs when n < 0 -> aux (before, x :: after) (n - 1) xs
      | _ :: xs when n = 0 -> aux acc (n - 1) xs
      | x :: xs -> aux (x :: before, after) (n - 1) xs
    in
    aux ([], []) n lst

  let nth n : ('a list, 'a) t =
    V
      ( (fun lst -> (List.nth lst n, splice_out lst n)),
        fun (n, (b, a)) -> b @ [ n ] @ a )

  let ( >> ) (type a b c) (V (f, g) : (a, b) t) (V (f', g') : (b, c) t) :
      (a, c) t =
    V
      ( (fun x ->
          let a, r1 = f x in
          let v, r2 = f' a in
          (v, (r1, r2))),
        fun (y, (r1, r2)) -> g (g' (y, r2), r1) )
end

module Prism = struct
  type ('s, 'a) t =
    | V : ('s -> ('a, 'r) result) * (('a, 'r) result -> 's) -> ('s, 'a) t

  let get (type s a) (V (f, _) : (s, a) t) (v : s) : a option =
    Result.to_option @@ f v

  let set (type s a) (V (_, g) : (s, a) t) (v : a) = g (Ok v)

  let some =
    V
      ( (function Some t -> Ok t | None -> Error ()),
        function Ok t -> Some t | Error () -> None )

  let none =
    V
      ( (function None -> Ok () | Some t -> Error t),
        function Ok () -> None | Error t -> Some t )

  let ( >> ) (type a b c) (V (f, g) : (a, b) t) (V (f', g') : (b, c) t) :
      (a, c) t =
    let first x =
      match f x with
      | Error r1 -> Error (Either.left r1)
      | Ok b -> (
          match f' b with Ok c -> Ok c | Error r2 -> Error (Either.right r2))
    in
    let second = function
      | Ok v -> g (Ok (g' (Ok v)))
      | Error (Either.Left r1) -> g (Error r1)
      | Error (Either.Right r2) -> g (Ok (g' (Error r2)))
    in
    V (first, second)
end

module Optional = struct
  type ('s, 'a) t = ('s, 'a option) Lens.t

  let lens (type a b) (Lens.V (f, g) : (a, b) Lens.t) : (a, b) t =
    let wrapped_focus x =
      let v, r = f x in
      (Some v, r)
    in
    let wrapped_return = function
      | Some x, r -> g (x, r)
      | None, _ -> undefined () (* Not possible for a lens! *)
    in
    Lens.V (wrapped_focus, wrapped_return)

  let prism (type a b) (Prism.V (f, g) : (a, b) Prism.t) : (a, b) t =
    let wrapped_focus x =
      match f x with Ok v -> (Some v, None) | Error r -> (None, Some r)
    in
    let wrapped_return = function
      | Some x, None -> g (Ok x)
      | None, Some r -> g (Error r)
      | _ -> undefined () (* Other cases are not possible *)
    in
    Lens.V (wrapped_focus, wrapped_return)

  let ( >& ) (type a b c) (Lens.V (f1, g1) : (a, b) Lens.t)
      (Prism.V (f2, g2) : (b, c) Prism.t) : (a, c) t =
    let wrapped_focus x =
      let b, r1 = f1 x in
      match f2 b with
      | Ok c -> (Some c, Either.left r1)
      | Error r2 -> (None, Either.right (r1, r2))
    in
    let wrapped_return = function
      | Some c, Either.Left r1 -> g1 (g2 (Ok c), r1)
      | None, Either.Right (r1, r2) -> g1 (g2 (Error r2), r1)
      | _ -> undefined ()
    in
    Lens.V (wrapped_focus, wrapped_return)

  let ( >$ ) (type a b c) (Prism.V (f1, g1) : (a, b) Prism.t)
      (Lens.V (f2, g2) : (b, c) Lens.t) : (a, c) t =
    let wrapped_focus x =
      match f1 x with
      | Ok b ->
          let c, r2 = f2 b in
          (Some c, Either.right r2)
      | Error r1 -> (None, Either.left r1)
    in
    let wrapped_return = function
      | Some c, Either.Right r2 -> g1 (Ok (g2 (c, r2)))
      | None, Either.Left r1 -> g1 (Error r1)
      | _ -> undefined ()
    in
    Lens.V (wrapped_focus, wrapped_return)

  let ( >> ) (type a b c) (Lens.V (f1, g1) : (a, b) t)
      (Lens.V (f2, g2) : (b, c) t) : (a, c) t =
    let wrapped_focus x =
      let b, r1 = f1 x in
      match b with
      | Some b ->
          let c, r2 = f2 b in
          (c, Either.right (r1, r2))
      | None -> (None, Either.left r1)
    in
    let wrapped_return = function
      | c, Either.Right (r1, r2) -> g1 (Some (g2 (c, r2)), r1)
      | None, Either.Left r1 -> g1 (None, r1)
      | _ -> undefined ()
    in
    Lens.V (wrapped_focus, wrapped_return)
end

module Infix = struct
  let ( >> ) = Optional.( >> )
  let ( &> ) o l = Optional.(o >> lens l)
  let ( $> ) o p = Optional.(o >> prism p)
  let ( >& ) = Optional.( >& )
  let ( >$ ) = Optional.( >$ )
  let ( & ) = Lens.( >> )
  let ( $ ) = Prism.( >> )
end
