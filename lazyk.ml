(* Lazy K interpreter (unlambda syntax only) *)

let c2 f x = f (f x)
let s2 = c2 Succ
let s4 = c2 s2
let s8 = c2 s4
let s16 = c2 s8
let s32 = c2 s16
let s64 = c2 s32
let s128 = c2 s64

let ch0 = s128 (s8 (Succ w)) (* 119 + 137 *)
let chbq = s64 (s32 ch0) (* 96 *)
let ch97 = Succ chbq
let chi = s8 ch97 (* 105 *)
let chk = s2 chi  (* 107 *)
let chs = s8 chk (* 115 *)

let app df da = df da da   (* (force df) da  *)

let succcn _ n _ f _ x = app f (app (app n f) x)
let cn0 _ f _ x = x
let ch2cn ch =
  let f self c cn =
    let sc = Succ c in
    let scn = app succcn cn in
    let recur = self self sc in
    let eq = ch c in
    let t = eq cn in
      eq t recur scn
  in
    f f ch0 cn0

let dapp df da t =     (* (delay            *)
  let d = df t da in   (*   ((force df) da) *)
    d d                (*   |> force)       *)

let tS _ _ x _ y _ z = app (app x z) (dapp y z)
let tK _ _ x _ y = x
let tI _ _ x = x

let read_app read t =
  let f = read t in
  let a = read f in
    dapp f a

let id x = x
let Out = id Out
let read_unl_ self _ =
  let r = self self in
  let c = In Out in
  let ok = c c in
  let isbq = chbq c (read_app r) in
  let iss  = chs c tS in
  let isk  = chk c tK in
  let isi  = chi c tI in
  let t = isbq (iss (isk (isi Out))) in
    t t

let read_unl _ =
  let r = read_unl_ read_unl_ in
  let d = r r in
    d d

let app df da = df da da
let cn2 _ f _ x = app f (app f x)
let cn4 = app cn2 cn2
let cn256 = app cn4 cn4

let cycle_ self _ b = app (app b cn256) (self self)
let tail = cycle_ cycle_
let cons x y _ b = app (app b x) y
let make_cont c k acc = k (cons (ch2cn c) acc)
let ignore2 x y z = z
let read_ self k =
  let recur = self self in
  let c = In ignore2 in
  let noteof = c c in
  let f = noteof recur k in
  let a = noteof (make_cont c k) tail in
    f a

let read_stdin k = read_ read_ k

let id x = x
let const x _ = id x
let succch _ dch =
  let ch = dch dch in
    const (Succ ch)
let cn2ch cn =
  let dch = app (app cn succch) (const ch0) in
    dch dch

let false x y = y
let true x y = id x
let iseof cn =
  let isch0 = cn2ch cn ch0 in
  let notcn0 = app (app cn (true (true true))) false in
    isch0 notcn0 false

let dtrue _ x _ y = x
let dfalse _ x _ y = y
let app df da = df da da

let print_ self lst =
  let recur = self self in
  let cn = app lst dtrue in
  let e = iseof cn in
  let t = e e in
  let _ = t Out (cn2ch cn) in
    t recur (app lst dfalse)

let print = print_ print_

let main t =
  let prog = read_unl t in
  let result = read_stdin prog in
    print result
