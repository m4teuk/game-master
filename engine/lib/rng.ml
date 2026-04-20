(* xoroshiro128+ — deterministic 128-bit-seeded PRNG.

   Chosen over PCG-XSL-RR-128 because it's simpler to implement in OCaml
   (no 128-bit multiply required). Good enough for game RNG: passes
   standard statistical tests, O(1) per call, fully deterministic.
   Seed stretching uses splitmix64 so a small-entropy input seed still
   yields well-dispersed state. *)

type t = {
  s0 : int64;
  s1 : int64;
}

let rotl64 (x : int64) (k : int) : int64 =
  let open Int64 in
  logor (shift_left x k) (shift_right_logical x (64 - k))

(* splitmix64 — stretches one 64-bit word into another with good mixing.
   Used only at seeding time so the two halves of the 128-bit seed each
   produce well-distributed state words. *)
let splitmix64 (z : int64) : int64 =
  let open Int64 in
  let z = add z 0x9E3779B97F4A7C15L in
  let z = mul (logxor z (shift_right_logical z 30)) 0xBF58476D1CE4E5B9L in
  let z = mul (logxor z (shift_right_logical z 27)) 0x94D049BB133111EBL in
  logxor z (shift_right_logical z 31)

let of_seed (seed : bytes) : t =
  if Bytes.length seed <> 16 then
    invalid_arg "Rng.of_seed: seed must be exactly 16 bytes";
  let get_i64 off =
    let b i = Int64.of_int (Char.code (Bytes.get seed (off + i))) in
    let open Int64 in
    logor (b 0) (
      logor (shift_left (b 1)  8) (
        logor (shift_left (b 2) 16) (
          logor (shift_left (b 3) 24) (
            logor (shift_left (b 4) 32) (
              logor (shift_left (b 5) 40) (
                logor (shift_left (b 6) 48)
                  (shift_left (b 7) 56))))))) in
  let s0 = splitmix64 (get_i64 0) in
  let s1 = splitmix64 (get_i64 8) in
  (* xoroshiro must not be seeded to all-zero state. *)
  let s0, s1 =
    if Int64.equal s0 0L && Int64.equal s1 0L then (1L, 2L) else (s0, s1)
  in
  { s0; s1 }

(* One step of the xoroshiro128+ generator. Emits a 64-bit result and
   advances to the next state. Functional — returns a fresh [t]. *)
let next (t : t) : int64 * t =
  let open Int64 in
  let s0 = t.s0 in
  let s1 = t.s1 in
  let result = add s0 s1 in
  let s1' = logxor s1 s0 in
  let new_s0 = logxor (rotl64 s0 24) (logxor s1' (shift_left s1' 16)) in
  let new_s1 = rotl64 s1' 37 in
  (result, { s0 = new_s0; s1 = new_s1 })

let random_int (t : t) ~(lo : int) ~(hi : int) : int * t =
  if lo > hi then
    invalid_arg (Printf.sprintf "Rng.random_int: lo (%d) > hi (%d)" lo hi);
  let range = hi - lo + 1 in
  let (r64, t') = next t in
  (* Mask off the sign bit then modulo. This introduces a small bias
     for [range] not a divisor of 2^63, but the bias is negligible for
     game-scale ranges. *)
  let r_nonneg = Int64.logand r64 0x7FFFFFFFFFFFFFFFL in
  let r = Int64.to_int (Int64.rem r_nonneg (Int64.of_int range)) in
  (lo + r, t')

let shuffle_list (t : t) (xs : 'a list) : 'a list * t =
  let arr = Array.of_list xs in
  let n = Array.length arr in
  let rec loop t i =
    if i <= 0 then t
    else
      let (j, t') = random_int t ~lo:0 ~hi:i in
      let tmp = arr.(i) in
      arr.(i) <- arr.(j);
      arr.(j) <- tmp;
      loop t' (i - 1)
  in
  let t' = loop t (n - 1) in
  (Array.to_list arr, t')
