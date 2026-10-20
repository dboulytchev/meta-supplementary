(* SPDX-License-Identifier: LGPL-2.1-or-later *)
(*
 * Metacomputations workout.
 * Copyright (C) 2015-2026
 * Dmitri Boulytchev, St.Petersburg State University
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU Library General Public License version 2.1 for more details
 * (enclosed in the file LICENSE).
 *)

open Format

type pp = formatter -> unit

let offs = ref 2

let get_offs () = !offs
let set_offs n  = offs := n
  
let (|>) a b = fun fmt ->
  a fmt;
  b fmt

let empty    = fun fmt -> ()
let space    = fun fmt -> Format.fprintf fmt "@ "
let nl       = fun fmt -> Format.fprintf fmt "@\n"
let string s = fun fmt -> Format.fprintf fmt "%s" s

let seq list = fun fmt ->
  List.iter (fun x -> x fmt) list
      
let listBy sep list = fun fmt ->
  List.iteri
    (fun i x ->
       (if i = 0
        then x 
        else sep |> x
       ) fmt
    ) list

let list = listBy (string "," |> space)

let box s p = fun fmt ->
  Format.fprintf fmt "@[<%s%d>" s !offs;
  p fmt;
  Format.fprintf fmt "@]"

let vbox   p = box "v"   p
let hbox   p = box "h"   p
let hvbox  p = box "hv"  p
let hovbox p = box "hov" p

let inbr l r p = l |> p |> r

let id   p = p
let rbr  p = inbr (string "(") (string ")") p
let sqbr p = inbr (string "[") (string "]") p
let cvbr p = inbr (string "{") (string "}") p
let anbr p = inbr (string "<") (string ">") p

let fields rsep fsep list =
  listBy rsep @@ List.map (fun (x, y) -> seq [x; fsep; y]) list
  
let vert = vbox
let hor  = hbox
    
let to_string p =
  let b = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer b in
  p ppf;
  Format.pp_print_flush ppf ();
  Buffer.contents b
