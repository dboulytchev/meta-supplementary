open Ostap
open GT

module Value =
  struct
    
    @type t =
    | Int   of int
    | Array of t array
    | Dyn
    with show

    let toPrintable v =
      let buf   = Buffer.create 256 in
      let put s = Buffer.add_string buf s in
      let rec inner = function
      | Int   n -> put @@ string_of_int n
      | Array a ->
         put "[";
         ignore @@ Array.fold_left (fun first v -> if first then inner v else (put ", "; inner v); false) true a;
         put "]"

      | Dyn -> put "?"
      in
      inner v;
      Buffer.contents buf
      
    module Printable =
      struct
        @type t = string with show
      end
    
    let rec join v1 v2 =
      match v1, v2 with
      | Int x , Int y when x = y ->
         Int x
      | Array xs, Array ys when Array.length xs = Array.length ys ->
         Array (Array.init (Array.length xs) (fun i -> join xs.(i) ys.(i)))
      | _ -> Dyn

    let (!?) = function
    | Int n -> n
    | v     -> invalid_arg (Printf.sprintf "integer value expected in evalBinop but %s is provided" (show(Printable.t) @@ toPrintable v))
        
    let int   n = Int n 
    let array a = Array a
    
    let checkArray a cont =
      match a with
      | Array v -> cont v
      | _       -> invalid_arg (Printf.sprintf "array value expected in length but %s is provided" (show(Printable.t) @@ toPrintable a))

    let checkIndex a i cont =
      let len = Array.length a in
      if i >= 0 && i < len
      then cont ()
      else invalid_arg (Printf.sprintf "array index %d is out of bounds [0..%d)" i len)
    
    let length a     = int @@ checkArray a Array.length    
    let elem   a i   = checkArray a (fun a -> a.(!? i))
    let subst  a ivs =
      checkArray a
        (fun orig ->
           let orig' = Array.copy orig in
           List.iter
             (fun (i, v) ->
                let i = !? i in               
                checkIndex orig' i (fun _ -> orig'.(i) <- v);
             ) ivs;
           Array orig'
        )

    ostap (
      parse:
        n:DECIMAL                      {Int n}
      | "[" vs:!(Util.list)[parse] "]" {Array (Array.of_list vs)}
      | "?"                            {Dyn} 
    )
    
  end

module State =
  struct
    
    module M = Map.Make (String)
      
    type t = Value.t M.t
    
    let toPrintable s = List.map (fun (name, v) -> name, Value.toPrintable v)(M.bindings s)
  
    let empty              = M.empty
    let set   m name value = M.add name value m
    let unset m name       = M.remove name m
    
    let peep state x =
      match M.find_opt x state with
      | Some v -> Printf.printf "%s=%s\n" x (show(Value.Printable.t) @@ Value.toPrintable v)
      | None   -> Printf.printf "%s undefined\n" x
    
    let diff st st' =
      List.fold_left
        (fun diffed (x, w) ->
          match M.find_opt x st' with
          | None -> M.add x w diffed
          | _    -> diffed
        )
        M.empty
        (M.bindings st)
    
    let join st st' =
      List.fold_left
        (fun joined (x, v) ->
          match M.find_opt x st' with
          | Some w -> M.add x (Value.join v w) joined
          | _ -> joined
        )
        M.empty
        (M.bindings st)
    
    exception Undefined_variable of string
    
    let get m name =
      match M.find_opt name m with
      | Some x -> x
      | None   -> raise (Undefined_variable name)
    
    let dynamic m name =
      match M.find_opt name m with
      | None           -> true
      | Some Value.Dyn -> true
      | Some _         -> false

    let residualize r z state =
      List.fold_left
        (fun acc (x, v) -> r acc x v)
        z
        (M.bindings state)
    
    module Printable =
      struct
        @type t = (string * Value.Printable.t) list with show
      end

    let parse s =
      Util.parse
        (object (self: 'self)
           inherit Matcher.t s
           inherit Util.Lexers.decimal s
           inherit Util.Lexers.lident [] s 
           inherit Util.Lexers.skip [
                       Matcher.Skip.whitespaces " \t\n\r";
                   ] s
         end) 
        (ostap (
             bs:!(Util.list)[ostap (LIDENT -"=" !(Value.parse))] EOF {
                 List.fold_left (fun st (x, v) -> set st x v) empty bs 
             }      
        ))
  
  end

module Expr =
  struct
  
    @type t =
      | Var    of string
      | Const  of int    
      | Binop  of string * t * t
      | Array  of t array
      | Elem   of t * t
      | Length of t
      | Subst  of t * (t * t) list
    with show
    
    @type assoc = No | Left | Right with show
    
    let prio = function
    | "!!" -> 1
    | "&&" -> 2
    | "==" -> 3
    | "!=" -> 3
    | "<=" -> 3
    | "<"  -> 3
    | ">=" -> 3
    | ">"  -> 3
    | "+"  -> 4
    | "-"  -> 4
    | "*"  -> 5
    | "/"  -> 5
    | "%"  -> 5
    | _    -> 0
    
    let assoc = function
    | "!!" -> Left
    | "&&" -> Left
    | "==" -> No
    | "!=" -> No
    | "<=" -> No
    | "<"  -> No
    | ">=" -> No
    | ">"  -> No
    | "+"  -> Left
    | "-"  -> Left
    | "*"  -> Left
    | "/"  -> Left
    | "%"  -> Left
    | _    -> No
  
    let pretty expr =
      let open Formeat in
      let top = ("", No) in
      let rec pretty (context, position) = function 
      | Var    x         -> string x
      | Const  n         -> string @@ string_of_int n
      | Binop (op, l, r) ->
         let lp = pretty (op, Left ) l in
         let rp = pretty (op, Right) r in
         let base = hovbox @@
                      listBy space [lp; string op; rp]
         in
         if prio op < prio context then rbr base
         else if prio op = prio context && assoc op <> position then rbr base
         else base
      | Array (es)      -> hovbox (sqbr @@ list (List.map (pretty top) @@ Array.to_list es))
      | Elem  (a, i)    -> hovbox @@ listBy space [pretty top a; sqbr @@ pretty ("", No) i] 
      | Length e        -> hovbox @@ seq [pretty top e; string ".length"] 
      | Subst  (t, ivs) -> hovbox (sqbr @@ seq [pretty top t;
                                                space;
                                                string "with";
                                                space;
                                                list @@ List.map (fun (i, v) -> listBy space [pretty top i; string "="; pretty top v]) ivs
                                               ]) 
      in
      pretty top expr
    
    let evalBinop xv yv =
      let open Value in
      let xv, yv = !? xv, !? yv in
      let i2b x = if x <> 0 then 1 else 0     in
      let b2i   = function true -> 1 | _ -> 0 in
      function
      | "!!" -> int @@ i2b (i2b xv + i2b yv)
      | "&&" -> int @@ i2b (i2b xv * i2b yv)
      | "==" -> int @@ b2i (xv =  yv)
      | "!=" -> int @@ b2i (xv <> yv)
      | "<=" -> int @@ b2i (xv <= yv)
      | "<"  -> int @@ b2i (xv <  yv)
      | ">=" -> int @@ b2i (xv >= yv)
      | ">"  -> int @@ b2i (xv >  yv)
      | "+"  -> int @@ xv + yv
      | "-"  -> int @@ xv - yv
      | "*"  -> int @@ xv * yv
      | "/"  -> int @@ xv / yv
      | "%"  -> int @@ xv mod yv
    
    let rec eval state = function
    | Var   x -> State.get state x
    | Const x -> Value.Int x
    | Binop (op, x, y) ->
       evalBinop (eval state x) (eval state y) op
    | Length a        -> Value.length @@ eval state a
    | Elem  (a, i)    -> Value.elem  (eval state a) (eval state i)
    | Subst (a, ivs)  -> Value.subst (eval state a) (List.map (fun (i, v) -> eval state i, eval state v) ivs)
    | Array es        -> Value.Array (Array.map (eval state) es)

    let rec residualizeValue base = function
      | Value.Dyn      -> base
      | Value.Array vs -> Array (Array.mapi (fun i v -> residualizeValue (Elem (base, Const i)) v) vs)
      | Value.Int   n  -> Const n

    let rec spec state = function
    | Var x as expr ->
       (try
          residualizeValue expr @@ State.get state x
        with
          State.Undefined_variable _ -> Var x
       )
    | Const x as expr -> expr
    | Binop (op, x, y) ->
       let x', y' = spec state x, spec state y in
       (match x', y' with
        | Const xv, Const yv ->
           Const (Value.(!?) @@ evalBinop (Value.int xv) (Value.int yv) op)
        | _, _ -> Binop (op, x', y')
       )
    | Array es -> Array (Array.map (spec state) es)
    | Length e ->
       (match spec state e with
        | Array es -> Const (Array.length es)
        | e'       -> Length e'        
       )
    | Elem (a, i) ->
       (match spec state a, spec state i with
        | Array es, Const i -> es.(i)
        | a', i'            -> Elem (a', i')
       )
    | Subst (a, ivs) ->
       (match spec state a, List.map (fun (i, v) -> spec state i, spec state v) ivs with
        | Array a', ivs' ->
           let prefix, suffix, _ =
             List.fold_left
               (fun (prefix, suffix, toSuffix) (i, v) ->
                 if toSuffix
                 then prefix, (i, v)::suffix, toSuffix
                 else
                   match i with
                   | Const i -> (i, v) :: prefix, suffix, toSuffix
                   | _       -> prefix, (i, v)::suffix  , true
               )
               ([], [], false)
               ivs'
           in
           let prefix, suffix = List.rev prefix, List.rev suffix in
           let copy = Array.copy a' in
           List.iter (fun (j, v) -> copy.(j) <- v) prefix;
           (match suffix with
            | [] -> Array copy
            | _  -> Subst (Array copy, suffix)
           )
        | a', ivs' -> Subst (a', ivs')
       )
    
  end

module Stmt =
  struct

    @type t =
    | Assn  of string * Expr.t
    | Seq   of t * t
    | Skip
    | If    of Expr.t * t * t
    | While of Expr.t * t
    | Do    of t * Expr.t
    with show

    let rec pretty stmt =
      let open Formeat in
      match stmt with
      | Assn (x, e) ->
         hovbox @@
           listBy space [string x; string "="; Expr.pretty e]
      | Seq (l, r) ->
         vbox @@
           listBy
             (string ";" |> space)
             [pretty l; pretty r]  
      | Skip -> string "skip"
      | If (c, t, e) ->
         hovbox @@
           listBy space
             [string "if";
              rbr @@ Expr.pretty c;
              cvbr @@ pretty t;
              string "else";
              cvbr @@ pretty e]
      | While (c, b) ->
         hovbox @@
           listBy space
             [string "while";
              rbr @@ Expr.pretty c;
              cvbr @@ pretty b]            
      | Do (b, c) ->
         hovbox @@
           listBy space
             [string "do";
              pretty b;
              string "while";
              rbr @@ Expr.pretty c]
    
    let rec eval state =
      let open Value in
      function
      | Assn (x, e)  -> State.set state x (Expr.eval state e)
      | Seq  (l, r)  -> eval (eval state l) r
      | Skip         -> state
      | If (c, t, e) -> eval state (if !?(Expr.eval state c) <> 0 then t else e)
      | While (c, b) as w ->
         if !?(Expr.eval state c) = 0
         then state
         else eval (eval state b) w                 
      | Do (b, c) as w -> eval state (Seq (b, While (c, b)))      
    
    let residualizeAfter state stmt =
      State.residualize
        (function
           Skip -> fun x v -> Assn (x, Expr.residualizeValue (Expr.Var x) v)
         | acc  -> fun x v -> Seq  (acc, Assn (x, Expr.residualizeValue (Expr.Var x) v))
        )
        stmt
        state
    
    let residualizeBefore state stmt =
      match residualizeAfter state Skip with
      | Skip -> stmt
      | s    -> Seq (s, stmt)
    
    let rec spec state =
      function
      | Skip        -> (Skip, state)
      | Assn (x, e) ->
         let e' = Expr.spec state e in
         (match e' with
          | Expr.Const ve -> (Skip        , State.set   state x (Value.int ve))
          | _             -> (Assn (x, e'), State.unset state x)
         )    
      | Seq (s1, s2) ->
         (match spec state s1 with
          | (Skip, state') -> spec state' s2
          | (s1' , state') ->
             (match spec state' s2 with
              | (Skip, state'') -> (s1', state'')
              | (s2' , state'') -> (Seq (s1', s2'), state'')
             )
         ) 
      | If (c, t, e) ->
         (match Expr.spec state c with
          | Expr.Const cv when cv != 0 -> spec state t
          | Expr.Const _               -> spec state e
          | c' ->
             let t', state'  = spec state t in
             let e', state'' = spec state e in
             let joined      = State.join state' state'' in
             If (c', residualizeAfter (State.diff state'  joined) t',
                 residualizeAfter (State.diff state'' joined) e'),
             joined
         ) 
      | While (c, s) as w ->
         (match Expr.spec state c with
          | Expr.Const cv when cv != 0 -> spec state (Seq (s, w)) 
          | Expr.Const cv              -> (Skip, state)
          | _ ->
             let _ , state'   = spec state s                     in
             let _ , state''  = spec (State.join state state') s in
             let state''      = State.join state state''         in
             let s', state''' = spec state'' s                   in
             residualizeBefore
               (State.diff state state'')
               (While (Expr.spec state'' c, residualizeAfter (State.diff state''' state'') s')),
             state''
         ) 
      | Do (s, c) -> spec state (Seq (s, While (c, s)))
     
  end

module Parser =
  struct

    let binop =
      ostap (
          s:("!!" | "&&" | "==" | "!=" | "<=" | "<" | ">=" | ">" |
             "+"  | "-"  | "*"  | "/"  | "%") {Matcher.Token.repr s} 
        )
      
    let rec expression s =
      let binop op x y = Expr.Binop (op, x, y) in
      let ops = Array.map
                  (fun (assoc, list) ->
                    assoc,           
                    List.map (fun op -> ostap ($(op)), binop op) list
                  )
                  [|
                    `Lefta , ["!!"]; 
                    `Lefta , ["&&"];
                    `Nona  , ["=="; "!="; "<="; "<"; ">="; ">"];
                    `Lefta , ["+" ; "-"];
                    `Lefta , ["*" ; "/"; "%"]; 
                  |]
      in
      let rec postfix s = ostap (
                            base:primary ops:("." %"length" {`Length} | "[" i:expression "]" {`Elem i})* {
                              List.fold_left (fun acc -> function
                                                         | `Length -> Expr.Length acc
                                                         | `Elem i -> Expr.Elem (acc, i)
                                             ) base ops
                            }
                          ) s     
      and primary s = ostap (
                          x:LIDENT                                                                          {Expr.Var   x}
                      |   c:DECIMAL                                                                         {Expr.Const c}
                      | "[" a:!(Util.list)[expression] "]"                                                  {Expr.Array (Array.of_list a)}
                      | "[" base:expression "with" ivs:!(Util.list)[ostap (expression -"=" expression)] "]" {Expr.Subst (base, ivs)}
                      | -"(" expression -")" 
                      ) s
      in  
      Util.expr (fun x -> x) ops postfix s
    
    let rec stmt s =
      let ostap (
        primary:
          %"skip"                       ";"? {Stmt.Skip   }    
        | d:LIDENT "=" s:expression     ";"? {Stmt.Assn (d, s)}
        | d:LIDENT op:binop "=" s:expression ";"?         {Stmt.Assn (d, Expr.Binop (op, Expr.Var (d), s))}
        | %"while" "(" c:expression ")" s:stmt            {Stmt.While (c, s)}
        | %"do" s:stmt %"while" "(" c:expression ")" ";"? {Stmt.Do (s, c)}
        | %"for" "(" i:stmt c:expression ";" s:stmt ")" b:stmt {
            Stmt.Seq (i, Stmt.While (c, Stmt.Seq (b, s)))
          }
        | %"if" s:ifPart {s}      
        | -"{" seq -"}";
        ifPart: "(" c:expression ")" t:stmt e:elsePart? {
          Stmt.If (c, t, match e with None -> Stmt.Skip | Some s -> s)
        };
        elsePart: %"else" s:stmt {s} | %"elif" s:ifPart {s};
        seq: ss:primary+ {let rec fold = function
                                         | [h]     -> h
                                         | h :: tl -> Stmt.Seq (h, fold tl)
                          in
                          fold ss
                         }
      )
      in
      primary s
    
    let parse s =
      Util.parse
        (object (self: 'self)
           inherit Matcher.t s
           inherit Util.Lexers.decimal s
           inherit Util.Lexers.lident ["if"; "else"; "elif"; "while"; "do"; "for"; "skip"; "with"; "length"] s 
           inherit Util.Lexers.skip [
                       Matcher.Skip.whitespaces " \t\n\r";
                       Matcher.Skip.lineComment "--";
                       Matcher.Skip.nestedComment "(*" "*)"
                   ] s
       end)
      (ostap (stmt -EOF))
    
  end

module Setup =
  struct

    exception Invalid_arg of string
    
    type 'a ref = 'a Stdlib.ref

    let ref = Stdlib.ref
    
    let mode  : [`Run | `Spec | `None] ref = ref `None
    let infile: string ref  = ref ""
    let input : State.t ref = ref State.empty

    let init () =
      let n = Array.length Sys.argv - 1 in
      let rec parse i =
        if i <= n
        then 
          match Sys.argv.(i) with
          | "--run"   -> mode := `Run; parse (i+1)
          | "--spec"  -> mode := `Spec; parse (i+1)
          | "--input" ->
             if i+1 <= n then (input := (match State.parse Sys.argv.(i+1) with
                                         | `Ok st     -> st
                                         | `Fail errs -> raise (Invalid_arg (Printf.sprintf "error parsing input state: %s" errs))
                                        );
                               parse (i+2))
             else raise (Invalid_arg "input specification expected after --input")
          | other -> infile := other; parse (i+1)
      in
      parse 1

    let getFile () =
      match !infile with
      | ""   -> raise (Invalid_arg "input file not specified")
      | file -> file
      
    let getMode () =
      match !mode with
      | `None -> raise (Invalid_arg "running mode not specified")
      | mode  -> mode
    
    let getInput () = !input
      
  end

let _ =
  try
    Setup.init ();
    (match Parser.parse @@ Util.read (Setup.getFile ()) with
     | `Ok stmt ->
        (match Setup.getMode () with
         | `Run  -> Printf.printf "%s\n" @@ show (State.Printable.t) (State.toPrintable @@ Stmt.eval (Setup.getInput ()) stmt)
         | `Spec -> 
            let (stmt', state') = Stmt.spec (Setup.getInput ()) stmt in
            let stmt'' = Stmt.residualizeAfter state' stmt' in
            Printf.printf "%s\n" @@ Formeat.to_string (Formeat.vbox (Formeat.cvbr (Stmt.pretty stmt'')))
        )
     | `Fail errs -> Printf.eprintf "ERROR: %s\n" errs
    )
  with
  | Setup.Invalid_arg        err -> Printf.eprintf "ERROR: %s\n" err
  | State.Undefined_variable x   -> Printf.eprintf "ERROR: undefined vriable \"%s\"\n" x
  | exn                          -> Printf.eprintf "%s\n" @@ Printexc.to_string exn
