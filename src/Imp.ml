open Ostap
open GT

module State =
struct
  module M = Map.Make (String)
      
  type t = int M.t

  let toPrintable s = M.bindings s

  let empty = M.empty
  let set m name value = M.add name value m

  exception Undefined_variable of string
      
  let get m name =
    match M.find_opt name m with
    | Some x -> x
    | None   -> raise (Undefined_variable name)
  
  module Printable =
  struct
    @type t = (string * int) list with show
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
        bs:!(Util.list)[ostap (LIDENT -"=" DECIMAL)] EOF {
            List.fold_left (fun st (x, v) -> set st x v) empty bs 
        }      
      ))
  
end

module Expr =
struct
  
  @type t =
  | Var   of string
  | Const of int    
  | Binop of string * t * t
  with show

  let evalBinop xv yv =
    let i2b x = if x <> 0 then 1 else 0     in
    let b2i   = function true -> 1 | _ -> 0 in
    function
    | "!!" -> i2b (i2b xv + i2b yv)
    | "&&" -> i2b (i2b xv * i2b yv)
    | "==" -> b2i (xv =  yv)
    | "!=" -> b2i (xv <> yv)
    | "<=" -> b2i (xv <= yv)
    | "<"  -> b2i (xv <  yv)
    | ">=" -> b2i (xv >= yv)
    | ">"  -> b2i (xv >  yv)
    | "+"  -> xv + yv
    | "-"  -> xv - yv
    | "*"  -> xv * yv
    | "/"  -> xv / yv
    | "%"  -> xv mod yv

  let rec eval state = function
  | Var   x -> State.get state x
  | Const x -> x
  | Binop (op, x, y) ->
    evalBinop (eval state x) (eval state y) op
    
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

  let rec eval state = function
  | Assn (x, e)  -> State.set state x (Expr.eval state e)
  | Seq  (l, r)  -> eval (eval state l) r
  | Skip         -> state
  | If (c, t, e) -> eval state (if Expr.eval state c <> 0 then t else e)
  | While (c, b) as w ->
     if Expr.eval state c = 0
     then state
     else eval (eval state b) w                 
  | Do (b, c) as w -> eval state (Seq (b, While (c, b)))      
                     
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
    let primary = ostap (
        x:LIDENT  {Expr.Var   x}
    |   c:DECIMAL {Expr.Const c}          
    | -"(" expression -")" 
    )
    in  
    Util.expr (fun x -> x) ops primary s

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
         inherit Util.Lexers.lident ["if"; "else"; "elif"; "while"; "do"; "for"; "skip"] s 
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
         | `Spec -> Printf.eprintf "ERROR: specialization not yet implemented\n"
        )
     | `Fail errs -> Printf.eprintf "ERROR: %s\n" errs
    )
  with
  | Setup.Invalid_arg        err -> Printf.eprintf "ERROR: %s\n" err
  | State.Undefined_variable x   -> Printf.eprintf "ERROR: undefined vriable \"%s\"\n" x
  | exn                          -> Printf.eprintf "%s\n" @@ Printexc.to_string exn
