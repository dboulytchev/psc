open GT
open Ostap

module Expr =
  struct

    @type t = 
    | Var   of int    * string (* var: the De Bruijn index, the name (for debugging) *) 
    | Cnstr of string * t list (* constructor: the name, the arguments               *)
    | App   of t * t           (* application                                        *)
    | Lam   of string * t      (* abstraction: the name (for debugging), the body    *)
    with show, html

    ostap (
      parse[env]: l:primary[env]+ {
        let h::t = l in
	List.fold_left (fun e a -> App (e, a)) h t
      };
      primary[env]: 
        x:LIDENT {Var (env#index x, x)}
      | c:UIDENT a:(-"(" !(Util.list (parse env)) -")")? {
          match a with None -> Cnstr (c, []) | Some a -> Cnstr (c, a)
        }
      | "\\" x:LIDENT "->" e:parse[env#put x] {Lam (x, e)}
      | -"(" parse[env] -")"
    )

    module S = Map.Make (String)

    class env =
      object
	val m = S.empty
	val i = 0
	method put   (x : string) = {< m = S.add x i m; i = i+1 >}
	method index (x : string) = 
	  try i - S.find x m 
          with Not_found -> 
            failwith (Printf.sprintf "Name %s not found" x)
      end

  end

module Env =
  struct

    type t = string -> Expr.t option

    let empty _ = None

    let append env name expr =
      match env name with
      | None   -> (fun s -> if s = name then Some expr else env s)
      | Some _ -> failwith (Printf.sprintf "Duplicate definition for %s" name)

    ostap (
      parse: items[empty];
      items[env]: env':item[env] env'':(%"and" items[env'])? {
	  match env'' with None -> env' | Some env'' -> env''
        };
      item[env]: 
        name:LIDENT args:LIDENT* "=" def:!(Expr.parse @@ List.fold_left (fun a n -> a#put n) (new Expr.env) args) {
          append env name def
        } 
    )

  end

type t = Expr.t * Env.t

ostap (
  parse: expr:!(Expr.parse (new Expr.env)) env:(%"where" !(Env.parse))? {
    expr, match env with None -> Env.empty | Some env -> env
  }
)

let parse infile =
  let s = Util.read infile in
  Util.parse
    (object
       inherit Matcher.t s
       inherit Util.Lexers.lident ["where"; "and"] s
       inherit Util.Lexers.uident [] s
       inherit Util.Lexers.decimal s
       inherit Util.Lexers.skip [
	 Matcher.Skip.whitespaces " \t\n";
	 Matcher.Skip.lineComment "--";
	 Matcher.Skip. nestedComment "(*" "*)"
       ] s
     end
    )
    (ostap (parse -EOF))


