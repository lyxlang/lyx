(*
 * SPDX-FileCopyrightText: 2025 Aljebriq <143266740+aljebriq@users.noreply.github.com>
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

open Ast

type program = decl list [@@deriving show]

(** [varMap] maps each variables to its associated numerical ids. *)
type varMap = (string, int) Hashtbl.t

(** [scope] represents an environment of variables, where lhs represents those
    of the current scope and rhs the parent one. *)
and scope = Scope of varMap * scope | Root

(** [id_map] maps each numerical ids to its associated variables. *)
let id_map : (int, string located) Hashtbl.t = Hashtbl.create 1000

(** [id] corresponds to a unique numerical identifier. *)
type id = int [@@deriving show]

(** [ids] represents a list of unique numeric identifiers that appear in the
    bind body. *)
and ids = id list [@@deriving show]

(** [params] represents a list of parameters given to a function *)
and params = Ast.param located list [@@deriving show]

(** [signature] represents the type signature of a function *)
and signature = Ast.ann [@@deriving show]

(** [bind] corresponds to a binding. It has the form `(Var, Parameters,
    Signature, Expr, fv(Expr)) located` *)
and bind = (id * params * signature * expr located * ids) located
[@@deriving show]

(** [scc] represents a strongly connected component (SCC) in a graph.

    - [AcyclicSCC] represents an acyclic SCC, containing a single `bind`
      element, e.g. [let x = y in x + y].
    - [CyclicSCC] represents a cyclic SCC, containing a list of `bind` elements,
      e.g. [let x = y; y = x in x + y].

    SCCs are used to identify groups of bindings that are mutually dependent and
    dead code. *)
type scc = AcyclicSCC of bind | CyclicSCC of bind list [@@deriving show]

(** [sccs] is a list of all SCCs found in the program. *)
(* let sccs : scc list ref = ref [] [@@deriving show] *)

let new_map () = Hashtbl.create 100

let counter = ref 0

let next_int () = incr counter ; !counter

(** [index_map] maps each node's id to its discovery index. *)
let index_map : (id, int) Hashtbl.t = Hashtbl.create 100

(** [lowlink_map] maps each node's id to the smallest index reachable from that
    node. *)
let lowlink_map : (id, int) Hashtbl.t = Hashtbl.create 100

(** [stack] is used to keep track of the current path in the depth-first search
    (DFS). *)
let stack : id Stack.t = Stack.create ()

(** [on_stack] keeps track of which nodes are currently on the stack. *)
let on_stack : (id, bool) Hashtbl.t = Hashtbl.create 100

(** [index] references the current discovery index, incremented with each new
    node visited. *)
let index = ref 0

(** [strongconnect] performs a depth-first search (DFS) starting from node [v],
    updating the discovery and low-link values, and identifying strongly
    connected components (SCCs).

    @param binds The list of bindings (nodes) in the graph.
    @param v The current node being visited.
    @param sccs The list of SCCs accumulated so far.
    @return The updated list of SCCs. *)
let rec strongconnect (binds : bind list) (v : bind) (sccs : scc list) =
  let {value= id, _, _, _, fvs; _} = v in
  Hashtbl.add index_map id !index ;
  Hashtbl.add lowlink_map id !index ;
  incr index ;
  Stack.push id stack ;
  Hashtbl.add on_stack id true ;
  List.fold_left
    (fun acc w ->
      if not (Hashtbl.mem index_map w) then (
        let w_bind =
          List.find (fun {value= id, _, _, _, _; _} -> id = w) binds
        in
        let acc = strongconnect binds w_bind acc in
        Hashtbl.replace lowlink_map id
          (min (Hashtbl.find lowlink_map id) (Hashtbl.find lowlink_map w)) ;
        acc )
      else if Hashtbl.find on_stack w then (
        Hashtbl.replace lowlink_map id
          (min (Hashtbl.find lowlink_map id) (Hashtbl.find index_map w)) ;
        acc )
      else acc )
    sccs fvs
  |> fun sccs ->
  if Hashtbl.find lowlink_map id = Hashtbl.find index_map id then (
    let scc = ref [] in
    let continue = ref true in
    while !continue do
      let w = Stack.pop stack in
      Hashtbl.replace on_stack w false ;
      let w_bind = List.find (fun {value= id, _, _, _, _; _} -> id = w) binds in
      scc := w_bind :: !scc ;
      if w = id then continue := false
    done ;
    match !scc with
    | [single_bind] ->
        AcyclicSCC single_bind :: sccs
    | _ ->
        CyclicSCC !scc :: sccs )
  else sccs

(** [tarjans_algorithm] initializes the process_expression and iterates over all
    nodes (binds) to find and add SCCs to the global [sccs] list.

    @param binds The list of bindings (nodes) in the graph.
    @return The list of SCCs. *)
let tarjans_algorithm (binds : bind list) =
  List.fold_left
    (fun sccs bind ->
      let {value= id, _, _, _, _; _} = bind in
      if not (Hashtbl.mem index_map id) then strongconnect binds bind sccs
      else sccs )
    [] binds

(** [rebuild_let_binds] Rebuilds a list of strongly connected components (SCCs)
    into a nested let-binding expression.

    @param sccs
      A list of SCCs, where each SCC can be either acyclic or cyclic. Each SCC
      contains bindings.
    @param ret The final expression to be wrapped by the let-bindings.
    @return An AST expression with nested let-bindings. *)
let rec rebuild_let_binds (sccs : scc list) (ret : Ast.expr located) =
  match sccs with
  | [] ->
      ret.value
  | AcyclicSCC {value= id, params, signature, body, _; loc} :: sccs' ->
      let name = Hashtbl.find id_map id in
      let bind : Ast.bind = {id= name; params; signature; body} in
      ELet
        { bind= {value= bind; loc}
        ; body= {value= rebuild_let_binds sccs' ret; loc= ret.loc} }
  | CyclicSCC binds :: sccs' ->
      let binds =
        List.map
          (fun {value= id, params, signature, body, _; loc} : Ast.bind located
             ->
            let name = Hashtbl.find id_map id in
            {value= {id= name; params; signature; body}; loc} )
          binds
      in
      ELets {binds; body= {value= rebuild_let_binds sccs' ret; loc= ret.loc}}

(** [rebuild_decl_binds sccs] Rebuilds the declaration binds by analyzing the
    occurrences in the given sccs.

    @param sccs The list of strongly connected components.
    @return The list of Ast.decl after rebuilding the declaration binds. *)
let rec rebuild_decl_binds (sccs : scc list) =
  let process_bind {value= id, params, signature, body, _; _} =
    let id =
      try
        let name = Hashtbl.find id_map id in
        L name.value
      with Not_found -> Wildcard
    in
    ( {id= {value= id; loc= {start= 0; fin= 0}}; params; signature; body}
      : tlbind )
  in
  match sccs with
  | [] ->
      []
  | AcyclicSCC bind :: sccs' ->
      Decl (process_bind bind) :: rebuild_decl_binds sccs'
  | CyclicSCC binds :: sccs' ->
      let binds' = List.map process_bind binds in
      Decls binds' :: rebuild_decl_binds sccs'

let rec find_identifier scope name =
  match scope with
  | Root ->
      None
  | Scope (map, parent) -> (
    match Hashtbl.find_opt map name with
    | Some id ->
        Some id
    | None ->
        find_identifier parent name )

let add_identifier scope name id =
  match scope with
  | Scope (map, _) ->
      Hashtbl.replace map name id
  | Root ->
      assert false

let rec analyze_pattern scope pattern =
  match pattern with
  | PInt _ | PFloat _ | PString _ | PBool _ ->
      ()
  | POLID olid | PTail olid -> (
    match olid.value with
    | Wildcard ->
        ()
    | L name ->
        let id_int = next_int () in
        add_identifier scope name id_int ;
        Hashtbl.add id_map id_int {loc= olid.loc; value= name} )
  | PConstructor {params; _} ->
      List.iter (fun pattern -> analyze_pattern scope pattern.value) params
  | PTuple lst | PList lst | PListSpd lst ->
      List.iter (fun pattern -> analyze_pattern scope pattern.value) lst.value
  | POr {l; r} ->
      analyze_pattern scope l.value ;
      analyze_pattern scope r.value
  | PParenthesized p ->
      analyze_pattern scope p.value

let rec analyze_param scope param =
  match param with
  | PRLID lid ->
      let id_int = next_int () in
      add_identifier scope lid.value id_int ;
      Hashtbl.add id_map id_int lid
  | PRTuple lst ->
      List.iter (fun param -> analyze_param scope param.value) lst

let rec process_expression (scope : scope) (expr : Ast.expr located) =
  let {value; loc} = expr in
  match value with
  | EParenthesized e ->
      process_expression scope e
  | ETyped {body; _} ->
      process_expression scope body
  | EUID _
  | ETuple _
  | EList _
  | EUnit
  | EBool _
  | EString _
  | EFloat _
  | EInt _ ->
      ([], expr)
  | ELID {value; _} -> (
    match find_identifier scope value with
    | Some id ->
        ([id], expr)
    | None ->
        assert false )
  | ELambda {params; body} ->
      (* Initiate a fresh new scope for parameters variables *)
      let scope' = Scope (new_map (), scope) in
      (* Record all identifiers in the current scope and add the corresponding
         reference in id_map *)
      List.iter (fun param -> analyze_param scope' param.value) params ;
      (* Process_expression the body of the lambda and get the free variables *)
      let fv, body = process_expression scope' body in
      (* Get the variables of the current scope *)
      let varMap =
        match scope' with Scope (map, _) -> map | Root -> assert false
      in
      (* Collects the identifiers of lambda parameters (which are therefore
         bind) *)
      let ids = Hashtbl.fold (fun _ id acc -> id :: acc) varMap [] in
      (* Remove the lambda parameters from the free variables *)
      (List.filter (fun id -> not (List.mem id ids)) fv, body)
  | EApp {fn; arg} ->
      let fn_fv, fn = process_expression scope fn in
      let arg_fv, arg = process_expression scope arg in
      (List.append fn_fv arg_fv, {value= EApp {fn; arg}; loc})
  | ELets {binds; body} ->
      let scope' = Scope (new_map (), scope) in
      (* Add the identifiers of the bindings to the current scope *)
      let ids =
        List.fold_left
          ( fun acc {value= {id; _}; _} ->
              let id_int = next_int () in
              add_identifier scope' id.value id_int ;
              Hashtbl.add id_map id_int id ;
              acc @ [id_int]
            : int list -> Ast.bind located -> int list )
          [] binds
      in
      (* Produce the list of binding : (VarId, e, fv(e)) *)
      let binds : bind list =
        List.fold_left
          (fun acc (bind : Ast.bind located) ->
            (* Initiate a fresh new scope for the bindings *)
            let scope'' = Scope (new_map (), scope') in
            List.iter
              (fun param -> analyze_param scope'' param.value)
              bind.value.params ;
            let fv, bind_body = process_expression scope'' bind.value.body in
            let index = List.length acc in
            { value=
                ( List.nth ids index
                , bind.value.params
                , bind.value.signature
                , bind_body
                , fv )
            ; loc= bind.loc }
            :: acc )
          [] binds
      in
      let bindings = rebuild_let_binds (tarjans_algorithm binds) body in
      let body_fv =
        List.filter
          (fun id -> not (List.mem id ids))
          (fst (process_expression scope' body))
      in
      (* Remove binds variables from the free variables in the body of all
         binds *)
      let cleaned_binds =
        List.map
          (fun {value= id, _, _, body, fv; _} ->
            (id, body, List.filter (fun id -> not (List.mem id ids)) fv) )
          binds
      in
      (* Get the free variables of the let-binding by removing duplicity *)
      let fv =
        List.sort_uniq compare
          (List.concat (List.map (fun (_, _, fv) -> fv) cleaned_binds) @ body_fv)
      in
      (fv, {value= bindings; loc= {start= 0; fin= 0}})
  | ELet _ ->
      assert false
  | EBoolOp {l; r; _}
  | ECompOp {l; r; _}
  | EPipeOp {l; r}
  | EConcatOp {l; r}
  | EAddOp {l; r; _}
  | EMulOp {l; r; _}
  | EExpOp {l; r}
  | EBitOp {l; r; _} ->
      let l_fv, l = process_expression scope l in
      let r_fv, r = process_expression scope r in
      let new_bin_op =
        match value with
        | EBoolOp {op; _} ->
            EBoolOp {l; r; op}
        | ECompOp {op; _} ->
            ECompOp {l; r; op}
        | EPipeOp _ ->
            EPipeOp {l; r}
        | EConcatOp _ ->
            EConcatOp {l; r}
        | EAddOp {op; _} ->
            EAddOp {l; r; op}
        | EMulOp {op; _} ->
            EMulOp {l; r; op}
        | EExpOp _ ->
            EExpOp {l; r}
        | EBitOp {op; _} ->
            EBitOp {l; r; op}
        | EParenthesized _
        | ETyped _
        | EUnOp _
        | EApp _
        | ELambda _
        | EMatch _
        | ELets _
        | ELet _
        | EIf _
        | EUID _
        | ELID _
        | ETuple _
        | EList _
        | EUnit
        | EBool _
        | EString _
        | EFloat _
        | EInt _ ->
            assert false
      in
      (List.append l_fv r_fv, {value= new_bin_op; loc})
  | EUnOp {body; _} ->
      process_expression scope body
  | EMatch {ref; cases} ->
      let ref_fv, ref_body = process_expression scope ref in
      let cases_fv, cases' =
        List.fold_left_map
          (fun acc case ->
            match case.value with
            | Case {pat; body} ->
                let scope' = Scope (new_map (), scope) in
                analyze_pattern scope' pat.value ;
                let bind_fv, bind_body = process_expression scope' body in
                ( bind_fv @ acc
                , {value= Case {pat; body= bind_body}; loc= case.loc} )
            | CaseGuard {pat; guard; body} ->
                let scope' = Scope (new_map (), scope) in
                analyze_pattern scope' pat.value ;
                let guard_fv, guard_body = process_expression scope' guard in
                let bind_fv, bind_body = process_expression scope' body in
                ( List.append guard_fv bind_fv @ acc
                , { value= CaseGuard {pat; guard= guard_body; body= bind_body}
                  ; loc= case.loc } ) )
          [] cases
      in
      let fv = List.sort_uniq compare (ref_fv @ cases_fv) in
      (fv, {value= EMatch {ref= ref_body; cases= cases'}; loc})
  | EIf {predicate; truthy; falsy} ->
      let pred_fv, pred = process_expression scope predicate in
      let truthy_fv, truthy = process_expression scope truthy in
      let falsy_fv, falsy = process_expression scope falsy in
      ( List.append pred_fv (List.append truthy_fv falsy_fv)
      , {value= EIf {predicate= pred; truthy; falsy}; loc= expr.loc} )

let process_program (program : Ast.program) =
  let scope = Scope (new_map (), Root) in
  let binds : bind list =
    List.fold_left
      (fun acc decl ->
        match decl.value with
        | Decl {id; body; params; signature} ->
            let id_int = next_int () in
            ( match id.value with
            | Wildcard ->
                ()
            | L name ->
                Hashtbl.add id_map id_int {loc= id.loc; value= name} ;
                add_identifier scope name id_int ) ;
            let scope' = Scope (new_map (), scope) in
            List.iter (fun param -> analyze_param scope' param.value) params ;
            { value=
                ( id_int
                , params
                , signature
                , body
                , fst (process_expression scope' body) )
            ; loc= decl.loc }
            :: acc
        | Decls _ | DeclADT _ | DeclAlias _ | Comment _ ->
            acc )
      [] program
  in
  tarjans_algorithm binds

let analyze program = process_program program |> rebuild_decl_binds

let debug_output output = Format.printf "%a@." pp_program output
