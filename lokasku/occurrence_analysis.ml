(*
 * SPDX-FileCopyrightText: 2025 Łukasz Bartkiewicz <lukasku@proton.me>
 *
 * SPDX-License-Identifier: GPL-3.0-only
 *)

open Ast

(** [variable_map] maps each variables to its associated numerical ids. *)
type variable_map = (string, int) Hashtbl.t

(** [scope] represents an environment of variables, where lhs represents those
    of the current scope and rhs the parent one. *)
and scope = Scope of variable_map * scope | Root

(** [id_map] maps each numerical ids to its associated variables. *)
let id_map : (int, string) Hashtbl.t = Hashtbl.create 1000

(** [id] corresponds to a unique numerical identifier. *)
type id = int [@@deriving show {with_path= false}]

(** [ids] represents a list of unique numeric identifiers that appear in the
    bind body. *)
and ids = id list [@@deriving show {with_path= false}]

(** [params] represents a list of parameters given to a function *)
and parameters = parameter list [@@deriving show {with_path= false}]

(** [bind] corresponds to a binding. It has the form `(Var, Parameters,
    Signature, Expr, fv(Expr))` *)
and bind = span * id * parameters * signature * expression * ids
[@@deriving show {with_path= false}]

(** [scc] represents a strongly connected component (SCC) in a graph.

    - [AcyclicSCC] represents an acyclic SCC, containing a single `bind`
      element, e.g. [let x = y in x + y].
    - [CyclicSCC] represents a cyclic SCC, containing a list of `bind` elements,
      e.g. [let x = y; y = x in x + y].

    SCCs are used to identify groups of bindings that are mutually dependent and
    dead code. *)
type scc = AcyclicSCC of bind | CyclicSCC of bind list
[@@deriving show {with_path= false}]

let new_map () = Hashtbl.create 100

let counter = ref 0

let next_int () = incr counter ; !counter

(** [index_map] maps each node’s id to its discovery index. *)
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
  let _, id, _, _, _, fvs = v in
  Hashtbl.add index_map id !index ;
  Hashtbl.add lowlink_map id !index ;
  incr index ;
  Stack.push id stack ;
  Hashtbl.add on_stack id true ;
  List.fold_left
    (fun acc w ->
      if not (Hashtbl.mem index_map w) then
        let w_bind_opt =
          List.find_opt (fun (_, id, _, _, _, _) -> id = w) binds
        in
        match w_bind_opt with
        | Some w_bind ->
            let acc = strongconnect binds w_bind acc in
            Hashtbl.replace lowlink_map id
              (min (Hashtbl.find lowlink_map id) (Hashtbl.find lowlink_map w)) ;
            acc
        | None ->
            acc
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
      let w_bind = List.find (fun (_, id, _, _, _, _) -> id = w) binds in
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
      let _, id, _, _, _, _ = bind in
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
let rec rebuild_let_binds (sccs : scc list) (ret : expression) =
  match sccs with
  | [] ->
      ret
  | AcyclicSCC (span, id, _, signature, body, _) :: sccs' ->
      let name = Hashtbl.find id_map id in
      let bind : binding = {span; id= name; signature; body} in
      ELet (span, {bindings= [bind]; body= rebuild_let_binds sccs' ret})
  | CyclicSCC binds :: sccs' ->
      let bindings =
        List.map
          (fun (span, id, _, signature, body, _) ->
            let name = Hashtbl.find id_map id in
            {span; id= name; signature; body} )
          binds
      in
      let span, _, _, _, _, _ = List.hd binds in
      ELet (span, {bindings; body= rebuild_let_binds sccs' ret})

(** [rebuild_decl_binds sccs] Rebuilds the declaration binds by analyzing the
    occurrences in the given sccs.

    @param sccs The list of strongly connected components.
    @return The list of declarations after rebuilding. *)
let rec rebuild_decl_binds (sccs : scc list) =
  let process_bind (span, id, _, signature, body, _) =
    let name = try Hashtbl.find id_map id with Not_found -> "_" in
    {span; id= name; signature; body}
  in
  match sccs with
  | [] ->
      []
  | AcyclicSCC bind :: sccs' ->
      let span, _, _, _, _, _ = bind in
      DValueBinding (span, process_bind bind) :: rebuild_decl_binds sccs'
  | CyclicSCC binds :: sccs' ->
      let span, _, _, _, _, _ = List.hd binds in
      let decls =
        List.map (fun bind -> DValueBinding (span, process_bind bind)) binds
      in
      decls @ rebuild_decl_binds sccs'

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
  | PLID (_, lid) ->
      if not (String.starts_with lid ~prefix:"_") then (
        let id_int = next_int () in
        add_identifier scope lid id_int ;
        Hashtbl.add id_map id_int lid )
  | PConstructor (_, {id= _; pattern}) ->
      Option.iter (fun p -> analyze_pattern scope p) pattern
  | PTuple (_, lst) | PList (_, lst) | PListSpread (_, lst) ->
      List.iter (fun pattern -> analyze_pattern scope pattern) lst
  | POr (_, {l; r}) ->
      analyze_pattern scope l ; analyze_pattern scope r

let rec analyze_parameter scope param =
  match param with
  | ALID (_, lid) ->
      if not (String.starts_with lid ~prefix:"_") then (
        let id_int = next_int () in
        add_identifier scope lid id_int ;
        Hashtbl.add id_map id_int lid )
  | ATuple (_, lst) ->
      List.iter (fun param -> analyze_parameter scope param) lst

let rec process_expression (scope : scope) (expr : expression) =
  match expr with
  | EUnit _ | EInt _ | EFloat _ | EBool _ | EString _ ->
      ([], expr)
  | EConstructor _ ->
      ([], expr) (* Constructors don’t contribute to variable references *)
  | ELID (_, lid) -> (
    match find_identifier scope lid with
    | Some id ->
        ([id], expr)
    | None ->
        ([], expr)
        (* External reference or undefined - semantic analysis will catch
           this *) )
  | ETuple (span, exprs) | EList (span, exprs) ->
      let fvs, exprs' =
        List.split (List.map (process_expression scope) exprs)
      in
      ( List.concat fvs
      , if expr = ETuple (span, exprs) then ETuple (span, exprs')
        else EList (span, exprs') )
  | ELambda (span, {parameters; body}) ->
      (* Create a new scope for lambda parameters *)
      let scope' = Scope (new_map (), scope) in
      List.iter (fun param -> analyze_parameter scope' param) parameters ;
      (* Process the body and get free variables *)
      let fv, body' = process_expression scope' body in
      (* Get bound variables from the lambda parameters *)
      let var_map =
        match scope' with Scope (map, _) -> map | Root -> assert false
      in
      let bound_ids = Hashtbl.fold (fun _ id acc -> id :: acc) var_map [] in
      (* Remove bound variables from free variables *)
      let free_vars = List.filter (fun id -> not (List.mem id bound_ids)) fv in
      (free_vars, ELambda (span, {parameters; body= body'}))
  | EApplication (span, {body; argument}) ->
      let body_fvs, body' = process_expression scope body in
      let arg_fvs, arg' = process_expression scope argument in
      ( List.append body_fvs arg_fvs
      , EApplication (span, {body= body'; argument= arg'}) )
  | ELet (_, {bindings; body}) ->
      let scope' = Scope (new_map (), scope) in
      (* Add binding IDs to current scope *)
      let ids =
        List.map
          (fun ({id; _} : binding) ->
            let id_int = next_int () in
            add_identifier scope' id id_int ;
            Hashtbl.add id_map id_int id ;
            id_int )
          bindings
      in
      (* Process each binding *)
      let binds : bind list =
        List.mapi
          (fun i {span; id= _; signature; body} ->
            (* Create new scope for binding body *)
            let scope'' = Scope (new_map (), scope') in
            let fv, body' = process_expression scope'' body in
            (span, List.nth ids i, [], signature, body', fv) )
          bindings
      in
      (* Process the body *)
      let body_fvs, body' = process_expression scope' body in
      (* Filter out IDs defined in the let bindings *)
      let filtered_fvs =
        List.filter (fun id -> not (List.mem id ids)) body_fvs
      in
      (* Get free variables from all bindings, removing any that are defined in
         this let *)
      let binding_fvs =
        List.map
          (fun (_, _, _, _, _, fvs) ->
            List.filter (fun id -> not (List.mem id ids)) fvs )
          binds
      in
      (* Combine all free variables *)
      let all_fvs =
        List.sort_uniq compare (filtered_fvs @ List.concat binding_fvs)
      in
      (* Rebuild the let expression using Tarjan’s algorithm *)
      let rebuilt_expr = rebuild_let_binds (tarjans_algorithm binds) body' in
      (all_fvs, rebuilt_expr)
  | EBinaryOperation (span, {l; operator; r}) ->
      let l_fvs, l' = process_expression scope l in
      let r_fvs, r' = process_expression scope r in
      (l_fvs @ r_fvs, EBinaryOperation (span, {l= l'; operator; r= r'}))
  | EUnaryOperation (span, {operator; body}) ->
      let fvs, body' = process_expression scope body in
      (fvs, EUnaryOperation (span, {operator; body= body'}))
  | EIf (span, {predicate; truthy; falsy}) ->
      let pred_fvs, pred' = process_expression scope predicate in
      let true_fvs, true' = process_expression scope truthy in
      let false_fvs, false' = process_expression scope falsy in
      ( pred_fvs @ true_fvs @ false_fvs
      , EIf (span, {predicate= pred'; truthy= true'; falsy= false'}) )
  | EMatch (span, {body; cases}) ->
      let body_fvs, body' = process_expression scope body in
      let case_results =
        List.map
          (fun (case : case) ->
            let scope' = Scope (new_map (), scope) in
            analyze_pattern scope' case.pattern ;
            let guard_fvs, guard' =
              match case.guard with
              | Some guard ->
                  let fvs, expr = process_expression scope' guard in
                  (fvs, Some expr)
              | None ->
                  ([], None)
            in
            let body_fvs, body' = process_expression scope' case.body in
            ( guard_fvs @ body_fvs
            , { span= case.span
              ; pattern= case.pattern
              ; guard= guard'
              ; body= body' } ) )
          cases
      in
      let case_fvs, cases' = List.split case_results in
      ( body_fvs @ List.concat case_fvs
      , EMatch (span, {body= body'; cases= cases'}) )
  | EExpression (span, {body; signature}) ->
      let fvs, body' = process_expression scope body in
      (fvs, EExpression (span, {body= body'; signature}))

let process_program (program : program) =
  let scope = Scope (new_map (), Root) in
  (* Collect function and value definitions *)
  let binds : bind list =
    List.fold_left
      (fun acc decl ->
        match decl with
        | DFunctionDefinition (span, {id; parameters; signature; body}) ->
            let id_int = next_int () in
            add_identifier scope id id_int ;
            Hashtbl.add id_map id_int id ;
            let scope' = Scope (new_map (), scope) in
            List.iter (fun param -> analyze_parameter scope' param) parameters ;
            let fvs, body' = process_expression scope' body in
            (span, id_int, parameters, signature, body', fvs) :: acc
        | DValueBinding (span, binding) ->
            let id_int = next_int () in
            add_identifier scope binding.id id_int ;
            Hashtbl.add id_map id_int binding.id ;
            let fvs, body' = process_expression scope binding.body in
            (span, id_int, [], binding.signature, body', fvs) :: acc
        | DADTDefinition _ | DTypeDefinition _ | DComment _ ->
            acc )
      [] program
  in
  tarjans_algorithm binds

let analyze program = process_program program |> rebuild_decl_binds
