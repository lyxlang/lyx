open Ast

type env = (string, typing) Hashtbl.t

let type_env : env = Hashtbl.create 100

type context = {mutable next: int; type_env: env}

let substitution : env = Hashtbl.create 100

let empty_substitution : env = Hashtbl.create 0

let rec apply_substitution_to_type sub = function
  | TInt t ->
      TInt t
  | TFloat t ->
      TFloat t
  | TBool t ->
      TBool t
  | TString t ->
      TString t
  | TUnit t ->
      TUnit t
  | TConstructor (span, {id; typing}) -> (
    match typing with
    | Some t ->
        TConstructor
          (span, {id; typing= Some (apply_substitution_to_type sub t)})
    | None ->
        TConstructor (span, {id; typing= None}) )
  | TPolymorphic (span, t) -> (
    match Hashtbl.find_opt sub t with
    | Some s ->
        s
    | None ->
        TPolymorphic (span, t) )
  | TTuple (span, t) ->
      TTuple (span, List.map (apply_substitution_to_type sub) t)
  | TList (span, t) ->
      TList (span, apply_substitution_to_type sub t)
  | TFunction (span, {l; r}) ->
      TFunction
        ( span
        , { l= apply_substitution_to_type sub l
          ; r= apply_substitution_to_type sub r } )

let add_to_context ctx name typing =
  let new_env = Hashtbl.copy ctx.type_env in
  Hashtbl.add new_env name typing ;
  {ctx with type_env= new_env}

let new_polymorphic ctx =
  let new_id = ctx.next in
  ctx.next <- ctx.next + 1 ;
  let dummy_span = (Lexing.dummy_pos, Lexing.dummy_pos) in
  TPolymorphic (dummy_span, "p" ^ string_of_int new_id)

let rec add_parameter_to_context ctx = function
  | ALID (_, id) ->
      let new_type = new_polymorphic ctx in
      let new_ctx = add_to_context ctx id new_type in
      (new_ctx, new_type)
  | ATuple (span, parameters) ->
      let _, types =
        List.fold_left
          (fun (ctx, acc) param ->
            let new_ctx, new_type = add_parameter_to_context ctx param in
            (new_ctx, new_type :: acc) )
          (ctx, []) parameters
      in
      let tuple_type = TTuple (span, List.rev types) in
      let new_ctx =
        add_to_context ctx ("t" ^ string_of_int ctx.next) tuple_type
      in
      (new_ctx, tuple_type)

let rec infer ctx = function[@warning "-4"]
  | EInt (span, _) ->
      (TInt span, empty_substitution)
  | EFloat (span, _) ->
      (TFloat span, empty_substitution)
  | EBool (span, _) ->
      (TBool span, empty_substitution)
  | EString (span, _) ->
      (TString span, empty_substitution)
  | EUnit span ->
      (TUnit span, empty_substitution)
  | ELID (_, id) -> (
    match Hashtbl.find_opt ctx.type_env id with
    | Some t ->
        (t, empty_substitution)
    | None ->
        failwith ("Unbound identifier: " ^ id) )
  | ELambda _ ->
      failwith "Lambdas should be desugared before type checking."
  | EDesugaredLambda (span, {parameter; body}) ->
      let new_ctx, new_type = add_parameter_to_context ctx parameter in
      let body_type, sub = infer new_ctx body in
      let inferred_type =
        TFunction
          (span, {l= apply_substitution_to_type sub new_type; r= body_type})
      in
      (inferred_type, sub)
  | _ ->
      failwith "Not implemented"
