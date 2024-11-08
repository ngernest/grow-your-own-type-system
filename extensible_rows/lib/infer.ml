open Expr

let current_id = ref 0

(** Increments the counter for type variables *)
let next_id () =
  let id = !current_id in
  current_id := id + 1;
  id

let reset_id () = current_id := 0

(** Generates a free type variable at the current [level] *)
let new_var (level : level) : ty = TVar (ref (Unbound (next_id (), level)))

(** Generates a quantified type variable *)
let new_gen_var () : ty = TVar (ref (Generic (next_id ())))

exception Error of string

let error msg = raise (Error msg)

(** A module for environments (map from variable names to types) *)
module Env = struct
  module StringMap = Map.Make (String)

  (** Environments are maps from [string -> ty] *)
  type t = ty StringMap.t

  (** The empty environment *)
  let empty : t = StringMap.empty

  (** Extends the environment with a new variable binding [name : ty] *)
  let extend env name ty = StringMap.add name ty env

  (** Lookups the type of [name] in the environment *)
  let lookup env name = StringMap.find name env
end

(** Checks that the type variable [tvar_id] being unified 
    doesn't occur within the type [ty] it is being unified with.
		- This prevents us from inferring recursive types. 
		- This function also updates the levels of type variables within 
		  [ty], ensuring that [ty] becomes generalized correctly *)
let occurs_check_adjust_levels (tvar_id : id) (tvar_level : level) (ty : ty) :
  unit =
  let rec f : ty -> unit = function
    | TVar { contents = Link ty } -> f ty
    | TVar { contents = Generic _ } -> assert false
    | TVar ({ contents = Unbound (other_id, other_level) } as other_tvar) ->
      if other_id = tvar_id then error "recursive types"
      else if other_level > tvar_level then
        other_tvar := Unbound (other_id, tvar_level)
      else ()
    | TApp (ty, ty_arg_list) ->
      f ty;
      List.iter f ty_arg_list
    | TArrow (param_ty_list, return_ty) ->
      List.iter f param_ty_list;
      f return_ty
    | TRecord row -> f row
    | TRowExtend (label, field_ty, row) ->
      f field_ty;
      f row
    | TConst _ | TRowEmpty -> () in
  f ty

(** Unifies two types *)
let rec unify (ty1 : ty) (ty2 : ty) : unit =
  if ty1 == ty2 then ()
  else
    match (ty1, ty2) with
    | TConst name1, TConst name2 when name1 = name2 -> ()
    | TApp (ty1, ty_arg_list1), TApp (ty2, ty_arg_list2) ->
      unify ty1 ty2;
      List.iter2 unify ty_arg_list1 ty_arg_list2
    | TArrow (param_ty_list1, return_ty1), TArrow (param_ty_list2, return_ty2)
      ->
      List.iter2 unify param_ty_list1 param_ty_list2;
      unify return_ty1 return_ty2
    | TVar { contents = Link ty1 }, ty2 | ty1, TVar { contents = Link ty2 } ->
      unify ty1 ty2
    | TVar { contents = Unbound (id1, _) }, TVar { contents = Unbound (id2, _) }
      when id1 = id2 ->
      assert false
      (* There is only a single instance of a particular type variable. *)
    | TVar ({ contents = Unbound (id, level) } as tvar), ty
    | ty, TVar ({ contents = Unbound (id, level) } as tvar) ->
      occurs_check_adjust_levels id level ty;
      tvar := Link ty
    | TRecord row1, TRecord row2 -> unify row1 row2
    | TRowEmpty, TRowEmpty -> ()
    (* If a row extension [<a : t | r>] is unified with another row,
       [rewrite_row] rewrites the second row by searching for the first field
       with label [a], and unifies its type with the field's type [t] *)
    | TRowExtend (label1, field_ty1, rest_row1), (TRowExtend _ as row2) ->
      let rest_row1_tvar_ref_option =
        match rest_row1 with
        | TVar ({ contents = Unbound _ } as tvar_ref) -> Some tvar_ref
        | _ -> None in
      let rest_row2 = rewrite_row row2 label1 field_ty1 in
      (match rest_row1_tvar_ref_option with
      | Some { contents = Link _ } -> error "recursive row types"
      | _ -> ());
      unify rest_row1 rest_row2
    | _, _ ->
      error
        ("can't unify types " ^ string_of_ty ty1 ^ " and " ^ string_of_ty ty2)

(** Rewrites a row variable [row2] by searching for the first field in 
    [row2] that is equal to [label1], and unifies the field's type with 
		the field type [field_ty1] *)
and rewrite_row (row2 : row) (label1 : name) (field_ty1 : ty) : ty =
  match row2 with
  | TRowEmpty -> error ("row does not contain label " ^ label1)
  | TRowExtend (label2, field_ty2, rest_row2) when label2 = label1 ->
    unify field_ty1 field_ty2;
    rest_row2
  | TRowExtend (label2, field_ty2, rest_row2) ->
    TRowExtend (label2, field_ty2, rewrite_row rest_row2 label1 field_ty1)
  | TVar { contents = Link row2 } -> rewrite_row row2 label1 field_ty1
  | TVar ({ contents = Unbound (id, level) } as tvar) ->
    let rest_row2 = new_var level in
    let ty2 = TRowExtend (label1, field_ty1, rest_row2) in
    tvar := Link ty2;
    rest_row2
  | _ -> error "row type expected"

(** Takes a [level] [l] and a type [tau], and turns all type variables 
    within [tau] that have level > [l] into universally-quantified 
		type variables *)
let rec generalize (l : level) (tau : ty) : ty =
  match tau with
  | TVar { contents = Unbound (id, other_level) } when other_level > l ->
    TVar (ref (Generic id))
  | TApp (ty, ty_arg_list) ->
    TApp (generalize l ty, List.map (generalize l) ty_arg_list)
  | TArrow (param_ty_list, return_ty) ->
    TArrow (List.map (generalize l) param_ty_list, generalize l return_ty)
  | TVar { contents = Link ty } -> generalize l ty
  | TRecord row -> TRecord (generalize l row)
  | TRowExtend (label, field_ty, row) ->
    TRowExtend (label, generalize l field_ty, generalize l row)
  | ( TVar { contents = Generic _ }
    | TVar { contents = Unbound _ }
    | TConst _ | TRowEmpty ) as ty ->
    ty

(** Transforms polymorphic type variables into normal unbound type variables *)
let instantiate (level : level) (ty : ty) : ty =
  let id_var_map = Hashtbl.create 10 in
  let rec f (ty : ty) : ty =
    match ty with
    | TConst _ -> ty
    | TVar { contents = Link ty } -> f ty
    | TVar { contents = Generic id } -> (
      try Hashtbl.find id_var_map id
      with Not_found ->
        let var = new_var level in
        Hashtbl.add id_var_map id var;
        var)
    | TVar { contents = Unbound _ } -> ty
    | TApp (ty, ty_arg_list) -> TApp (f ty, List.map f ty_arg_list)
    | TArrow (param_ty_list, return_ty) ->
      TArrow (List.map f param_ty_list, f return_ty)
    | TRecord row -> TRecord (f row)
    | TRowEmpty -> ty
    | TRowExtend (label, field_ty, row) -> TRowExtend (label, f field_ty, f row)
  in
  f ty

(** Helper function for inferring function types based on [num_params]
    (the no. of arguments to the function) *)
let rec match_fun_ty (num_params : int) : ty -> ty list * ty = function
  | TArrow (param_ty_list, return_ty) ->
    if List.length param_ty_list <> num_params then
      error "unexpected number of arguments"
    else (param_ty_list, return_ty)
  | TVar { contents = Link ty } -> match_fun_ty num_params ty
  | TVar ({ contents = Unbound (id, level) } as tvar) ->
    let param_ty_list =
      let rec f = function
        | 0 -> []
        | n -> new_var level :: f (n - 1) in
      f num_params in
    let return_ty = new_var level in
    tvar := Link (TArrow (param_ty_list, return_ty));
    (param_ty_list, return_ty)
  | _ -> error "expected a function"

(** Infers a type for an expression given the current environment 
    and the level for let-generalization *)
let rec infer (env : Env.t) (level : level) : expr -> ty = function
  | Var name -> (
    try instantiate level (Env.lookup env name)
    with Not_found -> error ("variable " ^ name ^ " not found"))
  | Fun (param_list, body_expr) ->
    let param_ty_list = List.map (fun _ -> new_var level) param_list in
    let fn_env =
      List.fold_left2
        (fun env param_name param_ty -> Env.extend env param_name param_ty)
        env param_list param_ty_list in
    let return_ty = infer fn_env level body_expr in
    TArrow (param_ty_list, return_ty)
  | Let (var_name, value_expr, body_expr) ->
    let var_ty = infer env (level + 1) value_expr in
    let generalized_ty = generalize level var_ty in
    infer (Env.extend env var_name generalized_ty) level body_expr
  | Call (fn_expr, arg_list) ->
    let param_ty_list, return_ty =
      match_fun_ty (List.length arg_list) (infer env level fn_expr) in
    List.iter2
      (fun param_ty arg_expr -> unify param_ty (infer env level arg_expr))
      param_ty_list arg_list;
    return_ty
  | RecordEmpty -> TRecord TRowEmpty
  | RecordSelect (record_expr, label) ->
    (* inlined code for Call of function with type "forall[a r] {label : a | r}
       -> a" *)
    let rest_row_ty = new_var level in
    let field_ty = new_var level in
    let param_ty = TRecord (TRowExtend (label, field_ty, rest_row_ty)) in
    let return_ty = field_ty in
    unify param_ty (infer env level record_expr);
    return_ty
  | RecordRestrict (record_expr, label) ->
    (* inlined code for Call of function with type "forall[a r] {label : a | r}
       -> {r}" *)
    let rest_row_ty = new_var level in
    let field_ty = new_var level in
    let param_ty = TRecord (TRowExtend (label, field_ty, rest_row_ty)) in
    let return_ty = TRecord rest_row_ty in
    unify param_ty (infer env level record_expr);
    return_ty
  | RecordExtend (label, expr, record_expr) ->
    (* inlined code for Call of function with type "forall[a r] (a, {r}) ->
       {label : a | r}" *)
    let rest_row_ty = new_var level in
    let field_ty = new_var level in
    let param1_ty = field_ty in
    let param2_ty = TRecord rest_row_ty in
    let return_ty = TRecord (TRowExtend (label, field_ty, rest_row_ty)) in
    unify param1_ty (infer env level expr);
    unify param2_ty (infer env level record_expr);
    return_ty
