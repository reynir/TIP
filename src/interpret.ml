open Wast

(** TIP types **)
type tip_type =
  | Integer of Z.t
  | Pointer of cell (* None represents null *)
  | Nil
  | FunctionPointer of Wast.identifier

(** Memory cells **)
and cell_value =
  | Initialized of tip_type
  | Unitialized

and cell =
    cell_value ref

let rec print_tip_type v =
  match v with
    | Integer i ->
        print_endline (Z.to_string i)
    | Nil -> print_endline "null"
    | FunctionPointer id ->
        print_endline (id ^ "()")
    | Pointer _ -> failwith "Printing pointers is not yet implemented"

(** Environments **)
module M =
  Map.Make
    (struct 
       type t = Wast.identifier
       let compare = compare
     end)

type env =
  | Empty
  | Env of cell M.t * env

let introduce (env : env) (ids : identifier list) =
  Env (List.fold_left
         (fun map id -> M.add id (ref Unitialized) map)
         M.empty
         ids
       , env)

let introduce_one (env : env) (id : identifier) =
  introduce env [id]

let extend env bindings =
  Env (List.fold_left
         (fun map (id, r) -> M.add id r map)
         M.empty
         bindings
       , env)

let extend_values env (bindings : (identifier * tip_type) list)=
  extend env (List.map (fun (id, v) -> (id, ref (Initialized v))) bindings)

let rec lookup id env : cell =
  match env with
    | Empty ->
        raise Not_found
    | Env (inner, outer) ->
        try
          M.find id inner
        with Not_found ->
          lookup id outer

let setbang env id v =
  let r = lookup id env
  in r := Initialized v

let lookup_func id (funcs : function_decl M.t) =
  M.find id funcs

(* Return type of execute. Decide later *)
type return_type = tip_type

let get_funcs funcs =
  List.fold_left
    (fun map ((id,_,_) as f) ->
       M.add id f map)
    M.empty
    funcs

let execute (other, main) args =
  (* (funcs : identifier M.t) *)
  let funcs = get_funcs (main :: other) in
  let rec eval env e : tip_type = match e with
    | Variable id ->
        (try match !(lookup id env) with
           | Unitialized -> failwith "Unitialized memory cell!"
           | Initialized v -> v
         with Not_found ->
           if M.mem id funcs
           then FunctionPointer id
           else failwith "Undeclared variable or function!")
    | IntegerLiteral i ->
        Integer i
    | Null ->
        Nil
    | Binop (e1, op, e2) ->
        eval_binop env op e1 e2
    | Complement e ->
        (match eval env e with
           | Integer i ->
               if Z.equal Z.zero i
               then Integer Z.one
               else Integer Z.zero
           | _ -> failwith "Wrong type for complement")
    | Deref e ->
        (match eval env e with
           | Nil ->
               failwith "Null pointer dereference!"
           | Pointer r ->
               (match !r with
                  | Initialized v -> v
                  | Unitialized -> failwith "Unitialized memory cell addressed")
           | _ ->
               failwith "Wrong type for dereference")
    | Ref id ->
        (* FIXME: Not_found exception *)
        Pointer (lookup id env)
    | StaticInvoke (f, args) ->
        let args = List.map (eval env) args
        in call f args
    | NonstaticInvoke (e, args) ->
        let f = eval env e
        and args = List.map (eval env) args
        in failwith "TODO!"
    | Malloc ->
        Pointer (ref Unitialized)
    | Input ->
        (* TODO *)
        Integer Z.zero
  and eval_binop env op e1 e2 = match op with
    | LazyOr | LazyAnd ->
        failwith "LazyAnd and LazyOr are not yet implemented"
    | Equal | NotEqual -> 
        (match eval env e1, eval env e2 with
           | Integer i1, Integer i2 ->
               if Z.equal i1 i2 then 1 else 0
           | _ -> 
               failwith "Non-integer comparison not yet implemented")
    | _ ->
        match eval env e1, eval env e2 with
          | Integer i1, Integer i2 ->
              (match op with
                 | EagerOr | ExclusiveOr | EagerAnd ->
                     failwith "Binary operation not yet implemented"
                 | Lt ->
                     Integer (if Z.lt i1 i2 then 1 else 0)
                 | Gt | Le | Ge ->
                     failwith "Binary operation not yet implemented"
                 | Plus ->
                     Integer (Z.add i1 i2)
                 | Minus ->
                     Integer (Z.sub i1 i2)
                 | Mul | Div | Mod ->
                     failwith "Binary operation not yet implemented")
          | _ -> failwith "Non-integer arguments in integer operations"

  (* TODO: restructure with continuations *)
  and exec_stms 
          (k : env -> return_type) 
          (return : tip_type -> return_type)
          env
          stms = match stms with
    | [] ->
        k env
    | stm :: stms ->
        exec_stm 
          (fun env' -> exec_stms k return env' stms)
          return
          env 
          stm
  and exec_stm
             (k : env -> return_type)
             (return : tip_type -> return_type)
             env
             stm = match stm with
    | LocalDecls ids -> k (introduce env ids)
    | IfThen (e, consequent) ->
        (match eval env e with
           | Integer i ->
               if Z.equal Z.zero i
               then k env
               else 
                 exec_stms 
                   (fun _ -> k env)
                   return
                   env
                   consequent
           | _ -> failwith "Non-integer in if test")
    | IfThenElse (e, consequent, alternative) ->
        (match eval env e with
           | Integer i ->
               if Z.equal Z.zero i
               then exec_stms (fun _ -> k env) return env alternative
               else exec_stms (fun _ -> k env) return env consequent
           | _ -> failwith "Non-integer in if test")
    | While (e, body) ->
        (match eval env e with
           | Integer i ->
               if Z.equal Z.zero i
               then k env
               else exec_stms 
                      (fun _ -> exec_stm k return env stm)
                      return
                      env
                      body)
    | Block b ->
        exec_stms (fun _ -> k env) return env b
    | VariableAssignment (id, e) ->
        let v = eval env e
        in k (setbang env id v; env)
    | PointerAssignment (e1, e2) ->
        let v1 = eval env e1
        and v2 = eval env e2
        in (match v1 with
              | Nil ->
                  failwith "Trying to assign to null pointer"
              | Pointer r ->
                  r := Initialized v2; k env
              | _ ->
                  failwith "Trying to assign to non-pointer")
    | Output e ->
        let v = eval env e
        in print_string "TODO: output";
           k env
    | ValueReturn e ->
        return (eval env e)
    | VoidReturn ->
        failwith "Void return not allowed (yet)"

  and call id args : tip_type =
    let (_, params, body) = M.find id funcs
    in 
      exec_stms 
        (fun _ -> failwith "Function ran off the end")
        (fun v -> v) 
        (extend_values Empty (List.combine params args))
        body

  in match main with
    | (_, main_params, main_body) ->
        exec_stms
          (fun _ -> print_endline "Main ended with no return value"; Integer Z.zero)
          (fun v -> print_tip_type v; v)
          (extend_values Empty (List.combine main_params args))
          main_body
