open Wast

(** TIP types **)
type tip_type =
  | Integer of Integer.t
  | Pointer of cell (* None represents null *)
  | Nil
  | FunctionPointer of Wast.identifier

(** Memory cells **)
and cell_value =
  | Initialized of tip_type
  | Unitialized

and cell =
    cell_value ref

let tip_f = Integer Integer.zero
and tip_t = Integer Integer.one

let tip_bool b =
  if b
  then tip_t
  else tip_f

let rec string_of_tip_value v =
  match v with
    | Integer i ->
        Integer.to_string i
    | Nil -> "null"
    | FunctionPointer id ->
        id ^ "()"
    | Pointer r -> 
        "@"^string_of_int (Obj.magic r)

let rec string_of_tip_type v =
  match v with
    | Integer _ -> "int"
    | Nil -> "&?"
    | FunctionPointer _ -> "? -> ?"
    | Pointer _ -> "&?"

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
  let rec eval env e : tip_type = 
    (*
    print_endline "####### Evaluating ###";
    Wastpp.pp_expression e;
    print_newline();
     *)
    match e with
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
               tip_bool (Integer.equal Integer.zero i)
           | _ -> failwith "Wrong type for complement")
    | Deref e ->
        (match eval env e with
           | Nil ->
               failwith "Null pointer dereference!"
           | Pointer r ->
               (match !r with
                  | Initialized v -> v
                  | Unitialized -> failwith "Unitialized memory cell addressed")
           | v ->
               failwith ("Wrong type for dereference: "
                         ^string_of_tip_value v
                         ^ " : "
                         ^string_of_tip_type v))
    | Ref id ->
        (try
           Pointer (lookup id env)
         with Not_found ->
           failwith ("Variable "^id^" was not found"))
    | StaticInvoke (f, args) ->
        let args = List.map (eval env) args
        in call f args
    | NonstaticInvoke (e, args) ->
        let f = match eval env e with
          | FunctionPointer f -> f
          | _ -> failwith ("Trying to call non-function")
        and args = List.map (eval env) args
        in call f args
    | Malloc ->
        Pointer (ref Unitialized)
    | Input ->
        let c = input_char stdin in
        Integer (Integer.of_int (Char.code c))
  and eval_binop env op e1 e2 = match op with
    | LazyOr | LazyAnd ->
        (match eval env e1 with
          | (Integer i1) as v1 ->
              (* Clever hack *)
              if (Integer.equal i1 Integer.zero) = (op = LazyOr)
              then (match eval env e2 with
                      | (Integer i2) as v2 ->
                          v2
                      | _ -> failwith ("Wrong type for "
                                       ^if op = LazyOr 
                                       then "LazyOr" 
                                       else "LazyAnd"))
              else v1
          | _ -> failwith ("Wrong type for "
                           ^if op = LazyOr 
                           then "LazyOr" 
                           else "LazyAnd"))
    | Equal | NotEqual -> 
        tip_bool
          ((match eval env e1, eval env e2 with
              | Integer i1, Integer i2 ->
                  Integer.equal i1 i2
              | Nil, Nil ->
                  true
              | Nil, Pointer _ | Pointer _, Nil ->
                  false
              | Pointer r1, Pointer r2 ->
                  r1 == r2
              | FunctionPointer f1, FunctionPointer f2 ->
                  f1 = f2
              | v1, v2 ->
                  (Wastpp.pp_expression e1;
                   print_newline ();
                   Wastpp.pp_expression e2;
                   print_newline ();
                   failwith ("Comparison between two different types: "
                            ^ string_of_tip_type v1
                            ^ " and "
                            ^string_of_tip_type v2)))
             = (op = Equal)) (* Clever hack *)
    | _ ->
        match eval env e1, eval env e2 with
          | Integer i1, Integer i2 ->
              (match op with
                 | EagerOr ->
                     Integer (Integer.(lor) i1 i2)
                 | ExclusiveOr ->
                     Integer (Integer.(lxor) i1 i2)
                 | EagerAnd ->
                     Integer (Integer.(land) i1 i2)
                 | Lt ->
                     tip_bool (Integer.lt i1 i2)
                 | Gt ->
                     tip_bool (Integer.lt i2 i1)
                 | Le | Ge ->
                     failwith "Binary operation not yet implemented"
                 | Plus ->
                     Integer (Integer.add i1 i2)
                 | Minus ->
                     Integer (Integer.sub i1 i2)
                 | Mul ->
                     Integer (Integer.mul i1 i2)
                 | Div ->
                     Integer (Integer.div i1 i2)
                 | Mod ->
                     Integer (Integer.(mod) i1 i2))
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
             stm = 
    (*
    print_endline "### Executing ###";
    Wastpp.pp_statement 1 stm;
     *)
    match stm with
    | LocalDecls ids -> k (introduce env ids)
    | IfThen (e, consequent) ->
        (match eval env e with
           | Integer i ->
               if Integer.equal Integer.zero i
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
               if Integer.equal Integer.zero i
               then exec_stms (fun _ -> k env) return env alternative
               else exec_stms (fun _ -> k env) return env consequent
           | _ -> failwith "Non-integer in if test")
    | While (e, body) ->
        (match eval env e with
           | Integer i ->
               if Integer.equal Integer.zero i
               then k env
               else exec_stms 
                      (fun _ -> exec_stm k return env stm)
                      return
                      env
                      body
           | _ -> failwith "Non-integer in while test")
    | Block b ->
        exec_stms (fun _ -> k env) return env b
    | VariableAssignment (id, e) ->
        let v = eval env e
        in (try setbang env id v
            with Not_found -> 
              failwith ("The variable "^id^" is not declared in assignment"));
        k env
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
        in print_endline (string_of_tip_value v);
           k env
    | ValueReturn e ->
        (match e with
           | StaticInvoke (f, args) ->
               print_endline ("Calling "^f);
               let args = List.map (eval env) args in
               let (_, params, body) = try
                 M.find f funcs
               with Not_found -> failwith ("The function "^f^" was not found")
               in exec_stms
                    (fun _ -> failwith "Function ran off the end")
                    return
                    (extend_values Empty (List.combine params args))
                    body
           | NonstaticInvoke (e, args) ->
               let f = match eval env e with
                 | FunctionPointer f -> f
                 | _ -> failwith "Trying to call non-function" in
               let args = List.map (eval env) args in
               let (_, params, body) = try
                 M.find f funcs
               with Not_found -> failwith ("The function "^f^" was not found")
               in exec_stms
                    (fun _ -> failwith "Function ran off the end")
                    return
                    (extend_values Empty (List.combine params args))
                    body
           | _ ->
               return (eval env e))
    | VoidReturn ->
        failwith "Void return not allowed (yet)"

  and call id args : tip_type =
    let (_, params, body) = try 
      M.find id funcs
    with Not_found -> failwith ("The function "^id^" was not found")
    in 
      exec_stms 
        (fun _ -> failwith "Function ran off the end")
        (fun v -> v) 
        (extend_values Empty (List.combine params args))
        body

  in match main with
    | (_, main_params, main_body) ->
        exec_stms
          (fun _ -> print_endline "Main ended with no return value"; Integer Integer.zero)
          (fun v -> print_endline (string_of_tip_value v); v)
          (extend_values Empty (List.combine main_params args))
          main_body
