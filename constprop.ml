open Ll
open Datastructures

(* The lattice of symbolic constants ---------------------------------------- *)
module SymConst =
  struct
    type t = NonConst           (* Uid may take on multiple values at runtime *)
           | Const of int64     (* Uid will always evaluate to const i64 or i1 *)
           | UndefConst         (* Uid is not defined at the point *)

    let compare s t =
      match (s, t) with
      | (Const i, Const j) -> Int64.compare i j
      | (NonConst, NonConst) | (UndefConst, UndefConst) -> 0
      | (NonConst, _) | (_, UndefConst) -> 1
      | (UndefConst, _) | (_, NonConst) -> -1

    let to_string : t -> string = function
      | NonConst -> "NonConst"
      | Const i -> Printf.sprintf "Const (%LdL)" i
      | UndefConst -> "UndefConst"


  end

(* The analysis computes, at each program point, which UIDs in scope will evaluate
   to integer constants *)
type fact = SymConst.t UidM.t



(* flow function across Ll instructions ------------------------------------- *)
(* - Uid of a binop or icmp with const arguments is constant-out
   - Uid of a binop or icmp with an UndefConst argument is UndefConst-out
   - Uid of a binop or icmp with an NonConst argument is NonConst-out
   - Uid of stores and void calls are UndefConst-out
   - Uid of all other instructions are NonConst-out
 *)

let op_to_symconst op fct =
	begin match op with
    | Id i ->
        begin match (UidM.exists (fun fct _ -> fct = i) fct) with
          | true -> UidM.find i fct
          | _ -> SymConst.UndefConst
        end
    | Gid gi ->
        begin match (UidM.exists (fun fct _ -> fct = gi) fct) with
          | true -> UidM.find gi fct
          | _ -> SymConst.UndefConst
        end
    | Null -> SymConst.UndefConst
	  | Const x -> SymConst.Const x
	end

let insn_flow (u,i:uid * insn) (d:fact) : fact =
  begin match i with
    | Call (Void, _, _) -> UidM.add u SymConst.UndefConst d
    | Store _ -> UidM.add u SymConst.UndefConst d
    | Icmp (cc, _, op1, op2) ->
        let s = begin match (op_to_symconst op1 d, op_to_symconst op2 d) with
                  | SymConst.Const x, SymConst.Const y ->
                      let open Int64 in SymConst.Const
                      begin match cc with
                        | Eq -> if compare x y = 0 then 1L else 0L
                        | Ne -> if compare x y <> 0 then 1L else 0L
                        | Sgt -> if compare x y >= 0 then 1L else 0L
                        | Sge -> if compare x y >= 0 then 1L else 0L
                        | Slt -> if compare x y < 0 then 1L else 0L
                        | Sle -> if compare x y <= 0 then 1L else 0L
                      end
                  | SymConst.NonConst, _ -> SymConst.NonConst
                  | _, SymConst.NonConst -> SymConst.NonConst
                  | SymConst.UndefConst, _ -> SymConst.UndefConst
                  | _, SymConst.UndefConst -> SymConst.UndefConst
                end in
        UidM.add u s d
    | Binop (binop, _, op1, op2) ->
        let s = begin match (op_to_symconst op1 d, op_to_symconst op2 d) with
                  | SymConst.Const x, SymConst.Const y ->
                      let open Int64 in
                      begin match binop with
                        | Add -> SymConst.Const (add x y)
                        | Sub -> SymConst.Const (sub x y)
                        | Mul -> SymConst.Const (mul x y)
                        | And -> SymConst.Const (logand x y)
                        | Or -> SymConst.Const (logor x y)
                        | Xor -> SymConst.Const (logxor x y)
                        | Lshr -> SymConst.Const (shift_right_logical x (to_int y))
                        | Shl -> SymConst.Const (shift_left x (to_int y))
                        | Ashr -> SymConst.Const (shift_right x (to_int y))
                      end
                  | SymConst.NonConst, _ -> SymConst.NonConst
                  | _, SymConst.NonConst -> SymConst.NonConst
                  | SymConst.UndefConst, _ -> SymConst.UndefConst
                  | _, SymConst.UndefConst -> SymConst.UndefConst
                end in
        UidM.add u s d
    | _ -> UidM.add u SymConst.NonConst d
	end

(* The flow function across terminators is trivial: they never change const info *)
let terminator_flow (t:terminator) (d:fact) : fact = d

(* module for instantiating the generic framework --------------------------- *)
module Fact =
  struct
    type t = fact
    let forwards = true

    let insn_flow = insn_flow
    let terminator_flow = terminator_flow

    let normalize : fact -> fact =
      UidM.filter (fun _ v -> v != SymConst.UndefConst)

    let compare (d:fact) (e:fact) : int  =
      UidM.compare SymConst.compare (normalize d) (normalize e)

    let to_string : fact -> string =
      UidM.to_string (fun _ v -> SymConst.to_string v)

    (* The constprop analysis should take the join over predecessors to compute the
       flow into a node. You may find the UidM.merge function useful *)
    let combine (ds:fact list) : fact =
      List.fold_left
      (fun acc fct -> UidM.merge
        (fun uid symconst1 symconst2 ->
          begin match symconst1, symconst2 with
            | None, Some ptr -> Some ptr
            | Some ptr, None -> Some ptr
            | Some ptr1, Some ptr2 ->
                begin match ptr1, ptr2 with
                  | SymConst.NonConst, SymConst.NonConst -> Some SymConst.NonConst
                  | SymConst.Const x, SymConst.Const y ->
                      begin match x = y with
                        | true -> Some (SymConst.Const x)
                        | _ -> Some SymConst.NonConst
                      end
                  | SymConst.NonConst, SymConst.Const _ -> Some SymConst.NonConst
                  | SymConst.Const _, SymConst.NonConst -> Some SymConst.NonConst
                  | SymConst.UndefConst, SymConst.UndefConst -> Some SymConst.UndefConst
                  | _, _-> failwith "invalid case"
                end
            | None, None -> failwith "invalid case"
          end)
      fct acc) UidM.empty ds
  end

(* instantiate the general framework ---------------------------------------- *)
module Graph = Cfg.AsGraph (Fact)
module Solver = Solver.Make (Fact) (Graph)

(* expose a top-level analysis operation ------------------------------------ *)
let analyze (g:Cfg.t) : Graph.t =
  (* the analysis starts with every node set to bottom (the map of every uid
     in the function to UndefConst *)
  let init l = UidM.empty in

  (* the flow into the entry node should indicate that any parameter to the
     function is not a constant *)
  let cp_in = List.fold_right
    (fun (u,_) -> UidM.add u SymConst.NonConst)
    g.Cfg.args UidM.empty
  in
  let fg = Graph.of_cfg init cp_in g in
  Solver.solve fg


(* run constant propagation on a cfg given analysis results ----------------- *)
(* HINT: your cp_block implementation will probably rely on several helper
   functions.                                                                 *)
let run (cg:Graph.t) (cfg:Cfg.t) : Cfg.t =
  let open SymConst in


  let cp_block (l:Ll.lbl) (cfg:Cfg.t) : Cfg.t =
    let b = Cfg.block cfg l in
    let cb = Graph.uid_out cg l in
    let b' =
      let to_const op fct =
        begin match op_to_symconst op fct with
          | SymConst.Const c -> Ll.Const c
          | _ -> op
        end in
      (fun cb b ->
        {insns =
          List.map (fun (id, instr) ->
			     let fct = cb id in
              (id,
                begin match instr with
                  | Call (t, op, args) -> Call (t, to_const op fct, List.map (fun (t, op) -> (t, to_const op fct)) args)
                  | Load (t, op) -> Load (t, to_const op fct)
                  | Store (t, op1, op2) -> Store (t, to_const op1 fct, to_const op2 fct)
                  | Binop (binop, t, op1, op2) -> Binop (binop, t, to_const op1 fct, to_const op2 fct)
                  | Bitcast (t1, op, t2) -> Bitcast (t1, to_const op fct, t2)
                  | Icmp (cnd, t, op1, op2) -> Icmp (cnd, t, to_const op1 fct, to_const op2 fct)
                  | Gep (t, op1, ops) -> Gep (t, to_const op1 fct, List.map (fun op -> to_const op fct) ops)
                  | _ -> instr
		            end)) b.insns;
        term =
          begin match b.term with
            | (id, term) ->
                let fct = cb id in
                  (id,
                    begin match term with
                      | Cbr (op, l1, l2) -> Cbr (to_const op fct, l1, l2)
                      | Ret (t, Some op) -> Ret (t, Some (to_const op fct))
                      | _ -> term
                    end)
          end
    })
    cb b in
    Cfg.add_block l b' cfg
  in

  LblS.fold cp_block (Cfg.nodes cfg) cfg
