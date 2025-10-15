open Riscv_virtasm
open Riscv_reg

(* RISC-V assembly commands. *)

let registers = [|
  (* Int registers *)
  "zero"; "ra"; "sp"; "gp"; "tp";
  "t0"; "t1"; "t2"; "fp"; "s1";
  "a0"; "a1"; "a2"; "a3"; "a4";
  "a5"; "a6"; "a7"; "s2"; "s3";
  "s4"; "s5"; "s6"; "s7"; "s8";
  "s9"; "s10"; "s11"; "t3"; "t4";
  "t5"; "t6";

  (* FP registers *)
  "ft0"; "ft1"; "ft2"; "ft3"; "ft4";
  "ft5"; "ft6"; "ft7"; "fs0"; "fs1";
  "fa0"; "fa1"; "fa2"; "fa3"; "fa4";
  "fa5"; "fa6"; "fa7"; "fs2"; "fs3";
  "fs4"; "fs5"; "fs6"; "fs7"; "fs8";
  "fs9"; "fs10"; "fs11"; "ft8"; "ft9";
  "ft10"; "ft11";
|]

let find_index_opt pred arr =
  let rec find_rec i =
    if i >= Array.length arr then
      None
    else if pred arr.(i) then
      Some i
    else
      find_rec (i + 1)
  in
  find_rec 0

type reg = int

let reg_of_string s =
  match find_index_opt (fun r -> r = s) registers with
  | Some r -> r
  | None -> failwith ("Unknown register: " ^ s)

let get_reg slot : reg =
  Option.get @@ find_index_opt (fun r -> r = Slot.to_string slot) registers

type imm = int64

type label = string

type rtype_t =
| Add
| Addw

| Sub
| Subw

| Xor
| Or
| And

| Sll
| Sllw

| Srl
| Srlw

| Sra
| Sraw

| Slt
| Sltw
| Sltu
| Sltuw

| Mul
| Mulw

| Div
| Divw
| Divu
| Divuw

| Rem
| Remw
| Remu
| Remuw

and itype_t =
| Addi
| Addiw

| Xori
| Ori
| Andi

| Slli
| Slliw

| Srli
| Srliw

| Srai
| Sraiw

| Slti
| Sltiw

and memtype_t =
| Lb
| Lh
| Lw
| Ld
| Lbu
| Lhu

| Sb
| Sh
| Sw
| Sd

| Jalr

and btype_t =
| Beq
| Bne
| Blt
| Bge
| Bltu
| Bgeu

| Bgt
| Ble
| Bgtu
| Bleu

and jtype_t = 
| Jal

and utype_t =
| Lui
| Auipc

and t =
| Section of string

| Data of string

| RType of rtype_t * reg * reg * reg
| IType of itype_t * reg * reg * imm
| MemType of memtype_t * reg * reg * imm
| BType of btype_t * reg * reg * label
| JType of jtype_t * reg * label
| UType of utype_t * reg * imm

(* Pseudo-instructions *)
| La of reg * label
| Lb of reg * label
| Lh of reg * label
| Lw of reg * label
| Ld of reg * label
| Sb of reg * label * reg
| Sh of reg * label * reg
| Sw of reg * label * reg
| Sd of reg * label * reg
| FLb of reg * label * reg
| FLh of reg * label * reg
| FLw of reg * label * reg
| FLd of reg * label * reg
| FSb of reg * label * reg
| FSh of reg * label * reg
| FSw of reg * label * reg
| FSd of reg * label * reg

| Li of reg * imm
| Mv of reg * reg
| Not of reg * reg
| Sext of reg * reg
| Neg of reg * reg
| Negw of reg * reg

| Beqz of reg * label
| Bnez of reg * label
| Bltz of reg * label
| Bgez of reg * label
| Bgtz of reg * label
| Blez of reg * label

| Call of label
| Tail of label
| Ret
| Jr of reg
| Jalr of reg

| Label of label
| Nop

let rtype_to_string = function
  | Add -> "add"
  | Addw -> "addw"
  | Sub -> "sub"
  | Subw -> "subw"
  | Xor -> "xor"
  | Or -> "or"
  | And -> "and"
  | Sll -> "sll"
  | Sllw -> "sllw"
  | Srl -> "srl"
  | Srlw -> "srlw"
  | Sra -> "sra"
  | Sraw -> "sraw"
  | Slt -> "slt"
  | Sltw -> "sltw"
  | Sltu -> "sltu"
  | Sltuw -> "sltuw"

  | Mul -> "mul"
  | Mulw -> "mulw"
  | Div -> "div"
  | Divw -> "divw"
  | Divu -> "divu"
  | Divuw -> "divuw"
  | Rem -> "rem"
  | Remw -> "remw"
  | Remu -> "remu"
  | Remuw -> "remuw"

let itype_to_string = function
  | Addi -> "addi"
  | Addiw -> "addiw"
  | Xori -> "xori"
  | Ori -> "ori"
  | Andi -> "andi"
  | Slli -> "slli"
  | Slliw -> "slliw"
  | Srli -> "srli"
  | Srliw -> "srliw"
  | Srai -> "srai"
  | Sraiw -> "sraiw"
  | Slti -> "slti"
  | Sltiw -> "sltiw"

let stype_to_string = function
  | Lb -> "lb"
  | Lh -> "lh"
  | Lw -> "lw"
  | Ld -> "ld"
  | Lbu -> "lbu"
  | Lhu -> "lhu"

  | Sb -> "sb"
  | Sh -> "sh"
  | Sw -> "sw"
  | Sd -> "sd"

  | Jalr -> "jalr"

let btype_to_string = function
  | Beq -> "beq"
  | Bne -> "bne"
  | Blt -> "blt"
  | Bge -> "bge"
  | Bltu -> "bltu"
  | Bgeu -> "bgeu"
  | Bgt -> "bgt"
  | Ble -> "ble"
  | Bgtu -> "bgtu"
  | Bleu -> "bleu"

let jtype_to_string = function
  | Jal -> "jal"

let utype_to_string = function
  | Lui -> "lui"
  | Auipc -> "auipc"

let to_string asm =
  let convert_global rs label =
    let rs_str = registers.(rs) in
    Printf.sprintf "%s, %s" rs_str label
  in

  let convert_global_tmp rs label rt =
    let rs_str = registers.(rs) in
    let rt_str = registers.(rt) in
    Printf.sprintf "%s, %s, %s" rs_str label rt_str
  in

  match asm with
  | Section sec -> Printf.sprintf ".section .%s" sec
  | Data str -> str
  | RType (ty, rd, rs1, rs2) -> 
      let ty_str = rtype_to_string ty in
      let rd_str = registers.(rd) in
      let rs1_str = registers.(rs1) in 
      let rs2_str = registers.(rs2) in 
      Printf.sprintf "%s %s, %s, %s" ty_str rd_str rs1_str rs2_str
  | IType (ty, rd, rs1, imm) ->
      let ty_str = itype_to_string ty in
      let rd_str = registers.(rd) in
      let rs1_str = registers.(rs1) in 
      Printf.sprintf "%s %s, %s, %Ld" ty_str rd_str rs1_str imm
  | MemType (ty, rs1, rs2, imm) ->
      let ty_str = stype_to_string ty in
      let rs1_str = registers.(rs1) in 
      let rs2_str = registers.(rs2) in 
      Printf.sprintf "%s %s, %Ld(%s)" ty_str rs2_str imm rs1_str
  | BType (ty, rs1, rs2, label) ->
      let ty_str = btype_to_string ty in
      let rs1_str = registers.(rs1) in 
      let rs2_str = registers.(rs2) in 
      Printf.sprintf "%s %s, %s, %s" ty_str rs1_str rs2_str label
  | JType (ty, rd, label) ->
      let ty_str = jtype_to_string ty in
      let rd_str = registers.(rd) in
      Printf.sprintf "%s %s, %s" ty_str rd_str label
  | UType (ty, rd, imm) ->
      let ty_str = utype_to_string ty in
      let rd_str = registers.(rd) in
      Printf.sprintf "%s %s, %Ld" ty_str rd_str imm
  | La (rd, label) -> Printf.sprintf "la %s" (convert_global rd label)
  | Lb (rd, label) -> Printf.sprintf "lb %s" (convert_global rd label)
  | Lh (rd, label) -> Printf.sprintf "lh %s" (convert_global rd label)
  | Lw (rd, label) -> Printf.sprintf "lw %s" (convert_global rd label)
  | Ld (rd, label) -> Printf.sprintf "ld %s" (convert_global rd label)
  | Sb (rs2, label, rs1) -> Printf.sprintf "sb %s" (convert_global_tmp rs1 label rs2)
  | Sh (rs2, label, rs1) -> Printf.sprintf "sh %s" (convert_global_tmp rs1 label rs2)
  | Sw (rs2, label, rs1) -> Printf.sprintf "sw %s" (convert_global_tmp rs1 label rs2)
  | Sd (rs2, label, rs1) -> Printf.sprintf "sd %s" (convert_global_tmp rs1 label rs2)
  | FLb (rd, label, rs1) -> Printf.sprintf "flb %s" (convert_global_tmp rs1 label rd)
  | FLh (rd, label, rs1) -> Printf.sprintf "flh %s" (convert_global_tmp rs1 label rd)
  | FLw (rd, label, rs1) -> Printf.sprintf "flw %s" (convert_global_tmp rs1 label rd)
  | FLd (rd, label, rs1) -> Printf.sprintf "fld %s" (convert_global_tmp rs1 label rd)
  | FSb (rs2, label, rs1) -> Printf.sprintf "fsb %s" (convert_global_tmp rs1 label rs2)
  | FSh (rs2, label, rs1) -> Printf.sprintf "fsh %s" (convert_global_tmp rs1 label rs2)
  | FSw (rs2, label, rs1) -> Printf.sprintf "fsw %s" (convert_global_tmp rs1 label rs2)
  | FSd (rs2, label, rs1) -> Printf.sprintf "fsd %s" (convert_global_tmp rs1 label rs2)
  | Li (rd, imm) -> Printf.sprintf "li %s, %Ld" (registers.(rd)) imm
  | Mv (rd, rs) -> Printf.sprintf "mv %s, %s" (registers.(rd)) (registers.(rs))
  | Not (rd, rs) -> Printf.sprintf "not %s, %s" (registers.(rd)) (registers.(rs))
  | Neg (rd, rs) -> Printf.sprintf "neg %s, %s" (registers.(rd)) (registers.(rs))
  | Negw (rd, rs) -> Printf.sprintf "negw %s, %s" (registers.(rd)) (registers.(rs))
  | Sext (rd, rs) -> Printf.sprintf "sext.w %s, %s" (registers.(rd)) (registers.(rs))
  | Beqz (rs, label) -> Printf.sprintf "beqz %s, %s" (registers.(rs)) label
  | Bnez (rs, label) -> Printf.sprintf "bnez %s, %s" (registers.(rs)) label
  | Bltz (rs, label) -> Printf.sprintf "bltz %s, %s" (registers.( rs)) label
  | Bgez (rs, label) -> Printf.sprintf "bgez %s, %s" (registers.(rs)) label
  | Bgtz (rs, label) -> Printf.sprintf "bgtz %s, %s" (registers.(rs)) label
  | Blez (rs, label) -> Printf.sprintf "blez %s, %s" (registers.(rs)) label
  | Call label -> Printf.sprintf "call %s" label
  | Tail label -> Printf.sprintf "tail %s" label
  | Ret -> "ret"
  | Jr rs -> Printf.sprintf "jr %s" (registers.(rs))
  | Jalr rs -> Printf.sprintf "jalr %s" (registers.(rs))
  | Label label -> label ^ ":"
  | Nop -> "nop"

(**
Used when emitting assembly.

We expect every non-label command to be indented by 4 spaces.
*)
let to_asm_string asm = 
  match asm with
  | Label _ -> to_string asm
  | _ -> "    " ^ to_string asm

let vprog = ref VProg.empty

let spill_map = ref SlotMap.empty

let alloca_offset = ref 0

let convert_single (inst : Inst.t) : t list =
  let open Inst in
  let convert_rtype op (slot : Slots.r_slot) =
    RType (op, get_reg slot.rd, get_reg slot.rs1, get_reg slot.rs2) in
  let convert_itype op (slot : Slots.i_slot) =
    IType (op, get_reg slot.rd, get_reg slot.rs1, Int64.of_int slot.imm) in
  let convert_memtype op (slot : Slots.mem_slot) =
    MemType (op, get_reg slot.base, get_reg slot.rd, Int64.of_int slot.offset) in
  match inst with
  | Add slots -> [ convert_rtype Add slots ]
  | Addw slots -> [ convert_rtype Addw slots ]
  | Sub slots -> [ convert_rtype Sub slots ]
  | Subw slots -> [ convert_rtype Subw slots ]
  | Xor slots -> [ convert_rtype Xor slots ]
  | Or slots -> [ convert_rtype Or slots ]
  | And slots -> [ convert_rtype And slots ]
  | Sll slots -> [ convert_rtype Sll slots ]
  | Sllw slots -> [ convert_rtype Sllw slots ]
  | Srl slots -> [ convert_rtype Srl slots ]
  | Srlw slots -> [ convert_rtype Srlw slots ]
  | Sra slots -> [ convert_rtype Sra slots ]
  | Sraw slots -> [ convert_rtype Sraw slots ]
  | Slt slots -> [ convert_rtype Slt slots ]
  | Sltw slots -> [ convert_rtype Sltw slots ]
  | Sltu slots -> [ convert_rtype Sltu slots ]
  | Sltuw slots -> [ convert_rtype Sltuw slots ]
  | Mul slots -> [ convert_rtype Mul slots ]
  | Mulw slots -> [ convert_rtype Mulw slots ]
  | Div slots -> [ convert_rtype Div slots ]
  | Divw slots -> [ convert_rtype Divw slots ]
  | Divu slots -> [ convert_rtype Divu slots ]
  | Divuw slots -> [ convert_rtype Divuw slots ]
  | Rem slots -> [ convert_rtype Rem slots ]
  | Remw slots -> [ convert_rtype Remw slots ]
  | Remu slots -> [ convert_rtype Remu slots ]
  | Remuw slots -> [ convert_rtype Remuw slots ]

  | Addi slots -> [ convert_itype Addi slots ]
  | Addiw slots -> [ convert_itype Addiw slots ]
  | Xori slots -> [ convert_itype Xori slots ]
  | Ori slots -> [ convert_itype Ori slots ]
  | Andi slots -> [ convert_itype Andi slots ]
  | Slli slots -> [ convert_itype Slli slots ]
  | Slliw slots -> [ convert_itype Slliw slots ]
  | Srli slots -> [ convert_itype Srli slots ]
  | Srliw slots -> [ convert_itype Srliw slots ]
  | Srai slots -> [ convert_itype Srai slots ]
  | Sraiw slots -> [ convert_itype Sraiw slots ]
  | Slti slots -> [ convert_itype Slti slots ]
  | Sltiw slots -> [ convert_itype Sltiw slots ]
  | Lb slots -> [ convert_memtype Lb slots ]
  | Lh slots -> [ convert_memtype Lh slots ]
  | Lw slots -> [ convert_memtype Lw slots ]
  | Ld slots -> [ convert_memtype Ld slots ]
  | Lbu slots -> [ convert_memtype Lbu slots ]
  | Lhu slots -> [ convert_memtype Lhu slots ]
  | Sb slots -> [ convert_memtype Sb slots ]
  | Sh slots -> [ convert_memtype Sh slots ]
  | Sw slots -> [ convert_memtype Sw slots ]
  | Sd slots -> [ convert_memtype Sd slots ]

  | Sextw { rd; rs } -> [ Sext (get_reg rd, get_reg rs) ]
  | Zextw { rd; rs } -> [ IType (Slli, get_reg rd, get_reg rs, 32L); IType (Srli, get_reg rd, get_reg rd, 32L) ]

  | Li { rd; imm } -> (match imm with
      | Imm.IntImm i -> [ Li (get_reg rd, Int64.of_int i) ]
      | Imm.Int64Imm i64 -> [ Li (get_reg rd, i64) ]
      | Imm.FloatImm _ -> failwith "Cannot load float immediate into integer register")
  | Mv { rd; rs } -> [ Mv (get_reg rd, get_reg rs) ]
  | La { rd; label } -> [ La (get_reg rd, Label.to_string label) ]
  | Alloca { rd; size } -> 
      alloca_offset := !alloca_offset - size;
      [ IType (Addi, get_reg rd, reg_of_string "sp", Int64.of_int !alloca_offset) ]
  | Spill { target; origin } ->
      let offset = SlotMap.find_exn !spill_map origin in
      [ MemType (Sd, get_reg target, reg_of_string "sp", Int64.of_int offset) ]
  | Reload { target; origin } ->
      let offset = SlotMap.find_exn !spill_map origin in
      [ MemType (Ld, get_reg target, reg_of_string "sp", Int64.of_int offset) ]
  | Call { rd; fn; args } ->
      let arg_insts = List.mapi (fun i slot -> 
        if i >= 8 then MemType (Sd, get_reg slot, reg_of_string "sp", Int64.of_int @@ (i - 7) * 8) 
        else Mv (reg_of_string ("a" ^ string_of_int i), get_reg slot)
      ) args in
      arg_insts @ [ Call (Label.to_string fn);
        Mv (get_reg rd, reg_of_string "a0") ]
  | CallIndirect { rd; fn; args } ->
      let arg_insts = List.mapi (fun i slot -> 
        if i >= 8 then MemType (Sd, get_reg slot, reg_of_string "sp", Int64.of_int @@ (i - 7) * 8) 
        else Mv (reg_of_string ("a" ^ string_of_int i), get_reg slot)
      ) args in
      arg_insts @ [ Jalr (get_reg fn);
        Mv (get_reg rd, reg_of_string "a0") ]
  | _ -> []

  (* The option and stack size are reserved for TCO *)
let convert_term (gen_epilogue : Slot.t option -> t list) (stack_frame_size : int) (term : Term.t) : t list =
  match term with
  | Beq { rs1; rs2; ifso; ifnot} -> [ BType (Beq, get_reg rs1, get_reg rs2, ifso.name);
    JType (Jal, reg_of_string "x0", ifnot.name) ]
  | Bne { rs1; rs2; ifso; ifnot} -> [ BType (Bne, get_reg rs1, get_reg rs2, ifso.name);
    JType (Jal, reg_of_string "x0", ifnot.name) ]
  | Blt { rs1; rs2; ifso; ifnot} -> [ BType (Blt, get_reg rs1, get_reg rs2, ifso.name);
    JType (Jal, reg_of_string "x0", ifnot.name) ]
  | Bge { rs1; rs2; ifso; ifnot} -> [ BType (Bge, get_reg rs1, get_reg rs2, ifso.name);
    JType (Jal, reg_of_string "x0", ifnot.name) ]
  | Bltu { rs1; rs2; ifso; ifnot} -> [ BType (Bltu, get_reg rs1, get_reg rs2, ifso.name);
    JType (Jal, reg_of_string "x0", ifnot.name) ]
  | Bgeu { rs1; rs2; ifso; ifnot} -> [ BType (Bgeu, get_reg rs1, get_reg rs2, ifso.name);
    JType (Jal, reg_of_string "x0", ifnot.name) ]
  | J lable -> [ JType (Jal, reg_of_string "x0", lable.name) ]
  (* Why Jal in virtasm as only one argument?  *)
  | Jal label -> [ Call label.name ]
  | Jalr { rd; rs1; offset } -> [ MemType (Jalr, get_reg rd, get_reg rs1, Int64.of_int offset) ]
  | Ret ret -> gen_epilogue (Some ret) @ [ Ret ]
  |_ -> []

let convert_fn f_label (func : VFunc.t) : t list =
  let rpo_func = Riscv_reg_util.RPO.get_func_rpo f_label !Riscv_reg_alloc.rpo in
  let spilled = ref SlotSet.empty in
  let reg_used = ref SlotSet.empty in
  let max_extern_args = ref 0 in

  let stack_frame_size = List.fold_left (fun acc bl ->
    let block = VProg.get_block !vprog bl in
    let len = Vec.fold_left ~f:(fun acc inst ->
      let _ = Inst.inst_convert inst (fun slot -> reg_used := SlotSet.add !reg_used slot;slot) in
      let delta = match inst with
      | Inst.Spill { origin } -> spilled := SlotSet.add !spilled origin; 0
      | Inst.Alloca { size } -> size
      | Inst.Call { args } | Inst.CallIndirect { args } -> max_extern_args := max !max_extern_args ((List.length args) - 8); 0
      | _ -> 0 
      in
      delta + acc
    ) 0 block.body in
    len + acc
  ) 0 rpo_func in
  let stack_frame_size = stack_frame_size + !max_extern_args * 8 + 8 in
  reg_used := SlotSet.filter !reg_used (
    fun slot -> List.exists (fun reg -> match slot with | Slot.Reg slot -> slot = reg |_ -> false)
  Riscv_reg.Reg.callee_saved_regs);
  let stack_frame_size = stack_frame_size + 8 * SlotSet.cardinal !reg_used in
  let stack_frame_size = stack_frame_size + 8 * SlotSet.cardinal !spilled in

  let einfo = Riscv_reg_alloc.get_allocinfo func.entry in
  let prelude = [
    IType (Addi, reg_of_string "sp", reg_of_string "sp", Int64.of_int @@ -stack_frame_size)
  ] in
  let prelude = prelude @ (
      List.mapi (fun i slot -> MemType (Sd, get_reg slot, reg_of_string "sp", Int64.of_int @@ stack_frame_size - (i + 1) * 8))
      (SlotSet.to_list !reg_used)
  ) in
  let prelude = prelude @ List.mapi (fun i slot -> if i >= 8 
    then MemType (Ld, get_reg (SlotMap.find_exn einfo.entry_map slot), reg_of_string "sp", Int64.of_int @@ stack_frame_size + (i - 7) * 8)
    else Mv (get_reg (SlotMap.find_exn einfo.entry_map slot), reg_of_string ("a" ^ string_of_int i))
    ) func.args in

  let spill_offset = stack_frame_size - 8 * SlotSet.cardinal !reg_used in
  spill_map := SlotMap.empty;
  alloca_offset := SlotSet.fold !spilled (spill_offset - 8) (fun slot offset ->
    spill_map := SlotMap.add !spill_map slot offset;
    offset - 8
  );

  let gen_epilogue (ret : Slot.t option) = 
    if Option.is_some ret then [ Mv (reg_of_string "a0", get_reg @@ Option.get ret) ] else []
    @ List.mapi (fun i slot -> MemType (Ld, get_reg slot, reg_of_string "sp", Int64.of_int @@ stack_frame_size - (i + 1) * 8))
      (SlotSet.to_list !reg_used)
    @ [IType (Addi, reg_of_string "sp", reg_of_string "sp", Int64.of_int @@ stack_frame_size)] in
  
  let body = List.fold_left (fun acc bl -> 
    let block = VProg.get_block !vprog bl in
    let body = block.body |> Vec.to_list
                          |> List.map convert_single
                          |> List.flatten
    in
    let term = convert_term gen_epilogue stack_frame_size block.term in
    acc @ [Label bl.name] @ body @ term
  ) [] rpo_func in
  [Label func.funn.name] @ prelude @ body

let last : Imm.t ref = ref @@ Imm.IntImm 0

let convert_const c_label (imm : Imm.t) : t list =
  let data = (match imm with
  | IntImm _ -> ".word"
  | Int64Imm _ -> ".dword"
  | FloatImm _ -> ".float"
  ) ^ Imm.to_string imm in
  let align = match !last, imm with
  | Int64Imm _, _ -> []
  | _, Int64Imm _ -> [Data ".align 3"]
  | _ -> [] in
  last := imm;
  align @ [Label c_label; Data data]

let size_used = ref 0

let convert_global name len : t list =
  let align = 
    if len >= 8 then [Data ".align 3"] else
    if len >= 4 then [Data ".align 2"] else
    if len >= 2 then [Data ".align 1"] else []
  in
  align @ [Label name; Data (Printf.sprintf ".skip %d" len)]

let convert_extarr (extarr : Riscv_ssa.extern_array) : t list =
  let len = if extarr.has_len then [Data ".align 3"; Data (Printf.sprintf ".dword %d" (List.length extarr.values))] else [] in
  let dtype = match extarr.elem_size with
    | 1 -> ".byte"
    | 2 -> ".half"
    | 4 -> ".word"
    | 8 -> ".dword"
    | _ -> failwith "Unsupported extern array element size"
  in
  len @ List.map (fun v -> Data (Printf.sprintf "%s %s" dtype v)) extarr.values

let generate (vprog_arg : VProg.t) : t list =
  vprog := vprog_arg;
  let text_sec = VFuncMap.fold !vprog.funcs [Section "text"] (fun f_label func acc -> acc @ convert_fn f_label func) in
  let rodata_sec = VSymbolMap.fold !vprog.consts [Section "rodata"] (fun label imm acc -> acc @ convert_const label.name imm) in
  let bss_sec = List.fold_left (fun acc (name, len) -> acc @ convert_global name len) [Section "rodata"] !vprog.globals in
  let data_sec = List.fold_left (fun acc extarr -> acc @ convert_extarr extarr) [Section "data"] !vprog.extarrs in

  bss_sec @ rodata_sec @ text_sec