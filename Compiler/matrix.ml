module L = Llvm
module A = Ast
(* 
open Sast  *)

let mat_size rows columns =  rows*columns

let mat_index width i j = (i*width) + j

let llvm_mat_index (a_addr : L.llvalue) (ival : L.llvalue) (jval : L.llvalue) (name : string) builder = 
	let width_loc = L.build_struct_gep a_addr 1 "" builder in
	let width = L.build_load width_loc "" builder in
	let index_a = L.build_mul width ival "" builder in
	let index = L.build_add index_a jval "" builder in
	(index, builder)

let llvm_mat_size (rows : L.llvalue) (columns: L.llvalue) name builder =
	let value = L.build_mul rows columns name builder in
	(value, builder)

let llvm_output_size op lval rval builder =
	let l_width = L.build_extractvalue lval 1 "" builder in
	let l_height = L.build_extractvalue lval 2 "" builder in
	let r_width = L.build_extractvalue rval 1 "" builder in
	(* let r_height = L.build_extractvalue rval 2 "" builder in *)
	match op with
	A.Add | A.Sub |A.Conv -> (l_width, l_height, builder)
	| A.Div | A.Mult -> (r_width, l_height, builder)
	| _ -> raise (Failure "This should never happen, illegal operation on matrices")