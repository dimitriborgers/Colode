(* Top-level of the compiler: scan & parse the input, check the resulting AST, generate LLVM IR, and dump the mudule *)
type action = Ast | Sast | LLVM_IR 
let () =
	let action = ref LLVM_IR in
	let set_action a () = action := a in
	let speclist = [
	("-a", Arg.Unit (set_action Ast), "Print the AST");
	("-s", Arg.Unit (set_action Sast), "Print the SAST");
	("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
	] in
	let usage_msg = "usage: ./colode.native [-a|-s|-l] [file.cld]" in
	let channel = ref stdin in
	Arg.parse speclist (fun file -> channel := open_in file) usage_msg; 
	let lexbuf = Lexing.from_channel !channel in
	let ast = Parser.program Scanner.token lexbuf in
	match !action with
	  Ast -> print_string (Ast.string_of_program ast)
	| _ -> let sast = Semant.check ast in
		match !action with
		  Ast -> ()
		| Sast -> print_string (Sast.string_of_sprogram sast)
		| LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate sast))

(*
type action = Ast | LLVM_IR | Compile

let _ =
		let action = if Array.length Sys.argv > 1 then
				List.assoc Sys.argv.(1) [ ("-a", Ast);
						("-l", LLVM_IR);
						("-c", Compile)]
		else Compile in
		let lexbuf = Lexing.from_channel stdin in
		let ast = Parser.program Scanner.token lexbuf in
		(match action with
				Ast -> print_string (Ast.string_of_program ast)
				| LLVM_IR -> print_string ("LLVM_IR not implemented yet")
				| Compile -> print_string ("Compile not implemented yet"))
*)