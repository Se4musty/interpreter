(* print string list contents *)
let rec print_string_list (s_list : string list) : unit =
        match s_list with
        | [] -> print_string ""
        | hd::tl -> print_string (hd^"\n"); print_string_list tl 


(* used to convert the remainder of a negative integer string into a returned sub-string  *)
let rec convert_int (str: string)(i: int): string =
        match i with
        | 0 -> ""
        | _ -> (convert_int str (i-1))^(Char.escaped (String.get str i))
 

(* error check to determine if the string is an integer *)
let int_check (str: string): string =
        try int_of_string str; str  
        with _ -> ":error:"        


(* check for 0 negation *)
let int_zero_check (str: string): string =
        match str with
        | "0" -> "0"
        | _ -> ("-"^str)


(* can a string can be an integer *)
let check_if_int (command: string): string =
        if ((String.get command 0) = '-') then 
                (int_zero_check (int_check (convert_int command ((String.length command)-1))))
        else int_check command

      
(* is a string valid *)
let check_if_str (command: string): string =
        if ((String.get command 0) = '"') then
                if ((String.get command ((String.length command)-1)) = '"') then command (*String.sub command 1 ((String.length command)-2)*)
                else ":error:"
        else ":error:" 


(* is a letter in the alphabet *)
let is_letter (c: char): bool =
        if (((Char.code c) >= 65) && ((Char.code c) <= 90) || ((Char.code c) >= 97) && ((Char.code c) <= 122)) 
                then true
        else false


(* is a letter a digit 0-9 *)
let is_digit (c: char): bool =
        if (((Char.code c) >= 48) && ((Char.code c) <= 57)) 
                then true
        else false

       
(* is a name's char valid (returned as a string of 't' or 'f' (true or false))  *)
let is_name_char (c: char): char =
        if (c = '_' || is_letter c || is_digit c) then 't'
        else 'f'


(* is a string a proper name value *)
let check_if_name (command: string): string =
        if (((String.get command 0) = '_') || (is_letter (String.get command 0)))
                then let bool_str = (String.map (is_name_char) command) in
                        if (String.contains bool_str 'f') then ":error:"
                        else command  
        else ":error:"


(* is a string a proprer bool value *)        
let check_if_bool (command: string): string =
        match command with
        | ":true:" -> command
        | ":false:" -> command
        | _ -> ":error:" 


(* is a string a proper push value *)
let check_if_push (command: string): string =
        match command with
        | ":unit:" -> command
        | _ -> ":error:"


(* check if the interpreter can be popped or not *)
let check_pop (stack: string list): bool =
        try List.hd stack; true
        with _ -> false 


(* check if the interpreter can perform an addition operation *)
let check_add (stack: string list): string list =
        match stack with
        | [] -> (":error:"::stack);  
        | hd::tl -> try let int1 = int_of_string hd in 
                        match tl with
                        | [] -> (":error:"::stack)
                        | hd2::tl2 -> try let int2 = int_of_string hd2 in
                                                let sum = int1 + int2 in
                                                        let str_sum = string_of_int sum in (str_sum::tl2)
                                      with _ -> (":error:"::stack)
                    with _ -> (":error:"::stack)


(* check if the interpreter can perform subtraction operation *)
let check_sub (stack: string list): string list =
        match stack with
        | [] -> (":error:"::stack);  
        | hd::tl -> try let int1 = int_of_string hd in 
                        match tl with
                        | [] -> (":error:"::stack)
                        | hd2::tl2 -> try let int2 = int_of_string hd2 in
                                                let diff = int2 - int1 in
                                                        let str_diff = string_of_int diff in (str_diff::tl2)
                                      with _ -> (":error:"::stack)
                    with _ -> (":error:"::stack)


(* check if the interpreter can perform multiplication operation *)
let check_mul (stack: string list): string list =
        match stack with
        | [] -> (":error:"::stack);  
        | hd::tl -> try let int1 = int_of_string hd in 
                        match tl with
                        | [] -> (":error:"::stack)
                        | hd2::tl2 -> try let int2 = int_of_string hd2 in
                                                let result = int2 * int1 in
                                                        let str_res = string_of_int result in (str_res::tl2)
                                      with _ -> (":error:"::stack)
                    with _ -> (":error:"::stack)


(* check if the interpreter can perform division operation *)
let check_div (stack: string list): string list =
        match stack with
        | [] -> (":error:"::stack);  
        | hd::tl -> try let int1 = int_of_string hd in 
                        match tl with
                        | [] -> (":error:"::stack)
                        | hd2::tl2 -> try let int2 = int_of_string hd2 in
                                                if (int1 = 0) then (":error:"::stack)
                                                else let result = int2 / int1 in
                                                        let str_res = string_of_int result in (str_res::tl2)
                                      with _ -> (":error:"::stack)
                    with _ -> (":error:"::stack)


(* check if interpreter can perform remainder op *)
let check_rem (stack: string list): string list =
        match stack with
        | [] -> (":error:"::stack);  
        | hd::tl -> try let int1 = int_of_string hd in 
                        match tl with
                        | [] -> (":error:"::stack)
                        | hd2::tl2 -> try let int2 = int_of_string hd2 in
                                                if (int1 = 0) then (":error:"::stack)
                                                else let result = (int2 mod int1) in
                                                        let str_res = string_of_int result in (str_res::tl2)
                                      with _ -> (":error:"::stack)
                    with _ -> (":error:"::stack)


(* check if interpreter can negate an integer *)
let check_neg (stack: string list): string list =
        match stack with
        | [] -> (":error:"::stack);
        | hd::tl -> try let int1 = int_of_string hd in
                        if (int1 = 0) then stack
                        else let str_res = (string_of_int(-int1)) in (str_res::tl)    
                    with _ -> (":error:"::stack)


(* check if interpreter can swap the first two elements *)
let check_swap (stack: string list): string list =
        match stack with
        | [] -> (":error:"::stack);  
        | hd::tl -> match tl with
                    | [] -> (":error:"::stack)
                    | hd2::tl2 -> (hd2::hd::tl2)


(* check if interp. can concatenate two strings *)
let check_cat (stack: string list): string list =
        match stack with
        | [] -> (":error:"::stack); 
        | hd::tl -> let str1 = check_if_str hd in
                        if (String.equal str1 ":error:") then (":error:"::stack)
                        else match tl with
                        | [] -> (":error:"::stack); 
                        | hd2::tl2 -> let str2 = check_if_str hd2 in
                                        if (String.equal str2 ":error:") then (":error:"::stack)
                                        else (((String.sub str2 0 ((String.length str2)-1))^(String.sub str1 1 ((String.length str1)-1)))::tl2)                               


(* helper to pop from command list, evaluate command, and then push result to interpreter stack *)                    
let rec evaluate_command (commands: string list) (stack: string list): string list =
       match commands with
       | [] -> stack (* shouldn't be executed *)
       | hd::tl -> (match hd with
                   | "pushi" -> (match tl with
                                 | [] -> evaluate_command [] (":error:"::stack)
                                 | v::tl2 -> evaluate_command tl2 ((check_if_int v)::stack)) 
                   | "pushs" -> (match tl with
                                 | [] -> evaluate_command [] (":error:"::stack)
                                 | v::tl2 -> evaluate_command tl2 ((check_if_str v)::stack)) 
                   | "pushn" -> (match tl with
                                | [] -> evaluate_command [] (":error:"::stack)
                                | v::tl2 -> evaluate_command tl2 ((check_if_name v)::stack))
                   | "pushb" -> (match tl with
                                | [] -> evaluate_command [] (":error:"::stack)
                                | v::tl2 -> evaluate_command tl2 ((check_if_bool v)::stack))
                   | "push" -> (match tl with
                                | [] -> evaluate_command [] (":error:"::stack)
                                | v::tl2 -> evaluate_command tl2 ((check_if_push v)::stack))
                   | "pop" -> ((if (check_pop stack) then evaluate_command tl (List.tl stack)
                                else evaluate_command tl (":error:"::stack)))
                   | "add" -> (let stack = check_add stack in evaluate_command tl stack)
                   | "sub" -> (let stack = check_sub stack in evaluate_command tl stack)
                   | "mul" -> (let stack = check_mul stack in evaluate_command tl stack)
                   | "div" -> (let stack = check_div stack in evaluate_command tl stack)
                   | "rem" -> (let stack = check_rem stack in evaluate_command tl stack)
                   | "neg" -> (let stack = check_neg stack in evaluate_command tl stack)
                   | "swap" -> (let stack = check_swap stack in evaluate_command tl stack)
                   | "cat" -> (let stack = check_cat stack in evaluate_command tl stack)
                   | "quit" -> stack
                   | _ -> evaluate_command tl (":error:"::stack))


(* main interpreter method *)
let interpreter (inFile: string) (outFile: string): unit =
(*let interpreter (inFile, outFile): unit =*) 
        (* open input file for reading *)
        let ic = open_in inFile in

        (* Read file line by line, separate each line by " " (spaces) *)
        let rec read_file (com_list: string list) =
                try
                        let l = input_line ic in
                        (* Parse by whitespace *)
                        print_string (l^"\n"); 
                        (*read_file (fix (String.split_on_char ' ' l) com_list); *)
                        (* split and put [0] onto command list *)
                        let command = (List.nth (String.split_on_char ' ' l) 0) in
                        print_string (command^"\n");
                        (* error handling for case of no 2nd arg *)
                        if ((String.length l)-(String.length command) = 0) 
                                then (* check for unary command  *) 
                                     match command with
                                     | "pop" -> (read_file (command::com_list)) 
                                     | "add" -> (read_file (command::com_list))
                                     | "sub" -> (read_file (command::com_list))
                                     | "mul" -> (read_file (command::com_list))
                                     | "div" -> (read_file (command::com_list))
                                     | "rem" -> (read_file (command::com_list))
                                     | "neg" -> (read_file (command::com_list))
                                     | "swap" -> (read_file (command::com_list))
                                     | "cat" -> (read_file (command::com_list))
                                     | "quit" -> (read_file (command::com_list))
                                     | _ -> read_file com_list
                        else (* get the value to the right of the command *)
                             let com_list = (command::com_list) in
                             let start = ((String.length command)+1) in
                             let new_length = (String.length l)-start in
                             let com_list = ((String.sub l start new_length)::com_list) in
                             read_file com_list;
                with
                | End_of_file -> List.rev com_list in 

        let commands = read_file [] in 
        print_string "\nafter parse:\n\n";
        print_string_list commands;
        let stack = evaluate_command commands [] in
        (* Now we have a string list of commands parsed, we need to throw the commands one by one into our stack and evaluate on each entry *)
        print_string "\nStack output :\n\n";
        print_string_list stack;
        
        (* open output file for writing *)
        let oc = open_out outFile in 
        let rec write_to_output (out_stack: string list): unit =
                match out_stack with
                | [] -> ()
                | hd::tl -> if ((String.get hd 0) = '"') then ((Printf.fprintf oc "%s\n" (String.sub hd 1 ((String.length hd)-2))); write_to_output tl)                           
                            else (Printf.fprintf oc "%s\n" hd); write_to_output tl
        in
        write_to_output stack;;
  

(* test to be removed*) 
interpreter "test_input1.txt" "output.txt"
