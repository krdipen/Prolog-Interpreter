open Structure

let table =
    let filename = Sys.argv.(1) in
    let file_handle = open_in filename in
    let lexbuf = Lexing.from_channel file_handle in
    let rec createTable (db:database) :database =
        let result = Parser.main Lexer.token lexbuf in
        match result with
              (C("EOF"),[]) -> db
            | _ -> createTable (result :: db) in
    createTable [] ;;

let rec pushelement (set0:variable list) (set1:variable list) (v:variable) :variable list = match set1 with
      [] -> set0 @ [v]
    | x :: xs -> if (x = v) then set0 @ set1 else pushelement (set0 @ [x]) xs v ;;

let rec pushset (set1:variable list) (set2:variable list) :variable list = match set2 with
      [] -> set1
    | x :: xs -> pushset (pushelement [] set1 x) xs ;;

let rec vars (t:term) :variable list = match t with
      V(z) -> [z]
    | C(c) -> []
    | Node(y,ys) -> varslist ys []
and varslist (tlist:term list) (set:variable list) :variable list = match tlist with
      [] -> set
    | x :: xs -> varslist xs (pushset set (vars x)) ;;

let rec replace (v:variable) (s:substitution) :term = match s with
      [] -> V(v)
    | (x1,x2) :: xs -> if (v = x1) then x2 else replace v xs ;;

let rec subst (t:term) (s:substitution) :term = match t with
      V(z) -> replace z s
    | C(c) -> C(c)
    | Node(y,ys) -> Node(y,substlist ys [] s)
and substlist (oldtl:term list) (newtl:term list) (s:substitution) :term list = match oldtl with
      [] -> newtl
    | x :: xs -> substlist xs (newtl @ [subst x s]) s ;;

let rec check_aval (v:variable) (set:variable list) :bool = match set with
      [] -> true
    | x :: xs -> if (v = x) then false else check_aval v xs ;;

let rec comp (s1:substitution) (s2:substitution) :substitution = match s1 with
      [] -> s2
    | (x1,x2) :: xs -> comp xs ((x1,subst x2 s2) :: s2) ;;

let rec mgu (g:term) (h:term) :substitution = match g with
      V(v1) -> (match h with
                  V(v2) -> if (v1 = v2) then [] else [(v1,V(v2))]
                | C(c2) -> [(v1,C(c2))]
                | Node(s2,xs2) -> if (check_aval v1 (vars h)) then [(v1,h)] else [("@",C("@"))])
    | C(c1) -> (match h with
                  V(v2) -> [(v2,C(c1))]
                | C(c2) -> if (c1 = c2) then [] else [("@",C("@"))]
                | Node(s2,xs2) -> [("@",C("@"))])
    | Node(s1,xs1) -> (match h with
                  V(v2) -> if (check_aval v2 (vars g)) then [(v2,g)] else [("@",C("@"))]
                | C(c2) -> [("@",C("@"))]
                | Node(s2,xs2) -> if (s1 = s2) then mgulist xs1 xs2 [] else [("@",C("@"))])
and mgulist (gs:term list) (hs:term list) (s:substitution) :substitution = match gs with
      [] -> (match hs with
               [] -> s
             | x2 :: xs2 -> [("@",C("@"))])
    | x1 :: xs1 -> (match hs with
                      [] -> [("@",C("@"))]
                    | x2 :: xs2 -> let u =(mgu (subst x1 s) (subst x2 s)) in
                        (match u with
                              [("@",C("@"))] -> [("@",C("@"))]
                            | _ -> mgulist xs1 xs2 (comp s u))) ;;

let rec print_term (t:term) :unit = match t with
      V(v) -> print_string v
    | C(c) -> print_string c
    | Node(s,xs) -> (print_string s ; print_string "(" ; print_termlist xs ; print_string ")")
and print_termlist (ts:term list) :unit = match ts with
      [] -> ()
    | x :: [] -> print_term x
    | x1 :: (x2 :: xs) -> (print_term x1 ; print_string "," ; print_termlist (x2 :: xs)) ;;

let rec print_sub (s:substitution) :string = match s with
      [] -> (print_string "true " ; read_line())
    | (x1,x2) :: xs -> (print_string x1 ; print_string " = " ; print_term x2 ; print_newline() ; print_sub xs) ;;

let rec update (s1:substitution) (s2:substitution) :substitution = match s1 with
      [] -> []
    | (x1,x2) :: xs -> (x1,subst x2 s2) :: (update xs s2) ;;

let rec append (t:term) :term = match t with
      V(v) -> V(v^"@")
    | C(c) -> C(c)
    | Node(s,xs) -> Node(s,appendlist xs [])
and appendlist (oldtl:term list) (newtl:term list) :term list = match oldtl with
      [] -> newtl
    | x :: xs -> appendlist xs (newtl @ [append x]) ;;

let rec appendsub (s:substitution) :substitution = match s with
      [] -> []
    | (x1,x2) :: xs -> (x1,append x2) :: (appendsub xs) ;;

let rec appendsub_h (vs:variable list) :substitution = match vs with
      [] -> []
    | x :: xs -> (x,V(x^"@")) :: (appendsub_h xs) ;;

let rec search (t:term) (db:database) (stack:term list) (s:substitution) :string = match db with
      [] -> ";"
    | (x,xs) :: xss -> (let str =
            let u = (mgu t x) in
            (match u with
                  [("@",C("@"))] -> ";"
                | _ -> (match xs with
                          [] -> (match stack with
                                    [] -> (print_newline() ; print_sub (update s u))
                                  | z :: zs -> search (append (subst z u)) table (appendlist (substlist zs [] u) []) (appendsub (update s u)))
                        | y :: ys -> search (append (subst y u)) table (appendlist (substlist (ys @ stack) [] u) []) (appendsub (update s u)))) in
            (match str with
                  ";" -> search t xss stack s
                | "." -> "."
                | _ -> "INVALID INPUT")) ;;

let _ =
    print_newline() ; print_string "?- " ; flush stdout ;
    let lexbuf = Lexing.from_channel stdin in
    while true do let str =
        let result = Parser.main Lexer.token lexbuf in
        (match result with
              (C("exit"),[]) -> "EXIT"
            | (x,[]) -> search (append x) table [] (appendsub_h (vars x))
            | _ -> "INVALID GOAL") in
        (match str with
              ";" -> (print_newline() ; print_string "false ." ; print_newline() ; print_newline() ; print_string "?- " ; flush stdout)
            | "." -> (print_newline() ; print_string "?- " ; flush stdout)
            | "INVALID INPUT" -> (print_newline() ; print_string "INVALID INPUT" ; print_newline() ; print_newline() ; print_string "?- " ; flush stdout)
            | "EXIT" -> (print_newline() ; print_string "EXITING" ; print_newline() ; print_newline() ; exit 0)
            | "INVALID GOAL" -> (print_newline() ; print_string "INVALID GOAL" ; print_newline() ; print_newline() ; print_string "?- "  ; flush stdout)
            | _ -> ())
    done ;;
