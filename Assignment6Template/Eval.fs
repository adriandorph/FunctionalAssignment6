module Eval

    open StateMonad

    (* Code for testing *)
    let hello = [('H',4);('E',1);('L',1);('L',1);('O',1)]
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    let add (a:SM<int>) (b:SM<int>) = 
        a >>= fun x-> 
        b >>= fun y -> 
        ret (x + y)

    let sub a b =
        a >>= fun x ->
        b >>= fun y ->
        ret (x - y)

    let div a b =
        a >>= fun x ->
        b >>= fun y ->
        if y <> 0 then ret (x / y) else fail DivisionByZero
    
    let mul a b =
      a >>= fun x ->
      b >>= fun y ->
      ret (x * y)
    
    let modulo a b =
        a >>= fun x ->
        b >>= fun y ->
        if y <> 0 then ret (x % y) else fail DivisionByZero

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval (a: aExp) : SM<int> = 
        match a with
        | N x -> ret (x)
        | V s -> lookup s 
        | WL -> wordLength
        | PV pos ->  arithEval pos >>= pointValue
        | Add (a,b) -> add (arithEval a) (arithEval b)
        | Sub (a,b) -> sub (arithEval a) (arithEval b)
        | Mul (a,b) -> mul (arithEval a) (arithEval b)
        | Div (a,b) -> div (arithEval a) (arithEval b)
        | Mod (a,b) -> modulo (arithEval a) (arithEval b)
        | CharToInt c -> (charEval c) >>= fun x -> ret (int x)

    and charEval c : SM<char> =
        match c with
        | C c -> ret (c) (* Character value *)
        | CV pos -> arithEval pos >>= characterValue (* Character lookup at word index *)
        | ToUpper c -> (charEval c) >>= fun c' -> charEval (C (System.Char.ToUpper c'))
        | ToLower c -> (charEval c) >>= fun c' -> charEval (C (System.Char.ToLower c'))
        | IntToChar a -> (arithEval a) >>= fun x -> ret (char x)

    let rec boolEval b : SM<bool> = 
        match b with
        | TT -> ret (true)
        | FF -> ret (false)

        | AEq (a,b) -> (arithEval a) >>= fun x -> (arithEval b) >>= fun y -> ret (x = y)
        | ALt (a,b) -> (arithEval a) >>= fun x -> (arithEval b) >>= fun y -> ret (x < y)

        | Not b -> (boolEval b) >>= fun b' -> ret (not b')
        | Conj (a,b) -> (boolEval a) >>= fun x -> (boolEval b) >>= fun y -> ret (x && y)

        | IsVowel c -> (charEval c) >>= fun c' -> ret ("AEIOUYaeiouy".Contains(c'))
        | IsLetter c -> (charEval c) >>= fun c' -> ret (System.Char.IsLetter c')
        | IsDigit c -> (charEval c) >>= fun c' -> ret (System.Char.IsDigit c')


    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm = failwith "Not implemented"


    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun stm m = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
    