(* 
Student: Renzo Guevarra (rfg9yy)
PL Project 2 - Final Submission
Unstructured Project
Reimplement Assignment from previous COA2 class in OCaml.
The assignment is to implement a Page Table.

Specfically to implement the 3 following functions
1) kth_vpn
2) translate
3) page_allocate

Link to assignment:
https://www.cs.virginia.edu/luther/COA2/S2020/pa02-pagetable.html
*)

open Int64
    
    
let pobits = 8;;
let level = 4;;


let pageSize = (Int.shift_left 1 pobits) + 1;;
let flagIndex = Int.shift_left 1 pobits;; 


let ptbr = Array.make pageSize (
    Some (Array.make pageSize (
        Some (Array.make pageSize (
            Some (Array.make pageSize (None : int option array option)))))));; 


let kth_vpn va k = 
  let level = Int64.of_int level 
  and pobits = Int64.of_int pobits 
  and k = Int64.of_int k in 
  if (Int64.compare k level) >= 0 ||(Int64.compare k 0L) < 0 then
    Int64.minus_one
  else
    let unused = Int64.sub 64L (Int64.add pobits (Int64.mul level (Int64.sub pobits 3L))) in
    let va = Int64.shift_right va (Int64.to_int pobits) in
    let va = Int64.shift_left va (Int64.to_int (Int64.add unused pobits)) in
    let vpnLength = Int64.sub pobits 3L in
    let leftShift = Int64.mul k vpnLength in
    let rightShift = Int64.sub 64L vpnLength in
    let va = Int64.shift_left va (Int64.to_int leftShift) in
    let va = Int64.shift_right va (Int64.to_int rightShift) in
    let mask = Int64.sub (Int64.shift_left 1L (Int64.to_int vpnLength)) 1L in
    (Int64.logand va mask)
    
    

let get_po va = 
  let mask = Int64.sub (Int64.shift_left 1L pobits) 1L in
  (Int64.logand va mask)

  
let translate va = 
  if ptbr.(flagIndex) == None then
    None
  else
    let i0 = Int64.to_int (kth_vpn va 0) in
    let lev1 = ptbr.(i0) in
    if (Option.get lev1).(flagIndex) == None then
      None
    else
      let i1 = Int64.to_int (kth_vpn va 1) in
      let lev2 = (Option.get lev1).(i1) in
      if (Option.get lev2).(flagIndex) == None then
        None
      else
        let i2 = Int64.to_int (kth_vpn va 2) in
        let lev3 = (Option.get lev2).(i2) in
        if (Option.get lev3).(flagIndex) == None then
          None
        else
          let i3 = Int64.to_int (kth_vpn va 3) in
          if (Option.get lev3).(i3) == None then
            None
          else
            let lev4 = (Option.get lev3).(i3) in
            let offset = get_po va in
            Some ((Option.get lev4), (Int64.to_int offset));;
  

let page_allocate va = 
  let i0 = Int64.to_int (kth_vpn va 0) 
  and i1 = Int64.to_int (kth_vpn va 1)
  and i2 = Int64.to_int (kth_vpn va 2)
  and i3 = Int64.to_int (kth_vpn va 3)
  and () = ptbr.(flagIndex) <- Some (Array.make 1 None) in
  let lev1 = ptbr.(i0) in
  let () = (Option.get lev1).(flagIndex) <- Some (Array.make 1 None) in
  let lev2 = (Option.get lev1).(i1) in
  let () = (Option.get lev2).(flagIndex) <- Some (Array.make 1 None) in
  let lev3 = (Option.get lev2).(i2) in
  let () = (Option.get lev3).(flagIndex) <- Some (Array.make 1 None) in
  let () = (Option.get lev3).(i3) <- Some (Array.make (pageSize - 1) (None : int option)) in
  if (Option.get lev3).(i3) == None then
    false
  else
    true;;


let allocated = page_allocate 84645543456236543L;;
let () = assert(allocated);;
let translate_result = translate 84645543456236543L;;
let () = assert(translate_result != None);;
let (arr, offset) = Option.get translate_result;;
arr.(offset) <- Some 3;;
let () = assert(arr.(offset) = (Some 3));;
let translate_result = translate 84645543456236328L;;
let () = assert(translate_result != None);;
let (arr, _) = Option.get translate_result;;
let () = assert(arr.(offset) = (Some 3));;
let translate_result = translate 0L;;
let () = assert(translate_result = None);; 