(* Luca Lombardo, Mat. 546688 *)

let e = emptyEnv;;

(* Testo il tipo Set *)

let emp1 = Empty(StringSet);;
let sin1 = Singleton(IntSet, CstInt(4));;
let of1 = Of(StringSet, [EString("Hello"); EString("world")]);;
let sinErr1 = Singleton(IntSet, EString("Ciao"));;
let ofErr1 = Of(StringSet, [EString("Ciao"); CstInt(2)]);;
let dup1 = Of(StringSet, [EString("Ciao"); EString("Ciao")]);;

eval emp1 e;; (* set vuoto di tipo StringSet *)
eval sin1 e;; (* set contenente l'elemento 4 *)
eval of1 e;; (* set contenente gli elementi: Hello, World *)
eval sinErr1 e;; (* errore di tipo *)
eval ofErr1 e;; (* errore di tipo *)
eval dup1 e;; (* errore: presenza di elementi duplicati *)

(* Testo Union *)

let emp2 = Empty(IntSet);;
let of2 = Of(IntSet, [CstInt(0); CstInt(4)]);;
let un1 = Union(emp1, emp1);;
let un2 = Union(sin1, emp2);;
let un3 = Union(of2, sin1);;
let un4 = Union(CstInt(2), sin1);;
let un5 = Union(sin1, of1);;

eval un1 e;; (* set vuoto di tipo StringSet *)
eval un2 e;; (* set contenente l'elemento 4 *)
eval un3 e;; (* set contenente gli elementi 0, 4 *)
eval un4 e;; (* errore: non è un set *)
eval un5 e;; (* errore: set di tipi diversi *)

(* Testo Intersec *)

let in1 = Intersec(emp2, sin1);;
let in2 = Intersec(of2, sin1);;
let in3 = Intersec(CstInt(2), sin1);;
let in4 = Intersec(sin1, of1);;

eval in1 e;; (* set vuoto di tipo IntSet *)
eval in2 e;; (* set contenente l'elemento 4 *)
eval in3 e;; (* errore: non è un set *)
eval in4 e;; (* errore: set di tipi diversi *)

(* Testo Diff *)

let dif1 = Diff(sin1, emp2);;
let dif2 = Diff(of2, sin1);;
let dif3 = Diff(CstInt(1), sin1);;
let dif4 = Diff(sin1, of1);;

eval dif1 e;; (* set contenente l'elemento 4 *)
eval dif2 e;; (* set contenente l'elemento 0 *)
eval dif3 e;; (* errore: non è un set *)
eval dif4 e;; (* errore: set di tipi diversi *)

(* Testo Add *)

let add1 = Add(emp1, EString("Ciao"));;
let add2 = Add(sin1, CstInt(1));;
let add3 = Add(sin1, CstInt(4));;
let add4 = Add(CstInt(1), CstInt(2));;
let add5 = Add(sin1, EString("Prova"));;

eval add1 e;; (* set contenente l'elemento Ciao *)
eval add2 e;; (* set contenente gli elementi 1, 4 *)
eval add3 e;; (* set contenente l'elemento 4 *)
eval add4 e;; (* errore: non è un set *)
eval add5 e;; (* errore: set ed elemento hanno tipi diversi *)

(* Testo Remove *)

let rem1 = Remove(sin1, CstInt(4));;
let rem2 = Remove(of1, EString("world"));;
let rem3 = Remove(of1, EString("Parola"));;
let rem4 = Remove(CstInt(1), CstInt(2));;
let rem5 = Remove(sin1, EString("Prova"));;

eval rem1 e;; (* set vuoto di tipo IntSet *)
eval rem2 e;; (* set contenente l'elemento Hello *)
eval rem3 e;; (* set contenente gli elementi Hello, world *)
eval rem4 e;; (* errore: non è un set *)
eval rem5 e;; (* errore: set ed elemento hanno tipi diversi *)

(* Testo IsEmpty *)

let ie1 = IsEmpty(emp1);;
let ie2 = IsEmpty(of1);;
let ie3 = IsEmpty(rem1);;
let ie4 = IsEmpty(CstInt(2));;

eval ie1 e;; (* true *)
eval ie2 e;; (* false *)
eval ie3 e;; (* true *)
eval ie4 e;; (* errore: non è un set *)

(* Testo Member *)

let mem1 = Member(emp1, EString("Ciao"));;
let mem2 = Member(of1, EString("Hello"));;
let mem3 = Member(CstInt(1), CstInt(2));;
let mem4 = Member(of1, CstInt(5));;

eval mem1 e;; (* false *)
eval mem2 e;; (* true *)
eval mem3 e;; (* errore: non è un set *)
eval mem4 e;; (* errore: set ed elemento hanno tipi diversi *)

(* Testo Subset *)

let sub1 = Subset(emp1, of1);;
let sub2 = Subset(sin1, of2);;
let sub3 = Subset(add2, add3);;
let sub4 = Subset(emp1, CstInt(7));;
let sub5 = Subset(sin1, of1);;

eval sub1 e;; (* true *)
eval sub2 e;; (* true *)
eval sub3 e;; (* false *)
eval sub4 e;; (* errore: non è un set *)
eval sub5 e;; (* errore: set di tipi diversi *)

(* Testo Min *)

let boo1 = Of(BoolSet, [CstTrue; CstFalse]);;
let min1 = Min(sin1);;
let min2 = Min(of1);;
let min3 = Min(of2);;
let min4 = Min(boo1);;
let min5 = Min(emp1);;
let min6 = Min(CstInt(2));;

eval min1 e;; (* 4 *)
eval min2 e;; (* Hello *)
eval min3 e;; (* 0 *)
eval min4 e;; (* false *)
eval min5 e;; (* errore: set vuoto *)
eval min6 e;; (* errore: non è un set *)

(* Testo Max *)

let max1 = Max(sin1);;
let max2 = Max(of1);;
let max3 = Max(of2);;
let max4 = Max(boo1);;
let max5 = Max(emp2);;
let max6 = Max(EString("Ciao"));;

eval max1 e;; (* 4 *)
eval max2 e;; (* World *)
eval max3 e;; (* 4 *)
eval max4 e;; (* true *)
eval max5 e;; (* errore: set vuoto *)
eval max6 e;; (* errore: non è un set *)

(* Testo For_all *)

let f1 = Fun("x", Ifthenelse(Eq(Den("x"), CstInt(4)), CstTrue, CstFalse));;
let f2 = Fun("x", Ifthenelse(Eq(Den("x"), CstInt(4)), EString("Sì"), EString("No")));;
let fa1 = For_all(f1, sin1);;
let fa2 = For_all(f1, of2);;
let fa3 = For_all(f1, emp1);;
let fa4 = For_all(f2, of2);;
let fa5 = For_all(f1, CstInt(2));;

eval fa1 e;; (* true *)
eval fa2 e;; (* false *)
eval fa3 e;; (* true *)
eval fa4 e;; (* errore: non è un predicato *)
eval fa5 e;; (* errore: non è un set *)

(* Testo Exists *)

let f3 = Fun("x", Ifthenelse(Eq(Den("x"), CstInt(1)), CstTrue, CstFalse));;
let exi1 = Exists(f1, sin1);;
let exi2 = Exists(f3, of2);;
let exi3 = Exists(f3, emp1);;
let exi4 = Exists(f2, of2);;
let exi5 = Exists(f1, CstTrue);;

eval exi1 e;; (* true *)
eval exi2 e;; (* false *)
eval exi3 e;; (* false *)
eval exi4 e;; (* errore: non è un predicato *)
eval exi5 e;; (* errore: non è un set *)

(* Testo Filter *)

let fil1 = Filter(f1, emp1);;
let fil2 = Filter(f1, of2);;
let fil3 = Filter(f2, of2);;
let fil4 = Filter(f1, CstInt(0));;

eval fil1 e;; (* set vuoto di tipo StringSet *)
eval fil2 e;; (* set contenente l'elemento 4 *)
eval fil3 e;; (* errore: non è un predicato *)
eval fil4 e;; (* errore: non è un set *)

(* Testo Map *)

let f4 = Fun("x", Sum(Den("x"), CstInt(1)));;
let f5Err = Fun("x", Ifthenelse(Eq(Den("x"), CstInt(4)), CstInt(2), CstFalse));;
let map1 = Map(f4, sin1);;
let map2 = Map(f4, of2);;
let map3 = Map(f4, emp2);;
let map4 = Map(CstInt(1), sin1);;
let map5 = Map(f4, EString("Ciao"));;
let map6 = Map(f5Err, of2);;

eval map1 e;; (* set contenente l'elemento 5 *)
eval map2 e;; (* set contenente gli elementi 1, 5 *)
eval map3 e;; (* set vuoto di tipo IntSet *)
eval map4 e;; (* errore: non è una funzione *)
eval map5 e;; (* errore: non è un set *)
eval map6 e;; (* errore: funzione con risultati di tipo diverso fra loro *)