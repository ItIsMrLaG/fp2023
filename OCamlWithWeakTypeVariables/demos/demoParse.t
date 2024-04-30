  $ ./demoParse.exe << EOF
  > let rec fac = fun n -> if n <= 1 then 1 else n * fac (n - 1) in fac 5
  > EOF
  (ELetIn
     { name = (MPVar "fac"); rec_flag = Rec;
       value =
       EFun {args = ((MPVar "n"), []);
         expr =
         Eite {
           cond =
           EBinOp {op = BLE; left = (EVar "n"); right = (EConst (CInt 1))};
           th = (EConst (CInt 1));
           el =
           EBinOp {op = BMul; left = (EVar "n");
             right =
             (EApply ((EVar "fac"),
                EBinOp {op = BSub; left = (EVar "n"); right = (EConst (CInt 1))}
                ))}}};
       expr = (EApply ((EVar "fac"), (EConst (CInt 5)))) })
