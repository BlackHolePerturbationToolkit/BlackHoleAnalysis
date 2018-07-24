(* Mathematica Test File *)


Test[
	
	 Simplify /@ (KerrToSchwarzschild[SasakiNakamuraEquation[]] /. SasakiNakamuraFunctionSymbol[] -> MasterFunctionSymbol[Parity -> "Odd"])
 	,
 
	ToFrequencyDomain[MasterEquation[Parity -> "Odd", Homogeneous -> True]],
 
 	TestID->"SasakiNakamuraToRW",
 	
 	EquivalenceFunction -> ((Simplify[#1-#2] == 0) &)
	
]

Test[
	psiESym = MasterFunctionSymbol[Parity -> "Even"];
	psiETeuk = MasterFunctionAsTeukolskyFunction[Parity -> "Even"];
	masterEqEven =  ToFrequencyDomain[MasterEquation[Parity -> "Even", Homogeneous -> True]];
	KerrToSchwarzschild[RemoveTeukolskyFunctionRDerivatives[masterEqEven /. {psiESym[r] -> psiETeuk, Derivative[n_][psiESym][r] :> D[psiETeuk, {r, n}]}]] // Simplify
 	,
 
	0,
 
 	TestID->"EvenMasterAsTeukolsky"
]

Test[
	psiOSym = MasterFunctionSymbol[Parity -> "Odd"];
	psiOTeuk = MasterFunctionAsTeukolskyFunction[Parity -> "Odd"];
	masterEqOdd =  ToFrequencyDomain[ MasterEquation[Parity -> "Odd", Homogeneous -> True]];
	KerrToSchwarzschild[RemoveTeukolskyFunctionRDerivatives[ masterEqOdd /. {psiOSym[r] -> psiOTeuk, Derivative[n_][psiOSym][r] :> D[psiOTeuk, {r, n}]}]] // Simplify
 	,
 
	0,
 
 	TestID->"OddMasterAsTeukolsky"
]

Test[
	xSNSym = SasakiNakamuraFunctionSymbol[];
	rTeukSym = RadialTeukolskyFunctionSymbol[];
	rTeukSN = TeukolskyFunctionAsSasakiNakamuraFunction[];
	teukEqSN = RemoveSasakiNakamuraFunctionRDerivatives[RadialTeukolskyEquation[] /. {rTeukSym[r] -> rTeukSN, Derivative[n_][rTeukSym][r] :> D[rTeukSN, {r, n}]}];
	Collect[teukEqSN, {xSNSym[r], Derivative[_][xSNSym][r]}, Simplify]
	,
	0,
	TestID->"TeukolskyToSasakiNakamura"
]

Test[
	rTeukSym = RadialTeukolskyFunctionSymbol[];
	xSNSym = SasakiNakamuraFunctionSymbol[];
	xSNTeuk = SasakiNakamuraFunctionAsTeukolskyFunction[];
	SNETeuk = RemoveTeukolskyFunctionRDerivatives[SasakiNakamuraEquation[] /. {xSNSym[r] -> xSNTeuk, Derivative[n_][xSNSym][r] :> D[xSNTeuk, {r, n}]}];
	Collect[SNETeuk, {rTeukSym[r], Derivative[_][rTeukSym][r]}, Simplify]
	,
	0,
	TestID->"SasakiNakamuraToTeukolsky"
]

