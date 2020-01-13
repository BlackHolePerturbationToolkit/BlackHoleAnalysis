syms=DefaultSymbols[];




VerificationTest[
    MasterFunction[syms],

    XZM[t, r],

    TestID->"MasterFunction[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    HttAmplitude[syms],

    h[Rule["Indices", List[Rule["Down", List["t", "t"]]]]][Rule["Tags", List[Rule["Up", List["RWZTag"]]]]][t, r],

    TestID->"HttAmplitude[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    HtrAmplitude[syms],

    h[Rule["Indices", List[Rule["Down", List["t", "r"]]]]][Rule["Tags", List[Rule["Up", List["RWZTag"]]]]][t, r],

    TestID->"HtrAmplitude[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    HrrAmplitude[syms],

    h[Rule["Indices", List[Rule["Down", List["r", "r"]]]]][Rule["Tags", List[Rule["Up", List["RWZTag"]]]]][t, r],

    TestID->"HrrAmplitude[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    JtAmplitude[syms],

    0,

    TestID->"JtAmplitude[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    JrAmplitude[syms],

    0,

    TestID->"JrAmplitude[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    GAmplitude[syms],

    0,

    TestID->"GAmplitude[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    KAmplitude[syms],

    \[ScriptCapitalK][Rule["Tags", List[Rule["Up", List["RWZTag"]]]]][t, r],

    TestID->"KAmplitude[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    HttInvariantAmplitude[syms],

    h[Rule["Indices", List[Rule["Down", List["t", "t"]]]]][Rule["Tags", List[Rule["Up", List["RWZTag"]]]]][t, r],

    TestID->"HttInvariantAmplitude[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    HtrInvariantAmplitude[syms],

    h[Rule["Indices", List[Rule["Down", List["t", "r"]]]]][Rule["Tags", List[Rule["Up", List["RWZTag"]]]]][t, r],

    TestID->"HtrInvariantAmplitude[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    HrrInvariantAmplitude[syms],

    h[Rule["Indices", List[Rule["Down", List["r", "r"]]]]][Rule["Tags", List[Rule["Up", List["RWZTag"]]]]][t, r],

    TestID->"HrrInvariantAmplitude[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    KInvariantAmplitude[syms],

    \[ScriptCapitalK][Rule["Tags", List[Rule["Up", List["RWZTag"]]]]][t, r],

    TestID->"KInvariantAmplitude[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    HtAmplitude[syms],

    h[Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["RWZTag"]]]]][t, r],

    TestID->"HtAmplitude[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    HrAmplitude[syms],

    h[Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["RWZTag"]]]]][t, r],

    TestID->"HrAmplitude[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    H2Amplitude[syms],

    0,

    TestID->"H2Amplitude[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    HtInvariantAmplitude[syms],

    h[Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["RWZTag"]]]]][t, r],

    TestID->"HtInvariantAmplitude[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    HrInvariantAmplitude[syms],

    h[Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["RWZTag"]]]]][t, r],

    TestID->"HrInvariantAmplitude[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    HttPush[syms],

    Plus[Times[-2, Derivative[1, 0][\[Xi][Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag"]]]]]][t, r]], Times[2, M, Plus[1, Times[-2, M, Power[r, -1]]], Power[r, -2], \[Xi][Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag"]]]]][t, r]]],

    TestID->"HttPush[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    HtrPush[syms],

    Plus[Times[-1, Derivative[0, 1][\[Xi][Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag"]]]]]][t, r]], Times[-1, Derivative[1, 0][\[Xi][Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag"]]]]]][t, r]], Times[2, M, Power[Plus[1, Times[-2, M, Power[r, -1]]], -1], Power[r, -2], \[Xi][Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag"]]]]][t, r]]],

    TestID->"HtrPush[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    HrrPush[syms],

    Plus[Times[-2, Derivative[0, 1][\[Xi][Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag"]]]]]][t, r]], Times[-2, M, Power[Plus[1, Times[-2, M, Power[r, -1]]], -1], Power[r, -2], \[Xi][Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag"]]]]][t, r]]],

    TestID->"HrrPush[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    JtPush[syms],

    Plus[Times[-1, Derivative[1, 0][\[Xi][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag"]]]]]][t, r]], Times[-1, \[Xi][Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag"]]]]][t, r]]],

    TestID->"JtPush[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    JrPush[syms],

    Plus[Times[2, Power[r, -1], \[Xi][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag"]]]]][t, r]], Times[-1, Derivative[0, 1][\[Xi][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag"]]]]]][t, r]], Times[-1, \[Xi][Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag"]]]]][t, r]]],

    TestID->"JrPush[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    KPush[syms],

    Plus[Times[2, Plus[1, Times[Rational[1, 2], Plus[-1, l], Plus[2, l]]], Power[r, -2], \[Xi][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag"]]]]][t, r]], Times[-2, Plus[1, Times[-2, M, Power[r, -1]]], Power[r, -1], \[Xi][Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag"]]]]][t, r]]],

    TestID->"KPush[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    GPush[syms],

    Times[-2, Power[r, -2], \[Xi][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag"]]]]][t, r]],

    TestID->"GPush[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    HtPush[syms],

    Times[-1, Derivative[1, 0][\[Xi][Rule["Tags", List[Rule["Up", List["OddTag", "RWZTag", "LorenzTag"]]]]]][t, r]],

    TestID->"HtPush[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    HrPush[syms],

    Plus[Times[2, Power[r, -1], \[Xi][Rule["Tags", List[Rule["Up", List["OddTag", "RWZTag", "LorenzTag"]]]]][t, r]], Times[-1, Derivative[0, 1][\[Xi][Rule["Tags", List[Rule["Up", List["OddTag", "RWZTag", "LorenzTag"]]]]]][t, r]]],

    TestID->"HrPush[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    H2Push[syms],

    Times[-2, \[Xi][Rule["Tags", List[Rule["Up", List["OddTag", "RWZTag", "LorenzTag"]]]]][t, r]],

    TestID->"H2Push[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    XiEvenAmplitude[syms],

    \[Xi][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag"]]]]][t, r],

    TestID->"XiEvenAmplitude[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    XiEvenTAmplitude[syms],

    \[Xi][Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag"]]]]][t, r],

    TestID->"XiEvenTAmplitude[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    XiEvenRAmplitude[syms],

    \[Xi][Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag"]]]]][t, r],

    TestID->"XiEvenRAmplitude[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    XiOddAmplitude[syms],

    \[Xi][Rule["Tags", List[Rule["Up", List["OddTag", "RWZTag", "LorenzTag"]]]]][t, r],

    TestID->"XiOddAmplitude[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (HttAmplitude[#1, Gauge -> "RWZ", Weak -> True] & )[syms],

    Plus[Times[\[Theta][Plus[Times[-1, r], rp[t]]], h[Rule["Indices", List[Rule["Down", List["t", "t"]]]]][Rule["Tags", List[Rule["Up", List["RWZTag", "MinusTag"]]]]][t, r]], Times[\[Theta][Plus[r, Times[-1, rp[t]]]], h[Rule["Indices", List[Rule["Down", List["t", "t"]]]]][Rule["Tags", List[Rule["Up", List["RWZTag", "PlusTag"]]]]][t, r]], Times[\[Delta][Plus[r, Times[-1, rp[t]]]], G[0][0, 0][h[Rule["Indices", List[Rule["Down", List["t", "t"]]]]][Rule["Tags", List[Rule["Up", List["RWZTag"]]]]]][t]]],

    TestID->"(HttAmplitude[#1, Gauge -> RWZ, Weak -> True] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (HttAmplitude[#1, Gauge -> "RWZ", Reconstruct -> True] & )[syms],

    Plus[Times[Rational[1, 2], Plus[Times[2, M], Times[-1, r]], Power[r, -3], Power[Plus[Times[6, M], Times[Plus[-2, l, Power[l, 2]], r]], -2], Plus[Times[72, Power[M, 3]], Times[36, Plus[-2, l, Power[l, 2]], Power[M, 2], r], Times[6, Power[Plus[-2, l, Power[l, 2]], 2], M, Power[r, 2]], Times[l, Plus[1, l], Power[Plus[-2, l, Power[l, 2]], 2], Power[r, 3]]], XZM[t, r]], Times[2, Power[l, -1], Power[Plus[1, l], -1], Power[r, 2], Power[Plus[Times[6, M], Times[Plus[-2, l, Power[l, 2]], r]], -2], Plus[Times[108, Power[M, 2]], Times[4, Plus[-19, Times[5, l], Times[5, Power[l, 2]]], M, r], Times[Plus[12, Times[-8, l], Times[-7, Power[l, 2]], Times[2, Power[l, 3]], Power[l, 4]], Power[r, 2]]], Q[Rule["Indices", List[Rule["Down", List["t", "t"]]]]][t, r]], Times[Plus[1, Times[-2, M, Power[r, -1]]], Q[Rule["Tags", List[Rule["Up", List["Sharp"]]]]][t, r]], Times[-1, Plus[Times[2, M], Times[-1, r]], Power[r, -2], Power[Plus[Times[6, M], Times[Plus[-2, l, Power[l, 2]], r]], -1], Plus[Times[6, Power[M, 2]], Times[-1, Plus[-2, l, Power[l, 2]], M, r], Times[Plus[-2, l, Power[l, 2]], Power[r, 2]]], Derivative[0, 1][XZM][t, r]], Times[4, Power[l, -1], Power[Plus[1, l], -1], Plus[Times[2, M], Times[-1, r]], Power[r, 3], Power[Plus[Times[6, M], Times[Plus[-2, l, Power[l, 2]], r]], -1], Derivative[0, 1][Q[Rule["Indices", List[Rule["Down", List["t", "t"]]]]]][t, r]], Times[Power[r, -1], Power[Plus[Times[-2, M], r], 2], Derivative[0, 2][XZM][t, r]]],

    TestID->"(HttAmplitude[#1, Gauge -> RWZ, Reconstruct -> True] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (HttAmplitude[#1, Gauge -> "Lorenz"] & )[syms],

    h[Rule["Indices", List[Rule["Down", List["t", "t"]]]]][Rule["Tags", List[Rule["Up", List["LorenzTag"]]]]][t, r],

    TestID->"(HttAmplitude[#1, Gauge -> Lorenz] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (HttAmplitude[#1, Gauge -> "Lorenz", Weak -> True] & )[syms],

    Plus[Times[\[Theta][Plus[Times[-1, r], rp[t]]], h[Rule["Indices", List[Rule["Down", List["t", "t"]]]]][Rule["Tags", List[Rule["Up", List["LorenzTag", "MinusTag"]]]]][t, r]], Times[\[Theta][Plus[r, Times[-1, rp[t]]]], h[Rule["Indices", List[Rule["Down", List["t", "t"]]]]][Rule["Tags", List[Rule["Up", List["LorenzTag", "PlusTag"]]]]][t, r]]],

    TestID->"(HttAmplitude[#1, Gauge -> Lorenz, Weak -> True] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (HttAmplitude[#1, Gauge -> "RWZ", Reconstruct -> True, SourceExpansion -> "Full"] & )[syms],

    Plus[Times[Rational[1, 2], Plus[Times[2, M], Times[-1, r]], Power[r, -3], Power[Plus[Times[6, M], Times[Plus[-2, l, Power[l, 2]], r]], -2], Plus[Times[72, Power[M, 3]], Times[36, Plus[-2, l, Power[l, 2]], Power[M, 2], r], Times[6, Power[Plus[-2, l, Power[l, 2]], 2], M, Power[r, 2]], Times[l, Plus[1, l], Power[Plus[-2, l, Power[l, 2]], 2], Power[r, 3]]], XZM[t, r]], Times[-32, Power[l, -1], Power[Plus[1, l], -1], Pi, Plus[Times[2, M], Times[-1, r]], Power[r, 3], Power[Plus[Times[6, M], Times[Plus[-2, l, Power[l, 2]], r]], -1], \[ScriptCapitalE], \[Mu], Plus[Times[2, M], Times[-1, rp[t]]], Power[rp[t], -3], Y[Rule["OverStr", "Bar"]][t], Derivative[1][\[Delta]][Plus[r, Times[-1, rp[t]]]]], Times[-1, Plus[Times[2, M], Times[-1, r]], Power[r, -2], Power[Plus[Times[6, M], Times[Plus[-2, l, Power[l, 2]], r]], -1], Plus[Times[6, Power[M, 2]], Times[-1, Plus[-2, l, Power[l, 2]], M, r], Times[Plus[-2, l, Power[l, 2]], Power[r, 2]]], Derivative[0, 1][XZM][t, r]], Times[Power[r, -1], Power[Plus[Times[-2, M], r], 2], Derivative[0, 2][XZM][t, r]], Times[16, Power[l, -1], Power[Plus[1, l], -1], Pi, Plus[1, Times[-2, M, Power[r, -1]]], Power[\[ScriptCapitalE], -1], \[Mu], Plus[Times[2, M], Times[-1, rp[t]]], Power[rp[t], -3], \[Delta][Plus[r, Times[-1, rp[t]]]], Plus[Times[Power[Plus[Times[2, M], Times[-1, r]], -1], Power[r, 3], Power[Plus[Times[6, M], Times[Plus[-2, l, Power[l, 2]], r]], -2], Plus[Times[108, Power[M, 2]], Times[4, Plus[-19, Times[5, l], Times[5, Power[l, 2]]], M, r], Times[Plus[12, Times[-8, l], Times[-7, Power[l, 2]], Times[2, Power[l, 3]], Power[l, 4]], Power[r, 2]]], Power[\[ScriptCapitalE], 2], Y[Rule["OverStr", "Bar"]][t]], Times[-2, Power[Plus[-2, l, Power[l, 2]], -1], Power[\[ScriptCapitalL], 2], Y[Rule["OverStr", "Bar"]][Rule["Indices", List[Rule["Down", List["phi", "phi"]]]]][t]]]]],

    TestID->"(HttAmplitude[#1, Gauge -> RWZ, Reconstruct -> True, SourceExpansion -> Full] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (HtrAmplitude[#1, Gauge -> "RWZ", Reconstruct -> True, SourceExpansion -> "Full"] & )[syms],

    Plus[Times[Derivative[1][rp][t], Plus[Times[16, Power[l, -1], Power[Plus[1, l], -1], Pi, Power[Plus[Times[2, M], Times[-1, r]], -1], Power[Plus[Times[6, M], Times[Plus[-2, l, Power[l, 2]], r]], -1], \[ScriptCapitalE], \[Mu], Power[Plus[Times[2, M], Times[-1, rp[t]]], -1], Power[rp[t], -4], Plus[Times[24, Power[M, 2], Power[r, 5]], Times[-20, M, Power[r, 5], rp[t]], Times[4, Power[r, 5], Power[rp[t], 2]], Times[Power[r, 2], Plus[Times[12, Power[M, 2]], Times[2, Plus[-5, l, Power[l, 2]], M, r], Times[-1, Plus[-2, l, Power[l, 2]], Power[r, 2]]], Power[rp[t], 3]]], \[Delta][Plus[r, Times[-1, rp[t]]]], Y[Rule["OverStr", "Bar"]][t]], Times[32, Power[l, -1], Power[Plus[1, l], -1], Pi, Power[Plus[Times[2, M], Times[-1, r]], -1], Power[r, 5], Power[Plus[Times[6, M], Times[Plus[-2, l, Power[l, 2]], r]], -1], \[ScriptCapitalE], \[Mu], Plus[Times[2, M], Times[-1, rp[t]]], Power[rp[t], -3], Y[Rule["OverStr", "Bar"]][t], Derivative[1][\[Delta]][Plus[r, Times[-1, rp[t]]]]]]], Times[-32, Power[l, -1], Power[Plus[1, l], -1], Pi, Power[Plus[Times[2, M], Times[-1, r]], -1], Power[r, 5], Power[Plus[Times[6, M], Times[Plus[-2, l, Power[l, 2]], r]], -1], \[ScriptCapitalE], \[Mu], Plus[Times[2, M], Times[-1, rp[t]]], Power[rp[t], -3], \[Delta][Plus[r, Times[-1, rp[t]]]], Derivative[1][Y[Rule["OverStr", "Bar"]]][t]], Times[Power[Plus[Times[2, M], Times[-1, r]], -1], Power[Plus[Times[6, M], Times[Plus[-2, l, Power[l, 2]], r]], -1], Plus[Times[6, Power[M, 2]], Times[3, Plus[-2, l, Power[l, 2]], M, r], Times[-1, Plus[-2, l, Power[l, 2]], Power[r, 2]]], Derivative[1, 0][XZM][t, r]], Times[r, Derivative[1, 1][XZM][t, r]]],

    TestID->"(HtrAmplitude[#1, Gauge -> RWZ, Reconstruct -> True, SourceExpansion -> Full] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (HrrAmplitude[#1, Gauge -> "RWZ", Reconstruct -> True, SourceExpansion -> "Full"] & )[syms],

    Plus[Times[Rational[1, 2], Power[Plus[Times[2, M], Times[-1, r]], -1], Power[r, -1], Power[Plus[Times[6, M], Times[Plus[-2, l, Power[l, 2]], r]], -2], Plus[Times[72, Power[M, 3]], Times[36, Plus[-2, l, Power[l, 2]], Power[M, 2], r], Times[6, Power[Plus[-2, l, Power[l, 2]], 2], M, Power[r, 2]], Times[l, Plus[1, l], Power[Plus[-2, l, Power[l, 2]], 2], Power[r, 3]]], XZM[t, r]], Times[-16, Power[l, -1], Power[Plus[1, l], -1], Pi, Power[r, 4], Power[Plus[Times[-2, M], r], -2], Power[Plus[Times[6, M], Times[Plus[-2, l, Power[l, 2]], r]], -2], Plus[Times[108, Power[M, 2]], Times[4, Plus[-19, Times[5, l], Times[5, Power[l, 2]]], M, r], Times[Plus[12, Times[-8, l], Times[-7, Power[l, 2]], Times[2, Power[l, 3]], Power[l, 4]], Power[r, 2]]], \[ScriptCapitalE], \[Mu], Plus[Times[2, M], Times[-1, rp[t]]], Power[rp[t], -3], \[Delta][Plus[r, Times[-1, rp[t]]]], Y[Rule["OverStr", "Bar"]][t]], Times[-32, Power[l, -1], Power[Plus[1, l], -1], Pi, Power[Plus[Times[2, M], Times[-1, r]], -1], Power[r, 5], Power[Plus[Times[6, M], Times[Plus[-2, l, Power[l, 2]], r]], -1], \[ScriptCapitalE], \[Mu], Plus[Times[2, M], Times[-1, rp[t]]], Power[rp[t], -3], Y[Rule["OverStr", "Bar"]][t], Derivative[1][\[Delta]][Plus[r, Times[-1, rp[t]]]]], Times[Power[Plus[Times[2, M], Times[-1, r]], -1], Power[Plus[Times[6, M], Times[Plus[-2, l, Power[l, 2]], r]], -1], Plus[Times[-6, Power[M, 2]], Times[Plus[-2, l, Power[l, 2]], M, r], Times[-1, Plus[-2, l, Power[l, 2]], Power[r, 2]]], Derivative[0, 1][XZM][t, r]], Times[r, Derivative[0, 2][XZM][t, r]]],

    TestID->"(HrrAmplitude[#1, Gauge -> RWZ, Reconstruct -> True, SourceExpansion -> Full] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (JtAmplitude[#1, Gauge -> "Lorenz", Weak -> True] & )[syms],

    Plus[Times[\[Theta][Plus[Times[-1, r], rp[t]]], j[Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["LorenzTag", "MinusTag"]]]]][t, r]], Times[\[Theta][Plus[r, Times[-1, rp[t]]]], j[Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["LorenzTag", "PlusTag"]]]]][t, r]]],

    TestID->"(JtAmplitude[#1, Gauge -> Lorenz, Weak -> True] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (JrAmplitude[#1, Gauge -> "Lorenz", Weak -> True] & )[syms],

    Plus[Times[\[Theta][Plus[Times[-1, r], rp[t]]], j[Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["LorenzTag", "MinusTag"]]]]][t, r]], Times[\[Theta][Plus[r, Times[-1, rp[t]]]], j[Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["LorenzTag", "PlusTag"]]]]][t, r]]],

    TestID->"(JrAmplitude[#1, Gauge -> Lorenz, Weak -> True] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (KAmplitude[#1, Gauge -> "RWZ", Reconstruct -> True, SourceExpansion -> "Full"] & )[syms],

    Plus[Times[Rational[1, 2], Power[r, -2], Power[Plus[Times[6, M], Times[Plus[-2, l, Power[l, 2]], r]], -1], Plus[Times[24, Power[M, 2]], Times[6, Plus[-2, l, Power[l, 2]], M, r], Times[l, Plus[-2, Times[-1, l], Times[2, Power[l, 2]], Power[l, 3]], Power[r, 2]]], XZM[t, r]], Times[32, Power[l, -1], Power[Plus[1, l], -1], Pi, Power[r, 3], Power[Plus[Times[6, M], Times[Plus[-2, l, Power[l, 2]], r]], -1], \[ScriptCapitalE], \[Mu], Plus[Times[2, M], Times[-1, rp[t]]], Power[rp[t], -3], \[Delta][Plus[r, Times[-1, rp[t]]]], Y[Rule["OverStr", "Bar"]][t]], Times[Plus[1, Times[-2, M, Power[r, -1]]], Derivative[0, 1][XZM][t, r]]],

    TestID->"(KAmplitude[#1, Gauge -> RWZ, Reconstruct -> True, SourceExpansion -> Full] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (GAmplitude[#1, Gauge -> "Lorenz", Weak -> True] & )[syms],

    Plus[Times[\[Theta][Plus[Times[-1, r], rp[t]]], \[ScriptCapitalG][Rule["Tags", List[Rule["Up", List["LorenzTag", "MinusTag"]]]]][t, r]], Times[\[Theta][Plus[r, Times[-1, rp[t]]]], \[ScriptCapitalG][Rule["Tags", List[Rule["Up", List["LorenzTag", "PlusTag"]]]]][t, r]]],

    TestID->"(GAmplitude[#1, Gauge -> Lorenz, Weak -> True] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (HtAmplitude[#1, Gauge -> "RWZ", Reconstruct -> True, SourceExpansion -> "Full"] & )[syms],

    Plus[Times[Plus[Rational[1, 2], Times[-1, M, Power[r, -1]]], XCPM[t, r]], Times[Rational[1, 2], Plus[Times[-2, M], r], Derivative[0, 1][XCPM][t, r]], Times[16, Power[Plus[-1, l], -1], Power[l, -1], Power[Plus[1, l], -1], Power[Plus[2, l], -1], Pi, Power[r, 2], \[ScriptCapitalL], \[Mu], Plus[Times[2, M], Times[-1, rp[t]]], Power[rp[t], -3], \[Delta][Plus[r, Times[-1, rp[t]]]], X[Rule["OverStr", "Bar"]][Rule["Indices", List[Rule["Down", List["phi"]]]]][t]]],

    TestID->"(HtAmplitude[#1, Gauge -> RWZ, Reconstruct -> True, SourceExpansion -> Full] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (HrAmplitude[#1, Gauge -> "RWZ", Reconstruct -> True, SourceExpansion -> "Full"] & )[syms],

    Plus[Times[-1, Power[Plus[Times[4, M], Times[-2, r]], -1], Power[r, 2], Derivative[1, 0][XCPM][t, r]], Times[-16, Power[Plus[-1, l], -1], Power[l, -1], Power[Plus[1, l], -1], Power[Plus[2, l], -1], Pi, Power[r, 2], \[ScriptCapitalL], \[Mu], Power[Plus[Times[2, M], Times[-1, rp[t]]], -1], Power[rp[t], -1], \[Delta][Plus[r, Times[-1, rp[t]]]], Derivative[1][rp][t], X[Rule["OverStr", "Bar"]][Rule["Indices", List[Rule["Down", List["phi"]]]]][t]]],

    TestID->"(HrAmplitude[#1, Gauge -> RWZ, Reconstruct -> True, SourceExpansion -> Full] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (H2Amplitude[#1, Gauge -> "Lorenz", Weak -> True] & )[syms],

    Plus[Times[\[Theta][Plus[Times[-1, r], rp[t]]], h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["LorenzTag", "MinusTag"]]]]][t, r]], Times[\[Theta][Plus[r, Times[-1, rp[t]]]], h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["LorenzTag", "PlusTag"]]]]][t, r]]],

    TestID->"(H2Amplitude[#1, Gauge -> Lorenz, Weak -> True] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (MasterFunction[#1, Variable -> "CPM1", Weak -> True] & )[syms],

    Plus[Times[\[Theta][Plus[Times[-1, r], rp[t]]], XCPM1[Rule["Tags", List[Rule["Up", List["MinusTag"]]]]][t, r]], Times[\[Theta][Plus[r, Times[-1, rp[t]]]], XCPM1[Rule["Tags", List[Rule["Up", List["PlusTag"]]]]][t, r]]],

    TestID->"(MasterFunction[#1, Variable -> CPM1, Weak -> True] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (MasterFunction[#1, Variable -> "ZM1", Weak -> True] & )[syms],

    Plus[Times[\[Theta][Plus[Times[-1, r], rp[t]]], XZM1[Rule["Tags", List[Rule["Up", List["MinusTag"]]]]][t, r]], Times[\[Theta][Plus[r, Times[-1, rp[t]]]], XZM1[Rule["Tags", List[Rule["Up", List["PlusTag"]]]]][t, r]]],

    TestID->"(MasterFunction[#1, Variable -> ZM1, Weak -> True] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (MasterFunction[#1, Variable -> "JT1", Weak -> True] & )[syms],

    Plus[Times[\[Theta][Plus[Times[-1, r], rp[t]]], XJT1[Rule["Tags", List[Rule["Up", List["MinusTag"]]]]][t, r]], Times[\[Theta][Plus[r, Times[-1, rp[t]]]], XJT1[Rule["Tags", List[Rule["Up", List["PlusTag"]]]]][t, r]]],

    TestID->"(MasterFunction[#1, Variable -> JT1, Weak -> True] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (MasterFunction[#1, Parity -> "Odd", MPs -> True] & )[syms],

    Times[2, Power[Plus[-1, l], -1], Power[Plus[2, l], -1], r, Plus[Derivative[0, 1][h[Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["RWZTag"]]]]]][t, r], Times[-1, Derivative[1, 0][h[Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["RWZTag"]]]]]][t, r]], Times[-2, Power[r, -1], h[Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["RWZTag"]]]]][t, r]]]],

    TestID->"(MasterFunction[#1, Parity -> Odd, MPs -> True] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (MasterFunction[#1, Parity -> "Odd", MPs -> True, Gauge -> "Undefined", Weak -> True] & )[syms],

    Times[2, Power[Plus[-1, l], -1], Power[Plus[2, l], -1], r, Plus[Times[\[Theta][Plus[Times[-1, r], rp[t]]], Derivative[0, 1][h[Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["GUTag", "MinusTag"]]]]]][t, r]], Times[\[Theta][Plus[r, Times[-1, rp[t]]]], Derivative[0, 1][h[Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["GUTag", "PlusTag"]]]]]][t, r]], Times[-1, Power[r, -1], Plus[Times[-1, h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["GUTag", "PlusTag"]]]]][t, r], Derivative[1][rp][t], Derivative[1][\[Theta]][Plus[r, Times[-1, rp[t]]]]], Times[h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["GUTag", "MinusTag"]]]]][t, r], Derivative[1][rp][t], Derivative[1][\[Theta]][Plus[Times[-1, r], rp[t]]]], Times[\[Theta][Plus[Times[-1, r], rp[t]]], Derivative[1, 0][h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["GUTag", "MinusTag"]]]]]][t, r]], Times[\[Theta][Plus[r, Times[-1, rp[t]]]], Derivative[1, 0][h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["GUTag", "PlusTag"]]]]]][t, r]]]], Times[-1, \[Theta][Plus[Times[-1, r], rp[t]]], Derivative[1, 0][h[Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["GUTag", "MinusTag"]]]]]][t, r]], Times[-1, \[Theta][Plus[r, Times[-1, rp[t]]]], Derivative[1, 0][h[Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["GUTag", "PlusTag"]]]]]][t, r]], Times[Rational[1, 2], Plus[Times[h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["GUTag", "PlusTag"]]]]][t, r], Derivative[1][rp][t], Derivative[2][\[Theta]][Plus[r, Times[-1, rp[t]]]]], Times[h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["GUTag", "MinusTag"]]]]][t, r], Derivative[1][rp][t], Derivative[2][\[Theta]][Plus[Times[-1, r], rp[t]]]], Times[-1, Derivative[1][rp][t], Derivative[1][\[Theta]][Plus[Times[-1, r], rp[t]]], Derivative[0, 1][h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["GUTag", "MinusTag"]]]]]][t, r]], Times[Derivative[1][rp][t], Derivative[1][\[Theta]][Plus[r, Times[-1, rp[t]]]], Derivative[0, 1][h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["GUTag", "PlusTag"]]]]]][t, r]], Times[Derivative[1][\[Theta]][Plus[Times[-1, r], rp[t]]], Derivative[1, 0][h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["GUTag", "MinusTag"]]]]]][t, r]], Times[-1, Derivative[1][\[Theta]][Plus[r, Times[-1, rp[t]]]], Derivative[1, 0][h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["GUTag", "PlusTag"]]]]]][t, r]], Times[-1, \[Theta][Plus[Times[-1, r], rp[t]]], Derivative[1, 1][h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["GUTag", "MinusTag"]]]]]][t, r]], Times[-1, \[Theta][Plus[r, Times[-1, rp[t]]]], Derivative[1, 1][h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["GUTag", "PlusTag"]]]]]][t, r]]]], Times[Rational[1, 2], Plus[Times[-1, h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["GUTag", "PlusTag"]]]]][t, r], Derivative[1][rp][t], Derivative[2][\[Theta]][Plus[r, Times[-1, rp[t]]]]], Times[-1, h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["GUTag", "MinusTag"]]]]][t, r], Derivative[1][rp][t], Derivative[2][\[Theta]][Plus[Times[-1, r], rp[t]]]], Times[Derivative[1][rp][t], Derivative[1][\[Theta]][Plus[Times[-1, r], rp[t]]], Derivative[0, 1][h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["GUTag", "MinusTag"]]]]]][t, r]], Times[-1, Derivative[1][rp][t], Derivative[1][\[Theta]][Plus[r, Times[-1, rp[t]]]], Derivative[0, 1][h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["GUTag", "PlusTag"]]]]]][t, r]], Times[-1, Derivative[1][\[Theta]][Plus[Times[-1, r], rp[t]]], Derivative[1, 0][h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["GUTag", "MinusTag"]]]]]][t, r]], Times[Derivative[1][\[Theta]][Plus[r, Times[-1, rp[t]]]], Derivative[1, 0][h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["GUTag", "PlusTag"]]]]]][t, r]], Times[\[Theta][Plus[Times[-1, r], rp[t]]], Derivative[1, 1][h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["GUTag", "MinusTag"]]]]]][t, r]], Times[\[Theta][Plus[r, Times[-1, rp[t]]]], Derivative[1, 1][h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["GUTag", "PlusTag"]]]]]][t, r]]]], Times[-1, Derivative[1][rp][t], Derivative[1][\[Theta]][Plus[Times[-1, r], rp[t]]], h[Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["GUTag", "MinusTag"]]]]][t, r]], Times[Derivative[1][rp][t], Derivative[1][\[Theta]][Plus[r, Times[-1, rp[t]]]], h[Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["GUTag", "PlusTag"]]]]][t, r]], Times[-1, Derivative[1][\[Theta]][Plus[Times[-1, r], rp[t]]], h[Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["GUTag", "MinusTag"]]]]][t, r]], Times[Derivative[1][\[Theta]][Plus[r, Times[-1, rp[t]]]], h[Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["GUTag", "PlusTag"]]]]][t, r]], Times[-2, Power[r, -1], Plus[Times[Rational[1, 2], Plus[Times[h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["GUTag", "PlusTag"]]]]][t, r], Derivative[1][rp][t], Derivative[1][\[Theta]][Plus[r, Times[-1, rp[t]]]]], Times[-1, h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["GUTag", "MinusTag"]]]]][t, r], Derivative[1][rp][t], Derivative[1][\[Theta]][Plus[Times[-1, r], rp[t]]]], Times[-1, \[Theta][Plus[Times[-1, r], rp[t]]], Derivative[1, 0][h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["GUTag", "MinusTag"]]]]]][t, r]], Times[-1, \[Theta][Plus[r, Times[-1, rp[t]]]], Derivative[1, 0][h[Rule["Tags", List[Rule["Down", List["2Tag"]], Rule["Up", List["GUTag", "PlusTag"]]]]]][t, r]]]], Times[\[Theta][Plus[Times[-1, r], rp[t]]], h[Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["GUTag", "MinusTag"]]]]][t, r]], Times[\[Theta][Plus[r, Times[-1, rp[t]]]], h[Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["GUTag", "PlusTag"]]]]][t, r]]]]]],

    TestID->"(MasterFunction[#1, Parity -> Odd, MPs -> True, Gauge -> Undefined, Weak -> True] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (XiEvenAmplitude[#1, Weak -> True] & )[syms],

    Plus[Times[\[Theta][Plus[Times[-1, r], rp[t]]], \[Xi][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag", "MinusTag"]]]]][t, r]], Times[\[Theta][Plus[r, Times[-1, rp[t]]]], \[Xi][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag", "PlusTag"]]]]][t, r]]],

    TestID->"(XiEvenAmplitude[#1, Weak -> True] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (XiEvenTAmplitude[#1, Weak -> True] & )[syms],

    Plus[Times[\[Theta][Plus[Times[-1, r], rp[t]]], \[Xi][Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag", "MinusTag"]]]]][t, r]], Times[\[Theta][Plus[r, Times[-1, rp[t]]]], \[Xi][Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag", "PlusTag"]]]]][t, r]]],

    TestID->"(XiEvenTAmplitude[#1, Weak -> True] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (XiEvenRAmplitude[#1, Weak -> True] & )[syms],

    Plus[Times[\[Theta][Plus[Times[-1, r], rp[t]]], \[Xi][Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag", "MinusTag"]]]]][t, r]], Times[\[Theta][Plus[r, Times[-1, rp[t]]]], \[Xi][Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag", "PlusTag"]]]]][t, r]]],

    TestID->"(XiEvenRAmplitude[#1, Weak -> True] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (XiOddAmplitude[#1, Weak -> True] & )[syms],

    Plus[Times[\[Theta][Plus[Times[-1, r], rp[t]]], \[Xi][Rule["Tags", List[Rule["Up", List["OddTag", "RWZTag", "LorenzTag", "MinusTag"]]]]][t, r]], Times[\[Theta][Plus[r, Times[-1, rp[t]]]], \[Xi][Rule["Tags", List[Rule["Up", List["OddTag", "RWZTag", "LorenzTag", "PlusTag"]]]]][t, r]]],

    TestID->"(XiOddAmplitude[#1, Weak -> True] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (HttPush[#1, Weak -> True] & )[syms],

    Plus[Times[2, M, Plus[1, Times[-2, M, Power[r, -1]]], Power[r, -2], Plus[Times[\[Theta][Plus[Times[-1, r], rp[t]]], \[Xi][Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag", "MinusTag"]]]]][t, r]], Times[\[Theta][Plus[r, Times[-1, rp[t]]]], \[Xi][Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag", "PlusTag"]]]]][t, r]]]], Times[-2, Plus[Times[\[Theta][Plus[Times[-1, r], rp[t]]], Derivative[1, 0][\[Xi][Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag", "MinusTag"]]]]]][t, r]], Times[\[Theta][Plus[r, Times[-1, rp[t]]]], Derivative[1, 0][\[Xi][Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag", "PlusTag"]]]]]][t, r]], Times[Derivative[1][rp][t], Derivative[1][\[Theta]][Plus[Times[-1, r], rp[t]]], \[Xi][Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag", "MinusTag"]]]]][t, r]], Times[-1, Derivative[1][rp][t], Derivative[1][\[Theta]][Plus[r, Times[-1, rp[t]]]], \[Xi][Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag", "PlusTag"]]]]][t, r]]]]],

    TestID->"(HttPush[#1, Weak -> True] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (HttPush[#1] & )[syms],

    Plus[Times[-2, Derivative[1, 0][\[Xi][Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag"]]]]]][t, r]], Times[2, M, Plus[1, Times[-2, M, Power[r, -1]]], Power[r, -2], \[Xi][Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag"]]]]][t, r]]],

    TestID->"(HttPush[#1] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (HtrPush[#1, Weak -> True] & )[syms],

    Plus[Times[-1, \[Theta][Plus[Times[-1, r], rp[t]]], Derivative[0, 1][\[Xi][Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag", "MinusTag"]]]]]][t, r]], Times[-1, \[Theta][Plus[r, Times[-1, rp[t]]]], Derivative[0, 1][\[Xi][Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag", "PlusTag"]]]]]][t, r]], Times[-1, \[Theta][Plus[Times[-1, r], rp[t]]], Derivative[1, 0][\[Xi][Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag", "MinusTag"]]]]]][t, r]], Times[-1, \[Theta][Plus[r, Times[-1, rp[t]]]], Derivative[1, 0][\[Xi][Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag", "PlusTag"]]]]]][t, r]], Times[-1, Derivative[1][rp][t], Derivative[1][\[Theta]][Plus[Times[-1, r], rp[t]]], \[Xi][Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag", "MinusTag"]]]]][t, r]], Times[Derivative[1][rp][t], Derivative[1][\[Theta]][Plus[r, Times[-1, rp[t]]]], \[Xi][Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag", "PlusTag"]]]]][t, r]], Times[Derivative[1][\[Theta]][Plus[Times[-1, r], rp[t]]], \[Xi][Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag", "MinusTag"]]]]][t, r]], Times[-1, Derivative[1][\[Theta]][Plus[r, Times[-1, rp[t]]]], \[Xi][Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag", "PlusTag"]]]]][t, r]], Times[2, M, Power[Plus[1, Times[-2, M, Power[r, -1]]], -1], Power[r, -2], Plus[Times[\[Theta][Plus[Times[-1, r], rp[t]]], \[Xi][Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag", "MinusTag"]]]]][t, r]], Times[\[Theta][Plus[r, Times[-1, rp[t]]]], \[Xi][Rule["Indices", List[Rule["Down", List["t"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag", "PlusTag"]]]]][t, r]]]]],

    TestID->"(HtrPush[#1, Weak -> True] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (HrrPush[#1] & )[syms],

    Plus[Times[-2, Derivative[0, 1][\[Xi][Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag"]]]]]][t, r]], Times[-2, M, Power[Plus[1, Times[-2, M, Power[r, -1]]], -1], Power[r, -2], \[Xi][Rule["Indices", List[Rule["Down", List["r"]]]]][Rule["Tags", List[Rule["Up", List["EvenTag", "RWZTag", "LorenzTag"]]]]][t, r]]],

    TestID->"(HrrPush[#1] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (HtPush[#1] & )[syms],

    Times[-1, Derivative[1, 0][\[Xi][Rule["Tags", List[Rule["Up", List["OddTag", "RWZTag", "LorenzTag"]]]]]][t, r]],

    TestID->"(HtPush[#1] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]


VerificationTest[
    (HrPush[#1, Weak -> True] & )[syms],

    Plus[Times[2, Power[r, -1], Plus[Times[\[Theta][Plus[Times[-1, r], rp[t]]], \[Xi][Rule["Tags", List[Rule["Up", List["OddTag", "RWZTag", "LorenzTag", "MinusTag"]]]]][t, r]], Times[\[Theta][Plus[r, Times[-1, rp[t]]]], \[Xi][Rule["Tags", List[Rule["Up", List["OddTag", "RWZTag", "LorenzTag", "PlusTag"]]]]][t, r]]]], Times[-1, \[Xi][Rule["Tags", List[Rule["Up", List["OddTag", "RWZTag", "LorenzTag", "PlusTag"]]]]][t, r], Derivative[1][\[Theta]][Plus[r, Times[-1, rp[t]]]]], Times[\[Xi][Rule["Tags", List[Rule["Up", List["OddTag", "RWZTag", "LorenzTag", "MinusTag"]]]]][t, r], Derivative[1][\[Theta]][Plus[Times[-1, r], rp[t]]]], Times[-1, \[Theta][Plus[Times[-1, r], rp[t]]], Derivative[0, 1][\[Xi][Rule["Tags", List[Rule["Up", List["OddTag", "RWZTag", "LorenzTag", "MinusTag"]]]]]][t, r]], Times[-1, \[Theta][Plus[r, Times[-1, rp[t]]]], Derivative[0, 1][\[Xi][Rule["Tags", List[Rule["Up", List["OddTag", "RWZTag", "LorenzTag", "PlusTag"]]]]]][t, r]]],

    TestID->"(HrPush[#1, Weak -> True] & )[syms]",

    SameTest->(If[MatchQ[Head[#1],List],Simplify[#1-#2]==Table[0,{Length@#1}],Simplify[#1-#2]== 0]&)
]

