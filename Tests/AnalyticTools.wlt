(* Mathematica Test File *)

VerificationTest[
	Simplify@RemoveRpDots[FourVelocity[].SchwarzschildMetric[Indices -> "Down", AtParticle -> True].FourVelocity[]],
	-1,
	TestID->"FourVelocitySquared"	
]

VerificationTest[
	Module[ {t,thp,fourV,met},
		
		t = TSymbol[];
		thp = ThetaPSymbol[];
		fourV= FourVelocity[Metric -> "Kerr"];
		met = KerrMetric[Indices -> "Down", AtParticle -> True] /. thp[t] -> Pi/2;
		Simplify@RemoveRpDots[fourV.met.fourV, Metric -> "Kerr"]
	],
	-1,
	TestID->"FourVelocitySquaredKerrEq"	
]

VerificationTest[
    Module[ {t,r,rp,th},
     	r = RSymbol[];
     	t = TSymbol[];
     	rp = RpSymbol[];
     	th = HeavisideSymbol[];
     	
        EvaluateHeavisides[D[r th[r - rp[t]], r]]
    ]
    ,
    Module[ {t,r,rp,th,DD},
     	r = RSymbol[];
     	t = TSymbol[];
     	rp = RpSymbol[];
     	th = HeavisideSymbol[];
     	DD = DiracDeltaSymbol[];
     	
        r DD[r-rp[t]] + th[r-rp[t]]
    ]		
    ,
    TestID->"Heaviside1",
  
  	SameTest -> ((Simplify[#1-#2] == 0) &)
]

VerificationTest[
    Module[ {t,r,rp,th},
     	r = RSymbol[];
     	t = TSymbol[];
     	rp = RpSymbol[];
     	th = HeavisideSymbol[];
     	
        EvaluateHeavisides[D[r th[rp[t]-r], r]]
    ]
    ,
    Module[ {t,r,rp,th,DD},
     	r = RSymbol[];
     	t = TSymbol[];
     	rp = RpSymbol[];
     	th = HeavisideSymbol[];
     	DD = DiracDeltaSymbol[];
     	
        -r DD[r-rp[t]] + th[rp[t]-r]
    ]		
    ,
    TestID->"Heaviside2",
  
  	SameTest -> ((Simplify[#1-#2] == 0) &)
]

Module[ {t,r,rp,DD,func},
     	r = RSymbol[];
     	t = TSymbol[];
     	rp = RpSymbol[];
     	DD = DiracDeltaSymbol[];
     	
	VerificationTest[
    	    EvaluateDeltas[func[r] DD'[r-rp[t]]],
     	
	        func[rp[t]] DD'[r-rp[t]]-func'[rp[t]] DD[r-rp[t]],
	        
    		TestID->"Delta1",
  
		  	SameTest -> ((Simplify[#1-#2] == 0) &)
		]
]


Module[ {t,r,rp,DD,func},
     	r = RSymbol[];
     	t = TSymbol[];
     	rp = RpSymbol[];
     	DD = DiracDeltaSymbol[];
     	
	VerificationTest[
    	    EvaluateDeltas[func[r] DD''[r-rp[t]]],
     	
	        func[rp[t]] DD''[r-rp[t]] - 2func'[rp[t]] DD'[r-rp[t]] + func''[rp[t]] DD[r-rp[t]],
	        
    		TestID->"Delta2",
  
		  	SameTest -> ((Simplify[#1-#2] == 0) &)
		]
]

Module[ {t,r,rp,DD,func},
     	r = RSymbol[];
     	t = TSymbol[];
     	rp = RpSymbol[];
     	DD = DiracDeltaSymbol[];
     	
	VerificationTest[
    	    EvaluateDeltas[func[r] DD'''[r-rp[t]]],
     	
	        func[rp[t]] DD'''[r-rp[t]] - 3func'[rp[t]] DD''[r-rp[t]] + 3func''[rp[t]] DD'[r-rp[t]] - func'''[rp[t]] DD[r-rp[t]],
	        
    		TestID->"Delta3",
  
		  	SameTest -> ((Simplify[#1-#2] == 0) &)
		]
]
