{
 "Application" -> "BlackHoleAnalysis",
 "Package" -> "BlackHoleAnalysis",
 "Title" -> "BlackHoleAnalysis: An application for mode-by-mode black hole calculations",
 "Summary" -> "BlackHoleAnalysis: An application for mode-by-mode black hole calculations",
 "Description" ->
   {"The BlackHoleAnalysis application provides functions for ",
    "black hole calculations, particularly at the mode-by-mode level."},
 "Keywords" -> {"BlackHoleAnalysis"},
 "Label" -> "Black holes application",
 "Synonyms" -> {},
 "URL" -> "http://bitbucket.org/hoppese/BlackHoleAnalysis",
 "Packages" -> {
   {"Title" -> "Field equations",
    "DetailedFunctions" -> {
      {"FieldEquations", "The field equations for the lm modes on Schwarzschild"},
      {"MasterEquation", "The RWZ - style master equation"},
      {"TeukolskyEquation", "The homogeneous Teukolsky equation"},
      {"SasakiNakamuraEquation", "The homogeneous Sasaki-Nakamura equation"}
    }
   },
   {"Title" -> "Fields",
     "DetailedFunctions" -> {
       {"MasterFunction", "RWZ - style master function"},
       {"TeukolskyFunction", "4D Teukolsky function"},
       {"RadialTeukolskyFunction", "1D radial Teukolsky function"},
       {"SasakiNakamuraFunction", "Spin -2 Sasaki-Nakamura function"},
       {"HttAmplitude", "Martel-Poisson even-parity metric perturbation amplitude htt"},
       {"HtrAmplitude", "Martel-Poisson even-parity metric perturbation amplitude htr"},
       {"HrrAmplitude", "Martel-Poisson even-parity metric perturbation amplitude hrr"},
       {"JtAmplitude", "Martel-Poisson even-parity metric perturbation amplitude jt"},
       {"JrAmplitude", "Martel-Poisson even-parity metric perturbation amplitude jr"},
       {"KAmplitude", "Martel-Poisson even-parity metric perturbation amplitude K"},
       {"GAmplitude", "Martel-Poisson even-parity metric perturbation amplitude G"},
       {"HtAmplitude", "Martel-Poisson odd-parity metric perturbation amplitude ht"},
       {"HrAmplitude", "Martel-Poisson odd-parity metric perturbation amplitude hr"},
       {"H2Amplitude", "Martel-Poisson odd-parity metric perturbation amplitude h2"},
       {"AmplitudeFunction", "Provides the function that gives a field for an associated label"}
     }
    },
    {"Title" -> "Source terms",
     "DetailedFunctions" -> {
       {"MasterEquationSource", "Source terms to the RWZ - style master equation"},
       {"QttSource", "Martel-Poisson even-parity source Qtt"},
       {"QtrSource", "Martel-Poisson even-parity source Qtr"},
       {"QrrSource", "Martel-Poisson even-parity source Qrr"},
       {"QtSource", "Martel-Poisson even-parity source Qt"},
       {"QrSource", "Martel-Poisson even-parity source Qr"},
       {"QSharpSource", "Martel-Poisson even-parity source QSharp"},
       {"QFlatSource", "Martel-Poisson even-parity source QFlat"},
       {"PtSource", "Martel-Poisson odd-parity source Pt"},
       {"PrSource", "Martel-Poisson odd-parity source Pr"},
       {"PSource", "Martel-Poisson odd-parity source P"},
       {"MetricPerturbationSourceFunction", "Provides the function that gives a source for an associated label"}
     }
    },
     {"Title"->"Spherical harmonics",
        "DetailedFunctions" -> {
            {"YHarmonic", "Scalar spherical harmonic, or spin-weighted spherical harmonic"},
            {"YTheta", "Component of Martel-Poisson even-parity vector spherical harmonic Y_theta"},
            {"YPhi", "Component of Martel-Poisson even-parity vector spherical harmonic Y_phi"},
            {"YThetaTheta", "Component of Martel-Poisson even-parity tensor spherical harmonic Y_theta_theta"},
            {"YThetaPhi", "Component of Martel-Poisson even-parity tensor spherical harmonic Y_tehta_phi"},
            {"YPhiPhi", "Component of Martel-Poisson even-parity tensor spherical harmonic Y_phi_phi"},
            {"XTheta", "Component of Martel-Poisson odd-parity vector spherical harmonic X_theta"},
            {"XPhi", "Component of Martel-Poisson odd-parity vector spherical harmonic X_phi"},
            {"XThetaTheta", "Component of Martel-Poisson odd-parity tensor spherical harmonic X_theta_theta"},
            {"XThetaPhi", "Component of Martel-Poisson odd-parity tensor spherical harmonic X_tehta_phi"},
            {"XPhiPhi", "Component of Martel-Poisson odd-parity tensor spherical harmonic X_phi_phi"},
            {"SpinWeightedSpheroidalHarmonicFunction", "Symbolic representation of the spin-weighted spheroidal harmonic"},
            {"SphericalHarmonicFunction", "Provides the function that gives a harmonic for an associated label"}
        }
    },
     {"Title"->"Orbital parameters",
        "DetailedFunctions" -> {
            {"FourVelocity", "Four-velocity for a particle in eccentric equatorial motion on Schwarzschild or Kerr"},
            {"SpecificEnergy", "Specific energy in terms of semi-latus rectum p & eccentricity e"},
            {"SpecificAngularMomentum", "Specific angular momentum in terms of semi-latus rectum p & eccentricity e"},
            {"Periapsis", "r_min in terms of semi-latus rectum p & eccentricity e"},
            {"Apoapsis", "r_max in terms of semi-latus rectum p & eccentricity e"},
            {"SemiLatusRectum", "Semi-latus rectum in terms of r_min and r_max"},
            {"OrbitalEccentricity", "Orbital eccentricity in terms of r_min and r_max"},
            {"USquared", "Effective potential on Schwarzschild"},
            {"Separatrix","Serapation between bound and unbound orbits on Schwarzschild or Kerr"}
        }
    },
    {"Title"->"Darwin's orbit parametrization",
       "DetailedFunctions" -> {
           {"ROfChi", "Radial position in terms of relativistic anomaly \[Chi] for eccentric equatorial motion on Schwarzschild or Kerr"},
           {"PhiOfChi", "Azimuthal position in terms of \[Chi] on Schwarzschild"},
           {"DTauDChi", "Differential equation for proper time in terms of \[Chi] on Schwarzschild or Kerr"},
           {"DTDChi", "Differential equation for coordinate time in terms of \[Chi] on Schwarzschild or Kerr"},     
           {"DPhiDChi", "Differential equation for Azimuthal position in terms of \[Chi] on Schwarzschild or Kerr"},  
           {"DRDT", "dr/dt in terms of \[Chi] on Schwarzschild or Kerr"}
       }
    },
    {"Title"->"Discontinuities",
       "DetailedFunctions" -> {
           {"EvaluateDiscontinuities", "Evaluate coefficients of delta functions, their derivatives, and the derivatives of step functions."},
           {"MasterFunctionDiscontinuities", "Discontinuities in the master function"},
           {"HttDiscontinuities", "Discontinuities in the the MP amplitude htt in RWZ or Lorenz gauge"},
           {"HtrDiscontinuities", "Discontinuities in the the MP amplitude htr in RWZ or Lorenz gauge"},
           {"HrrDiscontinuities", "Discontinuities in the the MP amplitude hrr in RWZ or Lorenz gauge"},
           {"JtDiscontinuities", "Discontinuities in the the MP amplitude jt in RWZ or Lorenz gauge"},
           {"JrDiscontinuities", "Discontinuities in the the MP amplitude jr in RWZ or Lorenz gauge"},
           {"KDiscontinuities", "Discontinuities in the the MP amplitude K in RWZ or Lorenz gauge"},
           {"GDiscontinuities", "Discontinuities in the the MP amplitude G in RWZ or Lorenz gauge"},
           {"HtDiscontinuities", "Discontinuities in the the MP amplitude ht in RWZ or Lorenz gauge"},
           {"HrDiscontinuities", "Discontinuities in the the MP amplitude hr in RWZ or Lorenz gauge"},
           {"H2Discontinuities", "Discontinuities in the the MP amplitude h2 in RWZ or Lorenz gauge"},
           {"DiscontinuitiesFunction", "Provides the function that gives discontinuities for an associated label"}
       }
    },
    {"Title"->"Labels",
       "DetailedFunctions" -> {
           {"MetricPerturbationLables", "Labels for the MP amplitudes"},
           {"MetricPerturbationSourceLables", "Labels for the source terms of the decomposed field equations"},
           {"SphericalHarmonicLabels", "Labels for the spherical harmonics"},
           {"GaugeVectorLabels", "Labels for the amplitudes of the gauge vector"}
       }
    },
    {"Title"->"Orbital functions and simplification",
       "DetailedFunctions" -> {
           {"RpDotSquared", "The coordinate time derivative of r squared on Schwarzschild or Kerr"},
           {"RpDotDot", "The second coordinate time derivative of r on Schwarzschild or Kerr"},
           {"RemoveRpDots", "Eliminate time derivative of r on Schwarzschild or Kerr"},
           {"RemovePhiPDots", "Eliminate time derivative of \[Phi] on Schwarzschild or Kerr"},
           {"RemoveSHDots", "Eliminate time derivatives spherical harmonics"},
           {"ReduceLegendreP", "Lower the order of Legendre polynomials"}
       }
    },
    {"Title"->"Relationships between functions",
       "DetailedFunctions" -> {
           {"EvenMasterFunctionAsOdd", "Even-parity Zerilli-Moncrief function in terms of the odd-parity Cunningham-Price-Moncrief function."},
           {"MasterFunctionAsTeukolskyFunction", "Frequency domain RWZ master function in terms of the radial Teukolsky function"},
           {"TeukolskyFunctionAsSasakiNakamuraFunction", "Radial Teukolsky function in terms of the Sasaki-Nakamura function"},
           {"SasakiNakamuraFunctionAsTeukolskyFunction", "Sasaki-Nakamura function in terms of the radial Teukolsky function"}
       }
    },
    {"Title"->"Removing derivatives with differential equations",
       "DetailedFunctions" -> {
           {"RemoveMasterFunctionRDerivatives", "Reduce order of r-derivatives of master functions (in time or freqeuncy domain)"},
           {"RemoveTeukolskyFunctionRDerivatives", "Reduce order of r-derivatives of the radial Teukolsky function"},
           {"RemoveSlmThetaDerivatives", "Reduce order of \[Theta]-derivatives of spin-weighted spheroidal harmonics"},
           {"RemoveSasakiNakamuraFunctionRDerivatives", "Reduce order of r-derivatives of the Sasaki-Nakamura function"}
       }
    },
    {"Title"->"Post-Newtonian expressions",
       "DetailedFunctions" -> {
           {"XPN", "PN parameter x in terms of semi-latus rectum p"},
           {"SemiLatusRectumPN", "Semi-latus rectum p in terms of x"},
           {"OmegaPhiPN", "Azimuthal angular frequency in a PN expansion"},
           {"OmegaRPN", "Radial angular frequency in a PN expansion"},
           {"SpecificEnergyPN", "Specific energy in a PN expansion"},
           {"SpecificAngularMomentumPN", "Specific angular momentum in a PN expansion"},
           {"RadialPeriodPN", "Radial period in a PN expansion"},
           {"AzimuthalAdvancePN", "Advance in \[Phi] during one radial period in a PN expansion"}
       }
    },
    {"Title"->"Odds and ends",
       "DetailedFunctions" -> {
           {"f", "1 - 2M/r"},
           {"KerrMetric", "The Kerr metric in Boyer-Lindquist coordinates"},
           {"SchwarzschildMetric", "The Schwarzschild metric in Schwarzschild coordinates"},
           {"RStarOfR", "Tortoise coordinate in terms of Boyer-Lindquist r"},
           {"RStarToR", "Transform Tortoise coordinate to Schwarzschild r"},
           {"ROfRStar", "Schwarzschild r in terms of Tortoise coordinate"},
           {"RToRStar", "Transform Schwarzschild r to Tortoise coordinate"},
           {"ToFrequencyDomain", "Replace time derivatives with -i \[Omega]"},
           {"SigmaKerr", "The quantity \[CapitalSigma] used on Kerr"},
           {"DeltaKerr", "The quantity \[CapitalDelta] used on Kerr"},
           {"RPlusKerr", "The quantity r+ used on Kerr"},
           {"RMinusKerr", "The quantity r- used on Kerr"},
           {"LambdaOfL", "Lambda (l+2)(l-1)/2 in terms of l"},
           {"LambdaToL", "Replace lambdas with (l+2)(l-1)/2"}
       }
    }
  },
 "Tutorials" -> {
     "Discontinuities",
     "Equations of motion",
     "Examples",
     "Field equations on Schwarzschild",
     "Kerr equations",
     "Post-Newtonian expressions",
     "RWZ master equations",
     "Simplifying expressions",
     "Source terms",
     "Spherical harmonics",
     "Time domain amplitudes",
     "Using labels to act on groups of variables",
     "Weak functions"
  }
}
