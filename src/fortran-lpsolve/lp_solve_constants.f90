module lp_solve_constants
    use iso_c_binding
    implicit none
        integer(c_int), parameter :: FALSE = 0
        integer(c_int), parameter :: TRUE = 1
        integer(c_int), parameter :: AUTOMATIC = 2
        integer(c_int), parameter :: DYNAMIC = 4
        integer(c_int), parameter :: FULLYBOUNDEDSIMPLEX = FALSE
        integer(c_int), parameter :: libBLAS = 2
        character(*), parameter :: libnameBLAS = "myBLAS"
        integer(c_int), parameter :: LoadInverseLib = TRUE
        integer(c_int), parameter :: DEF_OBJINBASIS = TRUE
        integer(c_int), parameter :: INVERSE_NONE = -1
        integer(c_int), parameter :: INVERSE_LEGACY = 0
        integer(c_int), parameter :: INVERSE_ETAPFI = 1
        integer(c_int), parameter :: INVERSE_LUMOD = 2
        integer(c_int), parameter :: INVERSE_LUSOL = 3
        integer(c_int), parameter :: INVERSE_GLPKLU = 4
        integer(c_int), parameter :: INVERSE_ACTIVE = INVERSE_LEGACY
        integer(c_int), parameter :: LoadLanguageLib = TRUE
        integer(c_int), parameter :: LANGUAGE_NONE = -1
        integer(c_int), parameter :: LANGUAGE_LEGACYLP = 0
        integer(c_int), parameter :: LANGUAGE_CPLEXLP = 1
        integer(c_int), parameter :: LANGUAGE_MPSX = 2
        integer(c_int), parameter :: LANGUAGE_LPFML = 3
        integer(c_int), parameter :: LANGUAGE_MATHPROG = 4
        integer(c_int), parameter :: LANGUAGE_AMPL = 5
        integer(c_int), parameter :: LANGUAGE_GAMS = 6
        integer(c_int), parameter :: LANGUAGE_ZIMPL = 7
        integer(c_int), parameter :: LANGUAGE_S = 8
        integer(c_int), parameter :: LANGUAGE_R = 9
        integer(c_int), parameter :: LANGUAGE_MATLAB = 10
        integer(c_int), parameter :: LANGUAGE_OMATRIX = 11
        integer(c_int), parameter :: LANGUAGE_SCILAB = 12
        integer(c_int), parameter :: LANGUAGE_OCTAVE = 13
        integer(c_int), parameter :: LANGUAGE_EMPS = 14
        integer(c_int), parameter :: LANGUAGE_ACTIVE = LANGUAGE_CPLEXLP
        integer(c_int), parameter :: OriginalPARAM = 0
        integer(c_int), parameter :: ProductionPARAM = 1
        integer(c_int), parameter :: ChvatalPARAM = 2
        integer(c_int), parameter :: LoosePARAM = 3
        integer(c_int), parameter :: ActivePARAM = LoosePARAM
        integer(c_int), parameter :: MAJORVERSION = 5
        integer(c_int), parameter :: MINORVERSION = 5
        integer(c_int), parameter :: RELEASE = 2
        integer(c_int), parameter :: BUILD = 0
        integer(c_int), parameter :: BFPVERSION = 12
        integer(c_int), parameter :: XLIVERSION = 12
        integer(c_int), parameter :: BFP_CALLMODEL = 0
        integer(c_int), parameter :: XLI_CALLMODEL = BFP_CALLMODEL
        integer(c_int), parameter :: SIMPLEX_UNDEFINED = 0
        integer(c_int), parameter :: SIMPLEX_Phase1_PRIMAL = 1
        integer(c_int), parameter :: SIMPLEX_Phase1_DUAL = 2
        integer(c_int), parameter :: SIMPLEX_Phase2_PRIMAL = 4
        integer(c_int), parameter :: SIMPLEX_Phase2_DUAL = 8
        integer(c_int), parameter :: SIMPLEX_DYNAMIC = 16
        integer(c_int), parameter :: SIMPLEX_AUTODUALIZE = 32
        integer(c_int), parameter :: SIMPLEX_PRIMAL_PRIMAL = (SIMPLEX_Phase1_PRIMAL+SIMPLEX_Phase2_PRIMAL)
        integer(c_int), parameter :: SIMPLEX_DUAL_PRIMAL = (SIMPLEX_Phase1_DUAL+SIMPLEX_Phase2_PRIMAL)
        integer(c_int), parameter :: SIMPLEX_PRIMAL_DUAL = (SIMPLEX_Phase1_PRIMAL+SIMPLEX_Phase2_DUAL)
        integer(c_int), parameter :: SIMPLEX_DUAL_DUAL = (SIMPLEX_Phase1_DUAL+SIMPLEX_Phase2_DUAL)
        integer(c_int), parameter :: SIMPLEX_DEFAULT = (SIMPLEX_DUAL_PRIMAL)
        integer(c_int), parameter :: ISREAL = 0
        integer(c_int), parameter :: ISINTEGER = 1
        integer(c_int), parameter :: ISSEMI = 2
        integer(c_int), parameter :: ISSOS = 4
        integer(c_int), parameter :: ISSOSTEMPINT = 8
        integer(c_int), parameter :: ISGUB = 16
        integer(c_int), parameter :: PRESOLVE_NONE = 0
        integer(c_int), parameter :: PRESOLVE_ROWS = 1
        integer(c_int), parameter :: PRESOLVE_COLS = 2
        integer(c_int), parameter :: PRESOLVE_LINDEP = 4
        integer(c_int), parameter :: PRESOLVE_AGGREGATE = 8
        integer(c_int), parameter :: PRESOLVE_SPARSER = 16
        integer(c_int), parameter :: PRESOLVE_SOS = 32
        integer(c_int), parameter :: PRESOLVE_REDUCEMIP = 64
        integer(c_int), parameter :: PRESOLVE_KNAPSACK = 128
        integer(c_int), parameter :: PRESOLVE_ELIMEQ2 = 256
        integer(c_int), parameter :: PRESOLVE_IMPLIEDFREE = 512
        integer(c_int), parameter :: PRESOLVE_REDUCEGCD = 1024
        integer(c_int), parameter :: PRESOLVE_PROBEFIX = 2048
        integer(c_int), parameter :: PRESOLVE_PROBEREDUCE = 4096
        integer(c_int), parameter :: PRESOLVE_ROWDOMINATE = 8192
        integer(c_int), parameter :: PRESOLVE_COLDOMINATE = 16384
        integer(c_int), parameter :: PRESOLVE_MERGEROWS = 32768
        integer(c_int), parameter :: PRESOLVE_IMPLIEDSLK = 65536
        integer(c_int), parameter :: PRESOLVE_COLFIXDUAL = 131072
        integer(c_int), parameter :: PRESOLVE_BOUNDS = 262144
        integer(c_int), parameter :: PRESOLVE_DUALS = 524288
        integer(c_int), parameter :: PRESOLVE_LASTMASKMODE = (PRESOLVE_DUALS-1)
        integer(c_int), parameter :: PRESOLVE_SENSDUALS = 1048576
        integer(c_int), parameter :: CRASH_NONE = 0
        integer(c_int), parameter :: CRASH_NONBASICBOUNDS = 1
        integer(c_int), parameter :: CRASH_MOSTFEASIBLE = 2
        integer(c_int), parameter :: CRASH_LEASTDEGENERATE = 3
        integer(c_int), parameter :: INITSOL_SHIFTZERO = 0
        integer(c_int), parameter :: INITSOL_USEZERO = 1
        integer(c_int), parameter :: INITSOL_ORIGINAL = 2
        integer(c_int), parameter :: ANTIDEGEN_NONE = 0
        integer(c_int), parameter :: ANTIDEGEN_FIXEDVARS = 1
        integer(c_int), parameter :: ANTIDEGEN_COLUMNCHECK = 2
        integer(c_int), parameter :: ANTIDEGEN_STALLING = 4
        integer(c_int), parameter :: ANTIDEGEN_NUMFAILURE = 8
        integer(c_int), parameter :: ANTIDEGEN_LOSTFEAS = 16
        integer(c_int), parameter :: ANTIDEGEN_INFEASIBLE = 32
        integer(c_int), parameter :: ANTIDEGEN_DYNAMIC = 64
        integer(c_int), parameter :: ANTIDEGEN_DURINGBB = 128
        integer(c_int), parameter :: ANTIDEGEN_RHSPERTURB = 256
        integer(c_int), parameter :: ANTIDEGEN_BOUNDFLIP = 512
        integer(c_int), parameter :: NEUTRAL = 0
        integer(c_int), parameter :: CRITICAL = 1
        integer(c_int), parameter :: SEVERE = 2
        integer(c_int), parameter :: IMPORTANT = 3
        integer(c_int), parameter :: NORMAL = 4
        integer(c_int), parameter :: DETAILED = 5
        integer(c_int), parameter :: FULL = 6
        integer(c_int), parameter :: MSG_NONE = 0
        integer(c_int), parameter :: MSG_PRESOLVE = 1
        integer(c_int), parameter :: MSG_ITERATION = 2
        integer(c_int), parameter :: MSG_INVERT = 4
        integer(c_int), parameter :: MSG_LPFEASIBLE = 8
        integer(c_int), parameter :: MSG_LPOPTIMAL = 16
        integer(c_int), parameter :: MSG_LPEQUAL = 32
        integer(c_int), parameter :: MSG_LPBETTER = 64
        integer(c_int), parameter :: MSG_MILPFEASIBLE = 128
        integer(c_int), parameter :: MSG_MILPEQUAL = 256
        integer(c_int), parameter :: MSG_MILPBETTER = 512
        integer(c_int), parameter :: MSG_MILPSTRATEGY = 1024
        integer(c_int), parameter :: MSG_MILPOPTIMAL = 2048
        integer(c_int), parameter :: MSG_PERFORMANCE = 4096
        integer(c_int), parameter :: MSG_INITPSEUDOCOST = 8192
        integer(c_int), parameter :: MPSFIXED = 1
        integer(c_int), parameter :: MPSFREE = 2
        integer(c_int), parameter :: MPSIBM = 4
        integer(c_int), parameter :: MPSNEGOBJCONST = 8
        integer(c_int), parameter :: MPSUNDEF = -4
        integer(c_int), parameter :: MPSNAME = -3
        integer(c_int), parameter :: MPSOBJSENSE = -2
        integer(c_int), parameter :: MPSOBJNAME = -1
        integer(c_int), parameter :: MPSROWS = 0
        integer(c_int), parameter :: MPSCOLUMNS = 1
        integer(c_int), parameter :: MPSRHS = 2
        integer(c_int), parameter :: MPSBOUNDS = 3
        integer(c_int), parameter :: MPSRANGES = 4
        integer(c_int), parameter :: MPSSOS = 5
        character(*), parameter :: MPSVARMASK = "%-8s"
        character(*), parameter :: MPSVALUEMASK = "%12g"
        integer(c_int), parameter :: ROWTYPE_EMPTY = 0
        integer(c_int), parameter :: ROWTYPE_LE = 1
        integer(c_int), parameter :: ROWTYPE_GE = 2
        integer(c_int), parameter :: ROWTYPE_EQ = 3
        integer(c_int), parameter :: ROWTYPE_CONSTRAINT = ROWTYPE_EQ
        integer(c_int), parameter :: ROWTYPE_OF = 4
        integer(c_int), parameter :: ROWTYPE_INACTIVE = 8
        integer(c_int), parameter :: ROWTYPE_RELAX = 16
        integer(c_int), parameter :: ROWTYPE_GUB = 32
        integer(c_int), parameter :: ROWTYPE_OFMAX = (ROWTYPE_OF+ROWTYPE_GE)
        integer(c_int), parameter :: ROWTYPE_OFMIN = (ROWTYPE_OF+ROWTYPE_LE)
        integer(c_int), parameter :: ROWTYPE_CHSIGN = ROWTYPE_GE
        integer(c_int), parameter :: FR = ROWTYPE_EMPTY
        integer(c_int), parameter :: LE = ROWTYPE_LE
        integer(c_int), parameter :: GE = ROWTYPE_GE
        integer(c_int), parameter :: EQ = ROWTYPE_EQ
        integer(c_int), parameter :: OF = ROWTYPE_OF
        integer(c_int), parameter :: ROWCLASS_Unknown = 0
        integer(c_int), parameter :: ROWCLASS_Objective = 1
        integer(c_int), parameter :: ROWCLASS_GeneralREAL = 2
        integer(c_int), parameter :: ROWCLASS_GeneralMIP = 3
        integer(c_int), parameter :: ROWCLASS_GeneralINT = 4
        integer(c_int), parameter :: ROWCLASS_GeneralBIN = 5
        integer(c_int), parameter :: ROWCLASS_KnapsackINT = 6
        integer(c_int), parameter :: ROWCLASS_KnapsackBIN = 7
        integer(c_int), parameter :: ROWCLASS_SetPacking = 8
        integer(c_int), parameter :: ROWCLASS_SetCover = 9
        integer(c_int), parameter :: ROWCLASS_GUB = 10
        integer(c_int), parameter :: ROWCLASS_MAX = ROWCLASS_GUB
        integer(c_int), parameter :: SCAN_USERVARS = 1
        integer(c_int), parameter :: SCAN_SLACKVARS = 2
        integer(c_int), parameter :: SCAN_ARTIFICIALVARS = 4
        integer(c_int), parameter :: SCAN_PARTIALBLOCK = 8
        integer(c_int), parameter :: USE_BASICVARS = 16
        integer(c_int), parameter :: USE_NONBASICVARS = 32
        integer(c_int), parameter :: SCAN_NORMALVARS = (SCAN_USERVARS+SCAN_ARTIFICIALVARS)
        integer(c_int), parameter :: SCAN_ALLVARS = (SCAN_SLACKVARS+SCAN_USERVARS+SCAN_ARTIFICIALVARS)
        integer(c_int), parameter :: USE_ALLVARS = (USE_BASICVARS+USE_NONBASICVARS)
        integer(c_int), parameter :: OMIT_FIXED = 64
        integer(c_int), parameter :: OMIT_NONFIXED = 128
        integer(c_int), parameter :: IMPROVE_NONE = 0
        integer(c_int), parameter :: IMPROVE_SOLUTION = 1
        integer(c_int), parameter :: IMPROVE_DUALFEAS = 2
        integer(c_int), parameter :: IMPROVE_THETAGAP = 4
        integer(c_int), parameter :: IMPROVE_BBSIMPLEX = 8
        integer(c_int), parameter :: IMPROVE_DEFAULT = (IMPROVE_DUALFEAS+IMPROVE_THETAGAP)
        integer(c_int), parameter :: IMPROVE_INVERSE = (IMPROVE_SOLUTION+IMPROVE_THETAGAP)
        integer(c_int), parameter :: SCALE_NONE = 0
        integer(c_int), parameter :: SCALE_EXTREME = 1
        integer(c_int), parameter :: SCALE_RANGE = 2
        integer(c_int), parameter :: SCALE_MEAN = 3
        integer(c_int), parameter :: SCALE_GEOMETRIC = 4
        integer(c_int), parameter :: SCALE_FUTURE1 = 5
        integer(c_int), parameter :: SCALE_FUTURE2 = 6
        integer(c_int), parameter :: SCALE_CURTISREID = 7
        integer(c_int), parameter :: SCALE_LINEAR = 0
        integer(c_int), parameter :: SCALE_QUADRATIC = 8
        integer(c_int), parameter :: SCALE_LOGARITHMIC = 16
        integer(c_int), parameter :: SCALE_USERWEIGHT = 31
        integer(c_int), parameter :: SCALE_MAXTYPE = (SCALE_QUADRATIC-1)
        integer(c_int), parameter :: SCALE_POWER2 = 32
        integer(c_int), parameter :: SCALE_EQUILIBRATE = 64
        integer(c_int), parameter :: SCALE_INTEGERS = 128
        integer(c_int), parameter :: SCALE_DYNUPDATE = 256
        integer(c_int), parameter :: SCALE_ROWSONLY = 512
        integer(c_int), parameter :: SCALE_COLSONLY = 1024
        integer(c_int), parameter :: SCALEMODEL_EQUILIBRATED = (SCALE_LINEAR+SCALE_EXTREME+SCALE_INTEGERS)
        integer(c_int), parameter :: SCALEMODEL_GEOMETRIC = (SCALE_LINEAR+SCALE_GEOMETRIC+SCALE_INTEGERS)
        integer(c_int), parameter :: SCALEMODEL_ARITHMETIC = (SCALE_LINEAR+SCALE_MEAN+SCALE_INTEGERS)
        integer(c_int), parameter :: SCALEMODEL_DYNAMIC = (SCALEMODEL_GEOMETRIC+SCALE_EQUILIBRATE)
        integer(c_int), parameter :: SCALEMODEL_CURTISREID = (SCALE_CURTISREID+SCALE_INTEGERS+SCALE_POWER2)
        integer(c_int), parameter :: ITERATE_MAJORMAJOR = 0
        integer(c_int), parameter :: ITERATE_MINORMAJOR = 1
        integer(c_int), parameter :: ITERATE_MINORRETRY = 2
        integer(c_int), parameter :: PRICER_FIRSTINDEX = 0
        integer(c_int), parameter :: PRICER_DANTZIG = 1
        integer(c_int), parameter :: PRICER_DEVEX = 2
        integer(c_int), parameter :: PRICER_STEEPESTEDGE = 3
        integer(c_int), parameter :: PRICER_LASTOPTION = PRICER_STEEPESTEDGE
        real(c_double), parameter :: PRICER_RANDFACT = 0.1
        integer(c_int), parameter :: DEVEX_RESTARTLIMIT = 1.0e+09
        integer(c_int), parameter :: DEVEX_MINVALUE = 0.000
        integer(c_int), parameter :: PRICE_PRIMALFALLBACK = 4
        integer(c_int), parameter :: PRICE_MULTIPLE = 8
        integer(c_int), parameter :: PRICE_PARTIAL = 16
        integer(c_int), parameter :: PRICE_ADAPTIVE = 32
        integer(c_int), parameter :: PRICE_HYBRID = 64
        integer(c_int), parameter :: PRICE_RANDOMIZE = 128
        integer(c_int), parameter :: PRICE_AUTOPARTIAL = 256
        integer(c_int), parameter :: PRICE_AUTOMULTIPLE = 512
        integer(c_int), parameter :: PRICE_LOOPLEFT = 1024
        integer(c_int), parameter :: PRICE_LOOPALTERNATE = 2048
        integer(c_int), parameter :: PRICE_HARRISTWOPASS = 4096
        integer(c_int), parameter :: PRICE_FORCEFULL = 8192
        integer(c_int), parameter :: PRICE_TRUENORMINIT = 16384
        integer(c_int), parameter :: PRICE_NOBOUNDFLIP = 65536
        integer(c_int), parameter :: PRICE_STRATEGYMASK = (PRICE_PRIMALFALLBACK+PRICE_MULTIPLE+PRICE_PARTIAL+ &
                                                           PRICE_ADAPTIVE+PRICE_HYBRID+PRICE_RANDOMIZE+PRICE_AUTOPARTIAL+ &
                                                           PRICE_AUTOMULTIPLE+PRICE_LOOPLEFT+PRICE_LOOPALTERNATE+ &
                                                           PRICE_HARRISTWOPASS+PRICE_FORCEFULL+PRICE_TRUENORMINIT)
        integer(c_int), parameter :: BB_REAL = 0
        integer(c_int), parameter :: BB_INT = 1
        integer(c_int), parameter :: BB_SC = 2
        integer(c_int), parameter :: BB_SOS = 3
        integer(c_int), parameter :: BB_GUB = 4
        integer(c_int), parameter :: NODE_FIRSTSELECT = 0
        integer(c_int), parameter :: NODE_GAPSELECT = 1
        integer(c_int), parameter :: NODE_RANGESELECT = 2
        integer(c_int), parameter :: NODE_FRACTIONSELECT = 3
        integer(c_int), parameter :: NODE_PSEUDOCOSTSELECT = 4
        integer(c_int), parameter :: NODE_PSEUDONONINTSELECT = 5
        integer(c_int), parameter :: NODE_PSEUDORATIOSELECT = 6
        integer(c_int), parameter :: NODE_USERSELECT = 7
        integer(c_int), parameter :: NODE_WEIGHTREVERSEMODE = 8
        integer(c_int), parameter :: NODE_STRATEGYMASK = (NODE_WEIGHTREVERSEMODE-1)
        integer(c_int), parameter :: NODE_PSEUDOFEASSELECT = (NODE_PSEUDONONINTSELECT+NODE_WEIGHTREVERSEMODE)
        integer(c_int), parameter :: NODE_BRANCHREVERSEMODE = 16
        integer(c_int), parameter :: NODE_GREEDYMODE = 32
        integer(c_int), parameter :: NODE_PSEUDOCOSTMODE = 64
        integer(c_int), parameter :: NODE_DEPTHFIRSTMODE = 128
        integer(c_int), parameter :: NODE_RANDOMIZEMODE = 256
        integer(c_int), parameter :: NODE_GUBMODE = 512
        integer(c_int), parameter :: NODE_DYNAMICMODE = 1024
        integer(c_int), parameter :: NODE_RESTARTMODE = 2048
        integer(c_int), parameter :: NODE_BREADTHFIRSTMODE = 4096
        integer(c_int), parameter :: NODE_AUTOORDER = 8192
        integer(c_int), parameter :: NODE_RCOSTFIXING = 16384
        integer(c_int), parameter :: NODE_STRONGINIT = 32768
        integer(c_int), parameter :: BRANCH_CEILING = 0
        integer(c_int), parameter :: BRANCH_FLOOR = 1
        integer(c_int), parameter :: BRANCH_AUTOMATIC = 2
        integer(c_int), parameter :: BRANCH_DEFAULT = 3
        integer(c_int), parameter :: ACTION_NONE = 0
        integer(c_int), parameter :: ACTION_ACTIVE = 1
        integer(c_int), parameter :: ACTION_REBASE = 2
        integer(c_int), parameter :: ACTION_RECOMPUTE = 4
        integer(c_int), parameter :: ACTION_REPRICE = 8
        integer(c_int), parameter :: ACTION_REINVERT = 16
        integer(c_int), parameter :: ACTION_TIMEDREINVERT = 32
        integer(c_int), parameter :: ACTION_ITERATE = 64
        integer(c_int), parameter :: ACTION_RESTART = 255
        integer(c_int), parameter :: UNKNOWNERROR = -5
        integer(c_int), parameter :: DATAIGNORED = -4
        integer(c_int), parameter :: NOBFP = -3
        integer(c_int), parameter :: NOMEMORY = -2
        integer(c_int), parameter :: NOTRUN = -1
        integer(c_int), parameter :: OPTIMAL = 0
        integer(c_int), parameter :: SUBOPTIMAL = 1
        integer(c_int), parameter :: INFEASIBLE = 2
        integer(c_int), parameter :: UNBOUNDED = 3
        integer(c_int), parameter :: DEGENERATE = 4
        integer(c_int), parameter :: NUMFAILURE = 5
        integer(c_int), parameter :: USERABORT = 6
        integer(c_int), parameter :: TIMEOUT = 7
        integer(c_int), parameter :: RUNNING = 8
        integer(c_int), parameter :: PRESOLVED = 9
        integer(c_int), parameter :: PROCFAIL = 10
        integer(c_int), parameter :: PROCBREAK = 11
        integer(c_int), parameter :: FEASFOUND = 12
        integer(c_int), parameter :: NOFEASFOUND = 13
        integer(c_int), parameter :: FATHOMED = 14
        integer(c_int), parameter :: SWITCH_TO_PRIMAL = 20
        integer(c_int), parameter :: SWITCH_TO_DUAL = 21
        integer(c_int), parameter :: SINGULAR_BASIS = 22
        integer(c_int), parameter :: LOSTFEAS = 23
        integer(c_int), parameter :: MATRIXERROR = 24
        integer(c_int), parameter :: OF_RELAXED = 0
        integer(c_int), parameter :: OF_INCUMBENT = 1
        integer(c_int), parameter :: OF_WORKING = 2
        integer(c_int), parameter :: OF_USERBREAK = 3
        integer(c_int), parameter :: OF_HEURISTIC = 4
        integer(c_int), parameter :: OF_DUALLIMIT = 5
        integer(c_int), parameter :: OF_DELTA = 8
        integer(c_int), parameter :: OF_PROJECTED = 16
        integer(c_int), parameter :: OF_TEST_BT = 1
        integer(c_int), parameter :: OF_TEST_BE = 2
        integer(c_int), parameter :: OF_TEST_NE = 3
        integer(c_int), parameter :: OF_TEST_WE = 4
        integer(c_int), parameter :: OF_TEST_WT = 5
        integer(c_int), parameter :: OF_TEST_RELGAP = 8
        integer(c_int), parameter :: MAT_START_SIZE = 10000
        integer(c_int), parameter :: DELTACOLALLOC = 100
        integer(c_int), parameter :: DELTAROWALLOC = 100
        integer(c_int), parameter :: RESIZEFACTOR = 4
        integer(c_int), parameter :: DEF_PARTIALBLOCKS = 10
        integer(c_int), parameter :: DEF_MAXRELAX = 7
        integer(c_int), parameter :: DEF_MAXPIVOTRETRY = 10
        integer(c_int), parameter :: DEF_MAXSINGULARITIES = 10
        integer(c_int), parameter :: MAX_MINITUPDATES = 60
        integer(c_int), parameter :: MIN_REFACTFREQUENCY = 5
        integer(c_int), parameter :: LAG_SINGULARLIMIT = 5
        real(c_double), parameter :: MIN_TIMEPIVOT = 5.0e-02
        integer(c_int), parameter :: MAX_STALLCOUNT = 12
        integer(c_int), parameter :: MAX_RULESWITCH = 5
        integer(c_int), parameter :: DEF_TIMEDREFACT = AUTOMATIC
        integer(c_int), parameter :: DEF_SCALINGLIMIT = 5
        integer(c_int), parameter :: DEF_NEGRANGE = -1.0e+06
        integer(c_int), parameter :: DEF_BB_LIMITLEVEL = -50
        integer(c_int), parameter :: MAX_FRACSCALE = 6
        integer(c_int), parameter :: RANDSCALE = 100
        integer(c_int), parameter :: DOUBLEROUND = 0.0e-02
        real(c_double), parameter :: DEF_EPSMACHINE = 2.22e-16
        integer(c_int), parameter :: MIN_STABLEPIVOT = 5.0
        integer(c_int), parameter :: LIMIT_ABS_REL = 10.0
        integer(c_int), parameter :: EPS_TIGHT = 0
        integer(c_int), parameter :: EPS_MEDIUM = 1
        integer(c_int), parameter :: EPS_LOOSE = 2
        integer(c_int), parameter :: EPS_BAGGY = 3
        integer(c_int), parameter :: EPS_DEFAULT = EPS_TIGHT
        real(c_double), parameter :: DEF_INFINITE = 1.0e+30
        real(c_double), parameter :: DEF_EPSVALUE = 1.0e-12
        real(c_double), parameter :: DEF_EPSPRIMAL = 1.0e-10
        real(c_double), parameter :: DEF_EPSDUAL = 1.0e-09
        real(c_double), parameter :: DEF_EPSPIVOT = 2.0e-07
        real(c_double), parameter :: DEF_PERTURB = 1.0e-05
        real(c_double), parameter :: DEF_EPSSOLUTION = 1.0e-05
        real(c_double), parameter :: DEF_EPSINT = 1.0e-07
        real(c_double), parameter :: DEF_MIP_GAP = 1.0e-11
        real(c_double), parameter :: SCALEDINTFIXRANGE = 1.6
        real(c_double), parameter :: MIN_SCALAR = 1.0e-10
        real(c_double), parameter :: MAX_SCALAR = 1.0e+10
        real(c_double), parameter :: DEF_SCALINGEPS = 1.0e-02
        real(c_double), parameter :: DEF_LAGACCEPT = 1.0e-03
        real(c_double), parameter :: DEF_LAGCONTRACT = 0.90
        integer(c_int), parameter :: DEF_LAGMAXITERATIONS = 100
        integer(c_int), parameter :: DEF_PSEUDOCOSTUPDATES = 7
        real(c_double), parameter :: DEF_PSEUDOCOSTRESTART = 0.15
        integer(c_int), parameter :: DEF_MAXPRESOLVELOOPS = 0
        integer(c_int), parameter :: LINEARSEARCH = 0
end module lp_solve_constants