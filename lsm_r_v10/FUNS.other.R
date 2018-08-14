flush.vars <- function(envir) {
  # update target variables in the global environment after beeing modified inside a function's environment 'envir'
  for (var in VARLIST) assign(var, get(var, envir = envir), envir = .GlobalEnv)
}

check.vars <- function(envir) {
  for (var in VARLIST) {
    v <- get(var, envir = envir)
    if (any(is.null(v)) | any(is.nan(v)) | any(is.na(v))) {
      callingFun = as.list(sys.call(-3))[[1]]
      calledFun  = as.list(sys.call(-2))[[1]]
      message(paste(callingFun, " is calling ", calledFun, "\n\nvar:", var, sep = ""))
      stop()
    }
  }
}

read.env <- function(i) {
  env <- w[i,] %>% select(-time)
  for (n in colnames(env)) assign(n, env[[n]], envir = .GlobalEnv)
}

housekeeping <- function() {
  #---------------------#
  # compute:
  #  - saturation mixing ratio (Q2SAT)
  #  - slope of the saturation specific humidity curve for PENMAN (DQSDT2)
  #  - virtual and potential temperatures at ground level (sub1) and 
  #     1st middle level above ground (sub2)
  #  - net incoming solar radiation
  #---------------------#
  # includes subroutines/functions:
  #   QDATAP
  #     - calculate saturation vapor pressure (Pa) at temperature T (K)
  #     - uses Clausius-Clapeyron
  #   DQS
  #     - calculate values of vapor pressure (E)
  #   DQSDT
  #     - calculate the change of the saturation mixing ratio with respect to 
  #     - the change in temperature
  #---------------------#

  ## QDATAP
  # ESAT = saturation vapor pressure for water (Pa)
  LW   <- LVH2O - (CW - CPV) * (air - FRZ)
  ESAT <- ESO * exp(LW * (1 / FRZ - 1 / air) / RV)

  # convert rh (%) to the fractional value
  RHF <- rh / 100
  
  # calculate saturation mixing ratio
  Q2SAT <- EPS * ESAT / (pres - (1 - EPS) * ESAT)
  
  # conversion from rh
  EP <- (pres * ESAT * RHF) / (pres - ESAT * (1 - RHF))
  Q2 <- EPS * EP / (pres - (1 - EPS) * EP)
  
  # ensure boundaries
  if (Q2 < 1e-6)   Q2 <- 1e-6
  if (Q2 >= Q2SAT) Q2 <- Q2SAT * 0.99

  
  ## DQSDT
  # this approximation for the slope is only valid between
  # 173 and 373 K (-100 to +100 C)
  
  # if (!between(air, 173, 373)) stop("DQSDT") 
  ## this is checked before during the preparation of the forcing data
  DESDT <- LW  * ESAT / (RV * air^2)
  DQS   <- EPS * DESDT

  DQSDT <- DQS / pres

  
  # calculate virtual and potential temperatures at ground level (sub1) and 
  #  1st middle level above ground (sub2)
  TH2  <- air   + (0.0098   * ZLVL)
  T2V  <- air   * (1 + 0.61 * Q2)
  T1V  <- TSKIN * (1 + 0.61 * Q2)
  TH2V <- TH2   * (1 + 0.61 * Q2)
  
  # determine total downward radiation (solar + longwave)
  FDOWN <- (sw * (1 - ALBEDO)) + lw
  
  # return
  check.vars(environment())
  flush.vars(environment())
}


#---------------------#
# SFCDIF
#---------------------#
# CALCULATE SURFACE LAYER EXCHANGE COEFFS VIA ITERATIVE PROCESS
#---------------------#
# PAULSON'S SURFACE FUNCTIONS
PSPMU <- function(XX) {
  x <- -2 * log((XX + 1) * 0.5) - log((XX * XX + 1) * 0.5)
  x + 2 * atan(XX) - (pi / 2)
}
PSPMS <- function(YY) { 5 * YY }
PSPHU <- function(XX) { -2 * log((XX * XX + 1) * 0.5) }
PSPHS <- PSPMS

SFCDIF <- function() {
  WWST2  = 1.2^2
  VKRM   = 0.40
  EXCM   = 0.001
  BETA_local = 1/270
  BTG    = BETA_local * G
  ELFC   = VKRM * BTG
  WOLD   = 0.15
  WNEW   = 1 - WOLD
  ITRMX  = 5

  EPSU2  = 1e-4
  EPSUST = 0.07
  ZTMIN  = -5
  ZTMAX  = 1
  HPBL   = 1000
  SQVISC = 258.2
  
  RDZ  = 1 / ZLVL
  CXCH = EXCM * RDZ
  DTHV = TH2V - T1V
  DU2  = max(wind^2, EPSU2)
  
  # BELJAARS CORRECTION OF USTAR
  BTGH   <- BTG * HPBL
  WSTAR2 <- WWST2 * abs(BTGH * CH * DTHV)^(2 / 3)
  USTAR  <- max(sqrt(CM * sqrt(DU2 + WSTAR2)), EPSUST)
  
  # CZIL: CONSTANT C IN Zilitinkevich, S. S.1995,:NOTE ABOUT ZT
  ZILFC <- -CZIL * VKRM * SQVISC
  
  # ZILITINKEVITCH APPROACH FOR ZT
  ZT <- exp(ZILFC * sqrt(USTAR * Z0)) * Z0
  
  ZSLU  = ZLVL + Z0
  ZSLT  = ZLVL + ZT
  
  RLOGU = log(ZSLU / Z0)
  RLOGT = log(ZSLT / ZT)
  
  RLMO  = ELFC * CH * DTHV / USTAR^3
  
  for (i in 1:ITRMX) {
    # 1./MONIN-OBUKKHOV LENGTH-SCALE
    ZETALT = max(ZSLT * RLMO, ZTMIN)
    RLMO   = ZETALT / ZSLT
    ZETALU = ZSLU * RLMO
    ZETAU  = Z0 * RLMO
    ZETAT  = ZT * RLMO
    
    if (RLMO < 0) {
      XLU4 = 1 - (16 * ZETALU)
      XLT4 = 1 - (16 * ZETALT)
      XU4  = 1 - (16 * ZETAU)
      XT4  = 1 - (16 * ZETAT)
      
      XLU = sqrt(sqrt(XLU4))
      XLT = sqrt(sqrt(XLT4))
      XU  = sqrt(sqrt(XU4))
      XT  = sqrt(sqrt(XT4))
      
      PSMZ = PSPMU(XU)
      SIMM = PSPMU(XLU) - PSMZ + RLOGU
      PSHZ = PSPHU(XT)
      SIMH = PSPHU(XLT) - PSHZ + RLOGT
    }else{
      ZETALU = min(ZETALU, ZTMAX)
      ZETALT = min(ZETALT, ZTMAX)
      PSMZ = PSPMS(ZETAU)
      SIMM = PSPMS(ZETALU) - PSMZ + RLOGU
      PSHZ = PSPHS(ZETAT)
      SIMH = PSPHS(ZETALT) - PSHZ + RLOGT
    }
    
    # BELJAARS CORRECTION OF USTAR
    USTAR = max(sqrt(CM * sqrt(DU2 + WSTAR2)), EPSUST)
    
    # ZILITINKEVITCH APPROACH FOR ZT
    ZT = exp(ZILFC * sqrt(USTAR * Z0)) * Z0
    ZSLT  = ZLVL + ZT
    RLOGT = log(ZSLT / ZT)
    
    USTARK = USTAR * VKRM
    CM = max(USTARK / SIMM, CXCH)
    CH = max(USTARK / SIMH, CXCH)

    RLMN = ELFC * CH * DTHV / USTAR^3
    RLMO = RLMO * WOLD + RLMN * WNEW
  }
  
  # return
  check.vars(environment())
  flush.vars(environment())
}
#---------------------#


#---------------------#
# NOPAC
#---------------------#
# CALCULATE SOIL MOISTURE AND HEAT FLUX VALUES AND UPDATE SOIL MOISTURE
# CONTENT AND SOIL HEAT CONTENT VALUES FOR THE CASE WHEN NO SNOW PACK IS
# PRESENT
#---------------------#
NOPAC <- function() {
  # CONVERT ETP FROM KG M-2 S-1 TO MS-1 AND INITIALIZE DEW
  RAIN1 = rain * 0.001
  ETP1  = ETP * 0.001
  DEW   = 0
  
  EDIR  = 0
  EDIR1 = 0
  ET  <- ZEROS
  ET1 <- ZEROS

  if (ETP > 0) {
    # CONVERT RAIN FROM 'KG M-2 S-1' TO 'M S-1'
    # CALCULATE SOIL MOISTURE FLUX
    #---------------------#
    # RETRIEVE DIRECT EVAPORATION FROM SOIL SURFACE
    # LINEAR WHEN FXEXP = 1
    # FX > 1 REPRESENTS DEMAND CONTROL
    # FX < 1 REPRESENTS FLUX CONTROL
    SRATIO <- (SMC[1] - SMCWLT) / (SMCMAX - SMCWLT)
    if (SRATIO > 0) {
      FX <- max(min(SRATIO^FXEXP, 1), 0)
    }else{
      FX <- 0
    }
    
    # ALLOW FOR THE DIRECT-EVAP-REDUCING EFFECT OF SHADE
    EDIR1 = FX * ETP1
    
    # TOTAL UP EVAP AND TRANSP TYPES TO OBTAIN ACTUAL EVAPOTRANSP
    ETA1 = EDIR1
  }else{
    # IF ETP < 0, ASSUME DEW FORMS (TRANSFORM ETP1 INTO DEW AND REINITIALIZE
    # ETP1 TO ZERO)
    DEW = -ETP1
    # CONVERT RAIN FROM 'KG M-2 S-1' TO 'M S-1' AND ADD DEW AMOUNT
    RAIN1 = RAIN1 + DEW
  }
  
  # CALCULATE SOIL MOISTURE FLUX
  flush.vars(environment())
  SMFLX()
  
  # CONVERT MODELED EVAPOTRANSPIRATION FM  M S-1  TO  KG M-2 S-1
  ETA  = ETA1  * 1000
  EDIR = EDIR1 * 1000
  ET   = ET1   * 1000

  flush.vars(environment())
  SHFLX()
  
  # return
  check.vars(environment())
  flush.vars(environment())
}
#---------------------#


#---------------------#
# SHFLX
#---------------------#
# UPDATE THE TEMPERATURE STATE OF THE SOIL COLUMN BASED ON THE THERMAL
# DIFFUSION EQUATION AND UPDATE THE FROZEN SOIL MOISTURE CONTENT BASED
# ON THE TEMPERATURE
#---------------------#
SHFLX <- function() {
  # DETERMINE BETA BASED ON THE VALUE OF ETP
  if (ETP < 0)  BETAlocal = 1
  if (ETP == 0) BETAlocal = 0
  if (ETP > 0)  BETAlocal = ETA / ETP
  
  # GET SOIL THERMAL DIFFUSIVITY/CONDUCTIVITY FOR TOP SOIL LYR,
  # CALC ADJUSTED TOP LYR SOIL TEMP AND ADJUSTED SOIL FLUX, THEN
  # CALL SHFLX TO COMPUTE/UPDATE SOIL HEAT FLUX AND SOIL TEMPS
  DF1 = BODY_DIFUSIVITY
  
  # COMPUTE INTERMEDIATE TERMS PASSED TO ROUTINE HRT (VIA ROUTINE 
  # SHFLX BELOW) FOR USE IN COMPUTING SUBSURFACE HEAT FLUX IN HRT
  YYNUM = FDOWN - SIGMA * T24 * EMISSIVITY
  YY    = air + (YYNUM / RCH + TH2 - air - BETAlocal * EPSCA) / RR
  ZZ1   = DF1 / (-0.5 * ZSOIL[1] * RCH * RR) + 1
  
  # HRT ROUTINE CALCS THE RIGHT HAND SIDE OF THE SOIL TEMP DIF EQN
  flush.vars(environment())
  HRT()
  flush.vars(environment())
  HSTEP()

  # IN THE NO SNOWPACK CASE (VIA ROUTINE NOPAC BRANCH,) UPDATE THE GRND
  # (SKIN) TEMPERATURE HERE IN RESPONSE TO THE UPDATED SOIL TEMPERATURE 
  # PROFILE ABOVE
  TSKIN = (YY + (ZZ1 - 1) * STC[1]) / ZZ1
  
  # CALCULATE SURFACE SOIL HEAT FLUX
  SSOIL = DF1 * (STC[1] - TSKIN) / (0.5 * ZSOIL[1])
  
  # return
  check.vars(environment())
  flush.vars(environment())
}
#---------------------#


#---------------------#
# HRT
#---------------------#
# UPDATE THE TEMPERATURE STATE OF THE SOIL COLUMN BASED ON THE THERMAL DIFFUSION EQUATION
#---------------------#
HRT <- function() {
  # CALCULATE THE RIGHT HAND SIDE OF THE TIME TENDENCY TERM OF THE SOIL
  # THERMAL DIFFUSION EQUATION.  ALSO TO COMPUTE ( PREPARE ) THE MATRIX
  # COEFFICIENTS FOR THE TRI-DIAGONAL MATRIX OF THE IMPLICIT TIME SCHEME
  # EXECUTED WITH SOIL LAYER TEMPERATURE AVERAGING
  AI <- BI <- CI <- ZEROS

  #### BEGIN OF SECTION FOR TOP SOIL LAYER
  # CALC THE HEAT CAPACITY OF THE TOP SOIL LAYER
  # ALLOW FOR LAYERS OF MUSSEL MODEL
  HCPCT = SMC[1] * CH2O + (1 - SMCMAX) * HTCP_ANIMAL + (SMCMAX - SMC[1]) * CAIR
  
  # CALC THE MATRIX COEFFICIENTS AI, BI, AND CI FOR THE TOP LAYER
  DDZ = 1 / (-0.5 * ZSOIL[2])
  AI[1] = 0
  CI[1] = (DF1 * DDZ) / (ZSOIL[1] * HCPCT)
  BI[1] = -CI[1] + DF1 / (0.5 * ZSOIL[1]^2 * HCPCT * ZZ1)

  # CALCULATE THE VERTICAL SOIL TEMP GRADIENT BTWN THE 1ST AND 2ND SOIL
  # LAYERS.  THEN CALCULATE THE SUBSURFACE HEAT FLUX. USE THE TEMP
  # GRADIENT AND SUBSFC HEAT FLUX TO CALC "RIGHT-HAND SIDE TENDENCY
  # TERMS", OR "RHSTS", FOR TOP SOIL LAYER
  DTSDZ = (STC[1] - STC[2]) / (-0.5 * ZSOIL[2])
  SSOIL = DF1 * (STC[1] - YY) / (0.5 * ZSOIL[1] * ZZ1)
  RHSTS[1] = (DF1 * DTSDZ - SSOIL) / (ZSOIL[1] * HCPCT)
  
  #### BEGIN OF SECTION FOR TOP SOIL LAYER
  
  # INITIALIZE DDZ2
  DDZ2 = 0
  
  # LOOP THRU THE REMAINING SOIL LAYERS, REPEATING THE ABOVE PROCESS
  # (EXCEPT SUBSFC OR "GROUND" HEAT FLUX NOT REPEATED IN LOWER LAYERS)
  DF1K = DF1
  for (K in 2:NSOIL) {
    # CALCULATE HEAT CAPACITY FOR THIS SOIL LAYER
    # ALLOW HEAT CAPACITY TO BE DEPENDENT ON LAYER (MUSSEL MODEL)
    if (K <= BED_DEPTH) {
      HCPCT = SMC[K] * CH2O + (1 - SMCMAX) * HTCP_ANIMAL + (SMCMAX - SMC[K]) * CAIR
      if (K < BED_DEPTH) HCPCT = HCPCT * CONTACT
    }else{
      HCPCT = HTCP_ROCK
    }
    
    if (K != NSOIL) {
      # THIS SECTION FOR LAYER 2 OR GREATER, BUT NOT LAST LAYER
      # CALCULATE THERMAL DIFFUSIVITY FOR THIS LAYER.
      # ALLOW THERM DIFF TO BE DEPENDENT ON LAYER (MUSSEL MODEL)
      if (K <  BED_DEPTH) DF1N = BODY_DIFUSIVITY
      if (K == BED_DEPTH) DF1N = BODY_DIFUSIVITY * CONTACT
      if (K >  BED_DEPTH) DF1N = ROCKDF1N
      
      # CALC THE VERTICAL SOIL TEMP GRADIENT THRU THIS LAYER
      DENOM  = 0.5 * (ZSOIL[K - 1] - ZSOIL[K + 1])
      DTSDZ2 = (STC[K] - STC[K + 1]) / DENOM
  
      # CALC THE MATRIX COEF, CI, AFTER CALC'NG ITS PARTIAL PRODUCT
      DDZ2  = 2 / (ZSOIL[K - 1] - ZSOIL[K + 1])
      CI[K] = -DF1N * DDZ2 / ((ZSOIL[K - 1] - ZSOIL[K]) * HCPCT)
    }else{
      # SPECIAL CASE OF BOTTOM SOIL LAYER:
      # CALCULATE THERMAL DIFFUSIVITY FOR BOTTOM LAYER
      DF1N = ROCKDF1N
      
      # CALC THE VERTICAL SOIL TEMP GRADIENT THRU BOTTOM LAYER
      DENOM  = 0.5 * (ZSOIL[K - 1] + ZSOIL[K]) - ZBOT
      DTSDZ2 = (STC[K] - BOTTOM_TEMP) / DENOM

      # SET MATRIX COEF, CI TO ZERO IF BOTTOM LAYER
      CI[K] = 0
    }
    
    # CALCULATE RHSTS FOR THIS LAYER AFTER CALC'NG A PARTIAL PRODUCT
    DENOM    = (ZSOIL[K] - ZSOIL[K - 1]) * HCPCT
    RHSTS[K] = (DF1N * DTSDZ2 - DF1K * DTSDZ) / DENOM

    # CALC MATRIX COEFS, AI, AND BI FOR THIS LAYER
    AI[K] = -DF1 * DDZ / ((ZSOIL[K - 1] - ZSOIL[K]) * HCPCT)
    BI[K] = -(AI[K] + CI[K])
    
    # RESET VALUES OF DF1, DTSDZ, DDZ, AND TBK FOR LOOP TO NEXT SOIL LAYER
    DF1K  = DF1N
    DTSDZ = DTSDZ2
    DDZ   = DDZ2
  }

  # return
  check.vars(environment())
  flush.vars(environment())
}
#---------------------#


#---------------------#
# HSTEP
#---------------------#
# CALCULATE/UPDATE THE SOIL TEMPERATURE FIELD
#---------------------#
HSTEP <- function() {
  # CREATE FINITE DIFFERENCE VALUES FOR USE IN ROSR12 ROUTINE
  for (K in 1:NSOIL) {
    RHSTS[K] = DT * RHSTS[K]
    AI[K]    = DT * AI[K]
    BI[K]    = DT * BI[K] + 1
    CI[K]    = DT * CI[K]
  }
  
  # COPY VALUES FOR INPUT VARIABLES BEFORE CALL TO ROSR12
  RHSTSin = RHSTS
  CIin    = CI

  # SOLVE THE TRI-DIAGONAL MATRIX EQUATION
  CI <- ROSR12(CI, AI, BI, CIin, RHSTSin, RHSTS)
  
  # CALC/UPDATE THE SOIL TEMPS USING MATRIX SOLUTION
  # DANGER MUSSELMODEL if tide is in, set mussel layer temperatures to SST
  # V1.6 - slowly pull temps to SST
  for (K in 1:NSOIL) {
    if (tide & K <= BED_DEPTH) {
      STC[K] = STC[K] - 0.48 * (STC[K] - sst)
    }else{
      STC[K] = STC[K] + CI[K]
    }
  }
  
  # return
  check.vars(environment())
  flush.vars(environment())
}
#---------------------#


#---------------------#
# ROSR12
#---------------------#
# INVERT (SOLVE) THE TRI-DIAGONAL MATRIX PROBLEM SHOWN BELOW
# ###                                            ### ###  ###   ###  ###
# #B(1), C(1),  0  ,  0  ,  0  ,   . . .  ,    0   # #      #   #      #
# #A(2), B(2), C(2),  0  ,  0  ,   . . .  ,    0   # #      #   #      #
# # 0  , A(3), B(3), C(3),  0  ,   . . .  ,    0   # #      #   # D(3) #
# # 0  ,  0  , A(4), B(4), C(4),   . . .  ,    0   # # P(4) #   # D(4) #
# # 0  ,  0  ,  0  , A(5), B(5),   . . .  ,    0   # # P(5) #   # D(5) #
# # .                                          .   # #  .   # = #   .  #
# # .                                          .   # #  .   #   #   .  #
# # .                                          .   # #  .   #   #   .  #
# # 0  , . . . , 0 , A(M-2), B(M-2), C(M-2),   0   # #P(M-2)#   #D(M-2)#
# # 0  , . . . , 0 ,   0   , A(M-1), B(M-1), C(M-1)# #P(M-1)#   #D(M-1)#
# # 0  , . . . , 0 ,   0   ,   0   ,  A(M) ,  B(M) # # P(M) #   # D(M) #
# ###                                            ### ###  ###   ###  ###
#---------------------#
ROSR12 <- function(P, A, B, C, D, DELTA) {
  # INITIALIZE EQN COEF C FOR THE LOWEST SOIL LAYER
  C[NSOIL] = 0

  # SOLVE THE COEFS FOR THE 1ST SOIL LAYER
  P[1]     = -C[1] / B[1]
  DELTA[1] =  D[1] / B[1]

  # SOLVE THE COEFS FOR SOIL LAYERS 2 THRU NSOIL
  for (K in 2:NSOIL) {
    P[K]     = -C[K] * (1 / (B[K] + A[K] * P[K - 1]))
    DELTA[K] = (D[K] - A[K] * DELTA[K - 1]) * (1 / (B[K] + A[K] * P[K - 1]))
  }

  # SET P TO DELTA FOR LOWEST SOIL LAYER
  P[NSOIL] = DELTA[NSOIL]

  # ADJUST P FOR SOIL LAYERS 2 THRU NSOIL
  for (K in 2:NSOIL) {
    KK    = NSOIL - K + 1
    P[KK] = P[KK] * P[KK + 1] + DELTA[KK]
  }
  
  P
}
#---------------------#


#---------------------#
# SMFLX
#---------------------#
# CALCULATE SOIL MOISTURE FLUX
# THE SOIL MOISTURE CONTENT (SMC - A PER UNIT VOLUME MEASUREMENT) 
# IS A DEPENDENT VARIABLE THAT IS UPDATED WITH PROGNOSTIC EQNS
#---------------------#
SMFLX <- function() {
  # CALL SUBROUTINES SRT AND SSTEP TO SOLVE THE SOIL MOISTURE TENDENCY EQUATIONS
  #
  # IF THE INFILTRATING PRECIP RATE IS NONTRIVIAL,
  #   (WE CONSIDER NONTRIVIAL TO BE A PRECIP TOTAL OVER THE TIME STEP 
  #    EXCEEDING ONE ONE-THOUSANDTH OF THE WATER HOLDING CAPACITY OF 
  #    THE FIRST SOIL LAYER)
  # THEN CALL THE SRT/SSTEP SUBROUTINE PAIR TWICE IN THE MANNER OF 
  #   TIME SCHEME "F" (IMPLICIT STATE, AVERAGED COEFFICIENT)
  #   OF SECTION 2 OF KALNAY AND KANAMITSU (1988, MWR, VOL 116, 
  #   PAGES 1945-1958)TO MINIMIZE 2-DELTA-T OSCILLATIONS IN THE 
  #   SOIL MOISTURE VALUE OF THE TOP SOIL LAYER THAT CAN ARISE BECAUSE
  #   OF THE EXTREME NONLINEAR DEPENDENCE OF THE SOIL HYDRAULIC 
  #   DIFFUSIVITY COEFFICIENT AND THE HYDRAULIC CONDUCTIVITY ON THE
  #   SOIL MOISTURE STATE
  # OTHERWISE CALL THE SRT/SSTEP SUBROUTINE PAIR ONCE IN THE MANNER OF
  #   TIME SCHEME "D" (IMPLICIT STATE, EXPLICIT COEFFICIENT) 
  #   OF SECTION 2 OF KALNAY AND KANAMITSU
  # RAIN1 IS UNITS OF KG/M**2/S OR MM/S, ZSOIL IS NEGATIVE DEPTH IN M 
  
  SRT(SMC)
  if ((RAIN1 * DT) > (-ZSOIL[1] * SMCMAX)) {
    # FROZEN GROUND VERSION:
    # SMC STATES REPLACED BY SH2O STATES IN SRT SUBR
    # SH2O & SICE STATES INCLUDED IN SSTEP SUBR
    # FROZEN GROUND CORRECTION FACTOR, FRZFACT ADDED
    # ALL WATER BALANCE CALCULATIONS USING UNFROZEN WATER
    SMC_OUT <- ZEROS
    SMC_IN  <- SMC
    flush.vars(environment())
    SMC_OUT <- SSTEP(SMC_OUT, SMC_IN)
    SMC_A = (SMC + SMC_OUT) / 2
    # now with SMC_A
    flush.vars(environment())
    SRT(SMC_A)
  }
  flush.vars(environment())
  SSTEP(SMC, SMC)
  
  # return
  check.vars(environment())
  flush.vars(environment())
}
#---------------------#


#---------------------#
# SRT
#---------------------#
# CALCULATE THE RIGHT HAND SIDE OF THE TIME TENDENCY TERM OF THE SOIL
# WATER DIFFUSION EQUATION.  ALSO TO COMPUTE (PREPARE) THE MATRIX
# COEFFICIENTS FOR THE TRI-DIAGONAL MATRIX OF THE IMPLICIT TIME SCHEME
#---------------------#
SRT <- function(SMC_A) {
  MXSMC = SMC_A[1]
  WCND <- 0
  # DETERMINE RAINFALL INFILTRATION RATE
  PDDUM = RAIN1
  if (RAIN1 != 0) {
    # MODIFIED BY Q. DUAN, 5/16/94
    DT1     = DT / 86400
    SMCAV   = SMCMAX - SMCWLT
    DMAX    = ZEROS
    DMAX[1] = -ZSOIL[1] * SMCAV
    
    DMAX[1] = DMAX[1] * (1 - (SMC_A[1] - SMCWLT) / SMCAV)
    DD = DMAX[1]
    for (KS in 2:NSOIL) {
      DMAX[KS] = (ZSOIL[KS - 1] - ZSOIL[KS]) * SMCAV
      DMAX[KS] = DMAX[KS] * (1 - (SMC_A[KS] - SMCWLT) / SMCAV)
      DD = DD + DMAX[KS]
    }
    
    VAL = (1 - exp(-KDT * DT1))
    DDT = DD * VAL
    PX  = RAIN1 * DT  
    if (PX < 0) PX = 0
    INFMAX = (PX * (DDT / (PX + DDT))) / DT
    
    tmp <- WDFCND(MXSMC)
    WDF  <- tmp$WDF
    WCND <- tmp$WCND
    
    INFMAX = max(INFMAX, WCND)
    INFMAX = min(INFMAX, PX)
    
    if (RAIN1 > INFMAX) PDDUM = INFMAX
  }
  
  tmp <- WDFCND(MXSMC)
  WDF  <- tmp$WDF
  WCND <- tmp$WCND
  
  # CALC THE MATRIX COEFFICIENTS AI, BI, AND CI FOR THE TOP LAYER
  DDZ   = 1 / (-0.5 * ZSOIL[2])
  AI[1] = 0
  BI[1] = WDF * DDZ / (-ZSOIL[1])
  CI[1] = -BI[1]
  
  # CALC RHSTT FOR THE TOP LAYER AFTER CALC'NG THE VERTICAL SOIL MOISTURE
  # GRADIENT BTWN THE TOP AND NEXT TO TOP LAYERS.
  DSMDZ    = (SMC[1] - SMC[2]) / (-0.5 * ZSOIL[2])
  RHSTT[1] = (WDF * DSMDZ + WCND - PDDUM + EDIR + ET[1]) / ZSOIL[1]
  
  # INITIALIZE DDZ2
  DDZ2  = 0
  
  # LOOP THRU THE REMAINING SOIL LAYERS, REPEATING THE ABV PROCESS
  WDF2 <- WCND2 <- 0
  for (K in 2:NSOIL) {
    DENOM2 = (ZSOIL[K - 1] - ZSOIL[K])
    
    # TO AVOID SPURIOUS DRAINAGE BEHAVIOR, 'UPSTREAM DIFFERENCING'
    # IN LINE BELOW REPLACED WITH NEW APPROACH IN 2ND LINE:
    # 'MXSMC2 = MAX (SH2OA(K), SH2OA(K+1))'
    MXSMC2 = SMC_A[K]
    tmp <- WDFCND(MXSMC2)
    WDF2  <- tmp$WDF
    WCND2 <- tmp$WCND
    
    if (K != NSOIL) {
      # CALC SOME PARTIAL PRODUCTS FOR LATER USE IN CALC'NG RHSTT
      DENOM = (ZSOIL[K - 1] - ZSOIL[K + 1])
      DSMDZ2 = (SMC[K] - SMC[K + 1]) / (DENOM * 0.5)
      
      # CALC THE MATRIX COEF, CI, AFTER CALC'NG ITS PARTIAL PRODUCT
      DDZ2 = 2 / DENOM
      CI[K] = -WDF2 * DDZ2 / DENOM2
    }else{
      # SET PARTIAL PRODUCT TO ZERO
      DSMDZ2 = 0
      
      # SET MATRIX COEF CI TO ZERO
      CI[K] = 0
    }
    
    # CALC RHSTT FOR THIS LAYER AFTER CALC'NG ITS NUMERATOR
    NUMER    = (WDF2 * DSMDZ2) + SLOPE * WCND2 - (WDF * DSMDZ) - WCND + ET[K]
    RHSTT[K] = NUMER / (-DENOM2)
    
    # CALC MATRIX COEFS, AI, AND BI FOR THIS LAYER
    AI[K] = -WDF * DDZ / DENOM2
    BI[K] = -(AI[K] + CI[K])
    
    # RESET VALUES OF WDF, WCND, DSMDZ, AND DDZ FOR LOOP TO NEXT LYR
    if (K != NSOIL) {
      WDF   = WDF2
      WCND  = WCND2
      DSMDZ = DSMDZ2
      DDZ   = DDZ2
    }
  }
  
  # return
  check.vars(environment())
  flush.vars(environment())
}
#---------------------#


#---------------------#
# WDFCND
#---------------------#
# CALCULATE SOIL WATER DIFFUSIVITY AND SOIL HYDRAULIC CONDUCTIVITY
#---------------------#
WDFCND <- function(smc) {
  # CALC THE RATIO OF THE ACTUAL TO THE MAX PSBL SOIL H2O CONTENT
  FACTR = smc / SMCMAX
  
  # PREP AN EXPNTL COEF AND CALC THE SOIL WATER DIFFUSIVITY
  EXPON = BB + 2
  WDF   = DWSAT * FACTR^EXPON
  
  # RESET THE EXPNTL COEF AND CALC THE HYDRAULIC CONDUCTIVITY
  EXPON = (2 * BB) + 3.0
  WCND  = DKSAT * FACTR^EXPON
  list(WDF = WDF, WCND = WCND)
}
#---------------------#


#---------------------#
# SSTEP
#---------------------#
# CALCULATE SOIL WATER DIFFUSIVITY AND SOIL HYDRAULIC CONDUCTIVITY
#---------------------#
SSTEP <- function(SMC_OUT, SMC_IN) {
  # CREATE 'AMOUNT' VALUES OF VARIABLES TO BE INPUT TO THE
  # TRI-DIAGONAL MATRIX ROUTINE.
  RHSTT = RHSTT * DT
  AI    = AI * DT
  BI    = 1 + (BI * DT)
  CI    = CI * DT
  
  # COPY VALUES FOR INPUT VARIABLES BEFORE CALL TO ROSR12
  RHSTTin = RHSTT
  CIin = CI
  
  # CALL ROSR12 TO SOLVE THE TRI-DIAGONAL MATRIX
  CI <- ROSR12(CI, AI, BI, CIin, RHSTTin, RHSTT)

  # SUM THE PREVIOUS SMC VALUE AND THE MATRIX SOLUTION TO GET A NEW VALUE
  # MIN ALLOWABLE VALUE OF SMC WILL BE 0.02
  WPLUS = 0
  DDZ   = -ZSOIL[1]
  
  for (K in 1:NSOIL) {
    if (K != 1) DDZ = ZSOIL[K - 1] - ZSOIL[K]
    
    # MUSSEL MODEL v1.8 - ALLOW FOR NON-ABSORBANT BEDROCK AND HORIZ RUNOFF
    SMC_IN[K]  = SMC_IN[K] * 0.5
    SMC_OUT[K] = SMC_IN[K] + CI[K] + WPLUS / DDZ
  
    STOT = SMC_OUT[K]
    if (STOT > SMCMAX) {
      WPLUS = (STOT - SMCMAX) * DDZ
    }else{
      WPLUS = 0
    }
  
    if (K > BED_DEPTH) {
      SMC[K] = 0
    }else{
      if (tide) {
        SMC[K] = SMCMAX
      }else{ 
        SMC[K] = max(min(STOT, SMCMAX), 0)
      }
    }
    
    SMC_OUT[K] = max(SMC[K], 0)
  }
  
  # return
  check.vars(environment())
  flush.vars(environment())
  SMC_OUT
}
#---------------------#


