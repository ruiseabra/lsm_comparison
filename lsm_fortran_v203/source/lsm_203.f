C ----------------------------------------------------------------------
C          Version 2.0.3 -- AUG 2018 (with snow)
C
C          !!!!    this version CAN model snow    !!!!
C          !!!!    thus it CAN be used even if    !!!!
C          !!!!!   temperature drops below ZERO  !!!!!
C
C         check lsm_202_info.txt for the version history
C         in version 203 every constant and parameter with
C            inconsistent values throughout the script
C            was made consistent and in agreement with the R version
C ----------------------------------------------------------------------

      PROGRAM  MAIN
      IMPLICIT NONE

C ---------------------------------------------------------------------
C VARIABLES MODIFIED USING R CODE - START
C ---------------------------------------------------------------------

C     Number of soil layers (2-20)
      INTEGER NSOIL

C     Freeze temperature (K)
      REAL FRZ_T
      COMMON /GLOBAL1/NSOIL
      COMMON /GLOBAL2/FRZ_T
      DATA NSOIL /XNSOILX/
      DATA FRZ_T /273.15/

C     Total # of simulation time steps
      INTEGER NRUN
      PARAMETER(NRUN = XNRUNX)

C     Time step for integration in sec (<= 3600)
      REAL DT
      PARAMETER(DT = XDTX)

C     Soil layer to be written to the output
      INTEGER NLAYER
      PARAMETER(NLAYER = XNLAYERX)

C     Height (above ground) of the forcing wind vector (m)
      REAL ZLVL
      PARAMETER(ZLVL = XZLVLX)

C     Filename of atmospheric data used for input forcing
      CHARACTER*100 FORCING_FILE
      PARAMETER(FORCING_FILE = "XFORCING_FILEX")

C     Thickness of each soil layers (m)
      REAL SLDPTH(XNSOILX)
      DATA SLDPTH /XSLDPTHX/

C     Roughness length (m)
      REAL ROUGHNESS
      PARAMETER(ROUGHNESS = XROUGHNESSX)

C     ALBEDO (snow free albedo)
      REAL ALB
      PARAMETER(ALB = XALBEDOX)

C     Initial temperature at layers
      REAL STC(XNSOILX)
      DATA STC /XSTCX/

C     Initial total moisture at layers
      REAL SMC(XNSOILX)
      DATA SMC /XSMCX/

C     Initial liquid moisture at layers
      REAL SH2O(XNSOILX)
      DATA SH2O /XSH2OX/

C     Animal body difusivity
      REAL BODY_DIFUSIVITY
      COMMON /BODY/BODY_DIFUSIVITY
      DATA BODY_DIFUSIVITY /XBODY_DIFUSIVITYX/

C     Annual constant bottom boundary soil temperature (K)
      REAL ANNUAL_BOTTOM_TEMP
      COMMON /SURFACE/ANNUAL_BOTTOM_TEMP
      DATA ANNUAL_BOTTOM_TEMP /XANNUAL_BOTTOM_TEMPX/

C ---------------------------------------------------------------------
C VARIABLES ADDED FOR MUSSEL MODEL
C ---------------------------------------------------------------------
      INTEGER BEDDEPTH
      REAL CONTACT
      REAL SST
      INTEGER TIDEFLAG
      REAL EMISSIVITY
      COMMON /TIDE/TIDEFLAG,SST
      COMMON /GEOMETRY/BEDDEPTH,CONTACT
      COMMON /EMISSIVITY/EMISSIVITY

      DATA BEDDEPTH   /XBEDDEPTHX/
      DATA CONTACT    /XCONTACTX/
      DATA EMISSIVITY /XEMISSIVITYX/

C ---------------------------------------------------------------------
C VARIABLES MODIFIED USING R CODE - END
C ---------------------------------------------------------------------

      INTEGER READ_FORCING
      INTEGER IJ
      INTEGER INDI

      REAL LWAVE
      REAL SWAVE
      REAL RAIN
      REAL WIND_SPEED
      REAL SFC_PRESS
      REAL SFC_TEMP

      REAL BETA
      REAL EDIR
      REAL ET(20)
      REAL ETNS
      REAL ESNOW
      REAL F
      REAL FLX1
      REAL FLX2
      REAL FLX3
      REAL DEW
      REAL Q1
      REAL ALBEDO
      REAL CH
      REAL CM
      REAL DQSDT
      REAL DQSDT2
      REAL ESAT
      REAL ETA
      REAL ETP
      REAL FUP

C sensible heat flux (W/m2)
      REAL H
C latent energy flux (W/m2)
      REAL LE

      REAL SMCWLT
      REAL SMCMAX
      REAL Q2
      REAL Q2SAT
      REAL RH
      REAL RNET
      REAL SNOMLT

      REAL S
C     Initial skin temperature (K)
      REAL TSKIN
      REAL T14
      REAL T1V
      REAL T2V
      REAL TH2
      REAL TH2V

      REAL SALP

C latent heat of vaporization of water at 0 C [ J/kg ]
      REAL LVH2O
      REAL SNCOVR
      REAL SNUP
      REAL SNUPX

      REAL SOLNET
C     Max albedo over very deep snow
      REAL SNOALB
C     Initial water equiv snow depth (m)
      REAL SNEQV
C     Initial actual snow depth (m)
      REAL SNOWH

C REFLECTED SOLAR
      REAL SOLUP
      REAL SNDENS
      REAL LSUBS
      PARAMETER(LVH2O = 2.501E+6)
      DATA SNUPX  /0.04/
      DATA SALP   /4.0/

C INITIALIZE SOME VARIABLES BEFORE THE TIME LOOP
      SFC_TEMP   = 0.
      RH         = 0.
      SFC_PRESS  = 0.
      SWAVE      = 0.
      LWAVE      = 0.
      RAIN       = 0.
      WIND_SPEED = 0.

      CH         = 1.E-4
      CM         = 1.E-4

      SNOALB     = 0.75
      SNEQV      = 0
      TSKIN      = 273.15
      SNOWH      = 0

C #############################################################

C -------------------------------------------------------------
C DRIVER STEP 1
C OPEN INPUT FILE WITH WEATHER DATA
      READ_FORCING = 29
      OPEN(READ_FORCING, FILE=FORCING_FILE, STATUS='OLD')
C -------------------------------------------------------------
C DRIVER STEP 2
C START THE TIME LOOP
      DO INDI = 1,NRUN
C -------------------------------------------------------------
C DRIVER STEP 3
C READ FORCING DATA
      CALL READ_FORCING_INPUT(READ_FORCING,SFC_TEMP,RH,
     & SFC_PRESS,SWAVE,LWAVE,RAIN,WIND_SPEED)
C -------------------------------------------------------------
C DRIVER STEP 4
C CALCULATE A SATURATION MIX RATIO (Q2SAT)
C NEED Q2 (FROM REL.HUMID.) USE SUBROUTINE QDATAP
      CALL QDATAP (SFC_TEMP,SFC_PRESS,RH,Q2,Q2SAT,ESAT)
      IF (Q2 .LT. 0.1E-5) Q2 = 0.1E-5
      IF (Q2 .GE.  Q2SAT) Q2 = Q2SAT*0.99
C CALCULATE SLOPE OF SAT SPECIFIC HUMIDITY CURVE FOR PENMAN: DQSDT2
      DQSDT2 = DQSDT (SFC_TEMP, SFC_PRESS)
C  CALC VIRTUAL TEMPS AND POTENTIAL TEMPS AT GRND (SUB 1) AND AT
C  THE 1ST MDL LVL ABV THE GRND (SUB 2). EXPON IS CP DIVD BY R
      TH2  = SFC_TEMP + (0.0098 * ZLVL)
      T2V  = SFC_TEMP * (1.0 + 0.61 * Q2)
      T1V  = TSKIN    * (1.0 + 0.61 * Q2)
      TH2V = TH2      * (1.0 + 0.61 * Q2)
C -------------------------------------------------------------
C DRIVER STEP 5
C DETERMINE NET INCOMING SOLAR
      ALBEDO = ALB
      SOLNET = SWAVE * (1.0 - ALBEDO)
C -------------------------------------------------------------
C DRIVER STEP 6
C CALL LAND-SURFACE PHYSICS
      CALL SFLX (
     C  DT,ZLVL,SLDPTH,
     F  LWAVE,SWAVE,SOLNET,SFC_PRESS,RAIN,SFC_TEMP,Q2,WIND_SPEED,
     I  TH2,Q2SAT,DQSDT2,
     S  ALB,SNOALB,ROUGHNESS,
     H  TSKIN,STC,SMC,SH2O,SNOWH,SNEQV,ALBEDO,CH,CM,
     O  ETA,EDIR,ET,ESNOW,DEW,
     O  BETA,ETP,S,FLX1,FLX2,FLX3,
     O  SNOMLT,SNCOVR,SNUPX,SALP,
     D  SMCWLT,SMCMAX)
C CALCULATE UPWARD LONGWAVE RAD USING UPDATED SKIN TEMPERATURE
      T14 = TSKIN * TSKIN * TSKIN * TSKIN
      FUP = 5.67E-8 * T14 * EMISSIVITY
C  CALCULATE RESIDUAL OF ALL SURFACE ENERGY BALANCE EQN TERMS
      S      = -S
      F      = SWAVE * (1.0 - ALBEDO) + LWAVE
      SNDENS = 0.0
      SOLUP  = SWAVE * ALBEDO
      ETNS   = EDIR
C -------------------------------------------------------------
C DRIVER STEP 7
C PRINT OUTPUT TO CONSOLE (TEMPERATURE AT LAYER N)
      PRINT 200, STC(NLAYER)

 200  FORMAT(F6.2)

C -------------------------------------------------------------
      END DO
C END OF TIME LOOP
C CLOSE FORCING FILE
      CLOSE(READ_FORCING)

      STOP 0
C END OF DRIVER PROGRAM
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      FUNCTION DQS (T)
      IMPLICIT NONE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC  CALCULATE VALUES OF VAPOR PRESSURE (E)
CC  AND P * DQS/DT (P TIMES CHG IN SAT MXG RATIO WITH RESPECT
CC  TO THE CHG IN TEMP) IN SUBSTITUTION TO THE LOOK-UP TABLES.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      REAL DESDT
      REAL DQS
      REAL LW
      REAL T
      REAL ES
      REAL CPV
      REAL RV
      REAL CW
      REAL EPS
      REAL ESO
      REAL LVH2O

      PARAMETER (LVH2O = 2.501000E+6)
      PARAMETER (CPV   = 1870.)
      PARAMETER (RV    = 461.5)
      PARAMETER (CW    = 4187.)
      PARAMETER (EPS   = 0.622)
      PARAMETER (ESO   = 611.2)

      REAL FRZ_T
      COMMON /GLOBAL2/FRZ_T
C
C     ABOUT THE PARAMETERS:
C
C     EPS - WATER / DRY AIR MOLECULAR MASS RATIO, EPSILON
C
C   VALUES FOR SPECIFIC HEAT CAPACITY AND INDIVIDUAL GAS CONSTANTS
C   IN [JOULES/(KG*KELVIN)] UNITS.
C
C     DRY AIR:
C             CP, CV
C     WATER VAPOR:
C                 CVV = 1410.
C                 CPV = 1870.
C                 RV  =  461.5
C     LIQUID WATER:
C                  CW = 4187.
C
C     ESO = ES(T=273.15 K) = SAT. VAPOR PRESSURE (IN PASCAL) AT T=FRZ_T
C
C     SAT. MIXING  RATIO: QS ~= EPS*ES/P
C     CLAUSIUS-CLAPEYRON: DES/DT = L*ES/(RV*T^2)
C     @QS/@T =  (EPS/P)*DES/DT
          LW    = LVH2O - (CW - CPV) * (T - FRZ_T)
          ES    = ESO * EXP(LW * (1 / FRZ_T - 1 / T) / RV)
          DESDT = LW  * ES / (RV * T * T)
          DQS   = EPS * DESDT

          RETURN
          END


      FUNCTION DQSDT (SFC_TEMP, SFC_PRESS)
      IMPLICIT NONE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC    RETRIEVE THE APPROPRIATE VALUE OF DQSDT (THE CHANGE
CC    OF THE SATURATION MIXING RATIO WITH RESPECT TO THE
CC    CHANGE IN TEMPERATURE)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      REAL SFC_TEMP
      REAL SFC_PRESS
      REAL DQS
      REAL DQSDT
C IF THE INPUT SFC AIR TEMP IS BTWN 173 K AND 373 K, USE
C FUNCTION DQS TO DETERMINE THE SLOPE OF SAT.MIX RATIO FUNCTION
      DQSDT = DQS (SFC_TEMP) / SFC_PRESS

      RETURN
      END


      FUNCTION E (T)
      IMPLICIT NONE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC  CALCULATE VALUES OF SAT. VAPOR PRESSURE (E)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      REAL LW
      REAL T
      REAL E
      REAL CPV
      REAL RV
      REAL CW
      REAL ESO
      REAL LVH2O

      PARAMETER (LVH2O = 2.501000E+6)
      PARAMETER (CPV   = 1870.)
      PARAMETER (RV    = 461.5)
      PARAMETER (CW    = 4187.)
      PARAMETER (ESO   = 611.2)

      REAL FRZ_T
      COMMON /GLOBAL2/FRZ_T

      LW = LVH2O - (CW - CPV) * (T - FRZ_T)
      E  = ESO * EXP(LW * (1 / FRZ_T - 1 / T) / RV)

      RETURN
      END


      SUBROUTINE QDATAP (SFC_TEMP,SFC_PRESS,RH,Q2,Q2SAT,ESAT)
      IMPLICIT NONE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC  OBTAIN SPECIFIC HUMIDITY (q) FROM RELATIVE HUMIDITY
CC   AND GIVEN PRESSURE AND TEMPERATURE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C----------------------------------------
C In:
C        SFC_TEMP    Temperature (K)
C        SFC_PRESS   Pressure (Pa)
C        RH          Relative humidity (%)
C-----------------------------------------
C Out:
C        Q2      Specific humidity (Kg/Kg)
C        Q2SAT   Saturation Specific humidity (Kg/Kg)
C        ESAT    Saturation vapor pressure for water (Pa)
C----------------------------------------
      REAL SFC_TEMP
      REAL SFC_PRESS
      REAL RH
      REAL RHF
      REAL Q2
      REAL Q2SAT
      REAL ESAT
      REAL EP
      REAL E

C     (Water)/(dry air) molecular mass ratio, epsilon
      REAL EPS
      PARAMETER (EPS = 0.622)
C    function E(T) = Sat. vapor pressure (in Pascal) at
C                    temperature T (uses Clausius-Clapeyron).
      ESAT = E(SFC_TEMP)
C     CONVERT REL. HUMIDITY (%) TO THE FRACTIONAL VALUE
      RHF = RH / 100.
C     CALCULATE SATURATION MIXING RATIO
      Q2SAT = EPS * ESAT / (SFC_PRESS - (1. - EPS) * ESAT)
C     CONVERSION FROM REL. HUMIDITY
      EP = (SFC_PRESS * ESAT * RHF) / (SFC_PRESS - ESAT * (1. - RHF))
      Q2 = EPS * EP / (SFC_PRESS - (1. - EPS) * EP)

      RETURN
      END


      SUBROUTINE READ_FORCING_INPUT(READ_FORCING,SFC_TEMP,RH,
     & SFC_PRESS,SWAVE,LWAVE,RAIN,WIND_SPEED)
      IMPLICIT NONE
C---------------------------------------------------------
C FORCING FILE LAYOUT:
C
C    WIND_SPEED wind speed (m/s)
C    SFC_TEMP   air temperature (K)
C    RH         relative humidity (%)
C    P          surface pressure in hPa
C    SWAVE      incoming short wave radiation (W/m2)
C    LWAVE      incoming long wave radiation (W/m2)
C    RAIN       precipitation rate (mm/h, kg/m2/h)
C    SST        sea surface temperature (K)
C    TIDEFLAG   1=underwater, 0=out-of-water
C---------------------------------------------------------
      INTEGER READ_FORCING

      REAL SFC_TEMP
      REAL RH
      REAL SFC_PRESS
      REAL SWAVE
      REAL LWAVE
      REAL RAIN
      REAL WIND_SPEED

      REAL SST
      INTEGER TIDEFLAG
      COMMON /TIDE/ TIDEFLAG,SST

      READ (READ_FORCING,*) WIND_SPEED,SFC_TEMP,SST,
     &   SWAVE,LWAVE,RH,SFC_PRESS,RAIN,TIDEFLAG

C    CONVERT hPascal to Pascal
      SFC_PRESS = SFC_PRESS * 100
C    CONVERT mm/h to mm/sec
      RAIN = RAIN / 3600

      RETURN
      END


      SUBROUTINE SFLX (
     C  DT,ZLVL,SLDPTH,
     F  LWAVE,SWAVE,SOLNET,SFC_PRESS,RAIN,SFC_TEMP,Q2,WIND_SPEED,
     I  TH2,Q2SAT,DQSDT2,
     S  ALB,SNOALB,ROUGHNESS,
     H  TSKIN,STC,SMC,SH2O,SNOWH,SNEQV,ALBEDO,CH,CM,
     O  ETA,
C ----------------------------------------------------------------------
C OUTPUTS, DIAGNOSTICS, PARAMETERS BELOW GENERALLY NOT NECESSARY WHEN
C COUPLED WITH E.G. A NWP MODEL (SUCH AS THE NOAA/NWS/NCEP MESOSCALE ETA
C MODEL).  OTHER APPLICATIONS MAY REQUIRE DIFFERENT OUTPUT VARIABLES.
C ----------------------------------------------------------------------
     O  EDIR,ET,ESNOW,DEW,
     O  BETA,ETP,SSOIL,
     O  FLX1,FLX2,FLX3,
     O  SNOMLT,SNCOVR,SNUPX,SALP,
     P  SMCWLT,SMCMAX)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C SUB-DRIVER FOR "NOAH/OSU LSM" FAMILY OF PHYSICS SUBROUTINES FOR A
C SOIL/VEG/SNOWPACK LAND-SURFACE MODEL TO UPDATE SOIL MOISTURE, SOIL
C ICE, SOIL TEMPERATURE, SKIN TEMPERATURE, SNOWPACK WATER CONTENT,
C SNOWDEPTH, AND ALL TERMS OF THE SURFACE ENERGY BALANCE AND SURFACE
C WATER BALANCE (EXCLUDING INPUT ATMOSPHERIC FORCINGS OF DOWNWARD
C RADIATION AND PRECIP)
C ----------------------------------------------------------------------
C SFLX ARGUMENT LIST KEY:
C ----------------------------------------------------------------------
C  C  CONFIGURATION INFORMATION
C  F  FORCING DATA
C  I  OTHER (INPUT) FORCING DATA
C  S  SURFACE CHARACTERISTICS
C  H  HISTORY (STATE) VARIABLES
C  O  OUTPUT VARIABLES
C  D  DIAGNOSTIC OUTPUT
C ----------------------------------------------------------------------
C 1. CONFIGURATION INFORMATION (C):
C ----------------------------------------------------------------------
C   DT	       TIMESTEP (SEC) (DT SHOULD NOT EXCEED 3600 SECS, RECOMMEND
C                1800 SECS OR LESS)
C   ZLVL       HEIGHT (M) ABOVE GROUND OF ATMOSPHERIC FORCING VARIABLES
C   NSOIL      NUMBER OF SOIL LAYERS (AT LEAST 2)
C   SLDPTH     THE THICKNESS OF EACH SOIL LAYER (M)
C ----------------------------------------------------------------------
C 2. FORCING DATA (F):
C ----------------------------------------------------------------------
C   LWAVE      LW DOWNWARD RADIATION (W M-2; POSITIVE, NOT NET LONGWAVE)
C   SWAVE      SOLAR DOWNWARD RADIATION (W M-2; POSITIVE, NOT NET SOLAR)
C   SFC_PRESS  PRESSURE AT HEIGHT ZLVL ABOVE GROUND (PASCALS)
C   RAIN       PRECIP RATE (KG M-2 S-1) (NOTE, THIS IS A RATE)
C   SFC_TEMP   AIR TEMPERATURE (K) AT HEIGHT ZLVL ABOVE GROUND
C   TH2        AIR POTENTIAL TEMPERATURE (K) AT HEIGHT ZLVL ABOVE GROUND
C   Q2         MIXING RATIO AT HEIGHT ZLVL ABOVE GROUND (KG KG-1)
C ----------------------------------------------------------------------
C 3. OTHER FORCING (INPUT) DATA (I):
C ----------------------------------------------------------------------
C   WIND_SPEED     WIND SPEED (M S-1) AT HEIGHT ZLVL ABOVE GROUND
C   Q2SAT      SAT MIXING RATIO AT HEIGHT ZLVL ABOVE GROUND (KG KG-1)
C   DQSDT2     SLOPE OF SAT SPECIFIC HUMIDITY CURVE AT T=SFC_TEMP
C                (KG KG-1 K-1)
C ----------------------------------------------------------------------
C 4. CANOPY/SOIL CHARACTERISTICS (S):
C ----------------------------------------------------------------------
C   ALB        BACKROUND SNOW-FREE SURFACE ALBEDO (FRACTION), FOR JULIAN
C                DAY OF YEAR (USUALLY FROM TEMPORAL INTERPOLATION OF
C                MONTHLY MEAN VALUES' CALLING PROG MAY OR MAY NOT
C                INCLUDE DIURNAL SUN ANGLE EFFECT)
C   SNOALB     UPPER BOUND ON MAXIMUM ALBEDO OVER DEEP SNOW (E.G. FROM
C                ROBINSON AND KUKLA, 1985, J. CLIM. & APPL. METEOR.)
C   TBOT       BOTTOM SOIL TEMPERATURE (LOCAL YEARLY-MEAN SFC AIR
C                TEMPERATURE)
C ----------------------------------------------------------------------
C 5. HISTORY (STATE) VARIABLES (H):
C ----------------------------------------------------------------------
C  TSKIN       GROUND/CANOPY/SNOWPACK) EFFECTIVE SKIN TEMPERATURE (K)
C  STC(NSOIL)  SOIL TEMP (K)
C  SMC(NSOIL)  TOTAL SOIL MOISTURE CONTENT (VOLUMETRIC FRACTION)
C  SH2O(NSOIL) UNFROZEN SOIL MOISTURE CONTENT (VOLUMETRIC FRACTION)
C                NOTE: FROZEN SOIL MOISTURE = SMC - SH2O
C  SNOWH       ACTUAL SNOW DEPTH (M)
C  SNEQV       LIQUID WATER-EQUIVALENT SNOW DEPTH (M)
C                NOTE: SNOW DENSITY = SNEQV/SNOWH
C  ALBEDO      SURFACE ALBEDO INCLUDING SNOW EFFECT (UNITLESS FRACTION)
C                =SNOW-FREE ALBEDO (ALB) WHEN SNEQV=0, OR
C                =FCT(MSNOALB,ALB) WHEN SNEQV>0
C  CH          SURFACE EXCHANGE COEFFICIENT FOR HEAT AND MOISTURE
C                (M S-1); NOTE: CH IS TECHNICALLY A CONDUCTANCE SINCE
C                IT HAS BEEN MULTIPLIED BY WIND SPEED.
C  CM          SURFACE EXCHANGE COEFFICIENT FOR MOMENTUM (M S-1); NOTE:
C                CM IS TECHNICALLY A CONDUCTANCE SINCE IT HAS BEEN
C                MULTIPLIED BY WIND SPEED.  CM IS NOT NEEDED IN SFLX
C ----------------------------------------------------------------------
C 6. OUTPUT (O):
C ----------------------------------------------------------------------
C OUTPUT VARIABLES NECESSARY FOR A COUPLED NUMERICAL WEATHER PREDICTION
C MODEL, E.G. NOAA/NWS/NCEP MESOSCALE ETA MODEL.  FOR THIS APPLICATION,
C THE REMAINING OUTPUT/DIAGNOSTIC/PARAMETER BLOCKS BELOW ARE NOT
C NECESSARY.  OTHER APPLICATIONS MAY REQUIRE DIFFERENT OUTPUT VARIABLES.

C   ETA        ACTUAL LATENT HEAT FLUX (W M-2: NEGATIVE, IF UP FROM
C	         SURFACE)
C ----------------------------------------------------------------------
C   EDIR       DIRECT SOIL EVAPORATION (W M-2)
C   ET(NSOIL)  PLANT TRANSPIRATION FROM A PARTICULAR ROOT (SOIL) LAYER
C                 (W M-2)
C   ESNOW      SUBLIMATION FROM SNOWPACK (W M-2)
C   DEW        DEWFALL (OR FROSTFALL FOR T<273.15) (M)
C ----------------------------------------------------------------------
C   BETA       RATIO OF ACTUAL/POTENTIAL EVAP (DIMENSIONLESS)
C   ETP        POTENTIAL EVAPORATION (W M-2)
C   SSOIL      SOIL HEAT FLUX (W M-2: NEGATIVE IF DOWNWARD FROM SURFACE)
C ----------------------------------------------------------------------
C   FLX1       PRECIP-SNOW SFC (W M-2)
C   FLX2       FREEZING RAIN LATENT HEAT FLUX (W M-2)
C   FLX3       PHASE-CHANGE HEAT FLUX FROM SNOWMELT (W M-2)
C ----------------------------------------------------------------------
C   SNOMLT     SNOW MELT (M) (WATER EQUIVALENT)
C   SNCOVR     FRACTIONAL SNOW COVER (UNITLESS FRACTION, 0-1)
C ----------------------------------------------------------------------
C 8. PARAMETERS (P):
C ----------------------------------------------------------------------
C   SMCWLT     WILTING POINT, DRY SOIL MOISTURE THRESHOLD WHERE DIRECT
c                EVAP FRM TOP LAYER ENDS (VOLUMETRIC)
C   SMCMAX     POROSITY, I.E. SATURATED VALUE OF SOIL MOISTURE
C                (VOLUMETRIC)
C ----------------------------------------------------------------------

C ----------------------------------------------------------------------
C DECLARATIONS - LOGICAL
C ----------------------------------------------------------------------
      LOGICAL FRZGRA
      LOGICAL SNOWNG
C ----------------------------------------------------------------------
C DECLARATIONS - INTEGER
C ----------------------------------------------------------------------
      INTEGER K
      INTEGER KZ
      INTEGER NSOIL
      COMMON /GLOBAL1/NSOIL
C ----------------------------------------------------------------------
C DECLARATIONS - REAL
C ----------------------------------------------------------------------
      REAL ALBEDO
      REAL ALB
      REAL BETA
      REAL CH
      REAL CM
      REAL CP
      REAL CSNOW
      REAL CZIL
      REAL DEW
      REAL DF1
      REAL DF1H
      REAL DF1A
      REAL DKSAT
      REAL DT
      REAL DWSAT
      REAL DQSDT2
      REAL DSOIL
      REAL DTOT
      REAL EDIR
      REAL ESNOW
      REAL ET(NSOIL)
      REAL FRCSNO
      REAL FRCSOI
      REAL EPSCA
      REAL ETA
      REAL ETP
      REAL FDOWN
      REAL F1
      REAL FLX1
      REAL FLX2
      REAL FLX3
      REAL FXEXP
      REAL FRZX
      REAL KDT
      REAL LWAVE
      REAL LVH2O
      REAL RAIN
      REAL RAIN1
      REAL Q2
      REAL Q2SAT
      REAL R
      REAL RCH
      REAL REFKDT
      REAL RR
      REAL RC
      REAL RSNOW
      REAL SNDENS
      REAL SNCOND
      REAL SSOIL
      REAL SFC_PRESS
      REAL WIND_SPEED
      REAL SFC_TEMP
      REAL SH2O(NSOIL)
      REAL SLDPTH(NSOIL)
      REAL SMCMAX
      REAL SMCWLT
      REAL SMC(NSOIL)
      REAL SNEQV
      REAL SNCOVR
      REAL SNOWH
      REAL SN_NEW
      REAL SLOPE
      REAL SNUP
      REAL SNUPX
      REAL SALP
      REAL SNOALB
      REAL STC(NSOIL)
      REAL SNOMLT
      REAL SWAVE
      REAL TSKIN
      REAL T1V
      REAL T24
      REAL T2V
      REAL TH2
      REAL TH2V
      REAL FRZ_T
      REAL TSNOW
      REAL ZLVL
      REAL ZBOT
      REAL Z0
C DEPTH (NEGATIVE) BELOW GROUND
      REAL ZSOIL(NSOIL)

      REAL SOLNET
      REAL LSUBS

      REAL BODY_DIFUSIVITY
      COMMON /BODY/BODY_DIFUSIVITY

      INTEGER BEDDEPTH
      REAL CONTACT
      REAL ROUGHNESS
      COMMON /GEOMETRY/BEDDEPTH,CONTACT

C ----------------------------------------------------------------------
C DECLARATIONS - PARAMETERS
C ----------------------------------------------------------------------
      PARAMETER(FRZ_T = 273.15)
      PARAMETER(LVH2O = 2.501E+6)
      PARAMETER(LSUBS = 2.83E+6)
      PARAMETER(R     = 287.04)
      PARAMETER(CP    = 1004.5)
C ---------------------------------------------------------------------
C   INITIALIZATION
C ----------------------------------------------------------------------
      SNOMLT  = 0.0
C ----------------------------------------------------------------------
C CALCULATE DEPTH (NEGATIVE) BELOW GROUND FROM TOP SKIN SFC TO BOTTOM OF
C   EACH SOIL LAYER.  NOTE:  SIGN OF ZSOIL IS NEGATIVE (DENOTING BELOW
C   GROUND)
C ----------------------------------------------------------------------
        ZSOIL(1) = -SLDPTH(1)
        DO KZ = 2,NSOIL
          ZSOIL(KZ) = -SLDPTH(KZ)+ZSOIL(KZ-1)
        END DO
C ----------------------------------------------------------------------
C NEXT IS CRUCIAL CALL TO SET THE LAND-SURFACE PARAMETERS, INCLUDING
C SOIL-TYPE AND VEG-TYPE DEPENDENT PARAMETERS.
C ----------------------------------------------------------------------
      CALL REDPRM (
     &      	   REFKDT,KDT,ZBOT,FRZX,SLOPE,SNUPX,
     &      	   SNUP,SALP,DKSAT,DWSAT,SMCMAX,SMCWLT,
     &      	   F1,FXEXP,SLDPTH,ZSOIL,Z0,CZIL,ROUGHNESS)
C ----------------------------------------------------------------------
C  INITIALIZE PRECIPITATION LOGICALS.
C ----------------------------------------------------------------------
      SNOWNG = .FALSE.
      FRZGRA = .FALSE.
C ----------------------------------------------------------------------
C IF INPUT SNOWPACK IS NONZERO, THEN COMPUTE SNOW DENSITY "SNDENS" AND
C   SNOW THERMAL CONDUCTIVITY "SNCOND" (NOTE THAT CSNOW IS A FUNCTION
C   SUBROUTINE)
C ----------------------------------------------------------------------
      IF (SNEQV .EQ. 0.0) THEN
        SNDENS = 0.0
        SNOWH  = 0.0
        SNCOND = 1.0
      ELSE
        SNDENS = SNEQV / SNOWH
        SNCOND = CSNOW(SNDENS)
      ENDIF
C ----------------------------------------------------------------------
C DETERMINE IF IT'S PRECIPITATING AND WHAT KIND OF PRECIP IT IS.
C IF IT'S RAINING AND THE AIR TEMP IS COLDER THAN 0 C, IT'S SNOWING!
C IF IT'S RAINING AND THE AIR TEMP IS WARMER THAN 0 C, BUT THE GRND
C TEMP IS COLDER THAN 0 C, FREEZING RAIN IS PRESUMED TO BE FALLING.
C ----------------------------------------------------------------------
      IF (RAIN .GT. 0.0) THEN
        IF (SFC_TEMP .LE. FRZ_T) THEN
          SNOWNG = .TRUE.
        ELSE
          IF (TSKIN .LE. FRZ_T) FRZGRA = .TRUE.
        ENDIF
      ENDIF
C ----------------------------------------------------------------------
C IF EITHER RAIN FLAG IS SET, DETERMINE NEW SNOWFALL (CONVERTING RAIN
C RATE FROM KG M-2 S-1 TO A LIQUID EQUIV SNOW DEPTH IN METERS) AND ADD
C IT TO THE EXISTING SNOWPACK.
C NOTE THAT SINCE ALL PRECIP IS ADDED TO SNOWPACK, NO PRECIP INFILTRATES
C INTO THE SOIL SO THAT RAIN1 IS SET TO ZERO.
C ----------------------------------------------------------------------
      IF ((SNOWNG) .OR. (FRZGRA)) THEN
        SN_NEW = RAIN * DT * 0.001
        SNEQV  = SNEQV + SN_NEW
        RAIN1  = 0.0
C ----------------------------------------------------------------------
C UPDATE SNOW DENSITY BASED ON NEW SNOWFALL, USING OLD AND NEW SNOW.
C UPDATE SNOW THERMAL CONDUCTIVITY
C ----------------------------------------------------------------------
        CALL SNOW_NEW (SFC_TEMP,SN_NEW,SNOWH,SNDENS)
        SNCOND = CSNOW (SNDENS)
      ELSE
C ----------------------------------------------------------------------
C PRECIP IS LIQUID (RAIN), HENCE SAVE IN THE PRECIP VARIABLE THAT
C LATER CAN WHOLELY OR PARTIALLY INFILTRATE THE SOIL
C ----------------------------------------------------------------------
        RAIN1 = RAIN
      ENDIF
C ----------------------------------------------------------------------
C DETERMINE SNOWCOVER AND ALBEDO OVER LAND.
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
C IF SNOW DEPTH=0, SET SNOW FRACTION=0, ALBEDO=SNOW FREE ALBEDO.
C ----------------------------------------------------------------------
      IF (SNEQV .EQ. 0.0) THEN
	SNCOVR = 0.0
	ALBEDO = ALB
      ELSE
C ----------------------------------------------------------------------
C DETERMINE SNOW FRACTIONAL COVERAGE.
C DETERMINE SURFACE ALBEDO MODIFICATION DUE TO SNOWDEPTH STATE.
C ----------------------------------------------------------------------
	CALL SNFRAC (SNEQV,SNUP,SALP,SNCOVR)
	CALL ALCALC (ALB,SNOALB,SNCOVR,ALBEDO)
      ENDIF
C ----------------------------------------------------------------------
C NEXT CALCULATE THE SUBSURFACE HEAT FLUX, WHICH FIRST REQUIRES
C CALCULATION OF THE THERMAL DIFFUSIVITY.  TREATMENT OF THE
C LATTER FOLLOWS THAT ON PAGES 148-149 FROM "HEAT TRANSFER IN
C COLD CLIMATES", BY V. J. LUNARDINI (PUBLISHED IN 1981
C BY VAN NOSTRAND REINHOLD CO.) I.E. TREATMENT OF TWO CONTIGUOUS
C "PLANE PARALLEL" MEDIUMS (NAMELY HERE THE FIRST SOIL LAYER
C AND THE SNOWPACK LAYER, IF ANY). THIS DIFFUSIVITY TREATMENT
C BEHAVES WELL FOR BOTH ZERO AND NONZERO SNOWPACK, INCLUDING THE
C LIMIT OF VERY THIN SNOWPACK.  THIS TREATMENT ALSO ELIMINATES
C THE NEED TO IMPOSE AN ARBITRARY UPPER BOUND ON SUBSURFACE
C HEAT FLUX WHEN THE SNOWPACK BECOMES EXTREMELY THIN.
C ----------------------------------------------------------------------
C FIRST CALCULATE THERMAL DIFFUSIVITY OF TOP SOIL LAYER, USING
C BOTH THE FROZEN AND LIQUID SOIL MOISTURE, FOLLOWING THE
C SOIL THERMAL DIFFUSIVITY FUNCTION OF PETERS-LIDARD ET AL.
C (1998,JAS, VOL 55, 1209-1224), WHICH REQUIRES THE SPECIFYING
C THE QUARTZ CONTENT OF THE GIVEN SOIL CLASS (SEE ROUTINE REDPRM)
C ----------------------------------------------------------------------
      DF1 = BODY_DIFUSIVITY
C ----------------------------------------------------------------------
C FINALLY "PLANE PARALLEL" SNOWPACK EFFECT FOLLOWING
C V.J. LINARDINI REFERENCE CITED ABOVE. NOTE THAT DTOT IS
C COMBINED DEPTH OF SNOWDEPTH AND THICKNESS OF FIRST SOIL LAYER
C ----------------------------------------------------------------------
      DSOIL = -(0.5 * ZSOIL(1))

      IF (SNEQV .EQ. 0.) THEN
        SSOIL = DF1 * (TSKIN - STC(1)) / DSOIL
      ELSE
        DTOT   = SNOWH + DSOIL
        FRCSNO = SNOWH / DTOT
        FRCSOI = DSOIL / DTOT
C
C 1. HARMONIC MEAN (SERIES FLOW)
        DF1H = (SNCOND*DF1)/(FRCSOI*SNCOND+FRCSNO*DF1)
C 2. ARITHMETIC MEAN (PARALLEL FLOW)
        DF1A = FRCSNO*SNCOND + FRCSOI*DF1
C 3. GEOMETRIC MEAN (INTERMEDIATE BETWEEN HARMONIC AND ARITHMETIC MEAN)
C weigh DF by snow fraction
        DF1 = DF1A * SNCOVR + DF1 * (1.0 - SNCOVR)
C ----------------------------------------------------------------------
C CALCULATE SUBSURFACE HEAT FLUX, SSOIL, FROM FINAL THERMAL DIFFUSIVITY
C OF SURFACE MEDIUMS, DF1 ABOVE, AND SKIN TEMPERATURE AND TOP
C MID-LAYER SOIL TEMPERATURE
C ----------------------------------------------------------------------
        SSOIL = DF1 * (TSKIN - STC(1) ) / DTOT
      ENDIF
C ----------------------------------------------------------------------
C DETERMINE SURFACE ROUGHNESS OVER SNOWPACK USING SNOW CONDITION FROM
C THE PREVIOUS TIMESTEP.
C ----------------------------------------------------------------------
      IF (SNCOVR .GT. 0.) THEN
        CALL SNOWZ0 (SNCOVR,Z0)
      ENDIF
C ----------------------------------------------------------------------
C NEXT CALL ROUTINE SFCDIF TO CALCULATE THE SFC EXCHANGE COEF (CH) FOR
C HEAT AND MOISTURE.
C
C NOTE !!!
C COMMENT OUT CALL SFCDIF, IF SFCDIF ALREADY CALLED IN CALLING PROGRAM
C (SUCH AS IN COUPLED ATMOSPHERIC MODEL).
C
C NOTE !!!
C DO NOT CALL SFCDIF UNTIL AFTER ABOVE CALL TO REDPRM, IN CASE
C ALTERNATIVE VALUES OF ROUGHNESS LENGTH (Z0) AND ZILINTINKEVICH COEF
C (CZIL) ARE SET THERE VIA NAMELIST I/O.
C
C NOTE !!!
C ROUTINE SFCDIF RETURNS A CH THAT REPRESENTS THE WIND SPD TIMES THE
C "ORIGINAL" NONDIMENSIONAL "Ch" TYPICAL IN LITERATURE.  HENCE THE CH
C RETURNED FROM SFCDIF HAS UNITS OF M/S.  THE IMPORTANT COMPANION
C COEFFICIENT OF CH, CARRIED HERE AS "RCH", IS THE CH FROM SFCDIF TIMES
C AIR DENSITY AND PARAMETER "CP".  "RCH" IS COMPUTED IN "CALL PENMAN".
C RCH RATHER THAN CH IS THE COEFF USUALLY INVOKED LATER IN EQNS.
C
C NOTE !!!
C SFCDIF ALSO RETURNS THE SURFACE EXCHANGE COEFFICIENT FOR MOMENTUM, CM,
C ALSO KNOWN AS THE SURFACE DRAGE COEFFICIENT, BUT CM IS NOT USED HERE.
C ----------------------------------------------------------------------
C CALC VIRTUAL TEMPS AND VIRTUAL POTENTIAL TEMPS NEEDED BY SUBROUTINES
C SFCDIF AND PENMAN.
C ----------------------------------------------------------------------
      T2V = SFC_TEMP * (1.0 + 0.61 * Q2 )
C ----------------------------------------------------------------------
C COMMENT OUT BELOW 2 LINES IF CALL SFCDIF IS COMMENTED OUT, I.E. IN THE
C COUPLED MODEL.
C ----------------------------------------------------------------------
      T1V  = TSKIN * (1.0 + 0.61 * Q2)
      TH2V = TH2   * (1.0 + 0.61 * Q2)
      CALL SFCDIF (ZLVL,Z0,T1V,TH2V,WIND_SPEED,CZIL,CM,CH)
C ----------------------------------------------------------------------
C CALCULATE TOTAL DOWNWARD RADIATION (SOLAR PLUS LONGWAVE) NEEDED IN
C PENMAN EP SUBROUTINE THAT FOLLOWS
C ----------------------------------------------------------------------
      FDOWN = SOLNET + LWAVE
C ----------------------------------------------------------------------
C CALL PENMAN SUBROUTINE TO CALCULATE POTENTIAL EVAPORATION (ETP), AND
C OTHER PARTIAL PRODUCTS AND SUMS SAVE IN COMMON/RITE FOR LATER
C ----------------------------------------------------------------------
       CALL PENMAN (SFC_TEMP,SFC_PRESS,CH,T2V,TH2,RAIN,FDOWN,T24,
     &              SSOIL,Q2,Q2SAT,ETP,RCH,EPSCA,RR,SNOWNG,FRZGRA,
     &              DQSDT2,FLX2)
C ----------------------------------------------------------------------
C NOW DECIDE MAJOR PATHWAY BRANCH TO TAKE DEPENDING ON WHETHER SNOWPACK
C EXISTS OR NOT:
C ----------------------------------------------------------------------
      ESNOW = 0.0
      IF (SNEQV .EQ. 0.0) THEN
        CALL NOPAC (ETP,ETA,RAIN,SMC,SMCMAX,SMCWLT,DT,
     &     	    Q2,TSKIN,SFC_TEMP,T24,TH2,FDOWN,F1,SSOIL,
     &     	    STC,EPSCA,RCH,RR,
     &     	    SH2O,SLOPE,KDT,FRZX,ZSOIL,
     &     	    DKSAT,DWSAT,ZBOT,EDIR,ET,
     &     	    FXEXP,BETA,DEW,FLX1,FLX2,FLX3)
      ELSE
        CALL SNOPAC (ETP,ETA,RAIN,RAIN1,SNOWNG,SMC,SMCMAX,SMCWLT,
     &               DT,DF1,
     &               Q2,TSKIN,SFC_TEMP,T24,TH2,FDOWN,F1,SSOIL,STC,
     &               EPSCA,SFC_PRESS,RCH,RR,SNCOVR,SNEQV,
     &               SNDENS,SNOWH,SH2O,SLOPE,KDT,FRZX,SNUP,
     &               ZSOIL,DWSAT,DKSAT,ZBOT,
     &               EDIR,ET,SNOMLT,
     &               FXEXP,BETA,DEW,FLX1,FLX2,FLX3,ESNOW)
      ENDIF

      EDIR = EDIR * LVH2O
      DO K=1,4
        ET(K) = ET(K) * LVH2O
      ENDDO
      ESNOW = ESNOW * LSUBS
      ETP   = ETP * ((1. - SNCOVR) * LVH2O + SNCOVR * LSUBS)
      IF (ETP .GT. 0.) THEN
        ETA = EDIR + ESNOW
      ELSE
        ETA = ETP
      ENDIF
      BETA  = ETA / ETP
C ----------------------------------------------------------------------
C CONVERT THE SIGN OF SOIL HEAT FLUX SO THAT:
C   SSOIL>0: WARM THE SURFACE  (NIGHT TIME)
C   SSOIL<0: COOL THE SURFACE  (DAY TIME)
C ----------------------------------------------------------------------
      SSOIL = -1.0 * SSOIL
C ----------------------------------------------------------------------
C END SUBROUTINE SFLX
C ----------------------------------------------------------------------
      RETURN
      END


      SUBROUTINE ALCALC (ALB,SNOALB,SNCOVR,ALBEDO)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C CALCULATE ALBEDO INCLUDING SNOW EFFECT (0 -> 1)
C   ALB     SNOWFREE ALBEDO
C   SNOALB  MAXIMUM (DEEP) SNOW ALBEDO
C   SNCOVR  FRACTIONAL SNOW COVER
C   ALBEDO  SURFACE ALBEDO INCLUDING SNOW EFFECT
C ----------------------------------------------------------------------
      REAL ALB, SNOALB, SNCOVR, ALBEDO
C ----------------------------------------------------------------------
C SNOALB IS ARGUMENT REPRESENTING MAXIMUM ALBEDO OVER DEEP SNOW,
C AS PASSED INTO SFLX, AND ADAPTED FROM THE SATELLITE-BASED MAXIMUM
C SNOW ALBEDO FIELDS PROVIDED BY D. ROBINSON AND G. KUKLA
C (1985, JCAM, VOL 24, 402-411)
C ----------------------------------------------------------------------
      ALBEDO = ALB + SNCOVR * (SNOALB - ALB)
      IF (ALBEDO .GT. SNOALB) ALBEDO = SNOALB
C ----------------------------------------------------------------------
C END SUBROUTINE ALCALC
C ----------------------------------------------------------------------
      RETURN
      END


      FUNCTION CSNOW (DSNOW)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C CALCULATE SNOW TERMAL CONDUCTIVITY
C ----------------------------------------------------------------------
      REAL C
      REAL DSNOW
      REAL CSNOW
      REAL UNIT

      PARAMETER(UNIT = 0.11631)
C ----------------------------------------------------------------------
C CSNOW IN UNITS OF CAL/(CM*HR*C), RETURNED IN W/(M*C)
C BASIC VERSION IS DYACHKOVA EQUATION (1960), FOR RANGE 0.1-0.4
C ----------------------------------------------------------------------
      C = 0.328 * 10**(2.25 * DSNOW)
      CSNOW = UNIT * C
C ----------------------------------------------------------------------
C END FUNCTION CSNOW
C ----------------------------------------------------------------------
      RETURN
      END


      SUBROUTINE DEVAP (EDIR1,ETP1,SMC,SMCMAX,SMCWLT,FXEXP)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C CALCULATE DIRECT SOIL EVAPORATION
C ----------------------------------------------------------------------
      REAL EDIR1
      REAL ETP1
      REAL FX
      REAL FXEXP
      REAL SMC
      REAL SMCWLT
      REAL SMCMAX
      REAL SRATIO
C ----------------------------------------------------------------------
C DIRECT EVAP A FUNCTION OF RELATIVE SOIL MOISTURE AVAILABILITY, LINEAR
C WHEN FXEXP=1.
C FX > 1 REPRESENTS DEMAND CONTROL
C FX < 1 REPRESENTS FLUX CONTROL
C ----------------------------------------------------------------------
      SRATIO = (SMC - SMCWLT) / (SMCMAX - SMCWLT)
      IF (SRATIO .GT. 0.) THEN
        FX = SRATIO**FXEXP
        FX = MAX( MIN(FX, 1.), 0.)
      ELSE
        FX = 0.
      ENDIF
C ----------------------------------------------------------------------
C ALLOW FOR THE DIRECT-EVAP-REDUCING EFFECT OF SHADE
C ----------------------------------------------------------------------
      EDIR1 = FX * ETP1
C ----------------------------------------------------------------------
C END SUBROUTINE DEVAP
C END FUNCTION DEVAP
C ----------------------------------------------------------------------
      RETURN
      END


      SUBROUTINE EVAPO (ETA1,ETP1,SH2O,SMCMAX,SMCWLT,EDIR1,ET1,FXEXP)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C CALCULATE SOIL MOISTURE FLUX.  THE SOIL MOISTURE CONTENT
C IS A DEPENDENT VARIABLE THAT IS UPDATED WITH
C PROGNOSTIC EQNS.
C FROZEN GROUND VERSION:  NEW STATES ADDED: SH2O, AND FROZEN GROUND
C CORRECTION FACTOR, FRZFACT AND PARAMETER SLOPE.
C ----------------------------------------------------------------------
      INTEGER K
      INTEGER NSOIL
      COMMON /GLOBAL1/NSOIL

      REAL EDIR1
      REAL ET1(NSOIL)
      REAL ETA1
      REAL ETP1
      REAL FXEXP
      REAL SH2O(NSOIL)
      REAL SMCWLT
      REAL SMCMAX
C ----------------------------------------------------------------------
C EXECUTABLE CODE BEGINS HERE IF THE POTENTIAL EVAPOTRANSPIRATION IS
C GREATER THAN ZERO.
C ----------------------------------------------------------------------
      EDIR1 = 0.
      DO K = 1,NSOIL
        ET1(K) = 0.
      END DO

      IF (ETP1 .GT. 0.0) THEN
C ----------------------------------------------------------------------
C RETRIEVE DIRECT EVAPORATION FROM SOIL SURFACE
C ----------------------------------------------------------------------
        CALL DEVAP (EDIR1,ETP1,SH2O(1),SMCMAX,SMCWLT,FXEXP)
      ENDIF
C ----------------------------------------------------------------------
C TOTAL UP EVAP AND TRANSP TYPES TO OBTAIN ACTUAL EVAPOTRANSP
C ----------------------------------------------------------------------
      ETA1 = EDIR1
C ----------------------------------------------------------------------
C END SUBROUTINE EVAPO
C ----------------------------------------------------------------------
      RETURN
      END


      SUBROUTINE HRT (RHSTS,STC,SMC,SMCMAX,ZSOIL,YY,ZZ1,
     &                ZBOT,SH2O,DT,F1,DF1,AI,BI,CI)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C CALCULATE THE RIGHT HAND SIDE OF THE TIME TENDENCY TERM OF THE SOIL
C THERMAL DIFFUSION EQUATION.  ALSO TO COMPUTE ( PREPARE ) THE MATRIX
C COEFFICIENTS FOR THE TRI-DIAGONAL MATRIX OF THE IMPLICIT TIME SCHEME.
C ----------------------------------------------------------------------
C EXECUTED WITH SOIL LAYER TEMPERATURE AVERAGING
C ----------------------------------------------------------------------
      INTEGER K
      INTEGER NSOIL
      COMMON /GLOBAL1/NSOIL
C ----------------------------------------------------------------------
C DECLARE WORK ARRAYS NEEDED IN TRI-DIAGONAL IMPLICIT SOLVER
C ----------------------------------------------------------------------
      REAL AI(NSOIL)
      REAL BI(NSOIL)
      REAL CI(NSOIL)
C ----------------------------------------------------------------------
C DECLARATIONS
C ----------------------------------------------------------------------
      REAL CAIR
      REAL CH2O
      REAL CICE
      REAL DDZ
      REAL DDZ2
      REAL DENOM
      REAL DF1
      REAL DF1N
      REAL DF1K
      REAL DT
      REAL DTSDZ
      REAL DTSDZ2
      REAL F1
      REAL HCPCT
      REAL QTOT
      REAL RHSTS(NSOIL)
      REAL SSOIL
      REAL SICE
      REAL SMC(NSOIL)
      REAL SH2O(NSOIL)
      REAL SMCMAX
      REAL SNKSRC
      REAL STC(NSOIL)
      REAL TAVG
      REAL TBK
      REAL TBK1
      REAL ZBOT
      REAL TSNSR
      REAL TSURF
      REAL YY
      REAL ZSOIL(NSOIL)
      REAL ZZ1
C -----------------------------------------------------------------------
C DECLARATIONS FOR MUSSEL MODEL
C -----------------------------------------------------------------------
      REAL HTCP_ROCK
      REAL ROCKDF1N
      REAL HTCP_ANIMAL
      REAL CONTACT

      REAL ANNUAL_BOTTOM_TEMP
      COMMON /SURFACE/ANNUAL_BOTTOM_TEMP

      REAL BODY_DIFUSIVITY
      COMMON /BODY/BODY_DIFUSIVITY

      INTEGER BEDDEPTH

      REAL FRZ_T
      COMMON /GLOBAL2/FRZ_T
C -----------------------------------------------------------------------
C SET CERTAIN HEAT CAP AND THERM CNDT AND ALLOW BEDDEPTH FOR MUSSEL MODEL
C -----------------------------------------------------------------------
      PARAMETER(HTCP_ROCK = XHTCP_ROCKX)
C     PARAMETER(HTCP_ROCK = 2.04E6)
      PARAMETER(ROCKDF1N  = 2.1)

      COMMON /GEOMETRY/BEDDEPTH,CONTACT
C ----------------------------------------------------------------------
C SET SPECIFIC HEAT CAPACITIES OF AIR, WATER, ICE, SOIL MINERAL
C ----------------------------------------------------------------------
      PARAMETER(CAIR  = 1.0045E6)
C     PARAMETER(CAIR  = 1.0E6)
      PARAMETER(CH2O  = 4.187E6)
C     PARAMETER(CH2O  = 4.2E6)
      PARAMETER(CICE  = 2.008E6)
C     PARAMETER(CICE  = 2.106E6)
      PARAMETER(HTCP_ANIMAL = XHTCP_ANIMALX)
C     PARAMETER(HTCP_ANIMAL = 5.23E6)
C ----------------------------------------------------------------------
C BEGIN SECTION FOR TOP SOIL LAYER
C ----------------------------------------------------------------------
C CALC THE HEAT CAPACITY OF THE TOP SOIL LAYER
C ----------------------------------------------------------------------
C ALLOW FOR LAYERS OF MUSSEL MODEL
C ----------------------------------------------------------------------
      HCPCT = SH2O(1)*CH2O + (1.0-SMCMAX) * HTCP_ANIMAL + (SMCMAX -
     &          SMC(1)) * CAIR + (SMC(1) - SH2O(1)) * CICE
C ----------------------------------------------------------------------
C CALC THE MATRIX COEFFICIENTS AI, BI, AND CI FOR THE TOP LAYER
C ----------------------------------------------------------------------
      DDZ = 1.0 / ( -0.5 * ZSOIL(2) )
      AI(1) = 0.0
      CI(1) = (DF1 * DDZ) / (ZSOIL(1) * HCPCT)
      BI(1) = -CI(1) + DF1 / (0.5 * ZSOIL(1) * ZSOIL(1)*HCPCT*ZZ1)
C ----------------------------------------------------------------------
C CALCULATE THE VERTICAL SOIL TEMP GRADIENT BTWN THE 1ST AND 2ND SOIL
C LAYERS.  THEN CALCULATE THE SUBSURFACE HEAT FLUX. USE THE TEMP
C GRADIENT AND SUBSFC HEAT FLUX TO CALC "RIGHT-HAND SIDE TENDENCY
C TERMS", OR "RHSTS", FOR TOP SOIL LAYER.
C ----------------------------------------------------------------------
      DTSDZ = (STC(1) - STC(2)) / (-0.5 * ZSOIL(2))
      SSOIL = DF1 * (STC(1) - YY) / (0.5 * ZSOIL(1) * ZZ1)
      RHSTS(1) = (DF1 * DTSDZ - SSOIL) / (ZSOIL(1) * HCPCT)
C ----------------------------------------------------------------------
C NEXT CAPTURE THE VERTICAL DIFFERENCE OF THE HEAT FLUX AT TOP AND
C BOTTOM OF FIRST SOIL LAYER FOR USE IN HEAT FLUX CONSTRAINT APPLIED TO
C POTENTIAL SOIL FREEZING/THAWING IN ROUTINE SNKSRC.
C ----------------------------------------------------------------------
      QTOT = SSOIL - DF1*DTSDZ
C ----------------------------------------------------------------------
C IF TEMPERATURE AVERAGING INVOKED:
C SET TEMP "TSURF" AT TOP OF SOIL COLUMN (FOR USE IN FREEZING SOIL
C PHYSICS LATER IN FUNCTION SUBROUTINE SNKSRC).  IF SNOWPACK CONTENT IS
C ZERO, THEN TSURF EXPRESSION BELOW GIVES TSURF = SKIN TEMP.  IF
C SNOWPACK IS NONZERO (HENCE ARGUMENT ZZ1=1), THEN TSURF EXPRESSION
C BELOW YIELDS SOIL COLUMN TOP TEMPERATURE UNDER SNOWPACK.  THEN
C CALCULATE TEMPERATURE AT BOTTOM INTERFACE OF 1ST SOIL LAYER FOR USE
C LATER IN FUNCTION SUBROUTINE SNKSRC
C ----------------------------------------------------------------------
      TSURF = (YY + (ZZ1 - 1) * STC(1)) / ZZ1
      CALL TBND (STC(1),STC(2),ZSOIL,ZBOT,1,TBK)
C ----------------------------------------------------------------------
C CALCULATE FROZEN WATER CONTENT IN 1ST SOIL LAYER.
C ----------------------------------------------------------------------
      SICE = SMC(1) - SH2O(1)
C ----------------------------------------------------------------------
C IF FROZEN WATER PRESENT OR ANY OF LAYER-1 MID-POINT OR BOUNDING
C INTERFACE TEMPERATURES BELOW FREEZING, THEN CALL SNKSRC TO
C COMPUTE HEAT SOURCE/SINK (AND CHANGE IN FROZEN WATER CONTENT)
C DUE TO POSSIBLE SOIL WATER PHASE CHANGE
C ----------------------------------------------------------------------
      IF ((SICE .GT. 0.) .OR. (TSURF .LT. FRZ_T) .OR.
     & (STC(1) .LT. FRZ_T) .OR. (TBK .LT. FRZ_T)) THEN
        TSNSR = SNKSRC (SMC(1),SH2O(1),ZSOIL,DT,1,QTOT)
        RHSTS(1) = RHSTS(1) - TSNSR / ( ZSOIL(1) * HCPCT )
      ENDIF
C ----------------------------------------------------------------------
C THIS ENDS SECTION FOR TOP SOIL LAYER.
C ----------------------------------------------------------------------
C INITIALIZE DDZ2
C ----------------------------------------------------------------------
      DDZ2 = 0.0
C ----------------------------------------------------------------------
C LOOP THRU THE REMAINING SOIL LAYERS, REPEATING THE ABOVE PROCESS
C (EXCEPT SUBSFC OR "GROUND" HEAT FLUX NOT REPEATED IN LOWER LAYERS)
C ----------------------------------------------------------------------
      DF1K = DF1
      DO K = 2,NSOIL
C ----------------------------------------------------------------------
C CALCULATE HEAT CAPACITY FOR THIS SOIL LAYER.
C ----------------------------------------------------------------------
C ALLOW HEAT CAPACITY TO BE DEPENDENT ON LAYER (MUSSEL MODEL)
C ----------------------------------------------------------------------
	IF (K .LE. BEDDEPTH) THEN
	  HCPCT = SH2O(K) * CH2O + (1.0 - SMCMAX) * HTCP_ANIMAL +
     &        (SMCMAX - SMC(K)) * CAIR + (SMC(K) - SH2O(K)) * CICE
	  IF (K .LT. BEDDEPTH) THEN
	    HCPCT = HCPCT * CONTACT
	  ENDIF
	ELSE
	  HCPCT = HTCP_ROCK
	ENDIF

        IF (K .NE. NSOIL) THEN
C ----------------------------------------------------------------------
C THIS SECTION FOR LAYER 2 OR GREATER, BUT NOT LAST LAYER.
C ----------------------------------------------------------------------
C CALCULATE THERMAL DIFFUSIVITY FOR THIS LAYER.
C ----------------------------------------------------------------------
C ALLOW THERM DIFF TO BE DEPENDENT ON LAYER (MUSSEL MODEL)
C ----------------------------------------------------------------------
	IF (K .LE. BEDDEPTH) THEN
	  IF (K .LT. BEDDEPTH) THEN
	      DF1N = BODY_DIFUSIVITY
	  ELSE
	      DF1N = BODY_DIFUSIVITY * CONTACT
	  ENDIF
	ELSE
	  DF1N = ROCKDF1N
	ENDIF
C ----------------------------------------------------------------------
C CALC THE VERTICAL SOIL TEMP GRADIENT THRU THIS LAYER
C ----------------------------------------------------------------------
          DENOM = 0.5 * ( ZSOIL(K-1) - ZSOIL(K+1) )
          DTSDZ2 = ( STC(K) - STC(K+1) ) / DENOM
C ----------------------------------------------------------------------
C CALC THE MATRIX COEF, CI, AFTER CALC'NG ITS PARTIAL PRODUCT
C ----------------------------------------------------------------------
          DDZ2 = 2. / (ZSOIL(K-1) - ZSOIL(K+1))
          CI(K) = -DF1N * DDZ2 / ((ZSOIL(K-1) - ZSOIL(K)) * HCPCT)
C ----------------------------------------------------------------------
C IF TEMPERATURE AVERAGING INVOKED:
C CALCULATE TEMP AT BOTTOM OF LAYER
C ----------------------------------------------------------------------
          CALL TBND (STC(K),STC(K+1),ZSOIL,ZBOT,K,TBK1)
        ELSE
C ----------------------------------------------------------------------
C SPECIAL CASE OF BOTTOM SOIL LAYER:
C CALCULATE THERMAL DIFFUSIVITY FOR BOTTOM LAYER
C ----------------------------------------------------------------------
	  DF1N = ROCKDF1N
C ----------------------------------------------------------------------
C CALC THE VERTICAL SOIL TEMP GRADIENT THRU BOTTOM LAYER.
C ----------------------------------------------------------------------
          DENOM = .5 * (ZSOIL(K-1) + ZSOIL(K)) - ZBOT
          DTSDZ2 = (STC(K) - ANNUAL_BOTTOM_TEMP) / DENOM
C ----------------------------------------------------------------------
C SET MATRIX COEF, CI TO ZERO IF BOTTOM LAYER.
C ----------------------------------------------------------------------
          CI(K) = 0.
C ----------------------------------------------------------------------
C IF TEMPERATURE AVERAGING INVOKED:
C CALCULATE TEMP AT BOTTOM OF LAST LAYER
C ----------------------------------------------------------------------
          CALL TBND (STC(K),ANNUAL_BOTTOM_TEMP,ZSOIL,ZBOT,K,TBK1)
        ENDIF
C ----------------------------------------------------------------------
C THIS ENDS SPECIAL LOOP FOR BOTTOM LAYER.
C ----------------------------------------------------------------------
C CALCULATE RHSTS FOR THIS LAYER AFTER CALC'NG A PARTIAL PRODUCT.
C ----------------------------------------------------------------------
        DENOM = ( ZSOIL(K) - ZSOIL(K-1) ) * HCPCT
        RHSTS(K) = ( DF1N * DTSDZ2 - DF1K * DTSDZ ) / DENOM
        QTOT = -1.0*DENOM*RHSTS(K)
        SICE = SMC(K) - SH2O(K)

        IF ( (SICE .GT. 0.) .OR. (TBK .LT. FRZ_T) .OR.
     &  (STC(K) .LT. FRZ_T) .OR. (TBK1 .LT. FRZ_T) ) THEN
          TSNSR = SNKSRC(SMC(K),SH2O(K),ZSOIL,DT,K,QTOT)
          RHSTS(K) = RHSTS(K) - TSNSR / DENOM
        ENDIF
C ----------------------------------------------------------------------
C CALC MATRIX COEFS, AI, AND BI FOR THIS LAYER.
C ----------------------------------------------------------------------
        AI(K) = - DF1 * DDZ / ((ZSOIL(K-1) - ZSOIL(K)) * HCPCT)
        BI(K) = -(AI(K) + CI(K))
C ----------------------------------------------------------------------
C RESET VALUES OF DF1, DTSDZ, DDZ, AND TBK FOR LOOP TO NEXT SOIL LAYER.
C ----------------------------------------------------------------------
        TBK   = TBK1
        DF1K  = DF1N
        DTSDZ = DTSDZ2
        DDZ   = DDZ2
      END DO
C ----------------------------------------------------------------------
C END SUBROUTINE HRT
C ----------------------------------------------------------------------
      RETURN
      END


      SUBROUTINE HSTEP (STCOUT,STCIN,RHSTS,DT,AI,BI,CI)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C CALCULATE/UPDATE THE SOIL TEMPERATURE FIELD
C ----------------------------------------------------------------------
      INTEGER K
      INTEGER NSOIL
      COMMON /GLOBAL1/NSOIL

      REAL AI(NSOIL)
      REAL BI(NSOIL)
      REAL CI(NSOIL)
      REAL CIin(NSOIL)
      REAL DT
      REAL RHSTS(NSOIL)
      REAL RHSTSin(NSOIL)
      REAL STCIN(NSOIL)
      REAL STCOUT(NSOIL)
C ---------------------------------------------------------------------
C DECLARATIONS FOR MUSSEL MODEL
C ---------------------------------------------------------------------
      INTEGER TIDEFLAG
      REAL SST
      COMMON /TIDE/TIDEFLAG, SST
      INTEGER BEDDEPTH
      REAL CONTACT
      COMMON /GEOMETRY/BEDDEPTH,CONTACT
C ----------------------------------------------------------------------
C CREATE FINITE DIFFERENCE VALUES FOR USE IN ROSR12 ROUTINE
C ----------------------------------------------------------------------
      DO K = 1,NSOIL
        RHSTS(K) = RHSTS(K) * DT
        AI(K) = AI(K) * DT
        BI(K) = 1. + BI(K) * DT
        CI(K) = CI(K) * DT
      END DO
C ----------------------------------------------------------------------
C COPY VALUES FOR INPUT VARIABLES BEFORE CALL TO ROSR12
C ----------------------------------------------------------------------
      DO K = 1,NSOIL
         RHSTSin(K) = RHSTS(K)
      END DO
      DO K = 1,NSOIL
        CIin(K) = CI(K)
      END DO
C ----------------------------------------------------------------------
C SOLVE THE TRI-DIAGONAL MATRIX EQUATION
C ----------------------------------------------------------------------
      CALL ROSR12(CI,AI,BI,CIin,RHSTSin,RHSTS)
C ----------------------------------------------------------------------
C CALC/UPDATE THE SOIL TEMPS USING MATRIX SOLUTION
C ----------------------------------------------------------------------
C DANGER MUSSELMODEL if tide is in, set mussel layer temperatures to SST
C V1.6 - slowly pull temps to SST
C ----------------------------------------------------------------------
      DO K = 1,NSOIL
	    IF (TIDEFLAG .EQ. 1) THEN
	      IF (K .LE. BEDDEPTH) THEN
	        STCOUT(K) = STCIN(K) - 0.48*(STCIN(K)-SST)
	      ELSE
	        STCOUT(K) = STCIN(K) + CI(K)
	      ENDIF
	    ELSE
	      STCOUT(K) = STCIN(K) + CI(K)
	    ENDIF
      END DO
C ----------------------------------------------------------------------
C END SUBROUTINE HSTEP
C ----------------------------------------------------------------------
      RETURN
      END


      SUBROUTINE NOPAC(ETP,ETA,RAIN,SMC,SMCMAX,SMCWLT,DT,
     &                 Q2,TSKIN,SFC_TEMP,T24,TH2,FDOWN,F1,
     &                 SSOIL,STC,EPSCA,RCH,RR,
     &                 SH2O,SLOPE,KDT,FRZFACT,ZSOIL,
     &                 DKSAT,DWSAT,ZBOT,EDIR,ET,
     &                 FXEXP,BETA,DEW,FLX1,FLX2,FLX3)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C CALCULATE SOIL MOISTURE AND HEAT FLUX VALUES AND UPDATE SOIL MOISTURE
C CONTENT AND SOIL HEAT CONTENT VALUES FOR THE CASE WHEN NO SNOW PACK IS
C PRESENT
C ----------------------------------------------------------------------
      INTEGER NSOIL
      COMMON /GLOBAL1/NSOIL

      REAL BETA
      REAL CP
      REAL DEW
      REAL DF1
      REAL DKSAT
      REAL DT
      REAL DWSAT
      REAL EDIR
      REAL EPSCA
      REAL ETA
      REAL ETA1
      REAL ETP
      REAL ETP1
      REAL ET(NSOIL)
      REAL FDOWN
      REAL F1
      REAL FXEXP
      REAL FLX1
      REAL FLX2
      REAL FLX3
      REAL FRZFACT
      REAL KDT
      REAL RAIN
      REAL RAIN1
      REAL Q2
      REAL RCH
      REAL RR
      REAL SSOIL
      REAL SFC_TEMP
      REAL SH2O(NSOIL)
      REAL SIGMA
      REAL SLOPE
      REAL SMC(NSOIL)
      REAL SMCMAX
      REAL SMCWLT
      REAL STC(NSOIL)
      REAL TSKIN
      REAL T24
      REAL TH2
      REAL YY
      REAL YYNUM
      REAL ZBOT
      REAL ZSOIL(NSOIL)
      REAL ZZ1

      REAL EDIR1
      REAL ET1(NSOIL)

      INTEGER K

      PARAMETER(CP = 1004.5)
      PARAMETER(SIGMA = 5.67E-8)

      REAL BODY_DIFUSIVITY
      COMMON /BODY/BODY_DIFUSIVITY

      REAL EMISSIVITY
      INTEGER BEDDEPTH
      REAL CONTACT
      COMMON /EMISSIVITY/EMISSIVITY
      COMMON /GEOMETRY/BEDDEPTH,CONTACT
C ----------------------------------------------------------------------
C CONVERT ETP FROM KG M-2 S-1 TO MS-1 AND INITIALIZE DEW.
C ----------------------------------------------------------------------
      RAIN1 = RAIN * 0.001
      ETP1  = ETP * 0.001
      DEW   = 0.0

      EDIR  = 0.
      EDIR1 = 0.
      DO K = 1,NSOIL
        ET(K)  = 0.
        ET1(K) = 0.
      END DO

      IF (ETP .GT. 0.0) THEN
C ----------------------------------------------------------------------
C CONVERT RAIN FROM 'KG M-2 S-1' TO 'M S-1'.
C ----------------------------------------------------------------------
	CALL EVAPO (ETA1,ETP1,SH2O,SMCMAX,SMCWLT,EDIR1,ET1,FXEXP)
	CALL SMFLX (SMC,DT,RAIN1,ZSOIL,SH2O,SLOPE,KDT,FRZFACT,
     &                 SMCMAX,SMCWLT,DKSAT,DWSAT,EDIR1,ET1)
      ELSE
C ----------------------------------------------------------------------
C IF ETP < 0, ASSUME DEW FORMS (TRANSFORM ETP1 INTO DEW AND REINITIALIZE
C ETP1 TO ZERO).
C ----------------------------------------------------------------------
        DEW = -ETP1
C ----------------------------------------------------------------------
C CONVERT RAIN FROM 'KG M-2 S-1' TO 'M S-1' AND ADD DEW AMOUNT.
C ----------------------------------------------------------------------
        RAIN1 = RAIN1 + DEW

        CALL SMFLX (SMC,DT,RAIN1,ZSOIL,SH2O,SLOPE,KDT,FRZFACT,
     &            SMCMAX,SMCWLT,DKSAT,DWSAT,EDIR1,ET1)
      ENDIF
C ----------------------------------------------------------------------
C CONVERT MODELED EVAPOTRANSPIRATION FM  M S-1  TO  KG M-2 S-1
C ----------------------------------------------------------------------
      ETA = ETA1 * 1000.0
C ----------------------------------------------------------------------
      EDIR = EDIR1 * 1000.0
      DO K = 1,NSOIL
        ET(K) = ET1(K) * 1000.0
      ENDDO
C ----------------------------------------------------------------------
C BASED ON ETP AND E VALUES, DETERMINE BETA
C ----------------------------------------------------------------------
      IF (ETP .LE. 0.0) THEN
        BETA = 0.0
        IF (ETP .LT. 0.0) THEN
          BETA = 1.0
        ENDIF
      ELSE
        BETA = ETA / ETP
      ENDIF
C ----------------------------------------------------------------------
C GET SOIL THERMAL DIFFUXIVITY/CONDUCTIVITY FOR TOP SOIL LYR,
C CALC. ADJUSTED TOP LYR SOIL TEMP AND ADJUSTED SOIL FLUX, THEN
C CALL SHFLX TO COMPUTE/UPDATE SOIL HEAT FLUX AND SOIL TEMPS.
C ----------------------------------------------------------------------
        DF1 = BODY_DIFUSIVITY
C ----------------------------------------------------------------------
C COMPUTE INTERMEDIATE TERMS PASSED TO ROUTINE HRT (VIA ROUTINE
C SHFLX BELOW) FOR USE IN COMPUTING SUBSURFACE HEAT FLUX IN HRT
C ----------------------------------------------------------------------
      YYNUM = FDOWN - SIGMA * T24 * EMISSIVITY
      YY    = SFC_TEMP + (YYNUM/RCH+TH2-SFC_TEMP-BETA*EPSCA) / RR
      ZZ1   = DF1 / (-0.5 * ZSOIL(1) * RCH * RR) + 1.0

      CALL SHFLX (SSOIL,STC,SMC,SMCMAX,TSKIN,DT,YY,ZZ1,ZSOIL,
     &            ZBOT,SH2O,F1,DF1)
C ----------------------------------------------------------------------
C SET FLX1 AND FLX3 (SNOPACK PHASE CHANGE HEAT FLUXES) TO ZERO SINCE
C THEY ARE NOT USED HERE IN SNOPAC. FLX2 (FREEZING RAIN HEAT FLUX) WAS
C SIMILARLY INITIALIZED IN THE PENMAN ROUTINE.
C ----------------------------------------------------------------------
      FLX1 = 0.0
      FLX3 = 0.0
C ----------------------------------------------------------------------
C END SUBROUTINE NOPAC
C ----------------------------------------------------------------------
      RETURN
      END


      SUBROUTINE PENMAN (SFC_TEMP,SFC_PRESS,CH,T2V,TH2,RAIN,FDOWN,
     &                   T24,SSOIL,Q2,Q2SAT,ETP,RCH,EPSCA,RR,
     &                   SNOWNG,FRZGRA,DQSDT2,FLX2)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C CALCULATE POTENTIAL EVAPORATION FOR THE CURRENT POINT. VARIOUS
C PARTIAL SUMS/PRODUCTS ARE ALSO CALCULATED AND PASSED BACK TO THE
C CALLING ROUTINE FOR LATER USE
C ----------------------------------------------------------------------
      LOGICAL SNOWNG
      LOGICAL FRZGRA

      REAL A
      REAL CH
      REAL CP
      REAL CW
      REAL CPICE
      REAL DELTA
      REAL DQSDT2
      REAL ELCP
      REAL EPSCA
      REAL ETP
      REAL FDOWN
      REAL FLX2
      REAL FNET
      REAL LVH2O
      REAL LSUBF
      REAL RAIN
      REAL Q2
      REAL Q2SAT
      REAL R
      REAL RAD
      REAL RCH
      REAL RHO
      REAL RR
      REAL SSOIL
      REAL SFC_PRESS
      REAL SFC_TEMP
      REAL SIGMA
      REAL T24
      REAL T2V
      REAL TH2

      PARAMETER(CP    = 1004.5)
      PARAMETER(CW    = 4187.)
      PARAMETER(CPICE = 2.106E+3)
      PARAMETER(R     = 287.04)
      PARAMETER(ELCP  = 2.4888E+3)
      PARAMETER(LSUBF = 3.335E+5)
      PARAMETER(LVH2O = 2.501E+6)
      PARAMETER(SIGMA = 5.67E-8)

      REAL EMISSIVITY
      COMMON /EMISSIVITY/ EMISSIVITY
C ----------------------------------------------------------------------
      FLX2 = 0.0
C ----------------------------------------------------------------------
C PREPARE PARTIAL QUANTITIES FOR PENMAN EQUATION.
C ----------------------------------------------------------------------
      DELTA = ELCP * DQSDT2
      T24   = SFC_TEMP * SFC_TEMP * SFC_TEMP * SFC_TEMP
      RR    = T24 * 6.48E-8 / (SFC_PRESS * CH) + 1.0
      RHO   = SFC_PRESS / (R * T2V)
      RCH   = RHO * CP * CH
C ----------------------------------------------------------------------
C ADJUST THE PARTIAL SUMS / PRODUCTS WITH THE LATENT HEAT
C EFFECTS CAUSED BY FALLING PRECIPITATION.
C ----------------------------------------------------------------------
      IF (.NOT. SNOWNG) THEN
        IF (RAIN .GT. 0.0) RR = RR + CW * RAIN / RCH
      ELSE
        RR = RR + CPICE * RAIN/RCH
      ENDIF

      FNET = FDOWN - SIGMA * T24 * EMISSIVITY - SSOIL
C ----------------------------------------------------------------------
C INCLUDE THE LATENT HEAT EFFECTS OF FRZNG RAIN CONVERTING TO ICE ON
C IMPACT IN THE CALCULATION OF FLX2 AND FNET.
C ----------------------------------------------------------------------
      IF (FRZGRA) THEN
        FLX2 = -LSUBF * RAIN
        FNET = FNET - FLX2
      ENDIF
C ----------------------------------------------------------------------
C FINISH PENMAN EQUATION CALCULATIONS.
C ----------------------------------------------------------------------
      RAD   = FNET / RCH + TH2 - SFC_TEMP
      A     = ELCP * (Q2SAT - Q2)
      EPSCA = (A * RR + RAD * DELTA) / (DELTA + RR)
      ETP   = EPSCA * RCH / LVH2O
C ----------------------------------------------------------------------
C END SUBROUTINE PENMAN
C ----------------------------------------------------------------------
      RETURN
      END


      SUBROUTINE REDPRM (REFKDT,KDT,ZBOT,FRZX,SLOPE,SNUPX,SNUP,SALP,
     &     		 DKSAT,DWSAT,SMCMAX,SMCWLT,F1,FXEXP,SLDPTH,
     &     		 ZSOIL,Z0,CZIL,ROUGHNESS)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C INTERNALLY SET (DEFAULT VALUES) FOR SOIL AND VEGETATION PARAMETERS
c REQUIRED FOR THE EXECUSION OF THE NOAH LSM
C
C OPTIONAL NON-DEFAULT PARAMETERS CAN BE READ IN, ACCOMMODATING UP TO 30
C SOIL, VEG, OR SLOPE CLASSES, IF THE DEFAULT MAX NUMBER OF SOIL, VEG,
C AND/OR SLOPE TYPES IS RESET.
C ----------------------------------------------------------------------
      REAL ROUGHNESS
      REAL Z0
C  SET-UP SOIL PARAMETERS FOR GIVEN SOIL TYPE
C  (OPTIMIZED FOR THE LIMPET MODEL)
C
C  SOIL PARAMETERS:
      REAL SLOPE
C    MAXSMC: MAX SOIL MOISTURE CONTENT (POROSITY)
      REAL MAXSMC, SMCMAX
      PARAMETER(MAXSMC = 0.181)
C    WLTSMC: WILTING PT SOIL MOISTURE CONTENTS
      REAL WLTSMC, SMCWLT, WLTSMC1
      DATA WLTSMC /0/
C    SATPSI: SATURATED SOIL POTENTIAL
      REAL SATPSI
      DATA SATPSI /0.04/
C    SATDK: SATURATED SOIL HYDRAULIC CONDUCTIVITY
      REAL SATDK, DKSAT
      DATA SATDK /0.01/
C    SATDW: SATURATED SOIL DIFFUSIVITY
      REAL SATDW, DWSAT
      DATA SAT DW /0.00/
C    BB: THE 'B' PARAMETER
      REAL BB
      PARAMETER(BB = 4)
C    F11: USED TO COMPUTE SOIL DIFFUSIVITY/CONDUCTIVITY
      REAL F11, F1
      DATA F11 /0.000/
C ----------------------------------------------------------------------
C SET-UP VEGETATION PARAMETERS FOR A GIVEN VEGETAION TYPE
C  (OPTIMIZED FOR THE LIMPET MODEL)

C   SNUP:   THRESHOLD SNOW DEPTH (IN WATER EQUIVALENT M) THAT
C   	    IMPLIES 100% SNOW COVER
      REAL SNUP, SNUPX
      REAL FRZFACT
C ----------------------------------------------------------------------
C CLASS PARAMETER 'SLOPETYP' WAS INCLUDED TO ESTIMATE LINEAR RESERVOIR
C COEFFICIENT 'SLOPE' TO THE BASEFLOW RUNOFF OUT OF THE BOTTOM LAYER.
C LOWEST CLASS (SLOPETYP=0) MEANS HIGHEST SLOPE PARAMETER = 1.
C DEFINITION OF SLOPETYP FROM 'ZOBLER' SLOPE TYPE:
C SLOPE CLASS  PERCENT SLOPE
C 1	       0-8
C 2	       8-30
C 3	       > 30
C 4	       0-30
C 5	       0-8 & > 30
C 6	       8-30 & > 30
C 7	       0-8, 8-30, > 30
C 9	       GLACIAL ICE
C BLANK        OCEAN/SEA
C ----------------------------------------------------------------------
C SET UNIVERSAL PARAMETERS (NOT DEPENDENT ON SOIL, VEG, SLOPE TYPE)
C ----------------------------------------------------------------------
      INTEGER I
      INTEGER NSOIL
      COMMON /GLOBAL1/NSOIL
C ----------------------------------------------------------------------
C PARAMETER USED TO CALCULATE ROUGHNESS LENGTH OF HEAT.
C ----------------------------------------------------------------------
      REAL CZIL
      REAL CZIL_DATA
      DATA CZIL_DATA /0.075/
C ----------------------------------------------------------------------
C BARE SOIL EVAPORATION EXPONENT USED IN DEVAP.
C ----------------------------------------------------------------------
      REAL FXEXP
      REAL FXEXP_DATA
      DATA FXEXP_DATA /2.0/
C ----------------------------------------------------------------------
C SPECIFY SNOW DISTRIBUTION SHAPE PARAMETER SALP - SHAPE PARAMETER OF
C DISTRIBUTION FUNCTION OF SNOW COVER. FROM ANDERSON'S DATA (HYDRO-17)
C BEST FIT IS WHEN SALP = 2.6
C ----------------------------------------------------------------------
      REAL SALP
      REAL SALP_DATA
      DATA SALP_DATA /4.0/
C ----------------------------------------------------------------------
C KDT IS DEFINED BY REFERENCE REFKDT AND DKSAT; REFDK=2.E-6 IS THE SAT.
C DK. VALUE FOR THE SOIL TYPE 2
C ----------------------------------------------------------------------
      REAL REFDK
      REAL REFDK_DATA
      DATA REFDK_DATA /2.0E-6/

      REAL REFKDT
      REAL REFKDT_DATA
      DATA REFKDT_DATA /3.0/

      REAL FRZX
      REAL KDT
C ----------------------------------------------------------------------
C FROZEN GROUND PARAMETER, FRZK, DEFINITION: ICE CONTENT THRESHOLD ABOVE
C WHICH FROZEN SOIL IS IMPERMEABLE REFERENCE VALUE OF THIS PARAMETER FOR
C THE LIGHT CLAY SOIL (TYPE=3) FRZK = 0.15 M.
C ----------------------------------------------------------------------
      REAL FRZK
      REAL FRZK_DATA
      DATA FRZK_DATA /0.15/

      REAL SLDPTH(NSOIL)
      REAL ZSOIL(NSOIL)
C ----------------------------------------------------------------------
C SPECIFY DEPTH[M] OF LOWER BOUNDARY SOIL TEMPERATURE.
C ----------------------------------------------------------------------
      REAL ZBOT
      REAL ZBOT_DATA
      DATA ZBOT_DATA /-8.0/
C ----------------------------------------------------------------------
C SET TWO SOIL MOISTURE WILT, SOIL MOISTURE REFERENCE PARAMETERS
C ----------------------------------------------------------------------
      REAL SMLOW
      DATA SMLOW /0.5/

      REAL SMHIGH
      DATA SMHIGH /6.0/

      SATDW   = BB * SATDK * (SATPSI / MAXSMC)
      F11     = ALOG10(SATPSI) + BB * ALOG10(MAXSMC) + 2.0
      WLTSMC1 = MAXSMC * (200.0 / SATPSI)**(-1.0 / BB)
      WLTSMC  = WLTSMC1 - SMLOW * WLTSMC1
C ----------------------------------------------------------------------
C SET-UP UNIVERSAL PARAMETERS
C ----------------------------------------------------------------------
      ZBOT   = ZBOT_DATA
      REFDK  = REFDK_DATA
      FRZK   = FRZK_DATA
      FXEXP  = FXEXP_DATA
      REFKDT = REFKDT_DATA
      CZIL   = CZIL_DATA
C ----------------------------------------------------------------------
C  SET-UP SOIL PARAMETERS
C ----------------------------------------------------------------------
      DKSAT   = SATDK
      DWSAT   = SATDW
      F1      = F11
      KDT     = REFKDT * DKSAT/REFDK
      SMCMAX  = MAXSMC
      SMCWLT  = WLTSMC
      FRZFACT = 0.412 / 0.468
C ----------------------------------------------------------------------
C TO ADJUST FRZK PARAMETER TO ACTUAL SOIL TYPE: FRZK * FRZFACT
C ----------------------------------------------------------------------
      FRZX = FRZK * FRZFACT
C ----------------------------------------------------------------------
C SET-UP VEGETATION PARAMETERS
C ----------------------------------------------------------------------
      SNUP  = 0.04
      Z0    = ROUGHNESS
      SLOPE = 1.
C ----------------------------------------------------------------------
C END SUBROUTINE REDPRM
C ----------------------------------------------------------------------
      RETURN
      END


      SUBROUTINE ROSR12 (P,A,B,C,D,DELTA)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C INVERT (SOLVE) THE TRI-DIAGONAL MATRIX PROBLEM SHOWN BELOW:
C ###                                            ### ###  ###   ###  ###
C #B(1), C(1),  0  ,  0  ,  0  ,   . . .  ,    0   # #      #   #      #
C #A(2), B(2), C(2),  0  ,  0  ,   . . .  ,    0   # #      #   #      #
C # 0  , A(3), B(3), C(3),  0  ,   . . .  ,    0   # #      #   # D(3) #
C # 0  ,  0  , A(4), B(4), C(4),   . . .  ,    0   # # P(4) #   # D(4) #
C # 0  ,  0  ,  0  , A(5), B(5),   . . .  ,    0   # # P(5) #   # D(5) #
C # .                                          .   # #  .   # = #   .  #
C # .                                          .   # #  .   #   #   .  #
C # .                                          .   # #  .   #   #   .  #
C # 0  , . . . , 0 , A(M-2), B(M-2), C(M-2),   0   # #P(M-2)#   #D(M-2)#
C # 0  , . . . , 0 ,   0   , A(M-1), B(M-1), C(M-1)# #P(M-1)#   #D(M-1)#
C # 0  , . . . , 0 ,   0   ,   0   ,  A(M) ,  B(M) # # P(M) #   # D(M) #
C ###                                            ### ###  ###   ###  ###
C ----------------------------------------------------------------------
      INTEGER K
      INTEGER KK
      INTEGER NSOIL
      COMMON /GLOBAL1/NSOIL

      REAL A(NSOIL)
      REAL B(NSOIL)
      REAL C(NSOIL)
      REAL D(NSOIL)
      REAL DELTA(NSOIL)
      REAL P(NSOIL)
C ----------------------------------------------------------------------
C INITIALIZE EQN COEF C FOR THE LOWEST SOIL LAYER
C ----------------------------------------------------------------------
      C(NSOIL) = 0.0
C ----------------------------------------------------------------------
C SOLVE THE COEFS FOR THE 1ST SOIL LAYER
C ----------------------------------------------------------------------
      P(1)     = -C(1) / B(1)
      DELTA(1) =  D(1) / B(1)
C ----------------------------------------------------------------------
C SOLVE THE COEFS FOR SOIL LAYERS 2 THRU NSOIL
C ----------------------------------------------------------------------
      DO K = 2,NSOIL
        P(K)     = -C(K) * ( 1.0 / (B(K) + A (K) * P(K-1)) )
        DELTA(K) = (D(K)-A(K)*DELTA(K-1))*(1.0/(B(K)+A(K)*P(K-1)))
      END DO
C ----------------------------------------------------------------------
C SET P TO DELTA FOR LOWEST SOIL LAYER
C ----------------------------------------------------------------------
      P(NSOIL) = DELTA(NSOIL)
C ----------------------------------------------------------------------
C ADJUST P FOR SOIL LAYERS 2 THRU NSOIL
C ----------------------------------------------------------------------
      DO K = 2,NSOIL
         KK    = NSOIL - K + 1
         P(KK) = P(KK) * P(KK + 1) + DELTA(KK)
      END DO
C ----------------------------------------------------------------------
C END SUBROUTINE ROSR12
C ----------------------------------------------------------------------
      RETURN
      END


      SUBROUTINE SFCDIF (ZLM,Z0,T1V,THLM,WIND_SPEED,CZIL,CM,CH)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C CALCULATE SURFACE LAYER EXCHANGE COEFFICIENTS VIA ITERATIVE PROCESS
C ----------------------------------------------------------------------
      REAL WWST2, G, VKRM, EXCM, BETA, BTG, ELFC, WOLD, WNEW
      REAL PIHF, EPSU2, EPSUST, ZTMIN, ZTMAX, HPBL, SQVISC
      REAL ZZ
      REAL XX, PSPMU, YY, PSPMS, PSPHU, PSPHS, ZLM, Z0, T1V, THLM
      REAL WIND_SPEED, CZIL, CM, CH, ZILFC, ZT, RDZ, CXCH
      REAL DTHV, DU2, BTGH, WSTAR2, USTAR, ZSLU, ZSLT, RLOGU, RLOGT
      REAL RLMO, ZETALT, ZETALU, ZETAU, ZETAT, XLU4, XLT4, XU4, XT4
      REAL XLU, XLT, XU, XT, PSMZ, SIMM, PSHZ, SIMH, USTARK, RLMN

      INTEGER ITRMX, ITR

      PARAMETER
     &     (WWST2=1.2**2,G=9.8,VKRM=0.40,EXCM=0.001
     &     ,BETA=1./270.,BTG=BETA*G,ELFC=VKRM*BTG
     &     ,WOLD=.15,WNEW=1.-WOLD,ITRMX=05,PIHF=3.14159265/2.)
C ----------------------------------------------------------------------
      PARAMETER
     &     (EPSU2=1.E-4,EPSUST=0.07
     &     ,ZTMIN=-5.,ZTMAX=1.,HPBL=1000.0
     &     ,SQVISC=258.2)
C ----------------------------------------------------------------------
C PAULSON'S SURFACE FUNCTIONS
C ----------------------------------------------------------------------
      PSPMU(XX) =-2.*log((XX+1.)*0.5)-log((XX*XX+1.)*0.5)+2.*ATAN(XX)
     &     -PIHF
      PSPMS(YY) =5.*YY
      PSPHU(XX) =-2.*log((XX*XX+1.)*0.5)
      PSPHS(YY) =5.*YY
C ----------------------------------------------------------------------
C     CZIL: CONSTANT C IN Zilitinkevich, S. S.1995,:NOTE ABOUT ZT
C ----------------------------------------------------------------------
      ZILFC = -CZIL * VKRM * SQVISC
C ----------------------------------------------------------------------
      RDZ  = 1. / ZLM
      CXCH = EXCM * RDZ
      DTHV = THLM - T1V
      DU2  = MAX(WIND_SPEED * WIND_SPEED, EPSU2)
C ----------------------------------------------------------------------
C BELJAARS CORRECTION OF USTAR
C ----------------------------------------------------------------------
      BTGH = BTG * HPBL
      WSTAR2 = WWST2 * ABS(BTGH * CH * DTHV)**(2./3.)
      USTAR = MAX(SQRT(CM * SQRT(DU2 + WSTAR2)), EPSUST)
C ----------------------------------------------------------------------
C ZILITINKEVITCH APPROACH FOR ZT
C ----------------------------------------------------------------------
      ZT    = EXP(ZILFC * SQRT(USTAR * Z0)) * Z0
C ----------------------------------------------------------------------
      ZSLU  = ZLM + Z0
      ZSLT  = ZLM + ZT

      RLOGU = log(ZSLU / Z0)
      RLOGT = log(ZSLT / ZT)

      RLMO  = ELFC * CH * DTHV / USTAR**3

      DO ITR=1,ITRMX
C ----------------------------------------------------------------------
C 1./MONIN-OBUKKHOV LENGTH-SCALE
C ----------------------------------------------------------------------
	  ZETALT = MAX(ZSLT * RLMO, ZTMIN)
	  RLMO   = ZETALT / ZSLT
	  ZETALU = ZSLU * RLMO
	  ZETAU  = Z0 * RLMO
	  ZETAT  = ZT * RLMO

	  IF(RLMO.LT.0.) THEN
	    XLU4=1.-16.*ZETALU
	    XLT4=1.-16.*ZETALT
	    XU4 =1.-16.*ZETAU
	    XT4 =1.-16.*ZETAT

	    XLU=SQRT(SQRT(XLU4))
	    XLT=SQRT(SQRT(XLT4))
	    XU =SQRT(SQRT(XU4))
	    XT =SQRT(SQRT(XT4))

	    PSMZ=PSPMU(XU)
	    SIMM=PSPMU(XLU)-PSMZ+RLOGU
	    PSHZ=PSPHU(XT)
	    SIMH=PSPHU(XLT)-PSHZ+RLOGT
	  ELSE
	    ZETALU=MIN(ZETALU,ZTMAX)
	    ZETALT=MIN(ZETALT,ZTMAX)
	    PSMZ=PSPMS(ZETAU)
	    SIMM=PSPMS(ZETALU)-PSMZ+RLOGU
	    PSHZ=PSPHS(ZETAT)
	    SIMH=PSPHS(ZETALT)-PSHZ+RLOGT
	  ENDIF
C ----------------------------------------------------------------------
C BELJAARS CORRECTION FOR USTAR
C ----------------------------------------------------------------------
	  USTAR = MAX(SQRT(CM * SQRT(DU2 + WSTAR2)), EPSUST)
C ----------------------------------------------------------------------
C ZILITINKEVITCH APPROACH FOR ZT
C ----------------------------------------------------------------------
      ZT    = EXP(ZILFC * SQRT(USTAR * Z0)) * Z0
C ----------------------------------------------------------------------
      ZSLT  = ZLM + ZT
      RLOGT = log(ZSLT / ZT)
C-----------------------------------------------------------------------
      USTARK = USTAR * VKRM
      CM = MAX(USTARK / SIMM, CXCH)
      CH = MAX(USTARK / SIMH, CXCH)
C-----------------------------------------------------------------------
C IF STATEMENTS TO AVOID TANGENT LINEAR PROBLEMS NEAR ZERO
C-----------------------------------------------------------------------
      IF (BTGH*CH*DTHV .NE. 0.0) THEN
        WSTAR2 = WWST2 * ABS(BTGH * CH * DTHV)**(2./3.)
      ELSE
        WSTAR2 = 0.0
      ENDIF
        RLMN = ELFC * CH * DTHV / USTAR**3
        RLMO = RLMO * WOLD + RLMN * WNEW
C-----------------------------------------------------------------------
      END DO
C ----------------------------------------------------------------------
C END SUBROUTINE SFCDIF
C ----------------------------------------------------------------------
      RETURN
      END


      SUBROUTINE SHFLX (SSOIL,STC,SMC,SMCMAX,TSKIN,DT,YY,ZZ1,ZSOIL,
     &                  ZBOT,SH2O,F1,DF1)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C UPDATE THE TEMPERATURE STATE OF THE SOIL COLUMN BASED ON THE THERMAL
C DIFFUSION EQUATION AND UPDATE THE FROZEN SOIL MOISTURE CONTENT BASED
C ON THE TEMPERATURE.
C ----------------------------------------------------------------------
      INTEGER I
      INTEGER IFRZ
      INTEGER NSOIL
      COMMON /GLOBAL1/NSOIL

      REAL AI(NSOIL)
      REAL BI(NSOIL)
      REAL CI(NSOIL)

      REAL DF1
      REAL DT
      REAL F1
      REAL RHSTS(NSOIL)
      REAL SSOIL
      REAL SH2O(NSOIL)
      REAL SMC(NSOIL)
      REAL SMCMAX
      REAL STC(NSOIL)
      REAL STCF(NSOIL)
      REAL TSKIN
      REAL YY
      REAL ZBOT
      REAL ZSOIL(NSOIL)
      REAL ZZ1
C ----------------------------------------------------------------------
C HRT ROUTINE CALCS THE RIGHT HAND SIDE OF THE SOIL TEMP DIF EQN
C ----------------------------------------------------------------------

      CALL HRT (RHSTS,STC,SMC,SMCMAX,ZSOIL,YY,ZZ1,
     &             ZBOT,SH2O,DT,F1,DF1,AI,BI,CI)
      CALL HSTEP (STCF,STC,RHSTS,DT,AI,BI,CI)

      DO I = 1,NSOIL
         STC(I) = STCF(I)
      END DO
C ----------------------------------------------------------------------
C IN THE NO SNOWPACK CASE (VIA ROUTINE NOPAC BRANCH,) UPDATE THE GRND
C (SKIN) TEMPERATURE HERE IN RESPONSE TO THE UPDATED SOIL TEMPERATURE
C PROFILE ABOVE.  (NOTE: INSPECTION OF ROUTINE SNOPAC SHOWS THAT TSKIN
C BELOW IS A DUMMY VARIABLE ONLY, AS SKIN TEMPERATURE IS UPDATED
C DIFFERENTLY IN ROUTINE SNOPAC)
C ----------------------------------------------------------------------
      TSKIN = (YY + (ZZ1 - 1.0) * STC(1)) / ZZ1
C ----------------------------------------------------------------------
C CALCULATE SURFACE SOIL HEAT FLUX
C ----------------------------------------------------------------------
      SSOIL = DF1 * (STC(1) - TSKIN) / (0.5 * ZSOIL(1))
C ----------------------------------------------------------------------
C END SUBROUTINE SHFLX
C ----------------------------------------------------------------------
      RETURN
      END


      SUBROUTINE SMFLX (SMC,DT,RAIN1,ZSOIL,SH2O,SLOPE,KDT,FRZFACT,
     &                  SMCMAX,SMCWLT,DKSAT,DWSAT,EDIR1,ET1)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C CALCULATE SOIL MOISTURE FLUX.  THE SOIL MOISTURE CONTENT (SMC - A PER
C UNIT VOLUME MEASUREMENT) IS A DEPENDENT VARIABLE THAT IS UPDATED WITH
C PROGNOSTIC EQNS.
C FROZEN GROUND VERSION:  NEW STATES ADDED: SH2O, AND FROZEN GROUND
C CORRECTION FACTOR, FRZFACT AND PARAMETER SLOPE.
C ----------------------------------------------------------------------
      INTEGER I
      INTEGER K
      INTEGER NSOIL
      COMMON /GLOBAL1/NSOIL

      REAL AI(NSOIL)
      REAL BI(NSOIL)
      REAL CI(NSOIL)

      REAL DKSAT
      REAL DWSAT
      REAL DT
      REAL EDIR1
      REAL ET1(NSOIL)
      REAL FRZFACT
      REAL KDT
      REAL RAIN1
      REAL RHSTT(NSOIL)
      REAL SMC(NSOIL)
      REAL SH2O(NSOIL)
      REAL SICE(NSOIL)
      REAL SH2OA(NSOIL)
      REAL SH2OFG(NSOIL)
      REAL SLOPE
      REAL SMCMAX
      REAL SMCWLT
      REAL ZSOIL(NSOIL)
C ----------------------------------------------------------------------
C STORE ICE CONTENT AT EACH SOIL LAYER BEFORE CALLING SRT & SSTEP
C ----------------------------------------------------------------------
      DO I = 1,NSOIL
        SICE(I) = SMC(I) - SH2O(I)
      END DO
C ----------------------------------------------------------------------
C CALL SUBROUTINES SRT AND SSTEP TO SOLVE THE SOIL MOISTURE
C TENDENCY EQUATIONS.
C
C IF THE INFILTRATING PRECIP RATE IS NONTRIVIAL,
C   (WE CONSIDER NONTRIVIAL TO BE A PRECIP TOTAL OVER THE TIME STEP
C    EXCEEDING ONE ONE-THOUSANDTH OF THE WATER HOLDING CAPACITY OF
C    THE FIRST SOIL LAYER)
C THEN CALL THE SRT/SSTEP SUBROUTINE PAIR TWICE IN THE MANNER OF
C   TIME SCHEME "F" (IMPLICIT STATE, AVERAGED COEFFICIENT)
C   OF SECTION 2 OF KALNAY AND KANAMITSU (1988, MWR, VOL 116,
C   PAGES 1945-1958)TO MINIMIZE 2-DELTA-T OSCILLATIONS IN THE
C   SOIL MOISTURE VALUE OF THE TOP SOIL LAYER THAT CAN ARISE BECAUSE
C   OF THE EXTREME NONLINEAR DEPENDENCE OF THE SOIL HYDRAULIC
C   DIFFUSIVITY COEFFICIENT AND THE HYDRAULIC CONDUCTIVITY ON THE
C   SOIL MOISTURE STATE
C OTHERWISE CALL THE SRT/SSTEP SUBROUTINE PAIR ONCE IN THE MANNER OF
C   TIME SCHEME "D" (IMPLICIT STATE, EXPLICIT COEFFICIENT)
C   OF SECTION 2 OF KALNAY AND KANAMITSU
C RAIN1 IS UNITS OF KG/M**2/S OR MM/S, ZSOIL IS NEGATIVE DEPTH IN M
C ----------------------------------------------------------------------
      IF ((RAIN1*DT) .GT. (0.001*1000.0*(-ZSOIL(1))*SMCMAX)) THEN
C ----------------------------------------------------------------------
C FROZEN GROUND VERSION:
C SMC STATES REPLACED BY SH2O STATES IN SRT SUBR.  SH2O & SICE STATES
C INCLUDED IN SSTEP SUBR.  FROZEN GROUND CORRECTION FACTOR, FRZFACT
C ADDED.  ALL WATER BALANCE CALCULATIONS USING UNFROZEN WATER
C ----------------------------------------------------------------------
        CALL SRT (RHSTT,EDIR1,ET1,SH2O,SH2O,RAIN1,ZSOIL,
     &            DWSAT,DKSAT,SMCMAX,
     &            DT,SMCWLT,SLOPE,KDT,FRZFACT,SICE,AI,BI,CI)

        CALL SSTEP (SH2OFG,SH2O,RHSTT,DT,SMCMAX,
     &              ZSOIL,SMC,SICE,AI,BI,CI)

        DO K = 1,NSOIL
          SH2OA(K) = (SH2O(K) + SH2OFG(K)) * 0.5
        END DO

        CALL SRT (RHSTT,EDIR1,ET1,SH2O,SH2OA,RAIN1,ZSOIL,
     &            DWSAT,DKSAT,SMCMAX,
     &            DT,SMCWLT,SLOPE,KDT,FRZFACT,SICE,AI,BI,CI)

        CALL SSTEP (SH2O,SH2O,RHSTT,DT,SMCMAX,
     &              ZSOIL,SMC,SICE,AI,BI,CI)

      ELSE

        CALL SRT (RHSTT,EDIR1,ET1,SH2O,SH2O,RAIN1,ZSOIL,
     &            DWSAT,DKSAT,SMCMAX,
     &            DT,SMCWLT,SLOPE,KDT,FRZFACT,SICE,AI,BI,CI)

        CALL SSTEP (SH2O,SH2O,RHSTT,DT,SMCMAX,
     &              ZSOIL,SMC,SICE,AI,BI,CI)

      ENDIF
C ----------------------------------------------------------------------
C END SUBROUTINE SMFLX
C ----------------------------------------------------------------------
      RETURN
      END


      SUBROUTINE SNFRAC (SNEQV,SNUP,SALP,SNCOVR)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C CALCULATE SNOW FRACTION (0 -> 1)
C SNEQV   SNOW WATER EQUIVALENT (M)
C SNUP    THRESHOLD SNEQV DEPTH ABOVE WHICH SNCOVR=1
C SALP    TUNING PARAMETER
C SNCOVR  FRACTIONAL SNOW COVER
C ----------------------------------------------------------------------
      REAL SNEQV, SNUP, SALP, SNCOVR, RSNOW
C ----------------------------------------------------------------------
C SNUP IS VEG-CLASS DEPENDENT SNOWDEPTH THRESHHOLD (SET IN ROUTINE
C REDPRM) ABOVE WHICH SNOCVR=1.
C ----------------------------------------------------------------------
          IF (SNEQV .LT. SNUP) THEN
            RSNOW  = SNEQV / SNUP
            SNCOVR = 1. - ( EXP(-SALP * RSNOW) - RSNOW * EXP(-SALP))
          ELSE
            SNCOVR = 1.0
          ENDIF
C ----------------------------------------------------------------------
C END SUBROUTINE SNFRAC
C ----------------------------------------------------------------------
      RETURN
      END


      FUNCTION SNKSRC (SMC,SH2O,ZSOIL,DT,K,QTOT)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C CALCULATE SINK/SOURCE TERM OF THE TERMAL DIFFUSION EQUATION. (SH2O) IS
C AVAILABLE LIQUID WATER.
C ----------------------------------------------------------------------
      INTEGER K
      INTEGER NSOIL
      COMMON /GLOBAL1/NSOIL

      REAL DH2O
      REAL DT
      REAL DZ
      REAL FREE
      REAL HLICE
      REAL QTOT
      REAL SH2O
      REAL SMC
      REAL SNKSRC
      REAL XH2O
      REAL ZSOIL(NSOIL)

      PARAMETER(DH2O  = 1.0000E3)
      PARAMETER(HLICE = 3.3350E5)

      REAL FRZ_T
      COMMON /GLOBAL2/FRZ_T

      DZ = ZSOIL(K-1) - ZSOIL(K)
C ----------------------------------------------------------------------
C IN NEXT BLOCK OF CODE, INVOKE EQN 18 OF V. KOREN ET AL (1999, JGR,
C VOL. 104, PG 19573.)  THAT IS, FIRST ESTIMATE THE NEW AMOUNT OF LIQUID
C WATER, 'XH2O', IMPLIED BY THE SUM OF (1) THE LIQUID WATER AT THE BEGIN
C OF CURRENT TIME STEP, AND (2) THE FREEZE OF THAW CHANGE IN LIQUID
C WATER IMPLIED BY THE HEAT FLUX 'QTOT' PASSED IN FROM ROUTINE HRT.
C SECOND, DETERMINE IF XH2O NEEDS TO BE BOUNDED BY 'FREE' (EQUIL AMT) OR
C IF 'FREE' NEEDS TO BE BOUNDED BY XH2O.
C ----------------------------------------------------------------------
      XH2O = SH2O + QTOT * DT / (DH2O * HLICE * DZ)
C ----------------------------------------------------------------------
C FIRST, IF FREEZING AND REMAINING LIQUID LESS THAN LOWER BOUND, THEN
C REDUCE EXTENT OF FREEZING, THEREBY LETTING SOME OR ALL OF HEAT FLUX
C QTOT COOL THE SOIL TEMP LATER IN ROUTINE HRT.
C ----------------------------------------------------------------------
      IF (XH2O .LT. SH2O .AND. XH2O .LT. FREE) THEN
        IF (FREE .GT. SH2O) THEN
          XH2O = SH2O
        ELSE
          XH2O = FREE
        ENDIF
      ENDIF
C ----------------------------------------------------------------------
C SECOND, IF THAWING AND THE INCREASE IN LIQUID WATER GREATER THAN UPPER
C BOUND, THEN REDUCE EXTENT OF THAW, THEREBY LETTING SOME OR ALL OF HEAT
C FLUX QTOT WARM THE SOIL TEMP LATER IN ROUTINE HRT.
C ----------------------------------------------------------------------
      IF (XH2O .GT. SH2O .AND. XH2O .GT. FREE)  THEN
        IF (FREE .LT. SH2O) THEN
          XH2O = SH2O
        ELSE
          XH2O = FREE
        ENDIF
      ENDIF

      IF (XH2O .LT. 0.0) XH2O = 0.
      IF (XH2O .GT. SMC) XH2O = SMC
C ----------------------------------------------------------------------
C CALCULATE PHASE-CHANGE HEAT SOURCE/SINK TERM FOR USE IN ROUTINE HRT
C AND UPDATE LIQUID WATER TO REFLCET FINAL FREEZE/THAW INCREMENT.
C ----------------------------------------------------------------------
      SNKSRC = -DH2O * HLICE * DZ * (XH2O-SH2O) / DT
      SH2O   = XH2O
C ----------------------------------------------------------------------
C END FUNCTION SNKSRC
C ----------------------------------------------------------------------
77    RETURN
      END


      SUBROUTINE SNOPAC (ETP,ETA,RAIN,RAIN1,SNOWNG,SMC,SMCMAX,SMCWLT,
     &                   DT,DF1,Q2,TSKIN,SFC_TEMP,T24,
     &                   TH2,FDOWN,F1,SSOIL,STC,EPSCA,SFC_PRESS,
     &                   RCH,RR,SNCOVR,ESD,SNDENS,SNOWH,
     &                   SH2O,SLOPE,KDT,FRZFACT,SNUP,ZSOIL,
     &                   DWSAT,DKSAT,ZBOT,EDIR,ET,SNOMLT,
     &                   FXEXP,BETA,DEW,FLX1,FLX2,FLX3,ESNOW)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C CALCULATE SOIL MOISTURE AND HEAT FLUX VALUES & UPDATE SOIL MOISTURE
C CONTENT AND SOIL HEAT CONTENT VALUES FOR THE CASE WHEN A SNOW PACK IS
C PRESENT
C ----------------------------------------------------------------------
      INTEGER NSOIL
      COMMON /GLOBAL1/NSOIL

      LOGICAL SNOWNG

      REAL BETA
      REAL CP
      REAL CW
      REAL CPICE
      REAL DENOM
      REAL DEW
      REAL DF1
      REAL DKSAT
      REAL DSOIL
      REAL DTOT
      REAL DT
      REAL DWSAT
      REAL EDIR
      REAL EPSCA
      REAL ESD
      REAL ESDMIN
      REAL EXPSNO
      REAL EXPSOI
      REAL ETA
      REAL ETA1
      REAL ETP
      REAL ETP1
      REAL ETP2
      REAL ET(NSOIL)
      REAL EX
      REAL EXPFAC
      REAL FDOWN
      REAL FXEXP
      REAL FLX1
      REAL FLX2
      REAL FLX3
      REAL F1
      REAL KDT
      REAL LSUBF
      REAL LVH2O
      REAL LSUBS
      REAL RAIN
      REAL RAIN1
      REAL Q2
      REAL RCH
      REAL RR
      REAL SSOIL
      REAL SSOIL1
      REAL SFC_TEMP
      REAL SIGMA
      REAL SMC(NSOIL)
      REAL SH2O(NSOIL)
      REAL SMCMAX
      REAL SMCWLT
      REAL SNOMLT
      REAL SNOWH
      REAL STC(NSOIL)
      REAL TSKIN
      REAL T11
      REAL T12
      REAL T12A
      REAL T12B
      REAL T24
      REAL ZBOT
      REAL TH2
      REAL YY
      REAL ZSOIL(NSOIL)
      REAL ZZ1
      REAL SALP
      REAL SFC_PRESS
      REAL SLOPE
      REAL FRZFACT
      REAL SNUP
      REAL SNDENS
      REAL SNCOND
      REAL RSNOW
      REAL SNCOVR
      REAL QSAT
      REAL ETP3
      REAL SEH
      REAL T14
      REAL CSNOW

      REAL EDIR1
      REAL ET1(NSOIL)

      REAL ETNS
      REAL ETNS1
      REAL ESNOW
      REAL ESNOW1
      REAL ESNOW2
      REAL ETANRG

      INTEGER K

      REAL SNOEXP

      PARAMETER(CP     = 1004.5)
      PARAMETER(CW     = 4187.)
      PARAMETER(CPICE  = 2.106E+3)
      PARAMETER(ESDMIN = 1.E-6)
      PARAMETER(LSUBF  = 3.335E+5)
      PARAMETER(LVH2O  = 2.501E+6)
      PARAMETER(LSUBS  = 2.83E+6)
      PARAMETER(SIGMA  = 5.67E-8)

      REAL FRZ_T
      COMMON /GLOBAL2/FRZ_T

      DATA SNOEXP /2.0/

      REAL EMISSIVITY
      COMMON /EMISSIVITY/ EMISSIVITY
C ----------------------------------------------------------------------
C CONVERT POTENTIAL EVAP (ETP) FROM KG M-2 S-1 TO M S-1 AND THEN TO AN
C AMOUNT (M) GIVEN TIMESTEP (DT) AND CALL IT AN EFFECTIVE SNOWPACK
C REDUCTION AMOUNT, ETP2 (M).  THIS IS THE AMOUNT THE SNOWPACK WOULD BE
C REDUCED DUE TO EVAPORATION FROM THE SNOW SFC DURING THE TIMESTEP.
C EVAPORATION WILL PROCEED AT THE POTENTIAL RATE UNLESS THE SNOW DEPTH
C IS LESS THAN THE EXPECTED SNOWPACK REDUCTION.
C ----------------------------------------------------------------------
      RAIN1 = RAIN1 * 0.001

      BETA = 1.0
      IF (ESD .LT. ETP2) THEN
      ENDIF
C ----------------------------------------------------------------------
      EDIR   = 0.0
      EDIR1  = 0.0
      DO K = 1,NSOIL
        ET(K)  = 0.0
        ET1(K) = 0.0
      ENDDO
      ETNS   = 0.0
      ETNS1  = 0.0
      ESNOW  = 0.0
      ESNOW1 = 0.0
      ESNOW2 = 0.0
C ----------------------------------------------------------------------
C IF ETP<0 (DOWNWARD) THEN DEWFALL (=FROSTFALL IN THIS CASE).
C ----------------------------------------------------------------------
      DEW = 0.0
      ETP1 = ETP * 0.001
      IF (ETP .LT. 0.0) THEN
        DEW    = -ETP1
        ESNOW2 = ETP1 * DT
        ETANRG = ETP  * ((1. - SNCOVR) * LVH2O + SNCOVR * LSUBS)
      ELSE
C ----------------------------------------------------------------------
	IF (SNCOVR .LT. 1.) THEN
	  CALL EVAPO (ETNS1,ETP1,SH2O,SMCMAX,SMCWLT,EDIR1,ET1,FXEXP)
C ----------------------------------------------------------------------
	  EDIR1 = EDIR1 * (1. - SNCOVR)
	  DO K = 1,NSOIL
	    ET1(K) = ET1(K) * (1. - SNCOVR)
	  END DO
	  ETNS1 = ETNS1*(1.-SNCOVR)
C ----------------------------------------------------------------------
	  EDIR = EDIR1 * 1000.0
	  DO K = 1,NSOIL
	    ET(K) = ET1(K) * 1000.0
	  END DO
	  ETNS = ETNS1 * 1000.0
C ----------------------------------------------------------------------
	ENDIF
	ESNOW  = ETP * SNCOVR
	ESNOW1 = ESNOW  * 0.001
	ESNOW2 = ESNOW1 * DT
	ETANRG = ESNOW  * LSUBS + ETNS * LVH2O
      ENDIF
C ----------------------------------------------------------------------
C IF PRECIP IS FALLING, CALCULATE HEAT FLUX FROM SNOW SFC TO NEWLY
C ACCUMULATING PRECIP.  NOTE THAT THIS REFLECTS THE FLUX APPROPRIATE FOR
C THE NOT-YET-UPDATED SKIN TEMPERATURE (TSKIN). ASSUMES TEMPERATURE OF THE
C SNOWFALL STRIKING THE GOUND IS =SFC_TEMP (LOWEST MODEL LEVEL AIR TEMP).
C ----------------------------------------------------------------------
      FLX1 = 0.0
      IF (SNOWNG) THEN
        FLX1 = CPICE * RAIN * (TSKIN - SFC_TEMP)
      ELSE
        IF (RAIN .GT. 0.0) FLX1 = CW * RAIN * (TSKIN - SFC_TEMP)
      ENDIF
C ----------------------------------------------------------------------
C CALCULATE AN 'EFFECTIVE SNOW-GRND SFC TEMP' (T12) BASED ON HEAT FLUXES
C BETWEEN THE SNOW PACK AND THE SOIL AND ON NET RADIATION.
C INCLUDE FLX1 (PRECIP-SNOW SFC) AND FLX2 (FREEZING RAIN LATENT HEAT)
C FLUXES.
C FLX2 REFLECTS FREEZING RAIN LATENT HEAT FLUX USING TSKIN CALCULATED IN
C PENMAN.
C ----------------------------------------------------------------------
      DSOIL = -(0.5 * ZSOIL(1))
      DTOT  = SNOWH + DSOIL
      DENOM = 1.0 + DF1 / (DTOT * RR * RCH)
      T12A  = ((FDOWN - FLX1 - FLX2 - SIGMA * T24 * EMISSIVITY) / RCH
     &       + TH2 - SFC_TEMP - ETANRG / RCH ) / RR
      T12B  = DF1 * STC(1) / (DTOT * RR * RCH)
      T12   = (SFC_TEMP + T12A + T12B) / DENOM
C ----------------------------------------------------------------------
C IF THE 'EFFECTIVE SNOW-GRND SFC TEMP' IS AT OR BELOW FREEZING, NO SNOW
C MELT WILL OCCUR.  SET THE SKIN TEMP TO THIS EFFECTIVE TEMP.  REDUCE
C (BY SUBLIMINATION ) OR INCREASE (BY FROST) THE DEPTH OF THE SNOWPACK,
C DEPENDING ON SIGN OF ETP.
C UPDATE SOIL HEAT FLUX (SSOIL) USING NEW SKIN TEMPERATURE (TSKIN)
C SINCE NO SNOWMELT, SET ACCUMULATED SNOWMELT TO ZERO, SET 'EFFECTIVE'
C PRECIP FROM SNOWMELT TO ZERO, SET PHASE-CHANGE HEAT FLUX FROM SNOWMELT
C TO ZERO.
C ----------------------------------------------------------------------
      IF (T12 .LE. FRZ_T) THEN
        TSKIN  = T12
        SSOIL  = DF1 * (TSKIN - STC(1)) / DTOT
        ESD    = MAX(0.0, ESD - ESNOW2)
        FLX3   = 0.0
        EX     = 0.0
        SNOMLT = 0.0
      ELSE
C ----------------------------------------------------------------------
C IF THE 'EFFECTIVE SNOW-GRND SFC TEMP' IS ABOVE FREEZING, SNOW MELT
C WILL OCCUR.  CALL THE SNOW MELT RATE,EX AND AMT, SNOMLT.  REVISE THE
C EFFECTIVE SNOW DEPTH.  REVISE THE SKIN TEMP BECAUSE IT WOULD HAVE CHGD
C DUE TO THE LATENT HEAT RELEASED BY THE MELTING. CALC THE LATENT HEAT
C RELEASED, FLX3. SET THE EFFECTIVE PRECIP, RAIN1 TO THE SNOW MELT RATE,
C EX FOR USE IN SMFLX.  ADJUSTMENT TO TSKIN TO ACCOUNT FOR SNOW PATCHES.
C CALCULATE QSAT VALID AT FREEZING POINT.  NOTE THAT ESAT (SATURATION
C VAPOR PRESSURE) VALUE OF 6.11E+2 USED HERE IS THAT VALID AT FRZZING
C POINT.  NOTE THAT ETP FROM CALL PENMAN IN SFLX IS IGNORED HERE IN
C FAVOR OF BULK ETP OVER 'OPEN WATER' AT FREEZING TEMP.
C UPDATE SOIL HEAT FLUX (S) USING NEW SKIN TEMPERATURE (TSKIN)
C ----------------------------------------------------------------------
        TSKIN = FRZ_T * SNCOVR**SNOEXP + T12 * (1. - SNCOVR**SNOEXP)
        BETA  = 1.
        SSOIL = DF1 * (TSKIN - STC(1)) / DTOT
C ----------------------------------------------------------------------
C IF POTENTIAL EVAP (SUBLIMATION) GREATER THAN DEPTH OF SNOWPACK.
C BETA<1
C SNOWPACK HAS SUBLIMATED AWAY, SET DEPTH TO ZERO.
C ----------------------------------------------------------------------
        IF (ESD - ESNOW2 .LE. ESDMIN) THEN
          ESD    = 0.0
          EX     = 0.0
          SNOMLT = 0.0
        ELSE
C ----------------------------------------------------------------------
C POTENTIAL EVAP (SUBLIMATION) LESS THAN DEPTH OF SNOWPACK, RETAIN
C   BETA=1.
C SNOWPACK (ESD) REDUCED BY POTENTIAL EVAP RATE
C ETP3 (CONVERT TO FLUX)
C ----------------------------------------------------------------------
          ESD  = ESD - ESNOW2
          SEH  = RCH * (TSKIN - TH2)
          T14  = TSKIN * TSKIN * TSKIN * TSKIN
          FLX3 = FDOWN - FLX1 - FLX2 - SIGMA * T14 *
     &             EMISSIVITY - SSOIL - SEH - ETANRG
          IF (FLX3 .LE .0.0) FLX3 = 0.0
          EX   = FLX3 * 0.001 / LSUBF
C ----------------------------------------------------------------------
C SNOWMELT REDUCTION DEPENDING ON SNOW COVER
C IF SNOW COVER LESS THAN 5% NO SNOWMELT REDUCTION
C ***NOTE:  DOES 'IF' BELOW FAIL TO MATCH THE MELT WATER WITH THE MELT
C           ENERGY?
C ----------------------------------------------------------------------
          SNOMLT = EX * DT
C ----------------------------------------------------------------------
C ESDMIN REPRESENTS A SNOWPACK DEPTH THRESHOLD VALUE BELOW WHICH WE
C CHOOSE NOT TO RETAIN ANY SNOWPACK, AND INSTEAD INCLUDE IT IN SNOWMELT.
C ----------------------------------------------------------------------
          IF (ESD - SNOMLT .GE. ESDMIN) THEN
            ESD = ESD - SNOMLT
          ELSE
C ----------------------------------------------------------------------
C SNOWMELT EXCEEDS SNOW DEPTH
C ----------------------------------------------------------------------
            EX     = ESD / DT
            FLX3   = EX * 1000.0 * LSUBF
            SNOMLT = ESD
            ESD    = 0.0
          ENDIF
C ----------------------------------------------------------------------
C END OF 'ESD .LE. ETP2' IF-BLOCK
C ----------------------------------------------------------------------
        ENDIF
        RAIN1 = RAIN1 + EX
C ----------------------------------------------------------------------
C END OF 'T12 .LE. FRZ_T' IF-BLOCK
C ----------------------------------------------------------------------
      ENDIF
C ----------------------------------------------------------------------
C SET THE EFFECTIVE POTNL EVAPOTRANSP (ETP1) TO ZERO SINCE THIS IS SNOW
C CASE, SO SURFACE EVAP NOT CALCULATED FROM EDIR IN SMFLX
C (BELOW).
C SMFLX RETURNS UPDATED SOIL MOISTURE VALUES.  IN THIS, THE SNOW PACK
C CASE, ETA1 IS NOT USED IN CALCULATION OF EVAP.
C ----------------------------------------------------------------------
        CALL SMFLX (SMC,DT,RAIN1,ZSOIL,SH2O,SLOPE,KDT,FRZFACT,
     &              SMCMAX,SMCWLT,DKSAT,DWSAT,EDIR1,ET1)
C ----------------------------------------------------------------------
C BEFORE CALL SHFLX IN THIS SNOWPACK CASE, SET ZZ1 AND YY ARGUMENTS TO
C SPECIAL VALUES THAT ENSURE THAT GROUND HEAT FLUX CALCULATED IN SHFLX
C MATCHES THAT ALREADY COMPUTER FOR BELOW THE SNOWPACK, THUS THE SFC
C HEAT FLUX TO BE COMPUTED IN SHFLX WILL EFFECTIVELY BE THE FLUX AT THE
C SNOW TOP SURFACE.  T11 IS A DUMMY ARGUEMENT SO WE WILL NOT USE THE
C SKIN TEMP VALUE AS REVISED BY SHFLX.
C ----------------------------------------------------------------------
      ZZ1 = 1.0
      YY  = STC(1) - 0.5 * SSOIL * ZSOIL(1) * ZZ1 / DF1
      T11 = TSKIN
C ----------------------------------------------------------------------
C SHFLX WILL CALC/UPDATE THE SOIL TEMPS.  NOTE:  THE SUB-SFC HEAT FLUX
C (SSOIL1) AND THE SKIN TEMP (T11) OUTPUT FROM THIS SHFLX CALL ARE NOT
C USED  IN ANY SUBSEQUENT CALCULATIONS. RATHER, THEY ARE DUMMY VARIABLES
C HERE IN THE SNOPAC CASE, SINCE THE SKIN TEMP AND SUB-SFC HEAT FLUX ARE
C UPDATED INSTEAD NEAR THE BEGINNING OF THE CALL TO SNOPAC.
C ----------------------------------------------------------------------
      CALL SHFLX (SSOIL1,STC,SMC,SMCMAX,T11,DT,YY,ZZ1,ZSOIL,
     &            ZBOT,SH2O,F1,DF1)
C ----------------------------------------------------------------------
C SNOW DEPTH AND DENSITY ADJUSTMENT BASED ON SNOW COMPACTION.  YY IS
C ASSUMED TO BE THE SOIL TEMPERTURE AT THE TOP OF THE SOIL COLUMN.
C ----------------------------------------------------------------------
      IF (ESD .GT. 0.) THEN
        CALL SNOWPACK (ESD,DT,SNOWH,SNDENS,TSKIN,YY)
      ELSE
        ESD    = 0.
        SNOWH  = 0.
        SNDENS = 0.
        SNCOND = 1.
        SNCOVR = 0.
      ENDIF
C ----------------------------------------------------------------------
C END SUBROUTINE SNOPAC
C ----------------------------------------------------------------------
      RETURN
      END


      SUBROUTINE SNOWPACK (ESD,DTSEC,SNOWH,SNDENS,TSNOW,TSOIL)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C CALCULATE COMPACTION OF SNOWPACK UNDER CONDITIONS OF INCREASING SNOW
C DENSITY, AS OBTAINED FROM AN APPROXIMATE SOLUTION OF E. ANDERSON'S
C DIFFERENTIAL EQUATION (3.29), NOAA TECHNICAL REPORT NWS 19, BY VICTOR
C KOREN, 03/25/95.
C ----------------------------------------------------------------------
C ESD     WATER EQUIVALENT OF SNOW (M)
C DTSEC   TIME STEP (SEC)
C SNOWH   SNOW DEPTH (M)
C SNDENS  SNOW DENSITY (G/CM3=DIMENSIONLESS FRACTION OF H2O DENSITY)
C TSNOW   SNOW SURFACE TEMPERATURE (K)
C TSOIL   SOIL SURFACE TEMPERATURE (K)
C
C SUBROUTINE WILL RETURN NEW VALUES OF SNOWH AND SNDENS
C ----------------------------------------------------------------------
      INTEGER IPOL, J

      REAL BFAC,C1,C2,SNDENS,DSX,DTHR,DTSEC,DW,SNOWHC,SNOWH,PEXP,TAVGC,
     &     TSNOW,TSNOWC,TSOIL,TSOILC,ESD,ESDC,ESDCX

      PARAMETER(C1 = 0.01, C2 = 21.0)
C ----------------------------------------------------------------------
C CONVERSION INTO SIMULATION UNITS
C ----------------------------------------------------------------------
      SNOWHC = SNOWH * 100.
      ESDC   = ESD * 100.
      DTHR   = DTSEC / 3600.
      TSNOWC = TSNOW - 273.15
      TSOILC = TSOIL - 273.15
C ----------------------------------------------------------------------
C CALCULATING OF AVERAGE TEMPERATURE OF SNOW PACK
C ----------------------------------------------------------------------
      TAVGC = 0.5 * (TSNOWC + TSOILC)
C ----------------------------------------------------------------------
C CALCULATING OF SNOW DEPTH AND DENSITY AS A RESULT OF COMPACTION
C  SNDENS=DS0*(EXP(BFAC*ESD)-1.)/(BFAC*ESD)
C  BFAC=DTHR*C1*EXP(0.08*TAVGC-C2*DS0)
C NOTE: BFAC*ESD IN SNDENS EQN ABOVE HAS TO BE CAREFULLY TREATED
C NUMERICALLY BELOW:
C   C1 IS THE FRACTIONAL INCREASE IN DENSITY (1/(CM*HR))
C   C2 IS A CONSTANT (CM3/G) KOJIMA ESTIMATED AS 21 CMS/G
C ----------------------------------------------------------------------
      IF (ESDC .GT. 1.E-2) THEN
        ESDCX = ESDC
      ELSE
        ESDCX = 1.E-2
      ENDIF
      BFAC = DTHR * C1 * EXP(0.08 * TAVGC - C2 * SNDENS)
C ----------------------------------------------------------------------
C THE FUNCTION OF THE FORM (e**x-1)/x IMBEDDED IN ABOVE EXPRESSION
C FOR DSX WAS CAUSING NUMERICAL DIFFICULTIES WHEN THE DENOMINATOR "x"
C (I.E. BFAC*ESDC) BECAME ZERO OR APPROACHED ZERO (DESPITE THE FACT THAT
C THE ANALYTICAL FUNCTION (e**x-1)/x HAS A WELL DEFINED LIMIT AS
C "x" APPROACHES ZERO), HENCE BELOW WE REPLACE THE (e**x-1)/x
C EXPRESSION WITH AN EQUIVALENT, NUMERICALLY WELL-BEHAVED
C POLYNOMIAL EXPANSION.
C
C NUMBER OF TERMS OF POLYNOMIAL EXPANSION, AND HENCE ITS ACCURACY,
C IS GOVERNED BY ITERATION LIMIT "IPOL".
C      IPOL GREATER THAN 9 ONLY MAKES A DIFFERENCE ON DOUBLE
C            PRECISION (RELATIVE ERRORS GIVEN IN PERCENT %).
C       IPOL=9, FOR REL.ERROR <~ 1.6 E-6 % (8 SIGNIFICANT DIGITS)
C       IPOL=8, FOR REL.ERROR <~ 1.8 E-5 % (7 SIGNIFICANT DIGITS)
C       IPOL=7, FOR REL.ERROR <~ 1.8 E-4 % ...
C ----------------------------------------------------------------------
      IPOL = 4
      PEXP = 0.
      DO J = IPOL,1,-1
        PEXP = (1. + PEXP) * BFAC * ESDCX / REAL(J + 1)
      END DO
      PEXP = PEXP + 1.

      DSX = SNDENS * PEXP
C ----------------------------------------------------------------------
C SET UPPER/LOWER LIMIT ON SNOW DENSITY
C ----------------------------------------------------------------------
      IF (DSX .GT. 0.40) DSX = 0.40
      IF (DSX .LT. 0.05) DSX = 0.05
      SNDENS = DSX
C ----------------------------------------------------------------------
C UPDATE OF SNOW DEPTH AND DENSITY DEPENDING ON LIQUID WATER DURING
C SNOWMELT.  ASSUMED THAT 13% OF LIQUID WATER CAN BE STORED IN SNOW PER
C DAY DURING SNOWMELT TILL SNOW DENSITY 0.40.
C ----------------------------------------------------------------------
      IF (TSNOWC .GE. 0.) THEN
        DW     = 0.13 * DTHR / 24.
        SNDENS = SNDENS * (1. - DW) + DW
        IF (SNDENS .GT. 0.40) SNDENS = 0.40
      ENDIF
C ----------------------------------------------------------------------
C CALCULATE SNOW DEPTH (CM) FROM SNOW WATER EQUIVALENT AND SNOW DENSITY.
C CHANGE SNOW DEPTH UNITS TO METERS
C ----------------------------------------------------------------------
      SNOWHC = ESDC / SNDENS
      SNOWH  = SNOWHC * 0.01
C ----------------------------------------------------------------------
C END SUBROUTINE SNOWPACK
C ----------------------------------------------------------------------
      RETURN
      END


      SUBROUTINE SNOWZ0 (SNCOVR,Z0)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C CALCULATE TOTAL ROUGHNESS LENGTH OVER SNOW
C SNCOVR  FRACTIONAL SNOW COVER
C Z0      ROUGHNESS LENGTH (m)
C ----------------------------------------------------------------------
      REAL SNCOVR, Z0
      Z0 = (1 - SNCOVR) * Z0 + SNCOVR * Z0
C ----------------------------------------------------------------------
C END SUBROUTINE SNOWZ0
C ----------------------------------------------------------------------
      RETURN
      END


      SUBROUTINE SNOW_NEW (TEMP,NEWSN,SNOWH,SNDENS)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C CALCULATE SNOW DEPTH AND DENSITITY TO ACCOUNT FOR THE NEW SNOWFALL.
C NEW VALUES OF SNOW DEPTH & DENSITY RETURNED.
C
C TEMP    AIR TEMPERATURE (K)
C NEWSN   NEW SNOWFALL (M)
C SNOWH   SNOW DEPTH (M)
C SNDENS  SNOW DENSITY (G/CM3=DIMENSIONLESS FRACTION OF H2O DENSITY)
C ----------------------------------------------------------------------
      REAL SNDENS
      REAL DSNEW
      REAL SNOWHC
      REAL HNEWC
      REAL SNOWH
      REAL NEWSN
      REAL NEWSNC
      REAL TEMP
      REAL TEMPC
C ----------------------------------------------------------------------
C CONVERSION INTO SIMULATION UNITS
C ----------------------------------------------------------------------
      SNOWHC = SNOWH * 100.
      NEWSNC = NEWSN * 100.
      TEMPC  = TEMP - 273.15
C ----------------------------------------------------------------------
C CALCULATING NEW SNOWFALL DENSITY DEPENDING ON TEMPERATURE
C EQUATION FROM GOTTLIB L. 'A GENERAL RUNOFF MODEL FOR SNOWCOVERED
C AND GLACIERIZED BASIN', 6TH NORDIC HYDROLOGICAL CONFERENCE,
C VEMADOLEN, SWEDEN, 1980, 172-177PP.
C-----------------------------------------------------------------------
      IF (TEMPC .LE. -15.) THEN
        DSNEW = 0.05
      ELSE
        DSNEW = 0.05 + 0.0017 * (TEMPC + 15.)**1.5
      ENDIF
C ----------------------------------------------------------------------
C ADJUSTMENT OF SNOW DENSITY DEPENDING ON NEW SNOWFALL
C ----------------------------------------------------------------------
      HNEWC  = NEWSNC / DSNEW
      SNDENS = (SNOWHC * SNDENS + HNEWC * DSNEW) / (SNOWHC + HNEWC)
      SNOWHC = SNOWHC + HNEWC
      SNOWH  = SNOWHC * 0.01
C ----------------------------------------------------------------------
C END SUBROUTINE SNOW_NEW
C ----------------------------------------------------------------------
      RETURN
      END


      SUBROUTINE SRT (RHSTT,EDIR,ET,SH2O,SH2OA,RAIN1,
     &                ZSOIL,DWSAT,DKSAT,SMCMAX,
     &                DT,SMCWLT,SLOPE,KDT,FRZX,SICE,AI,BI,CI)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C CALCULATE THE RIGHT HAND SIDE OF THE TIME TENDENCY TERM OF THE SOIL
C WATER DIFFUSION EQUATION.  ALSO TO COMPUTE ( PREPARE ) THE MATRIX
C COEFFICIENTS FOR THE TRI-DIAGONAL MATRIX OF THE IMPLICIT TIME SCHEME.
C ----------------------------------------------------------------------
      INTEGER CVFRZ
      INTEGER IALP1
      INTEGER J
      INTEGER JJ
      INTEGER K
      INTEGER KS
      INTEGER NSOIL
      COMMON /GLOBAL1/NSOIL

      REAL ACRT
      REAL AI(NSOIL)
      REAL BI(NSOIL)
      REAL CI(NSOIL)
      REAL DD
      REAL DDT
      REAL DDZ
      REAL DDZ2
      REAL DENOM
      REAL DENOM2
      REAL DICE
      REAL DKSAT
      REAL DMAX(NSOIL)
      REAL DSMDZ
      REAL DSMDZ2
      REAL DT
      REAL DT1
      REAL DWSAT
      REAL EDIR
      REAL ET(NSOIL)
      REAL FCR
      REAL FRZX
      REAL INFMAX
      REAL KDT
      REAL MXSMC
      REAL MXSMC2
      REAL NUMER
      REAL RAIN1
      REAL PDDUM
      REAL PX
      REAL RHSTT(NSOIL)
      REAL SH2O(NSOIL)
      REAL SH2OA(NSOIL)
      REAL SICE(NSOIL)
      REAL SICEMAX
      REAL SLOPE
      REAL SMCAV
      REAL SMCMAX
      REAL SMCWLT
      REAL SSTT
      REAL SUM
      REAL VAL
      REAL WCND
      REAL WCND2
      REAL WDF
      REAL WDF2
      REAL ZSOIL(NSOIL)
C ----------------------------------------------------------------------
C FROZEN GROUND VERSION:
C REFERENCE FROZEN GROUND PARAMETER, CVFRZ, IS A SHAPE PARAMETER OF
C AREAL DISTRIBUTION FUNCTION OF SOIL ICE CONTENT WHICH EQUALS 1/CV.
C CV IS A COEFFICIENT OF SPATIAL VARIATION OF SOIL ICE CONTENT.  BASED
C ON FIELD DATA CV DEPENDS ON AREAL MEAN OF FROZEN DEPTH, AND IT IS CLOSE
C TO CONSTANT = 0.6 IF AREAL MEAN FROZEN DEPTH IS ABOVE 20 CM.  THAT IS
C WHY PARAMETER CVFRZ = 3 (INT{1/0.6*0.6}).
C CURRENT LOGIC DOESN'T ALLOW CVFRZ BE BIGGER THAN 3
C ----------------------------------------------------------------------
        PARAMETER(CVFRZ = 3)
C ----------------------------------------------------------------------
C LET SICEMAX BE THE GREATEST, IF ANY, FROZEN WATER CONTENT WITHIN SOIL
C LAYERS.
C ----------------------------------------------------------------------
      SICEMAX = 0.0
      DO KS=1,NSOIL
       IF (SICE(KS) .GT. SICEMAX) SICEMAX = SICE(KS)
      END DO
C ----------------------------------------------------------------------
C DETERMINE RAINFALL INFILTRATION RATE
C ----------------------------------------------------------------------
      PDDUM = RAIN1
      IF (RAIN1 .NE. 0.0) THEN
C ----------------------------------------------------------------------
C MODIFIED BY Q. DUAN, 5/16/94
C ----------------------------------------------------------------------
        DT1     = DT / 86400.
        SMCAV   = SMCMAX - SMCWLT
        DMAX(1) = -ZSOIL(1) * SMCAV
C ----------------------------------------------------------------------
C FROZEN GROUND VERSION:
C ----------------------------------------------------------------------
        DICE = -ZSOIL(1) * SICE(1)
        DMAX(1)=DMAX(1)*(1.0 - (SH2OA(1) + SICE(1) - SMCWLT)/SMCAV)
        DD=DMAX(1)
        DO KS=2,NSOIL
          DICE = DICE + ( ZSOIL(KS-1) - ZSOIL(KS) ) * SICE(KS)
          DMAX(KS) = (ZSOIL(KS-1) - ZSOIL(KS))*SMCAV
          DMAX(KS) = DMAX(KS)*(1.0 - (SH2OA(KS)+SICE(KS)-SMCWLT)/SMCAV)
          DD = DD+DMAX(KS)
        END DO
C ----------------------------------------------------------------------
C VAL = (1.-EXP(-KDT*SQRT(DT1)))
C IN BELOW, REMOVE THE SQRT IN ABOVE
C ----------------------------------------------------------------------
        VAL = (1. - EXP(-KDT * DT1))
        DDT = DD * VAL
        PX  = RAIN1 * DT
        IF (PX .LT. 0.0) PX = 0.0
        INFMAX = (PX * (DDT / (PX + DDT))) / DT
C ----------------------------------------------------------------------
C FROZEN GROUND VERSION:
C REDUCTION OF INFILTRATION BASED ON FROZEN GROUND PARAMETERS
C ----------------------------------------------------------------------
        FCR = 1.
        IF (DICE .GT. 1.E-2) THEN
          ACRT = CVFRZ * FRZX / DICE
          SUM = 1.
          IALP1 = CVFRZ - 1
          DO J = 1,IALP1
            K = 1
            DO JJ = J+1,IALP1
              K = K * JJ
            END DO
            SUM = SUM + (ACRT**(CVFRZ - J)) / FLOAT (K)
          END DO
          FCR = 1. - EXP(-ACRT) * SUM
        ENDIF
        INFMAX = INFMAX * FCR
C ----------------------------------------------------------------------
C CORRECTION OF INFILTRATION LIMITATION:
C IF INFMAX .LE. HYDROLIC CONDUCTIVITY ASSIGN INFMAX THE VALUE OF
C HYDROLIC CONDUCTIVITY
C ----------------------------------------------------------------------
        MXSMC = SH2OA(1)

        CALL WDFCND (WDF,WCND,MXSMC,SMCMAX,DKSAT,DWSAT,SICEMAX)

        INFMAX = MAX(INFMAX,WCND)
        INFMAX = MIN(INFMAX,PX)

        IF (RAIN1 .GT. INFMAX) THEN
          PDDUM = INFMAX
        ENDIF
      ENDIF
C ----------------------------------------------------------------------
C TO AVOID SPURIOUS DRAINAGE BEHAVIOR, 'UPSTREAM DIFFERENCING' IN LINE
C BELOW REPLACED WITH NEW APPROACH IN 2ND LINE:
C 'MXSMC = MAX(SH2OA(1), SH2OA(2))'
C ----------------------------------------------------------------------
      MXSMC = SH2OA(1)
      CALL WDFCND (WDF,WCND,MXSMC,SMCMAX,DKSAT,DWSAT,SICEMAX)
C ----------------------------------------------------------------------
C CALC THE MATRIX COEFFICIENTS AI, BI, AND CI FOR THE TOP LAYER
C ----------------------------------------------------------------------
      DDZ   = 1. / ( -.5 * ZSOIL(2) )
      AI(1) = 0.0
      BI(1) = WDF * DDZ / ( -ZSOIL(1) )
      CI(1) = -BI(1)
C ----------------------------------------------------------------------
C CALC RHSTT FOR THE TOP LAYER AFTER CALC'NG THE VERTICAL SOIL MOISTURE
C GRADIENT BTWN THE TOP AND NEXT TO TOP LAYERS.
C ----------------------------------------------------------------------
      DSMDZ    = (SH2O(1) - SH2O(2)) / (-.5 * ZSOIL(2))
      RHSTT(1) = (WDF * DSMDZ + WCND - PDDUM + EDIR + ET(1))/ZSOIL(1)
      SSTT     = WDF * DSMDZ + WCND + EDIR + ET(1)
C ----------------------------------------------------------------------
C INITIALIZE DDZ2
C ----------------------------------------------------------------------
      DDZ2  = 0.0
C ----------------------------------------------------------------------
C LOOP THRU THE REMAINING SOIL LAYERS, REPEATING THE ABV PROCESS
C ----------------------------------------------------------------------
      DO K = 2,NSOIL
        DENOM2 = (ZSOIL(K-1) - ZSOIL(K))
        SLOPE = 1.
        IF (K .NE. NSOIL) THEN
C ----------------------------------------------------------------------
C AGAIN, TO AVOID SPURIOUS DRAINAGE BEHAVIOR, 'UPSTREAM DIFFERENCING' IN
C LINE BELOW REPLACED WITH NEW APPROACH IN 2ND LINE:
C 'MXSMC2 = MAX (SH2OA(K), SH2OA(K+1))'
C ----------------------------------------------------------------------
          MXSMC2 = SH2OA(K)
          CALL WDFCND (WDF2,WCND2,MXSMC2,SMCMAX,DKSAT,DWSAT,SICEMAX)
C ----------------------------------------------------------------------
C CALC SOME PARTIAL PRODUCTS FOR LATER USE IN CALC'NG RHSTT
C ----------------------------------------------------------------------
          DENOM = (ZSOIL(K-1) - ZSOIL(K+1))
          DSMDZ2 = (SH2O(K) - SH2O(K+1)) / (DENOM * 0.5)
C ----------------------------------------------------------------------
C CALC THE MATRIX COEF, CI, AFTER CALC'NG ITS PARTIAL PRODUCT
C ----------------------------------------------------------------------
          DDZ2 = 2.0 / DENOM
          CI(K) = -WDF2 * DDZ2 / DENOM2
        ELSE
C ----------------------------------------------------------------------
C RETRIEVE THE SOIL WATER DIFFUSIVITY AND HYDRAULIC CONDUCTIVITY FOR
C THIS LAYER
C ----------------------------------------------------------------------
          CALL WDFCND (WDF2,WCND2,SH2OA(NSOIL),SMCMAX,DKSAT,DWSAT,
     &                 SICEMAX)
C ----------------------------------------------------------------------
C CALC A PARTIAL PRODUCT FOR LATER USE IN CALC'NG RHSTT
C ----------------------------------------------------------------------
          DSMDZ2 = 0.0
C ----------------------------------------------------------------------
C SET MATRIX COEF CI TO ZERO
C ----------------------------------------------------------------------
          CI(K) = 0.0
        ENDIF
C ----------------------------------------------------------------------
C CALC RHSTT FOR THIS LAYER AFTER CALC'NG ITS NUMERATOR
C ----------------------------------------------------------------------
        NUMER    = (WDF2 * DSMDZ2) + SLOPE * WCND2 - (WDF * DSMDZ)
     &    - WCND + ET(K)
        RHSTT(K) = NUMER / (-DENOM2)
C ----------------------------------------------------------------------
C CALC MATRIX COEFS, AI, AND BI FOR THIS LAYER
C ----------------------------------------------------------------------
        AI(K) = -WDF * DDZ / DENOM2
        BI(K) = -(AI(K) + CI(K))
C ----------------------------------------------------------------------
C RESET VALUES OF WDF, WCND, DSMDZ, AND DDZ FOR LOOP TO NEXT LYR
C ----------------------------------------------------------------------
        IF (K .NE. NSOIL) THEN
          WDF   = WDF2
          WCND  = WCND2
          DSMDZ = DSMDZ2
          DDZ   = DDZ2
        ENDIF
      END DO
C ----------------------------------------------------------------------
C END SUBROUTINE SRT
C ----------------------------------------------------------------------
      RETURN
      END


      SUBROUTINE SSTEP (SH2OOUT,SH2OIN,RHSTT,DT,SMCMAX,
     &                  ZSOIL,SMC,SICE,AI,BI,CI)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C CALCULATE/UPDATE SOIL MOISTURE CONTENT VALUES AND CANOPY MOISTURE
C CONTENT VALUES.
C ----------------------------------------------------------------------
      INTEGER K
      INTEGER NSOIL
      COMMON /GLOBAL1/NSOIL

      REAL AI(NSOIL)
      REAL BI(NSOIL)
      REAL CI(NSOIL)
      REAL CIin(NSOIL)
      REAL DDZ
      REAL DT
      REAL RHSTT(NSOIL)
      REAL RHSTTin(NSOIL)
      REAL SH2OIN(NSOIL)
      REAL SH2OOUT(NSOIL)
      REAL SICE(NSOIL)
      REAL SMC(NSOIL)
      REAL SMCMAX
      REAL STOT
      REAL WPLUS
      REAL ZSOIL(NSOIL)
C ---------------------------------------------------------------------
C DECLARATIONS FOR MUSSEL MODEL
C ---------------------------------------------------------------------
      INTEGER TIDEFLAG
      INTEGER BEDDEPTH
      REAL CONTACT
      REAL SST
      COMMON /TIDE/TIDEFLAG, SST
      COMMON /GEOMETRY/BEDDEPTH,CONTACT
C ----------------------------------------------------------------------
C CREATE 'AMOUNT' VALUES OF VARIABLES TO BE INPUT TO THE
C TRI-DIAGONAL MATRIX ROUTINE.
C ----------------------------------------------------------------------
      DO K = 1,NSOIL
        RHSTT(K) = RHSTT(K) * DT
        AI(K)    = AI(K) * DT
        BI(K)    = 1. + BI(K) * DT
        CI(K)    = CI(K) * DT
      END DO
C ----------------------------------------------------------------------
C COPY VALUES FOR INPUT VARIABLES BEFORE CALL TO ROSR12
C ----------------------------------------------------------------------
      DO K = 1,NSOIL
        RHSTTin(K) = RHSTT(K)
      END DO
      DO K = 1,NSOIL
        CIin(K) = CI(K)
      END DO
C ----------------------------------------------------------------------
C CALL ROSR12 TO SOLVE THE TRI-DIAGONAL MATRIX
C ----------------------------------------------------------------------
      CALL ROSR12 (CI,AI,BI,CIin,RHSTTin,RHSTT)
C ----------------------------------------------------------------------
C SUM THE PREVIOUS SMC VALUE AND THE MATRIX SOLUTION TO GET A
C NEW VALUE.  MIN ALLOWABLE VALUE OF SMC WILL BE 0.02.
C ----------------------------------------------------------------------
      WPLUS = 0.
      DDZ   = -ZSOIL(1)

      DO K = 1,NSOIL
        IF (K .NE. 1) DDZ = ZSOIL(K - 1) - ZSOIL(K)
C ----------------------------------------------------------------------
C MUSSEL MODEL v1.8 - ALLOW FOR NON-ABSORBANT BEDROCK AND HORIZ RUNOFF
C ----------------------------------------------------------------------
        SH2OIN(K)  = SH2OIN(K) * 0.5
        SH2OOUT(K) = SH2OIN(K) + CI(K) + WPLUS / DDZ

        STOT = SH2OOUT(K) + SICE(K)
        IF (STOT .GT. SMCMAX) THEN
	  DDZ = -ZSOIL(K) + ZSOIL(K - 1)
          WPLUS = (STOT - SMCMAX) * DDZ
        ELSE
          WPLUS = 0.
        ENDIF

        IF (TIDEFLAG .EQ. 1) THEN
          IF (K .LE. BEDDEPTH) THEN
            SMC(K) = SMCMAX
          ELSE
            SMC(K) = 0
          ENDIF
        ELSE
          IF (K .LE. BEDDEPTH) THEN
            SMC(K) = MAX(MIN(STOT,SMCMAX),0.0)
          ELSE
            SMC(K) = 0
          ENDIF
        ENDIF

        SH2OOUT(K) = MAX((SMC(K) - SICE(K)),0.0)
      END DO
C ----------------------------------------------------------------------
C END SUBROUTINE SSTEP
C ----------------------------------------------------------------------
      RETURN
      END


      SUBROUTINE TBND (TU,TB,ZSOIL,ZBOT,K,TBND1)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C CALCULATE TEMPERATURE ON THE BOUNDARY OF THE LAYER BY INTERPOLATION OF
C THE MIDDLE LAYER TEMPERATURES
C ----------------------------------------------------------------------
      INTEGER NSOIL
      COMMON /GLOBAL1/NSOIL

      INTEGER K

      REAL TBND1
      REAL TU
      REAL TB
      REAL ZB
      REAL ZBOT
      REAL ZUP
      REAL ZSOIL(NSOIL)

      REAL FRZ_T
      COMMON /GLOBAL2/FRZ_T
C ----------------------------------------------------------------------
C USE SURFACE TEMPERATURE ON THE TOP OF THE FIRST LAYER
C ----------------------------------------------------------------------
      IF (K .EQ. 1) THEN
        ZUP = 0.
      ELSE
        ZUP = ZSOIL(K-1)
      ENDIF
C ----------------------------------------------------------------------
C USE DEPTH OF THE CONSTANT BOTTOM TEMPERATURE WHEN INTERPOLATE
C TEMPERATURE INTO THE LAST LAYER BOUNDARY
C ----------------------------------------------------------------------
      IF (K .EQ. NSOIL) THEN
        ZB = 2. * ZBOT - ZSOIL(K)
      ELSE
        ZB = ZSOIL(K+1)
      ENDIF
C ----------------------------------------------------------------------
C LINEAR INTERPOLATION BETWEEN THE AVERAGE LAYER TEMPERATURES
C ----------------------------------------------------------------------
      TBND1 = TU + (TB - TU) * (ZUP - ZSOIL(K)) / (ZUP - ZB)
C ----------------------------------------------------------------------
C END SUBROUTINE TBND
C ----------------------------------------------------------------------
      RETURN
      END


      SUBROUTINE WDFCND (WDF,WCND,SMC,SMCMAX,DKSAT,DWSAT,SICEMAX)
      IMPLICIT NONE
C ----------------------------------------------------------------------
C CALCULATE SOIL WATER DIFFUSIVITY AND SOIL HYDRAULIC CONDUCTIVITY.
C ----------------------------------------------------------------------
      REAL DKSAT
      REAL DWSAT
      REAL EXPON
      REAL FACTR1
      REAL FACTR2
      REAL SICEMAX
      REAL SMC
      REAL SMCMAX
      REAL VKWGT
      REAL WCND
      REAL WDF
      REAL BB
      PARAMETER(BB = 4)
C ----------------------------------------------------------------------
C     CALC THE RATIO OF THE ACTUAL TO THE MAX PSBL SOIL H2O CONTENT
C ----------------------------------------------------------------------
      FACTR1 = 0.2 / SMCMAX
      FACTR2 = SMC / SMCMAX
C ----------------------------------------------------------------------
C PREP AN EXPNTL COEF AND CALC THE SOIL WATER DIFFUSIVITY
C ----------------------------------------------------------------------
      EXPON = BB + 2.0
      WDF   = DWSAT * FACTR2**EXPON
C ----------------------------------------------------------------------
C FROZEN SOIL HYDRAULIC DIFFUSIVITY.  VERY SENSITIVE TO THE VERTICAL
C GRADIENT OF UNFROZEN WATER. THE LATTER GRADIENT CAN BECOME VERY
C EXTREME IN FREEZING/THAWING SITUATIONS, AND GIVEN THE RELATIVELY
C FEW AND THICK SOIL LAYERS, THIS GRADIENT SUFFERES SERIOUS
C TRUNCTION ERRORS YIELDING ERRONEOUSLY HIGH VERTICAL TRANSPORTS OF
C UNFROZEN WATER IN BOTH DIRECTIONS FROM HUGE HYDRAULIC DIFFUSIVITY.
C THEREFORE, WE FOUND WE HAD TO ARBITRARILY CONSTRAIN WDF
C ----------------------------------------------------------------------
      IF (SICEMAX .GT. 0.0)  THEN
        VKWGT = 1. / (1. + (500.* SICEMAX)**3.)
        WDF   = VKWGT * WDF + (1.- VKWGT) * DWSAT * FACTR1**EXPON
      ENDIF
C ----------------------------------------------------------------------
C RESET THE EXPNTL COEF AND CALC THE HYDRAULIC CONDUCTIVITY
C ----------------------------------------------------------------------
      EXPON = (2.0 * BB) + 3.0
      WCND  = DKSAT * FACTR2**EXPON
C ----------------------------------------------------------------------
C END SUBROUTINE WDFCND
C ----------------------------------------------------------------------
      RETURN
      END
