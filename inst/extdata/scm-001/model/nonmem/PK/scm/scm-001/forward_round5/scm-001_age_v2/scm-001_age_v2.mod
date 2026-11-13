$PROBLEM 2-cmt oral 500 mg single dose (scm-001) -- SCM base, type2 comments created from pharos see scm-001_age_v2_metadata.json for details.

$INPUT C ID TIME AMT CMT EVID MDV DV WT AGE CRCL ALB SEX

$DATA /data/user-homes/brendan/projects/scm-demo/data/PK/scm-001.csv IGNORE=@

    ; Units:
    ; DV is concentration (mg/L)
    ; TIME is actual time after dose (h)
    ; AMT is in mg
    ; The pre-dose sample (TIME = 0) is carried with MDV = 1

$SUBROUTINES ADVAN4 TRANS4

 ; COMP = (DEPOT)      ; 1 depot
 ; COMP = (CENTRAL)    ; 2 central
 ; COMP = (PERIPH)     ; 3 peripheral

$PK
;; Covariate effects -- all six are SCM candidates, held out at THETA = 0
;; so each term collapses to 1. Five are continuous (power models), one is
;; categorical (SEX, 0 = male, 1 = female; fractional change).

  WT_CL   = (WT/70)**THETA(6)      ; WT on CL
  CRCL_CL = (CRCL/100)**THETA(7)   ; CRCL on CL
  ALB_CL  = (ALB/4.2)**THETA(8)    ; ALB on CL

  WT_V2   = (WT/70)**THETA(9)      ; WT on V2
  AGE_V2  = (AGE/50)**THETA(10)    ; AGE on V2
  SEX_V2  = (1 + SEX*THETA(11))    ; SEX on V2

;; Structural parameters

  TVCL = THETA(1)                                  ; clearance [L/h]
    CL = TVCL*EXP(ETA(1))*WT_CL*CRCL_CL*ALB_CL
  TVV2 = THETA(2)                                  ; central volume [L]
    V2 = TVV2*EXP(ETA(2))*WT_V2*AGE_V2*SEX_V2
  TVQ  = THETA(3)                                  ; intercomp. clearance [L/h]
    Q  = TVQ
  TVV3 = THETA(4)                                  ; peripheral volume [L]
    V3 = TVV3
  TVKA = THETA(5)                                  ; absorption rate [1/h]
    KA = TVKA*EXP(ETA(3))

;; Volume scaling -- dose in mg, concentration in mg/L

  S2 = V2

$ERROR
  IPRED = F
  Y = IPRED*(1+EPS(1))

$THETA
; --- structural --------------------------------------------------
  (0, 4.832)     ; 1 TVCL (L/h)
  (0, 51.783)    ; 2 TVV2 (L)
  (0, 2.989)     ; 3 TVQ (L/h)
  (0, 60.666)    ; 4 TVV3 (L)
  (0, 0.946)     ; 5 TVKA (1/h)
; --- SCM candidate effects ---------------------------------------
  0.864517      ; 6 WT_CL
  0.578179      ; 7 CRCL_CL
  (0 FIX)      ; 8 ALB_CL
  0.736413      ; 9 WT_V2
  0.1      ; 10 AGE_V2
  (-0.99, -0.246046, 5)      ; 11 SEX_V2

$OMEGA
  0.0856     ; 1 IIV_CL TVCL :EXP
  0.074289   ; 2 IIV_V2 TVV2 :EXP
  0.1564     ; 3 IIV_KA TVKA :EXP

$SIGMA
  0.009     ; 1 PropErr ;prop

$ESTIMATION METHOD=COND INTERACTION MAXEVAL=9999 SIGL=9 NSIG=3 PRINT=5 NOABORT
$TABLE ID TIME DV PRED IPRED CWRES NOPRINT ONEHEADER FILE=sdtab1
