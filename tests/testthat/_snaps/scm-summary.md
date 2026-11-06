# an SCM lineage summarizes as one row per fit with its round's dOFV and p-value

    Code
      print(shown, digits = 4, row.names = FALSE)
    Output
                section                  model     based_on
              Reference              1001_base         <NA>
        Forward round 1           1001_crcl_cl    1001_base
        Forward round 1             1001_wt_cl    1001_base
        Forward round 1              1001_wt_v    1001_base
        Forward round 2           1001_crcl_cl   1001_wt_cl
        Forward round 2         1001_wt_v_try2   1001_wt_cl
        Forward round 3              1001_wt_v 1001_crcl_cl
          Forward model 1001_scm_forward_final 1001_crcl_cl
       Backward round 1           1001_crcl_cl 1001_crcl_cl
       Backward round 1             1001_wt_cl 1001_crcl_cl
       Backward round 2             1001_wt_cl 1001_crcl_cl
            Final model         1001_scm_final 1001_crcl_cl
                         description n_parameters    ofv  dofv df    pvalue
                          base model            6 1000.0    NA NA        NA
                         add CRCL_CL            7  996.0  -4.0  1 4.550e-02
                           add WT_CL            7  980.0 -20.0  1 7.744e-06
                            add WT_V            7  999.0  -1.0  1 3.173e-01
                         add CRCL_CL            8  974.0  -6.0  1 1.431e-02
                            add WT_V            8  978.5  -1.5  1 2.207e-01
                            add WT_V            9  973.0  -1.0  1 3.173e-01
       forward model: WT_CL, CRCL_CL            8  973.8  -0.2 NA        NA
                        drop CRCL_CL            7  980.0   6.0  1 1.431e-02
                          drop WT_CL            7  995.0  21.0  1 4.593e-06
                          drop WT_CL            6 1000.0  20.0  1 7.744e-06
                  final model: WT_CL            7  979.5  -0.5 NA        NA

