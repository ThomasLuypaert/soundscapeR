# `ss_pairdis()` works as expected when q = 0

    Code
      soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study,
        qvalue = 0)
    Output
      $Level_1_beta
                    Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline            NA         NA         NA         NA         NA
      Mascote_A1 1.547012         NA         NA         NA         NA
      Mascote_A2 1.605814   1.406660         NA         NA         NA
      Mascote_B1 1.551107   1.307517   1.371799         NA         NA
      Mascote_B2 1.576810   1.271276   1.368335   1.278808         NA
      
      $L1_CqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.5470120         NA         NA         NA         NA
      Mascote_A2 0.6058140  0.4066599         NA         NA         NA
      Mascote_B1 0.5511069  0.3075169  0.3717985         NA         NA
      Mascote_B2 0.5768096  0.2712756  0.3683346  0.2788082         NA
      
      $L1_UqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.7071852         NA         NA         NA         NA
      Mascote_A2 0.7545257  0.5781923         NA         NA         NA
      Mascote_B1 0.7105982  0.4703831  0.5420599         NA         NA
      Mascote_B2 0.7316160  0.4267770  0.5383692  0.4360438         NA
      
      $L1_VqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.5470120         NA         NA         NA         NA
      Mascote_A2 0.6058140  0.4066599         NA         NA         NA
      Mascote_B1 0.5511069  0.3075169  0.3717985         NA         NA
      Mascote_B2 0.5768096  0.2712756  0.3683346  0.2788082         NA
      
      $L1_SqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.7071852         NA         NA         NA         NA
      Mascote_A2 0.7545257  0.5781923         NA         NA         NA
      Mascote_B1 0.7105982  0.4703831  0.5420599         NA         NA
      Mascote_B2 0.7316160  0.4267770  0.5383692  0.4360438         NA
      

# `ss_pairdis()` works as expected when q = 1

    Code
      soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study,
        qvalue = 1)
    Output
      $Level_1_beta
                 Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline         NA         NA         NA         NA         NA
      Mascote_A1     2         NA         NA         NA         NA
      Mascote_A2     2          2         NA         NA         NA
      Mascote_B1     2          2          2         NA         NA
      Mascote_B2     2          2          2          2         NA
      
      $L1_CqN
                 Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline         NA         NA         NA         NA         NA
      Mascote_A1     1         NA         NA         NA         NA
      Mascote_A2     1          1         NA         NA         NA
      Mascote_B1     1          1          1         NA         NA
      Mascote_B2     1          1          1          1         NA
      
      $L1_UqN
                 Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline         NA         NA         NA         NA         NA
      Mascote_A1     1         NA         NA         NA         NA
      Mascote_A2     1          1         NA         NA         NA
      Mascote_B1     1          1          1         NA         NA
      Mascote_B2     1          1          1          1         NA
      
      $L1_VqN
                 Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline         NA         NA         NA         NA         NA
      Mascote_A1     1         NA         NA         NA         NA
      Mascote_A2     1          1         NA         NA         NA
      Mascote_B1     1          1          1         NA         NA
      Mascote_B2     1          1          1          1         NA
      
      $L1_SqN
                 Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline         NA         NA         NA         NA         NA
      Mascote_A1     1         NA         NA         NA         NA
      Mascote_A2     1          1         NA         NA         NA
      Mascote_B1     1          1          1         NA         NA
      Mascote_B2     1          1          1          1         NA
      

# `ss_pairdis()` works as expected when q = 2

    Code
      soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study,
        qvalue = 2)
    Output
      $Level_1_beta
                    Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline            NA         NA         NA         NA         NA
      Mascote_A1 1.296813         NA         NA         NA         NA
      Mascote_A2 1.319875   1.248484         NA         NA         NA
      Mascote_B1 1.302879   1.163570   1.233007         NA         NA
      Mascote_B2 1.309972   1.118689   1.221863   1.155154         NA
      
      $L1_CqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.4577575         NA         NA         NA         NA
      Mascote_A2 0.4847054  0.3980569         NA         NA         NA
      Mascote_B1 0.4649381  0.2811520  0.3779490         NA         NA
      Mascote_B2 0.4732501  0.2121925  0.3631557  0.2686287         NA
      
      $L1_UqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.2968129         NA         NA         NA         NA
      Mascote_A2 0.3198753  0.2484838         NA         NA         NA
      Mascote_B1 0.3028791  0.1635700  0.2330068         NA         NA
      Mascote_B2 0.3099723  0.1186887  0.2218633  0.1551537         NA
      
      $L1_VqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.2968129         NA         NA         NA         NA
      Mascote_A2 0.3198753  0.2484838         NA         NA         NA
      Mascote_B1 0.3028791  0.1635700  0.2330068         NA         NA
      Mascote_B2 0.3099723  0.1186887  0.2218633  0.1551537         NA
      
      $L1_SqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.4577575         NA         NA         NA         NA
      Mascote_A2 0.4847054  0.3980569         NA         NA         NA
      Mascote_B1 0.4649381  0.2811520  0.3779490         NA         NA
      Mascote_B2 0.4732501  0.2121925  0.3631557  0.2686287         NA
      

# `ss_pairdis()` works as expected when q = 0 and frequency subsetting

    Code
      soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study,
        qvalue = 0, minfreq = 500, maxfreq = 2000)
    Output
      $Level_1_beta
                    Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline            NA         NA         NA         NA         NA
      Mascote_A1 1.919107         NA         NA         NA         NA
      Mascote_A2 1.970588   1.701625         NA         NA         NA
      Mascote_B1 1.967320   1.688013   1.583916         NA         NA
      Mascote_B2 1.923681   1.545045   1.438308   1.568327         NA
      
      $L1_CqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.9191074         NA         NA         NA         NA
      Mascote_A2 0.9705882  0.7016248         NA         NA         NA
      Mascote_B1 0.9673203  0.6880131  0.5839161         NA         NA
      Mascote_B2 0.9236813  0.5450450  0.4383079  0.5683269         NA
      
      $L1_UqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.9578488         NA         NA         NA         NA
      Mascote_A2 0.9850746  0.8246528         NA         NA         NA
      Mascote_B1 0.9833887  0.8151751  0.7373068         NA         NA
      Mascote_B2 0.9603267  0.7055394  0.6094771  0.7247557         NA
      
      $L1_VqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.9191074         NA         NA         NA         NA
      Mascote_A2 0.9705882  0.7016248         NA         NA         NA
      Mascote_B1 0.9673203  0.6880131  0.5839161         NA         NA
      Mascote_B2 0.9236813  0.5450450  0.4383079  0.5683269         NA
      
      $L1_SqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.9578488         NA         NA         NA         NA
      Mascote_A2 0.9850746  0.8246528         NA         NA         NA
      Mascote_B1 0.9833887  0.8151751  0.7373068         NA         NA
      Mascote_B2 0.9603267  0.7055394  0.6094771  0.7247557         NA
      

# `ss_pairdis()` works as expected when q = 1 and frequency subsetting

    Code
      soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study,
        qvalue = 1, minfreq = 500, maxfreq = 2000)
    Output
      $Level_1_beta
                 Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline         NA         NA         NA         NA         NA
      Mascote_A1     2         NA         NA         NA         NA
      Mascote_A2     2          2         NA         NA         NA
      Mascote_B1     2          2          2         NA         NA
      Mascote_B2     2          2          2          2         NA
      
      $L1_CqN
                 Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline         NA         NA         NA         NA         NA
      Mascote_A1     1         NA         NA         NA         NA
      Mascote_A2     1          1         NA         NA         NA
      Mascote_B1     1          1          1         NA         NA
      Mascote_B2     1          1          1          1         NA
      
      $L1_UqN
                 Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline         NA         NA         NA         NA         NA
      Mascote_A1     1         NA         NA         NA         NA
      Mascote_A2     1          1         NA         NA         NA
      Mascote_B1     1          1          1         NA         NA
      Mascote_B2     1          1          1          1         NA
      
      $L1_VqN
                 Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline         NA         NA         NA         NA         NA
      Mascote_A1     1         NA         NA         NA         NA
      Mascote_A2     1          1         NA         NA         NA
      Mascote_B1     1          1          1         NA         NA
      Mascote_B2     1          1          1          1         NA
      
      $L1_SqN
                 Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline         NA         NA         NA         NA         NA
      Mascote_A1     1         NA         NA         NA         NA
      Mascote_A2     1          1         NA         NA         NA
      Mascote_B1     1          1          1         NA         NA
      Mascote_B2     1          1          1          1         NA
      

# `ss_pairdis()` works as expected when q = 2 and frequency subsetting

    Code
      soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study,
        qvalue = 2, minfreq = 500, maxfreq = 2000)
    Output
      $Level_1_beta
                    Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline            NA         NA         NA         NA         NA
      Mascote_A1 1.883079         NA         NA         NA         NA
      Mascote_A2 1.971332   1.558836         NA         NA         NA
      Mascote_B1 1.959400   1.572837   1.354126         NA         NA
      Mascote_B2 1.896549   1.402335   1.248019    1.38717         NA
      
      $L1_CqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.9379094         NA         NA         NA         NA
      Mascote_A2 0.9854575  0.7169912         NA         NA         NA
      Mascote_B1 0.9792794  0.7284120  0.5230321         NA         NA
      Mascote_B2 0.9454528  0.5738069  0.3974597  0.5582153         NA
      
      $L1_UqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.8830785         NA         NA         NA         NA
      Mascote_A2 0.9713320  0.5588357         NA         NA         NA
      Mascote_B1 0.9594000  0.5728365  0.3541256         NA         NA
      Mascote_B2 0.8965486  0.4023347  0.2480185  0.3871697         NA
      
      $L1_VqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.8830785         NA         NA         NA         NA
      Mascote_A2 0.9713320  0.5588357         NA         NA         NA
      Mascote_B1 0.9594000  0.5728365  0.3541256         NA         NA
      Mascote_B2 0.8965486  0.4023347  0.2480185  0.3871697         NA
      
      $L1_SqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.9379094         NA         NA         NA         NA
      Mascote_A2 0.9854575  0.7169912         NA         NA         NA
      Mascote_B1 0.9792794  0.7284120  0.5230321         NA         NA
      Mascote_B2 0.9454528  0.5738069  0.3974597  0.5582153         NA
      

# `ss_pairdis()` works as expected when q = 0 and temporal subsetting

    Code
      soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study,
        qvalue = 0, mintime = "06:00:00", maxtime = "18:00:00")
    Output
      $Level_1_beta
                    Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline            NA         NA         NA         NA         NA
      Mascote_A1 1.582363         NA         NA         NA         NA
      Mascote_A2 1.591030   1.329692         NA         NA         NA
      Mascote_B1 1.612784   1.322864   1.294212         NA         NA
      Mascote_B2 1.603838   1.281737   1.282380   1.262195         NA
      
      $L1_CqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.5823629         NA         NA         NA         NA
      Mascote_A2 0.5910296  0.3296918         NA         NA         NA
      Mascote_B1 0.6127839  0.3228643  0.2942121         NA         NA
      Mascote_B2 0.6038382  0.2817370  0.2823803  0.2621951         NA
      
      $L1_UqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.7360674         NA         NA         NA         NA
      Mascote_A2 0.7429523  0.4958920         NA         NA         NA
      Mascote_B1 0.7599082  0.4881292  0.4546582         NA         NA
      Mascote_B2 0.7529915  0.4396175  0.4404003  0.4154589         NA
      
      $L1_VqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.5823629         NA         NA         NA         NA
      Mascote_A2 0.5910296  0.3296918         NA         NA         NA
      Mascote_B1 0.6127839  0.3228643  0.2942121         NA         NA
      Mascote_B2 0.6038382  0.2817370  0.2823803  0.2621951         NA
      
      $L1_SqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.7360674         NA         NA         NA         NA
      Mascote_A2 0.7429523  0.4958920         NA         NA         NA
      Mascote_B1 0.7599082  0.4881292  0.4546582         NA         NA
      Mascote_B2 0.7529915  0.4396175  0.4404003  0.4154589         NA
      

# `ss_pairdis()` works as expected when q = 1 and temporal subsetting

    Code
      soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study,
        qvalue = 1, mintime = "06:00:00", maxtime = "18:00:00")
    Output
      $Level_1_beta
                 Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline         NA         NA         NA         NA         NA
      Mascote_A1     2         NA         NA        NaN         NA
      Mascote_A2     2          2         NA         NA         NA
      Mascote_B1     2        NaN          2         NA         NA
      Mascote_B2     2          2          2          2         NA
      
      $L1_CqN
                 Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline         NA         NA         NA         NA         NA
      Mascote_A1     1         NA         NA        NaN         NA
      Mascote_A2     1          1         NA         NA         NA
      Mascote_B1     1        NaN          1         NA         NA
      Mascote_B2     1          1          1          1         NA
      
      $L1_UqN
                 Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline         NA         NA         NA         NA         NA
      Mascote_A1     1         NA         NA        NaN         NA
      Mascote_A2     1          1         NA         NA         NA
      Mascote_B1     1        NaN          1         NA         NA
      Mascote_B2     1          1          1          1         NA
      
      $L1_VqN
                 Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline         NA         NA         NA         NA         NA
      Mascote_A1     1         NA         NA        NaN         NA
      Mascote_A2     1          1         NA         NA         NA
      Mascote_B1     1        NaN          1         NA         NA
      Mascote_B2     1          1          1          1         NA
      
      $L1_SqN
                 Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline         NA         NA         NA         NA         NA
      Mascote_A1     1         NA         NA        NaN         NA
      Mascote_A2     1          1         NA         NA         NA
      Mascote_B1     1        NaN          1         NA         NA
      Mascote_B2     1          1          1          1         NA
      

# `ss_pairdis()` works as expected when q = 2 and temporal subsetting

    Code
      soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study,
        qvalue = 2, mintime = "06:00:00", maxtime = "18:00:00")
    Output
      $Level_1_beta
                    Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline            NA         NA         NA         NA         NA
      Mascote_A1 1.266899         NA         NA         NA         NA
      Mascote_A2 1.336580   1.183420         NA         NA         NA
      Mascote_B1 1.399460   1.179114   1.200447         NA         NA
      Mascote_B2 1.336914   1.128301   1.185114   1.180381         NA
      
      $L1_CqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.4213419         NA         NA         NA         NA
      Mascote_A2 0.5036432  0.3099824         NA         NA         NA
      Mascote_B1 0.5708771  0.3038115  0.3339546         NA         NA
      Mascote_B2 0.5040176  0.2274237  0.3123985  0.3056317         NA
      
      $L1_UqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.2668988         NA         NA         NA         NA
      Mascote_A2 0.3365796  0.1834196         NA         NA         NA
      Mascote_B1 0.3994597  0.1791142  0.2004475         NA         NA
      Mascote_B2 0.3369141  0.1283012  0.1851139  0.1803809         NA
      
      $L1_VqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.2668988         NA         NA         NA         NA
      Mascote_A2 0.3365796  0.1834196         NA         NA         NA
      Mascote_B1 0.3994597  0.1791142  0.2004475         NA         NA
      Mascote_B2 0.3369141  0.1283012  0.1851139  0.1803809         NA
      
      $L1_SqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.4213419         NA         NA         NA         NA
      Mascote_A2 0.5036432  0.3099824         NA         NA         NA
      Mascote_B1 0.5708771  0.3038115  0.3339546         NA         NA
      Mascote_B2 0.5040176  0.2274237  0.3123985  0.3056317         NA
      

# `ss_pairdis()` works as expected when q = 0 and spectro-temporal subsetting

    Code
      soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study,
        qvalue = 0, mintime = "06:00:00", maxtime = "18:00:00", minfreq = 500,
        maxfreq = 2000)
    Output
      $Level_1_beta
                    Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline            NA         NA         NA         NA         NA
      Mascote_A1 1.892157         NA         NA         NA         NA
      Mascote_A2 1.959698   1.676840         NA         NA         NA
      Mascote_B1 1.953757   1.660079   1.547475         NA         NA
      Mascote_B2 1.900662   1.518325   1.402390   1.538462         NA
      
      $L1_CqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.8921569         NA         NA         NA         NA
      Mascote_A2 0.9596977  0.6768402         NA         NA         NA
      Mascote_B1 0.9537572  0.6600791  0.5474747         NA         NA
      Mascote_B2 0.9006623  0.5183246  0.4023904  0.5384615         NA
      
      $L1_UqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.9430052         NA         NA         NA         NA
      Mascote_A2 0.9794344  0.8072805         NA         NA         NA
      Mascote_B1 0.9763314  0.7952381  0.7075718         NA         NA
      Mascote_B2 0.9477352  0.6827586  0.5738636        0.7         NA
      
      $L1_VqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.8921569         NA         NA         NA         NA
      Mascote_A2 0.9596977  0.6768402         NA         NA         NA
      Mascote_B1 0.9537572  0.6600791  0.5474747         NA         NA
      Mascote_B2 0.9006623  0.5183246  0.4023904  0.5384615         NA
      
      $L1_SqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.9430052         NA         NA         NA         NA
      Mascote_A2 0.9794344  0.8072805         NA         NA         NA
      Mascote_B1 0.9763314  0.7952381  0.7075718         NA         NA
      Mascote_B2 0.9477352  0.6827586  0.5738636        0.7         NA
      

# `ss_pairdis()` works as expected when q = 1 and spectro-temporal subsetting

    Code
      soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study,
        qvalue = 1, mintime = "06:00:00", maxtime = "18:00:00", minfreq = 500,
        maxfreq = 2000)
    Output
      $Level_1_beta
                 Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline         NA         NA         NA         NA         NA
      Mascote_A1     2         NA         NA         NA         NA
      Mascote_A2     2          2         NA         NA         NA
      Mascote_B1     2          2          2         NA         NA
      Mascote_B2     2          2          2          2         NA
      
      $L1_CqN
                 Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline         NA         NA         NA         NA         NA
      Mascote_A1     1         NA         NA         NA         NA
      Mascote_A2     1          1         NA         NA         NA
      Mascote_B1     1          1          1         NA         NA
      Mascote_B2     1          1          1          1         NA
      
      $L1_UqN
                 Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline         NA         NA         NA         NA         NA
      Mascote_A1     1         NA         NA         NA         NA
      Mascote_A2     1          1         NA         NA         NA
      Mascote_B1     1          1          1         NA         NA
      Mascote_B2     1          1          1          1         NA
      
      $L1_VqN
                 Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline         NA         NA         NA         NA         NA
      Mascote_A1     1         NA         NA         NA         NA
      Mascote_A2     1          1         NA         NA         NA
      Mascote_B1     1          1          1         NA         NA
      Mascote_B2     1          1          1          1         NA
      
      $L1_SqN
                 Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline         NA         NA         NA         NA         NA
      Mascote_A1     1         NA         NA         NA         NA
      Mascote_A2     1          1         NA         NA         NA
      Mascote_B1     1          1          1         NA         NA
      Mascote_B2     1          1          1          1         NA
      

# `ss_pairdis()` works as expected when q = 2 and spectro-temporal subsetting

    Code
      soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study,
        qvalue = 2, mintime = "06:00:00", maxtime = "18:00:00", minfreq = 500,
        maxfreq = 2000)
    Output
      $Level_1_beta
                    Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline            NA         NA         NA         NA         NA
      Mascote_A1 1.849159         NA         NA         NA         NA
      Mascote_A2 1.960725   1.564027         NA         NA         NA
      Mascote_B1 1.948211   1.557907   1.333100         NA         NA
      Mascote_B2 1.857490   1.388170   1.244231   1.373671         NA
      
      $L1_CqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.9184271         NA         NA         NA         NA
      Mascote_A2 0.9799689  0.7212500         NA         NA         NA
      Mascote_B1 0.9734171  0.7162260  0.4997378         NA         NA
      Mascote_B2 0.9232784  0.5592541  0.3925814  0.5440473         NA
      
      $L1_UqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.8491588         NA         NA         NA         NA
      Mascote_A2 0.9607246  0.5640274         NA         NA         NA
      Mascote_B1 0.9482110  0.5579066  0.3331003         NA         NA
      Mascote_B2 0.8574904  0.3881698  0.2442309   0.373671         NA
      
      $L1_VqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.8491588         NA         NA         NA         NA
      Mascote_A2 0.9607246  0.5640274         NA         NA         NA
      Mascote_B1 0.9482110  0.5579066  0.3331003         NA         NA
      Mascote_B2 0.8574904  0.3881698  0.2442309   0.373671         NA
      
      $L1_SqN
                     Aline Mascote_A1 Mascote_A2 Mascote_B1 Mascote_B2
      Aline             NA         NA         NA         NA         NA
      Mascote_A1 0.9184271         NA         NA         NA         NA
      Mascote_A2 0.9799689  0.7212500         NA         NA         NA
      Mascote_B1 0.9734171  0.7162260  0.4997378         NA         NA
      Mascote_B2 0.9232784  0.5592541  0.3925814  0.5440473         NA
      

