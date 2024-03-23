# `ss_divpart()` works as expected when q = 0

    Code
      soundscapeR::ss_divpart(soundscape_list = soundscape_obj_CVR_case_study,
        qvalue = 0)
    Output
      # A tibble: 1 x 7
        levels     q alpha_l1 gamma    N1    N2 beta_l1
         <dbl> <dbl>    <dbl> <dbl> <dbl> <dbl>   <dbl>
      1      2     0     33.1  62.0     5     1    1.88

# `ss_divpart()` works as expected when q = 1

    Code
      soundscapeR::ss_divpart(soundscape_list = soundscape_obj_CVR_case_study,
        qvalue = 1)
    Output
      # A tibble: 1 x 7
        levels     q alpha_l1 gamma    N1    N2 beta_l1
         <dbl> <dbl>    <dbl> <dbl> <dbl> <dbl>   <dbl>
      1      2     1     27.2  43.9     5     1    1.61

# `ss_divpart()` works as expected when q = 2

    Code
      soundscapeR::ss_divpart(soundscape_list = soundscape_obj_CVR_case_study,
        qvalue = 2)
    Output
      # A tibble: 1 x 7
        levels     q alpha_l1 gamma    N1    N2 beta_l1
         <dbl> <dbl>    <dbl> <dbl> <dbl> <dbl>   <dbl>
      1      2     2     23.1  33.6     5     1    1.45

# `ss_divpart()` works as expected when q = 0 and frequency subsetting

    Code
      soundscapeR::ss_divpart(soundscape_list = soundscape_obj_CVR_case_study,
        qvalue = 0, minfreq = 500, maxfreq = 2000)
    Output
      # A tibble: 1 x 7
        levels     q alpha_l1 gamma    N1    N2 beta_l1
         <dbl> <dbl>    <dbl> <dbl> <dbl> <dbl>   <dbl>
      1      2     0     14.0  43.0     5     1    3.06

# `ss_divpart()` works as expected when q = 1 and frequency subsetting

    Code
      soundscapeR::ss_divpart(soundscape_list = soundscape_obj_CVR_case_study,
        qvalue = 1, minfreq = 500, maxfreq = 2000)
    Output
      # A tibble: 1 x 7
        levels     q alpha_l1 gamma    N1    N2 beta_l1
         <dbl> <dbl>    <dbl> <dbl> <dbl> <dbl>   <dbl>
      1      2     1     12.5  33.4     5     1    2.68

# `ss_divpart()` works as expected when q = 2 and frequency subsetting

    Code
      soundscapeR::ss_divpart(soundscape_list = soundscape_obj_CVR_case_study,
        qvalue = 2, minfreq = 500, maxfreq = 2000)
    Output
      # A tibble: 1 x 7
        levels     q alpha_l1 gamma    N1    N2 beta_l1
         <dbl> <dbl>    <dbl> <dbl> <dbl> <dbl>   <dbl>
      1      2     2     10.9  26.3     5     1    2.41

# `ss_divpart()` works as expected when q = 0 and temporal subsetting

    Code
      soundscapeR::ss_divpart(soundscape_list = soundscape_obj_CVR_case_study,
        qvalue = 0, mintime = "06:00:00", maxtime = "18:00:00")
    Output
      # A tibble: 1 x 7
        levels     q alpha_l1 gamma    N1    N2 beta_l1
         <dbl> <dbl>    <dbl> <dbl> <dbl> <dbl>   <dbl>
      1      2     0     32.2  57.6     5     1    1.79

# `ss_divpart()` works as expected when q = 1 and temporal subsetting

    Code
      soundscapeR::ss_divpart(soundscape_list = soundscape_obj_CVR_case_study,
        qvalue = 1, mintime = "06:00:00", maxtime = "18:00:00")
    Output
      # A tibble: 1 x 7
        levels     q alpha_l1 gamma    N1    N2 beta_l1
         <dbl> <dbl>    <dbl> <dbl> <dbl> <dbl>   <dbl>
      1      2     1     25.7  40.3     5     1    1.57

# `ss_divpart()` works as expected when q = 2 and temporal subsetting

    Code
      soundscapeR::ss_divpart(soundscape_list = soundscape_obj_CVR_case_study,
        qvalue = 2, mintime = "06:00:00", maxtime = "18:00:00")
    Output
      # A tibble: 1 x 7
        levels     q alpha_l1 gamma    N1    N2 beta_l1
         <dbl> <dbl>    <dbl> <dbl> <dbl> <dbl>   <dbl>
      1      2     2     20.4  30.2     5     1    1.48

# `ss_divpart()` works as expected when q = 0 and spectro-temporal subsetting

    Code
      soundscapeR::ss_divpart(soundscape_list = soundscape_obj_CVR_case_study,
        qvalue = 0, mintime = "06:00:00", maxtime = "18:00:00", minfreq = 500,
        maxfreq = 2000)
    Output
      # A tibble: 1 x 7
        levels     q alpha_l1 gamma    N1    N2 beta_l1
         <dbl> <dbl>    <dbl> <dbl> <dbl> <dbl>   <dbl>
      1      2     0     21.2  56.3     5     1    2.66

# `ss_divpart()` works as expected when q = 1 and spectro-temporal subsetting

    Code
      soundscapeR::ss_divpart(soundscape_list = soundscape_obj_CVR_case_study,
        qvalue = 1, mintime = "06:00:00", maxtime = "18:00:00", minfreq = 500,
        maxfreq = 2000)
    Output
      # A tibble: 1 x 7
        levels     q alpha_l1 gamma    N1    N2 beta_l1
         <dbl> <dbl>    <dbl> <dbl> <dbl> <dbl>   <dbl>
      1      2     1     17.7  45.1     5     1    2.55

# `ss_divpart()` works as expected when q = 2 and spectro-temporal subsetting

    Code
      soundscapeR::ss_divpart(soundscape_list = soundscape_obj_CVR_case_study,
        qvalue = 2, mintime = "06:00:00", maxtime = "18:00:00", minfreq = 500,
        maxfreq = 2000)
    Output
      # A tibble: 1 x 7
        levels     q alpha_l1 gamma    N1    N2 beta_l1
         <dbl> <dbl>    <dbl> <dbl> <dbl> <dbl>   <dbl>
      1      2     2     14.7  38.2     5     1    2.59

