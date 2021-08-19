rmfd_guide_shortcuts <- tibble::tribble(
  ~shortcut, ~name,
  "pval", "parameter_value_file",
  "nam", "name_file"
)
rmfd_supported_codes <- c("MODFLOW-2005", "MODFLOW-OWHM", "MODFLOW-NWT",
                          "MODFLOW-LGR", "MODFLOW-CFP")
rmfd_supported_packages <- tibble::tribble(
  ~ftype,   ~rmf,
   'HOB',  'hob',
  'PVAL',  'pval',
   'DIS',  'dis',
  'ZONE',  'zon', 
  'MULT',  'mlt',
  'BAS6',  'bas',
  'HUF2',  'huf',
    'OC',   'oc',
   'WEL',  'wel',
   'GHB',  'ghb',
   'PCG',  'pcg',
  'KDEP', 'kdep',
   'LPF',  'lpf',
   'RCH',  'rch',
   'CHD',  'chd',
  'BCF6',  'bcf', 
  'HFB6',  'hfb',
   'RIV',  'riv',
   'DRN',  'drn',
   'EVT',  'evt',
   'SIP',  'sip',
   'DE4',  'de4',
   'NWT',  'nwt',
   'UPW',  'upw',
  'LVDA', 'lvda',
   'GMG',  'gmg',
  'LMT6',  'lmt'
)
rmfd_supported_length_units <- tibble::tribble(
     ~unit,        ~conv,
      "km",         1000,
       "m",            1,
      "dm",          0.1,
      "cm",         0.01,
      "mm",        0.001,
     "kmi",         1852,
      "in",       0.0254,
      "ft",       0.3048,
      "yd",       0.9144,
      "mi",     1609.344,
    "fath",     1.828804,
      "ch",     20.11684,
    "link",    0.2011684,
   "us-in",   0.02540005,
   "us-ft",    0.3048006,
   "us-yd",    0.9144018,
   "us-ch", 20.116840234,
   "us-mi",     1609.347,
  "ind-yd",    0.9143988,
  "ind-ft",    0.3047996
)
usethis::use_data(
  rmfd_supported_codes,
  rmfd_supported_packages,
  rmfd_guide_shortcuts,
  rmfd_supported_length_units,
  internal = TRUE,
  overwrite = TRUE
)
