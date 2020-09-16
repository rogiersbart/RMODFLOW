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
usethis::use_data(
  rmfd_supported_codes,
  rmfd_supported_packages,
  rmfd_guide_shortcuts,
  internal = TRUE,
  overwrite = TRUE
)
