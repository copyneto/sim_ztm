"Name: \TY:/SCMTMS/CL_UI_TBI_TOR_ITEM_EQI\IN:/SCMTMS/IF_UI_TBI_FPM\ME:ADAPT_EVENT\SE:END\EI
ENHANCEMENT 0 ZEI_TM_EQUI_SELECTED.

  " Export de dados para a classe : ZCLTM_ALOC_MANUAL_ACTION
  "                        método : CHECK_SELECTION
  "                        método : EXECUTE
  DATA: lt_compartimentos_keys TYPE /bobf/t_frw_key.
  lt_compartimentos_keys = is_selection-selection.
  EXPORT lt_compartimentos_keys FROM lt_compartimentos_keys to MEMORY ID 'ZTM_EQUI_INDEX'.

ENDENHANCEMENT.
