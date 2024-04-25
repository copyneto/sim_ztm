*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTMT_DET_CONTA..................................*
DATA:  BEGIN OF STATUS_ZTMT_DET_CONTA                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTMT_DET_CONTA                .
CONTROLS: TCTRL_ZTMT_DET_CONTA
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTMT_DET_CONTA                .
TABLES: ZTMT_DET_CONTA                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
