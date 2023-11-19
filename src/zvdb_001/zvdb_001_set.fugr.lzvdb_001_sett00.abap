*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZVDB_001_SET....................................*
DATA:  BEGIN OF STATUS_ZVDB_001_SET                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZVDB_001_SET                  .
CONTROLS: TCTRL_ZVDB_001_SET
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZVDB_001_SET                  .
TABLES: ZVDB_001_SET                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
