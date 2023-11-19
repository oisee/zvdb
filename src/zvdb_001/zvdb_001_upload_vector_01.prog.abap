*&---------------------------------------------------------------------*
*& Report ZVDB_001_UPLOAD_VECTOR_01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zvdb_001_upload_vector_01 MESSAGE-ID zcol_177.

*&---------------------------------------------------------------------*
*& Report ZVDB_001_UPLOAD_01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*


CLASS lcl_ DEFINITION.
  PUBLIC SECTION.
    CONSTANTS: cr_lf TYPE string VALUE cl_abap_char_utilities=>cr_lf.
    "TYPES: ts_ TYPE string.
    TYPES: ts_ TYPE zvdb_001_vector.
    TYPES: tt_ TYPE STANDARD TABLE OF ts_ WITH DEFAULT KEY.

    TYPES: ts_vhs TYPE zvdb_001_vhs.
    TYPES: tt_vhs TYPE STANDARD TABLE OF ts_vhs WITH DEFAULT KEY.

    TYPES: ts_set TYPE zvdb_001_set.
    TYPES: tt_set TYPE STANDARD TABLE OF ts_set WITH DEFAULT KEY.

    METHODS preview_list IMPORTING ir_ TYPE REF TO tt_ RETURNING VALUE(rv_) TYPE sy-ucomm.
    METHODS import     RETURNING VALUE(rt_) TYPE tt_.
    METHODS import_vhs RETURNING VALUE(rt_) TYPE tt_vhs.
    METHODS import_set RETURNING VALUE(rt_) TYPE tt_set.
    METHODS go.
    METHODS vhs.
    METHODS set.
    DATA: gv_msg.
ENDCLASS.


PARAMETERS: p_bid TYPE zvdb_001_vector-bid.
PARAMETERS: q TYPE guid DEFAULT '000D3A7A9A961EDE9CD283424391F4F2' . "ABAP is programming language for SAP R/3.

CLASS lcl_ IMPLEMENTATION.

  METHOD import.
    DATA: lt_ft TYPE filetable.
    DATA: lv_rc TYPE i.

    DATA(lv_default_filename) = |*V*.txt|.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
*      window_title            = window_title      " Title Of File Open Dialog
         default_extension       = 'TXT'
         default_filename        = CONV #( lv_default_filename )
*      file_filter             = file_filter       " File Extension Filter String
*      with_encoding           = with_encoding     " File Encoding
*      initial_directory       = initial_directory " Initial Directory
*      multiselection          = multiselection    " Multiple selections poss.
      CHANGING
        file_table              = lt_ft        " Table Holding Selected Files
        rc                      = lv_rc        " Return Code, Number of Files or -1 If Error Occurred
*      user_action             = user_action       " User Action (See Class Constants ACTION_OK, ACTION_CANCEL)
*      file_encoding           = file_encoding
      EXCEPTIONS
        file_open_dialog_failed = 1                 " "Open File" dialog failed
        cntl_error              = 2                 " Control error
        error_no_gui            = 3                 " No GUI available
        not_supported_by_gui    = 4                 " GUI does not support this
        OTHERS                  = 5
    ).
    IF sy-subrc <> 0.
      zcl_cpu=>ok( ).
    ENDIF.

    DATA(ls_file) = VALUE #( lt_ft[ 1 ] OPTIONAL ).
    DATA lt_ LIKE rt_.
*--------------------------------------------------------------------*
    cl_gui_frontend_services=>gui_upload(
     EXPORTING
       filename                = CONV #( ls_file-filename )
       filetype                = 'DAT'              " File Type (ASCII, Binary)
       has_field_separator     = 'X'
     IMPORTING
       filelength              = DATA(lv_filelength)" File Length
       header                  = DATA(lv_header)    " File Header in Case of Binary Upload
     CHANGING
       data_tab                = lt_ " Transfer table for file contents
     EXCEPTIONS
       file_open_error         = 1                  " File does not exist and cannot be opened
       file_read_error         = 2                  " Error when reading file
       no_batch                = 3                  " Cannot execute front-end function in background
       gui_refuse_filetransfer = 4                  " Incorrect front end or error on front end
       invalid_type            = 5                  " Incorrect parameter FILETYPE
       no_authority            = 6                  " No upload authorization
       unknown_error           = 7                  " Unknown error
       bad_data_format         = 8                  " Cannot Interpret Data in File
       header_not_allowed      = 9                  " Invalid header
       separator_not_allowed   = 10                 " Invalid separator
       header_too_long         = 11                 " Header information currently restricted to 1023 bytes
       unknown_dp_error        = 12                 " Error when calling data provider
       access_denied           = 13                 " Access to File Denied
       dp_out_of_memory        = 14                 " Not enough memory in data provider
       disk_full               = 15                 " Storage medium is full.
       dp_timeout              = 16                 " Data provider timeout
       not_supported_by_gui    = 17                 " GUI does not support this
       error_no_gui            = 18                 " GUI not available
       OTHERS                  = 19
   ).
    IF sy-subrc <> 0.
      MESSAGE e000 WITH 'File Error' INTO gv_msg.
      zcx_s=>raise( sy ).
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    rt_ = lt_.

  ENDMETHOD.

  METHOD import_vhs.
    DATA: lt_ft TYPE filetable.
    DATA: lv_rc TYPE i.

    DATA(lv_default_filename) = |*V*.txt|.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
*      window_title            = window_title      " Title Of File Open Dialog
         default_extension       = 'TXT'
         default_filename        = CONV #( lv_default_filename )
*      file_filter             = file_filter       " File Extension Filter String
*      with_encoding           = with_encoding     " File Encoding
*      initial_directory       = initial_directory " Initial Directory
*      multiselection          = multiselection    " Multiple selections poss.
      CHANGING
        file_table              = lt_ft        " Table Holding Selected Files
        rc                      = lv_rc        " Return Code, Number of Files or -1 If Error Occurred
*      user_action             = user_action       " User Action (See Class Constants ACTION_OK, ACTION_CANCEL)
*      file_encoding           = file_encoding
      EXCEPTIONS
        file_open_dialog_failed = 1                 " "Open File" dialog failed
        cntl_error              = 2                 " Control error
        error_no_gui            = 3                 " No GUI available
        not_supported_by_gui    = 4                 " GUI does not support this
        OTHERS                  = 5
    ).
    IF sy-subrc <> 0.
      zcl_cpu=>ok( ).
    ENDIF.

    DATA(ls_file) = VALUE #( lt_ft[ 1 ] OPTIONAL ).
    DATA lt_ LIKE rt_.
*--------------------------------------------------------------------*
    cl_gui_frontend_services=>gui_upload(
     EXPORTING
       filename                = CONV #( ls_file-filename )
       filetype                = 'DAT'              " File Type (ASCII, Binary)
       has_field_separator     = 'X'
     IMPORTING
       filelength              = DATA(lv_filelength)" File Length
       header                  = DATA(lv_header)    " File Header in Case of Binary Upload
     CHANGING
       data_tab                = lt_ " Transfer table for file contents
     EXCEPTIONS
       file_open_error         = 1                  " File does not exist and cannot be opened
       file_read_error         = 2                  " Error when reading file
       no_batch                = 3                  " Cannot execute front-end function in background
       gui_refuse_filetransfer = 4                  " Incorrect front end or error on front end
       invalid_type            = 5                  " Incorrect parameter FILETYPE
       no_authority            = 6                  " No upload authorization
       unknown_error           = 7                  " Unknown error
       bad_data_format         = 8                  " Cannot Interpret Data in File
       header_not_allowed      = 9                  " Invalid header
       separator_not_allowed   = 10                 " Invalid separator
       header_too_long         = 11                 " Header information currently restricted to 1023 bytes
       unknown_dp_error        = 12                 " Error when calling data provider
       access_denied           = 13                 " Access to File Denied
       dp_out_of_memory        = 14                 " Not enough memory in data provider
       disk_full               = 15                 " Storage medium is full.
       dp_timeout              = 16                 " Data provider timeout
       not_supported_by_gui    = 17                 " GUI does not support this
       error_no_gui            = 18                 " GUI not available
       OTHERS                  = 19
   ).
    IF sy-subrc <> 0.
      MESSAGE e000 WITH 'File Error' INTO gv_msg.
      zcx_s=>raise( sy ).
      RETURN.
    ENDIF.

*--------------------------------------------------------------------*
    rt_ = lt_.

  ENDMETHOD.
  METHOD import_set.
    DATA: lt_ft TYPE filetable.
    DATA: lv_rc TYPE i.

    DATA(lv_default_filename) = |*V*.txt|.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
*      window_title            = window_title      " Title Of File Open Dialog
         default_extension       = 'TXT'
         default_filename        = CONV #( lv_default_filename )
*      file_filter             = file_filter       " File Extension Filter String
*      with_encoding           = with_encoding     " File Encoding
*      initial_directory       = initial_directory " Initial Directory
*      multiselection          = multiselection    " Multiple selections poss.
      CHANGING
        file_table              = lt_ft        " Table Holding Selected Files
        rc                      = lv_rc        " Return Code, Number of Files or -1 If Error Occurred
*      user_action             = user_action       " User Action (See Class Constants ACTION_OK, ACTION_CANCEL)
*      file_encoding           = file_encoding
      EXCEPTIONS
        file_open_dialog_failed = 1                 " "Open File" dialog failed
        cntl_error              = 2                 " Control error
        error_no_gui            = 3                 " No GUI available
        not_supported_by_gui    = 4                 " GUI does not support this
        OTHERS                  = 5
    ).
    IF sy-subrc <> 0.
      zcl_cpu=>ok( ).
    ENDIF.

    DATA(ls_file) = VALUE #( lt_ft[ 1 ] OPTIONAL ).
    DATA lt_ LIKE rt_.
*--------------------------------------------------------------------*
    cl_gui_frontend_services=>gui_upload(
     EXPORTING
       filename                = CONV #( ls_file-filename )
       filetype                = 'DAT'              " File Type (ASCII, Binary)
       has_field_separator     = 'X'
     IMPORTING
       filelength              = DATA(lv_filelength)" File Length
       header                  = DATA(lv_header)    " File Header in Case of Binary Upload
     CHANGING
       data_tab                = lt_ " Transfer table for file contents
     EXCEPTIONS
       file_open_error         = 1                  " File does not exist and cannot be opened
       file_read_error         = 2                  " Error when reading file
       no_batch                = 3                  " Cannot execute front-end function in background
       gui_refuse_filetransfer = 4                  " Incorrect front end or error on front end
       invalid_type            = 5                  " Incorrect parameter FILETYPE
       no_authority            = 6                  " No upload authorization
       unknown_error           = 7                  " Unknown error
       bad_data_format         = 8                  " Cannot Interpret Data in File
       header_not_allowed      = 9                  " Invalid header
       separator_not_allowed   = 10                 " Invalid separator
       header_too_long         = 11                 " Header information currently restricted to 1023 bytes
       unknown_dp_error        = 12                 " Error when calling data provider
       access_denied           = 13                 " Access to File Denied
       dp_out_of_memory        = 14                 " Not enough memory in data provider
       disk_full               = 15                 " Storage medium is full.
       dp_timeout              = 16                 " Data provider timeout
       not_supported_by_gui    = 17                 " GUI does not support this
       error_no_gui            = 18                 " GUI not available
       OTHERS                  = 19
   ).
    IF sy-subrc <> 0.
      MESSAGE e000 WITH 'File Error' INTO gv_msg.
      zcx_s=>raise( sy ).
      RETURN.
    ENDIF.

*--------------------------------------------------------------------*
    rt_ = lt_.

  ENDMETHOD.


  METHOD preview_list.
    RETURN.
    rv_ = NEW zcl_eui_alv(
      ir_table   = ir_
      is_layout  = VALUE #( zebra = 'X' no_toolbar = 'X' )
      it_mod_catalog = VALUE lvc_t_fcat( ( fieldname = 'PROCESSED' tech = 'X' edit = 'X' ) )
    )->popup( )->show( io_handler = me ).

  ENDMETHOD.


  METHOD go.
    DATA(lt_) = me->import( ).
    IF lt_ IS INITIAL.
      RETURN.
    ENDIF.

    MODIFY zvdb_001_vector FROM TABLE lt_.
    COMMIT WORK AND WAIT.
  ENDMETHOD.

  METHOD vhs.
    DATA(lt_) = me->import_vhs( ).
    IF lt_ IS INITIAL.
      RETURN.
    ENDIF.

    MODIFY zvdb_001_vhs FROM TABLE lt_.
    COMMIT WORK AND WAIT.
  ENDMETHOD.

  METHOD set.
    DATA(lt_) = me->import_set( ).
    IF lt_ IS INITIAL.
      RETURN.
    ENDIF.

    MODIFY zvdb_001_set FROM TABLE lt_.
    COMMIT WORK AND WAIT.
  ENDMETHOD.

ENDCLASS.

AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'ONLI'.
      DATA(lo_) = NEW lcl_( ).
      lo_->go( ).
    WHEN 'VHS'.
      lo_ = NEW lcl_( ).
      lo_->vhs( ).
    WHEN 'SET'.
      lo_ = NEW lcl_( ).
      lo_->set( ).
    WHEN OTHERS.
      RETURN.
  ENDCASE.
