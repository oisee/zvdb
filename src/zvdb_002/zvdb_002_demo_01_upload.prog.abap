*&---------------------------------------------------------------------*
*& Report Zvdb_002_UPLOAD_01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zvdb_002_demo_01_upload MESSAGE-ID zvdb_002.


CLASS lcl_ DEFINITION.
  PUBLIC SECTION.
    CONSTANTS: cr_lf TYPE string VALUE cl_abap_char_utilities=>cr_lf.
    "TYPES: ts_ TYPE string.
    TYPES: BEGIN OF ts_,
             p TYPE string,
             v TYPE string,
           END OF ts_.
    TYPES: tt_ TYPE STANDARD TABLE OF ts_ WITH DEFAULT KEY.

    METHODS preview_list IMPORTING ir_ TYPE REF TO tt_ RETURNING VALUE(rv_) TYPE sy-ucomm.
    METHODS import RETURNING VALUE(rt_) TYPE tt_.
    METHODS go.
    DATA: gv_msg.
ENDCLASS.


PARAMETERS: p_bid TYPE zvdb_002_vector-bid.
PARAMETERS: q TYPE guid DEFAULT '000D3A7A9A961EDE9CD283424391F4F2' . "ABAP is programming language for SAP R/3.

CLASS lcl_ IMPLEMENTATION.

  METHOD import.
    DATA: lt_ft TYPE filetable.
    DATA: lv_rc TYPE i.

    DATA(lv_default_filename) = |sample*.tsv|.

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
*
    ENDIF.

    DATA(ls_file) = VALUE #( lt_ft[ 1 ] OPTIONAL ).
    DATA lt_ TYPE tt_.
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
      "zcx_s=>raise( sy ).
      RETURN.
    ENDIF.

    IF lt_ IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_exit_command) = me->preview_list( REF #( lt_ ) ).
*    IF lv_exit_command NE 'OK'.
*      RETURN.
*    ENDIF.
*--------------------------------------------------------------------*
    rt_ = lt_.

  ENDMETHOD.

  METHOD preview_list.

*    rv_ = NEW zcl_eui_alv(
*      ir_table   = ir_
*      is_layout  = VALUE #( zebra = 'X' no_toolbar = 'X' )
*      it_mod_catalog = VALUE lvc_t_fcat( ( fieldname = 'PROCESSED' tech = 'X' edit = 'X' ) )
*    )->popup( )->show( io_handler = me ).
    DATA: lo_alv TYPE REF TO cl_salv_table.
    TRY.
        " Create an ALV grid instance
        cl_salv_table=>factory( IMPORTING r_salv_table   = lo_alv
                                CHANGING  t_table        = ir_->*
                   ).

        lo_alv->get_display_settings( )->set_striped_pattern( 'X' ).
        lo_alv->get_functions( )->set_all( abap_true ).
        lo_alv->display( ).
      CATCH cx_salv_msg INTO DATA(lx_salv_msg).
        " Exception handling
        " Handle the exception as per your program's error handling strategy
    ENDTRY.

    " Set the return value as needed
    rv_ = 'OK'.

  ENDMETHOD.

  METHOD go.
    DATA:lt_s TYPE string_table.
    DATA:lt_v TYPE zcl_vdb_002_lib=>tt_embedding.
    DATA(lt_) = me->import( ).

    IF lt_ IS INITIAL.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA(lo_l) = zcl_vdb_002_lib=>new( p_bid ).

    LOOP AT lt_ REFERENCE INTO DATA(lr_).
      DATA(lx_v) = lo_l->quantize_from_string( lr_->v ).
      DATA(ls_v) = lo_l->create_vector(
                     ix_        = lx_v
                     iv_payload = lr_->p
                   ).
      lo_l->save_vector( ls_v ).
    ENDLOOP.

    lo_l->regen_recal_hash_and_reindex( p_bid ).

  ENDMETHOD.

ENDCLASS.

AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'ONLI'.
      DATA(lo_) = NEW lcl_( ).
      lo_->go( ).
  ENDCASE.
