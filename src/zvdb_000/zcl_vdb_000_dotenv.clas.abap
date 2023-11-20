CLASS zcl_vdb_000_dotenv DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_env,
             api_url       TYPE string,
             api_ver       TYPE string,
             api_key       TYPE string,
             api_dep       TYPE string,
             api_dep_embed TYPE string,
           END OF ts_env.
*    TYPES: ts_openai TYPE zif_oai_types=>ts_env.

    TYPES: BEGIN OF ts_kv,
             k TYPE string,
             v TYPE string,
           END OF ts_kv.
    TYPES: tt_kv TYPE TABLE OF ts_kv WITH KEY k.

    DATA: mv_path TYPE string.
    DATA: mt_kv TYPE tt_kv.

    CLASS-METHODS new
      IMPORTING
        iv_path            TYPE string DEFAULT 'C:\TEMP\.ENV'
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_vdb_000_dotenv.

    METHODS: v
      IMPORTING
        k          TYPE string
      RETURNING
        VALUE(rv_) TYPE string.

    CLASS-METHODS: get_default_env_path
      RETURNING
        VALUE(rv_) TYPE string.


    CLASS-METHODS: get_azure_openai
      IMPORTING iv_path    TYPE string DEFAULT 'C:\TEMP\.ENV'
      RETURNING VALUE(rs_) TYPE ts_env.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS: constructor
      IMPORTING
        iv_path TYPE string.

    METHODS: parse_file.

ENDCLASS.



CLASS ZCL_VDB_000_DOTENV IMPLEMENTATION.


  METHOD constructor.
    me->mv_path = COND #( WHEN iv_path IS NOT INITIAL THEN iv_path
                          ELSE me->get_default_env_path( ) ).
    me->parse_file( ).
  ENDMETHOD.


  METHOD get_azure_openai.
    DATA: lo_ TYPE REF TO zcl_vdb_000_dotenv.
* initialize the class with the path to the .env file
    lo_ = NEW zcl_vdb_000_dotenv( iv_path ).

* create a structure to hold the environmental variables
    rs_ = VALUE ts_env(
      api_url       = lo_->v( k = 'API_URL' )
      api_ver       = lo_->v( k = 'API_VER' )
      api_key       = lo_->v( k = 'API_KEY' )
      api_dep       = lo_->v( k = 'API_DEP' )
      api_dep_embed = lo_->v( k = 'API_DEP_EMBED' )
    ).

  ENDMETHOD.


  METHOD get_default_env_path.
    cl_gui_frontend_services=>get_platform(
      RECEIVING
        platform             =  DATA(lv_os)
      EXCEPTIONS
        error_no_gui         = 1                " No GUI available
        cntl_error           = 2                " Control error
        not_supported_by_gui = 3                " GUI does not support this
        OTHERS               = 4
    ).
    IF sy-subrc <> 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    "@TODO: replace with STVARV
    CASE lv_os.
      WHEN cl_gui_frontend_services=>platform_windowsxp.
        rv_ = '%USERPROFILE%\.env'.
        "rv_ = 'C:\Temp\.env'.
      WHEN cl_gui_frontend_services=>platform_linux.
        rv_ = '$HOME/.env'.
      WHEN cl_gui_frontend_services=>platform_macosx OR
           cl_gui_frontend_services=>platform_mac.
        rv_ = '$HOME/.env'.
      WHEN OTHERS.
        rv_ = '$HOME/.env'.
    ENDCASE.

  ENDMETHOD.


  METHOD new.
    ro_instance = NEW zcl_vdb_000_dotenv( iv_path = iv_path ).
  ENDMETHOD.


  METHOD parse_file.
    DATA: lt_file_content TYPE string_t,
          lt_split        TYPE string_t.

    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename = me->mv_path
      CHANGING
        data_tab = lt_file_content.

    LOOP AT lt_file_content REFERENCE INTO DATA(lr_).
      DATA(lv_line) = condense( lr_->* ).
      IF lv_line IS NOT INITIAL AND NOT lv_line+0(1) = '#'.
        SPLIT lv_line AT '=' INTO TABLE lt_split.
        IF lines( lt_split ) = 2.
          APPEND VALUE #( k = lt_split[ 1 ] v = lt_split[ 2 ] ) TO me->mt_kv.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD v.
    DATA(lr_kv) = REF #( me->mt_kv[ k = k ] OPTIONAL ).
    IF lr_kv IS BOUND.
      rv_ = lr_kv->v.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
