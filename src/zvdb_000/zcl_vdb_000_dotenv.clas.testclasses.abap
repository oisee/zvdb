CLASS lcl_ DEFINITION DEFERRED.
CLASS zcl_vdb_000_dotenv DEFINITION LOCAL FRIENDS lcl_.

CLASS lcl_ DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
  PRIVATE SECTION.
    DATA:
      cut TYPE REF TO zcl_vdb_000_dotenv.  "class under test
    METHODS: setup.
    METHODS: get_azure_openai FOR TESTING.
    METHODS: get_default_env_path FOR TESTING.
    METHODS: new FOR TESTING.
    METHODS: parse_file FOR TESTING.
    METHODS: v FOR TESTING.
ENDCLASS.       "lcl_


CLASS lcl_ IMPLEMENTATION.
  METHOD setup.
    cut = zcl_vdb_000_dotenv=>new( ).
  ENDMETHOD.
  METHOD get_azure_openai.
    DATA lv_path TYPE string.
    DATA rs_ TYPE zcl_vdb_000_dotenv=>ts_openai.
    rs_ = zcl_vdb_000_dotenv=>get_azure_openai( lv_path ).
  ENDMETHOD.


  METHOD get_default_env_path.
    DATA(lv_path) = cut->get_default_env_path(  ).
  ENDMETHOD.


  METHOD new.
    DATA lv_path TYPE string.
    cut = zcl_vdb_000_dotenv=>new( lv_path ).
  ENDMETHOD.

  METHOD parse_file.
    cut->parse_file( ).
  ENDMETHOD.

  METHOD v.
    DATA(lv_) = cut->v( 'API_URL' ).
  ENDMETHOD.

ENDCLASS.
