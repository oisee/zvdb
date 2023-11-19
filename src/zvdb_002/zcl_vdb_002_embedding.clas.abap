class ZCL_VDB_002_EMBEDDING definition
  public
  final
  create private .

public section.

  class-methods NEW
    importing
      !IV_ type ZVDB_002_VECTOR-BID
    returning
      value(RO_) type ref to ZCL_VDB_002_EMBEDDING .
  methods ANSWER
    importing
      !IV_ type STRING
    exporting
      !EV_ type STRING
    returning
      value(RX_) type ZCL_VDB_002_LIB=>TS_VECTOR-Q1B .
  methods RAG
    importing
      !IV_ type STRING
      !IT_ type ZCL_VDB_002_LIB=>TT_VECTOR
    exporting
      !EV_PROMPT type STRING
      !ET_USED type ZCL_VDB_002_LIB=>TT_VECTOR
    returning
      value(RV_) type STRING .
  methods ANSWER_AND_SAVE
    importing
      !IV_ type STRING
    exporting
      !EV_ type STRING
    returning
      value(RS_) type ZCL_VDB_002_LIB=>TS_VECTOR .
  methods EMBED
    importing
      !IV_ type STRING
    returning
      value(RX_) type ZCL_VDB_002_LIB=>TS_VECTOR-Q1B .
  methods EMBED_AND_SAVE
    importing
      !IV_ type STRING
    returning
      value(RS_) type ZCL_VDB_002_LIB=>TS_VECTOR .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: mo_env TYPE REF TO zcl_oai_01_dotenv.
    DATA: mo_sdk_lite TYPE REF TO zcl_oai_01_sdk_lite.
    DATA: mo_lib TYPE REF TO zcl_vdb_002_lib.
    DATA: ms_env TYPE zif_oai_types=>ts_env.
    METHODS: constructor
      IMPORTING iv_ TYPE zvdb_002_vector-bid.
ENDCLASS.



CLASS ZCL_VDB_002_EMBEDDING IMPLEMENTATION.


  METHOD answer.

    DATA(lo_in) = zcl_oai_01_chat_in=>new( VALUE #(
      messages =  VALUE #(
        ( role = 'system' content = |Behave as "prototype answer generator". Generate a sample passage that looks like an answer to a question:| )
        ( role = 'user'   content = iv_ )
      ) )
    ).

    DATA(lo_out) = mo_sdk_lite->chat_completions( lo_in ).
    DATA(lv_reply) = lo_out->get_reply( ).

    rx_ = me->embed( lv_reply ).
    ev_ = lv_reply.
  ENDMETHOD.


  METHOD answer_and_save.
    DATA lv_answer TYPE string.
    DATA(lx_) = answer( EXPORTING iv_ = iv_
                        IMPORTING ev_ = lv_answer  ).
    rs_ = mo_lib->create_vector(
                   ix_        = lx_
                   iv_payload = lv_answer
    ).
    mo_lib->save_vector( rs_ ).

  ENDMETHOD.


  METHOD constructor.
    mo_env = zcl_oai_01_dotenv=>new( ).
    ms_env = mo_env->get_azure_openai( ).
    mo_sdk_lite = zcl_oai_01_sdk_lite=>new( ms_env ).

    mo_lib = zcl_vdb_002_lib=>new( iv_  ).
  ENDMETHOD.


  METHOD embed.

    DATA(lo_in) = zcl_oai_01_embed_in=>new( VALUE #( input = iv_ ) ).
    DATA(lo_out) = mo_sdk_lite->embeddings( lo_in ).
    rx_ = lo_out->get_vector_q1b( ).

  ENDMETHOD.


  METHOD embed_and_save.
    DATA(lx_) = embed( iv_ ).
    rs_ = mo_lib->create_vector(
                   ix_        = lx_
                   iv_payload = iv_
    ).
    mo_lib->save_vector( rs_ ).

  ENDMETHOD.


  METHOD new.
    ro_ = NEW zcl_vdb_002_embedding( iv_ ).

  ENDMETHOD.


  METHOD rag.
    DATA(lv_lf) = cl_abap_char_utilities=>cr_lf.
    DATA(lt_) = it_.
    SORT lt_ BY rank DESCENDING.
    DELETE lt_ WHERE rank < 512.

    DATA:lv_data   TYPE string.
    DATA:lv_prompt TYPE string.
    DATA(lv_i) = 0.
    LOOP AT lt_ REFERENCE INTO DATA(lr_).
      IF strlen( lv_data ) > 20000.
        EXIT.
      ENDIF.
      lv_data = |{ lv_data }{ lr_->p }{ lv_lf }|.
      ADD 1 TO lv_i.
    ENDLOOP.
    APPEND LINES OF lt_ FROM 1 TO ( lv_i ) TO et_used.

    lv_prompt = |####START{ lv_lf }{ lv_lf }{ lv_data }{ lv_lf }{ lv_lf }####END| && lv_lf &&
                |####QUESTION { lv_lf }{ lv_lf } | &&
                |Using data mentioned above, please provide the answer to: { lv_lf }| &&
                |{ iv_ }|.

    DATA(lo_in) = zcl_oai_01_chat_in=>new( VALUE #(
       messages = VALUE #(
      ( role    = 'system'
        content = | You are helpful AI Assistant. | &&
                  | Answer the following question, using only pieces of information that mentioned after the ####START and before ####END,| &&
                  | do not make things up, if you do not have information, tell the user "I do not know.". Question will be marked as ####QUESTION: :| )
      ( role    = 'user'
        content = lv_prompt ) )
    ) )
    .
    ev_prompt = lv_prompt.

    data(lo_out) = mo_sdk_lite->chat_completions( lo_in ).

    rv_ = lo_out->get_reply( ).

  ENDMETHOD.
ENDCLASS.
