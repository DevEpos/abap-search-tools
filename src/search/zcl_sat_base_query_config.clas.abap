"! <p class="shorttext synchronized">Base configuration for Object Search Query</p>
CLASS zcl_sat_base_query_config DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_sat_object_search_config
      ABSTRACT METHODS get_type.
    INTERFACES zif_sat_c_object_search.

    METHODS constructor.

  PROTECTED SECTION.
    ALIASES c_search_option   FOR zif_sat_c_object_search~c_search_option.
    ALIASES c_general_options FOR zif_sat_c_object_search~c_general_search_params.

    CONSTANTS:
      BEGIN OF c_general_image_keys,
        type_group  TYPE string VALUE 'ABAP:IMG_TYPE_GROUP',
        type_folder TYPE string VALUE 'ABAP:IMG_TYPE_FOLDER',
        column      TYPE string VALUE 'ABAP:IMG_COLUMN',
        param       TYPE string VALUE 'ABAP:IMG_PARAM',
        checked_box TYPE string VALUE 'ABAP:IMG_CHECKED_BOX',
      END OF c_general_image_keys.

    DATA mt_options         TYPE zif_sat_ty_object_search=>ty_t_query_filter.
    DATA mt_allowed_options TYPE zif_sat_ty_object_search=>ty_t_options.
    DATA ms_search_type     TYPE zif_sat_ty_object_search=>ty_s_search_type.
    DATA mf_fill_add_data   TYPE abap_bool.

    "! Builds configuration
    "! NOTE: Must be overridden in sub classes
    METHODS build_config.

    METHODS get_user_filt_conf
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_s_query_filter.

    METHODS get_package_filt_conf
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_s_query_filter.

    METHODS get_description_filt_conf
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_s_query_filter.

    METHODS get_max_rows_filt_conf
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_s_query_filter.

    METHODS get_rel_state_filt_conf
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_s_query_filter.

    METHODS get_general_image
      IMPORTING
        iv_image_key  TYPE string
      RETURNING
        VALUE(result) TYPE string.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_image_keys,
        owner       TYPE string VALUE 'ABAP:IMG_USER',
        owner_entry TYPE string VALUE 'ABAP:IMG_USER_EDIT',
        package     TYPE string VALUE 'ABAP:IMG_PACKAGE',
        api         TYPE string VALUE 'ABAP:IMG_API',
        description TYPE string VALUE 'ABAP:IMG_DESCRIPTION',
      END OF c_image_keys.

    METHODS get_image
      IMPORTING
        iv_image_key  TYPE string
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.


CLASS zcl_sat_base_query_config IMPLEMENTATION.
  METHOD constructor.
    build_config( ).
  ENDMETHOD.

  METHOD build_config.
    RETURN.
  ENDMETHOD.

  METHOD zif_sat_object_search_config~enable_additional_data.
    IF if_enable = abap_true AND mf_fill_add_data <> if_enable.
      CLEAR: mt_options,
             ms_search_type.
    ENDIF.
    mf_fill_add_data = if_enable.

    IF ms_search_type IS INITIAL.
      build_config( ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_allowed_options.
    IF mt_allowed_options IS INITIAL.
      mt_allowed_options = VALUE #( FOR option IN mt_options
                                    ( sign = 'I' option = 'EQ' low = option-name ) ).
    ENDIF.
    rt_options = mt_allowed_options.
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_option_config.
    rs_option = mt_options[ name = iv_option ].
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_search_config.
    result = ms_search_type.
  ENDMETHOD.

  METHOD zif_sat_object_search_config~has_option.
    rf_has_option = xsdbool( line_exists( mt_options[ name = iv_option ] ) ).
  ENDMETHOD.

  METHOD zif_sat_object_search_config~get_options.
    rt_options = mt_options.
  ENDMETHOD.

  METHOD get_description_filt_conf.
    result = VALUE #( name           = c_general_options-description
                      allowed_length = 40
                      patterns       = abap_true
                      no_uppercase   = abap_true
                      img_key        = c_image_keys-description
                      img_encoded    = get_image( c_image_keys-description ) ).
  ENDMETHOD.

  METHOD get_max_rows_filt_conf.
    result = VALUE #( name = c_general_options-max_rows single = abap_true no_negation = abap_true internal = abap_true ).
  ENDMETHOD.

  METHOD get_package_filt_conf.
    result = VALUE #(
        name           = c_general_options-package
        img_key        = c_image_keys-package
        img_encoded    = get_image( c_image_keys-package )
        allowed_length = 30
        patterns       = abap_true
        content_assist = VALUE #( assist_type      = zif_sat_c_object_search=>c_filter_content_assist_type-ris
                                  adt_object_types = VALUE #( ( zif_sat_c_object_types=>package ) ) ) ).
  ENDMETHOD.

  METHOD get_user_filt_conf.
    result = VALUE #( name           = c_general_options-user
                      img_key        = c_image_keys-owner
                      img_encoded    = get_image( c_image_keys-owner )
                      patterns       = abap_true
                      allowed_length = 12
                      content_assist = VALUE #(
                          assist_type     = zif_sat_c_object_search=>c_filter_content_assist_type-user
                          proposal_images = VALUE #( ( img_key     = c_image_keys-owner_entry
                                                       img_encoded = get_image( c_image_keys-owner_entry ) ) ) ) ).
  ENDMETHOD.

  METHOD get_rel_state_filt_conf.
    result = VALUE #( name           = c_general_options-release_state
                      img_key        = c_image_keys-api
                      img_encoded    = get_image( c_image_keys-api )
                      caching        = abap_true
                      content_assist = VALUE #(
                          assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
                          category_scheme       = zif_sat_c_object_search=>c_content_assist-category_scheme
                          category_term         = zif_sat_c_object_search=>c_content_assist-terms-release_state
                          proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter ) ).
  ENDMETHOD.

  METHOD get_image.
    CHECK mf_fill_add_data = abap_true.

    CASE iv_image_key.

      WHEN c_image_keys-owner.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAACGElEQVR4nGNgAIKGhk6Z1q4JG5vaez+AcHNn/0aQGAMxAKSwsb3nw4HDx35//vzl/7PnL/5v2rrzd0Nb9weQXE1jx/GKhs7/BbU9/zOq+g7EVPVLohjQ2j1x08Ejx37/B4KPnz79f/zk6f8r1278X7xi9W+g` &&
                 `wZvKOzr4CxraFTKrJugllE88GVk0qR/FgMaO3vefPn3+//Hjp/+PoJqPnjj9f+OWnf/rWrreI6uNLp5UHFw4ZT+GAU+fPkfRvH3X3v9Llq/5X9vcCTcgt6qzJ7aw67VfZtd3h8TOHQwM/xnBEi1d/Zs2bdvx+/LV60DNp8CaV6zZ8L9n4tTfQBdsghmQUtapGZrb+dctpfOf` &&
                 `dlhXJEog1gEDbNGy1b83btnxf8mKNf+7J0z7Xd3c8QE9JvwyOpfYJXReAmpiwoiJhraejUAbPwCdDcJYo9ExqVNdN7zDH3+8YgF6KbNlzDMWbDRJnf/BGIhN0xZsBIkRrRmkKblz5+/Ghaf/V8058T+sYctvo5R5H4gyxCx9wabkrp2/O1dc+N+46Mz/yjkn/xdOO/rfr3rD` &&
                 `b5O0+ZsIGgB09vsGoM3ImrMmHvof2777v1HyvPcEDQA6/33l7OMompO69/0Pa9z+35AYA0zT528Ka9zyG1lzVOuu/45Fq0HhQNgLoIACBZhf1cbfcUBnhwNtdipc/dsweS5xgQg3JHXeRqCTP0AxPBoB1JlikXfjC/kAAAAASUVORK5CYII=`.

      WHEN c_image_keys-owner_entry.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABL0lEQVR4nGNgoAfYmCXjsTKK++OKCI7PK2OZfUk2AKT55Ysl/x/f7P+/PJTpM4aC5NTk/ylpKf+3btr6f+HChf+XLFnyf8XyFf/XrF7zf8O6Df9hBjy82o7bgNT0VLAB79++///65ev/z589/3/v/r3/G9as` &&
                 `BRrA2b00kOE/CC8LYVo4P4GBA6cBfy+X/v8jK/tf9o8s2IA1lR7/N2RLfzm5MOLvkWnO/9cksH1ZEc68imgXLA/n+P/y1fL/779v/f/4VsP/0wvl/y8PYfxGtAEgZ4M0g/DDq+X/zy1TBIthGJCWnkbQAJBmsgwAG/JtM1wzdgMysBsAikaQhrcfV2G3nVgDXr9ZSJwB2BIS` &&
                 `SNPL5zNxGwACxaXF/7FhkBxI0/PHE/AbgA/AAg5rAFIKAIUslAwZrqbdAAAAAElFTkSuQmCC`.

      WHEN c_image_keys-api.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAACi0lEQVR4nGNgoBXYP/OMyOE5F/6D8P7ZF2WI0zTrrA9QowaIfXjORQ+YAYdmXwwBiU3KXaA8qWBeJpDJiKl5xlmPAzPPfgbi3zumHpu8tf/Atp2Tj73bOeno+029+w4srFs3b0v3wffbe4/878ufWYhhyP7p` &&
                 `5xVABuyddubvrsnHf+2ecuI/CO+adPz/jv6j/7d2Hfm7uXP/D5AB0S4JUUAtTBiuANo8AaZ5S++hr5ML526elDdn07rG3e83Ne//v75+79+JebN2ApUqo7hgUf06sbmVq/xWtWzfvbP/2P/NHQe+Ohm6RkIVynubBXmvLNvyfk3Frv/T0hedaw7vTe+Ona0I08+2tevwfxDe` &&
                 `0Lb305b2g//7s2ZvAopLAzEL1Kn8ExLnrFhRsPX/wqx1n1cUbvsPwiBxkAGcMAPW1O/8vKF+7/8J6bPWAcW5kHzHNjl+3sLFGRv/z01a+QXJAHGQJLuplqVzZkBhzeSMBWfXlO/8v6xoy5dMt2JjqO2Mlf5tWvMS17yZn7D2f0fQlCtJTtn91uoO0TAXMEEZ4j3xM5Ysy9/8` &&
                 `e2nO5v+L0td9nhq7aM7U6EWzZseufD03evX/GaFL/1a6tIACURaIBYCYGe7G9qhpSkvzN39elL3h14LkdX9Bts2LXfN/TtSq/zNDl4M1Tw1e9HV2xIo/iZa59ugxyNQZNTl5ecHWPyC/NQT37Gv26Ds7OXjhFxBu8ZpwucKpaeesiOV/Z0es/F/sWN+InpBAHIF878o6d32f` &&
                 `DCDbLNkip3B25Mr/IJxgltkBFNOxU3KOzrAp6gSylTASETQsQP4SA8VMoH64GswAKyVHb6h/QWElAooVbAagA04gNgViSyBWwKUIAC0NTrBGCkkHAAAAAElFTkSuQmCC`.

      WHEN c_image_keys-package.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABwUlEQVR4nM2R2yuDYRzH/QkuXLpwJReIXDiUNc2ZyOQ47J1jXqFIY9jW2Exy3AWbsclMkVOynEYvacPrlEJSwoUL/4ALfL2N2OG5cOlXv5769PZ5nj6vn9+/H3Fc6P6fP2Z6yl4ZLfXG9HKr/do6QRS8GaMR` &&
                 `PRMFdnU5LhbluLapcbmkgMNAQ1ss4Fgnbr7ZwRgNe3fZG1GwoRTj4ciAlxsrnlgjzuZlmG7Kw8Oh/oudGMFapdhUisiCtY4S3O4O4fF4HHfMCI4sLdBVpv+yPR2c5ibY2gvJghVpEZzTrWDn5HBa2mAfroIqjw/HNzucacP2QAVWpfm+AnFs2D2dEAV1AR+mBiEGxSlQ5MRD` &&
                 `ls13nUohD4NUKibrhajlR8Ln71AxEUGmmiywi1242hzA6bIG27o6yHN4OF5Q4cLWh/MVDXZ0NCarM95F0eGBPq+w0NlcRP1PRHa2lbud5xHRYWqGpTaD3MBclekR0WFudj3bPeKeoRFTlalkgVGS5op4Mq/ggsmw1V/B9cj1YOs9FCYkyWSBXiT4GC1Jgvuq8hPhzQylCa9E` &&
                 `gbUoOGCWCglyX0ls2JM3M1ER/kQBaYi13eYT+2ZcJOhXQdgAAAAASUVORK5CYII=`.

      WHEN c_image_keys-description.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAACoElEQVR4nJWT208TURCHefCBB9RytRBBVB5458n/gEcTjTERIybqA6C0u7RYkLCl3V52l91epBc0FrDl0lIupaTKxQQlVqLEiGm8JEZJIAYRC1TRtJRxzyHWEDDEk0yyO2d+35k5MyctbZ9FUdSB/WL2XG1t` &&
                 `VRl2oYay8TVg5WpK/0vczleftwu1S1brrQ1zG71l5sllnpeX7Ct08FXlTlPtU7tFuW5wTyZbBj4CMvaOa5NnlfMMo5DuKbzNyk6J6T5wmK4nuM6+hHpkCejeMAisDJuhIwAGmzPBMKpFo1F5dIfY3Eo0oiCLSZnUDM2DxhdJCWfCE/Am8hx/a32vgLa5NmlD4weKInNSAIEj` &&
                 `ozr3BPBCA0oV9B1BLHg9F4a11RWwWRu3Ad6XQI2sgMbSHlfrmiNid9K3AeImqlPjfwc8VyemOwi8w5LKQjC3gLb3BaiHF4EKRoEKfIUmhk+o1NRnDEFBOs8U3tT2zWKIvjMEdPc09vcSBeAj8sBPZO0ytar6UhrPyMs5pu7njbszfyEsCfT9R/i/myiEaOgIfB/PhF+TB2E1` &&
                 `mAEBlRSC7Ve+IQguA0FaWTJ+1RbGInQygmh7noGbKIaV0QJYf5gDsdAhfPJ4v37B11yWSAHQMrG1FZwIuSg8gctdy2C45wW9vgG6iBL4MlIkZiGF2FgmTOryk36FdGuoPjs5WJcd29FSgZOdZsVyzjCP4YL9Pb7ATnkJvHWfBA9xDBb6xVLGJLiMT57DMFyfFdk1VNt3QsbP` &&
                 `6QLgdGoxwK0oBSdZtjbaUvQjGsoTIVkw55CsD5CZ9J6TKTCySjyBxiYMMN48SyJ/j+LE7LRQuLHol8KQMjfqlef++30IgkzCskSFi6pM/+NzyYolHsXxMOqMV5l/Dfl+AwJivu+ppQ8rAAAAAElFTkSuQmCC`.
    ENDCASE.
  ENDMETHOD.

  METHOD get_general_image.
    CHECK mf_fill_add_data = abap_true.

    CASE iv_image_key.

      WHEN c_general_image_keys-column.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAACESURBVDhPY6AUMEJpMPDKX/sfyoSDbRODUdTgBSADHr79C8fYDEQHTFAaBdx6DcHEAOp6YfaWO//ff/37/86rv//P` &&
                 `Pfr7H8SHSuEEWL1ACsAwYM3++wz7T92H8ggDDANCHBUZHM0UoTzCYBB6AeR8irzw6QcEEwswDDh76T4cDwXAwAAAvGtEmZMujD0AAAAASUVORK5CYII=`.

      WHEN c_general_image_keys-type_folder.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAVFBMVEUAAAD48Mj46LDo0Ijw2JD44Jj44KD42Ij40HjgwHj42JD40IDYsHC8hTLDiza0fzKlbCStciu8fzLDhTatbCSeZietci+eXx2eXyCPUhn////46JgjdYhrAAAAAXRSTlMAQObYZgAAAFlJREFUeJyd` &&
                 `z0kSgCAMRNEoKojiBMaB+9/TEFPo0vLtfu8a4JeBvDucJDztY6xU2/uEB1dkjoelzHYexoZgBjBba7EWioZJa9QiNWwGO8ENBxpxN8CafTt2AcOvBf0oDmf4AAAAAElFTkSuQmCC`.

      WHEN c_general_image_keys-type_group.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAACl0lEQVR4nIWSS08TURiGWbjwB7hwpwt/gr/ChSsTXGqihIWGXgBLh8JMmZm2c2Y6M4VeuNhSW0og2DsXESgEBLExYILBcEmUYCIqKmqRmLSvnZFAGgFPcpKTk+95znc5VVWnrID73lW/UrflVeocp8WcuPxy` &&
                 `7WW/xzjiUy3fhXCmqMqWA1Uysf8FO8WaCz5Pvc+vGveEyMiB/fEa+GgObP8CRJnZJ8TqPBEURfOldsmwGPAYf0iR9IE9vgEuNgdPOwNFMkEmBvCxWTgVV8HppFz/CNzupiUtSJGpIpP8ACkY1aGpyQT2vu3qZ9nrBJPcBkvYgp2nKnsiE2ORTn8G8XuLstpSEkKD+svZVAi+` &&
                 `DhtktwXOUBx09ivoxBZoQSg0c20dNE2fOxQYdDud2YUQ6ISkMuUS5uHq7oWjN6vf67C2059AqbGS0hVBYwtz+28PSMMXQbTCntg8lHRBVBjQqY/HYHnbAmlInWEMTcwXn7xYQTmD80cZOIJxENKIuz2v9WAh0AOicvqLtu4xKA/7kMwtQgPH5vKl4EB8x0ixtcclZHbgCGfh` &&
                 `Ig9Q7XmlSySFhujtOQbz6+hNzYDi3Wh1qrBQLdW6wEWaunh3W4FJvQcfGYNQzuQ6WQTlCSI6PIXR/AbC2WdodXWg2amUHqWf/taEFZMQSIPPQWyFmtAm+L5piKReH18glix3XQPVUl9mUq8993L17eDw5LLJyt2qkEiknmYF2/5N3xq4gXxZYgbj4BBNjZc0cGZpdT05Pr3A` &&
                 `COqG2crdORpjRSZCw31OoArXpBU0q90/B0cntmeX36xmcnMLvOx/Z6JYw4lg5ccymQSh8ZdWQv9Q4jkvezfNFNvWQNMXzwQrJKLhhiaw0HTaaHFcOSv2D/3B33rlnt49AAAAAElFTkSuQmCC`.

      WHEN c_general_image_keys-param.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAArElEQVR4nGNgGAUkA3mfpc0UGrDsv4LPkna8iiJb9v6HYf+GXf9darb/t6jY8h9mgEbgsg94DQFpvPnuJwrWLdkINyAig4AhIAMOPv6KgpUK1sENiMmCGAJiYzUA5Ox5lz+gYPGc1SgG2EUv/63g` &&
                 `u+wtVgNAft567wsK5slYATcApFnZb9ldKd9lIlgNAAUYyM8gZ4NsBmtOXgI3AK9mQgBowHeyNYMARZppAgBR6p3qYaEtFgAAAABJRU5ErkJggg==`.

      WHEN c_general_image_keys-checked_box.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA5ElEQVR4nGNgGDSge9rq/8Tinmlr7mM14Ozb//8PvySMcRoAknzw5f//e5+x431Xnvy/+/n/N7wG3AMa8O4XJm6bte6/WWDh/2NXH7zAa8D9r///f/iNikGajf3ywfS7n/8e4zVg65nb/z8BNcFwO1QziAbx` &&
                 `P+AzoHf9SbDiBRsO/P/y5///jtkQzSAaxAfhT/gMOPjs7/+c1nlgTcmVk8F0J1Dz17//4fjzbwJeuPfp7//i9nlwzd+BmpDxV0IGPPr2//+333//r99z8v/Pf/8x8Pc/RBjw6x9u/AOfARQl5QEDAMqA2SocOZJBAAAAAElFTkSuQmCC`.

    ENDCASE.
  ENDMETHOD.
ENDCLASS.
