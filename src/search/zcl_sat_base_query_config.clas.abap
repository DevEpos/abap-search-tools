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
    CONSTANTS c_object_name_input_key     TYPE string VALUE 'objectName'.
    CONSTANTS c_object_name_input_label   TYPE string VALUE 'Object Name'.
    CONSTANTS c_object_filter_input_key   TYPE string VALUE 'objectFilter'.
    CONSTANTS c_object_filter_input_label TYPE string VALUE 'Object Filters'.

    ALIASES c_search_option   FOR zif_sat_c_object_search~c_search_option.
    ALIASES c_general_options FOR zif_sat_c_object_search~c_general_search_params.

    CONSTANTS:
      c_type_image_key_prefix TYPE string VALUE `ABAP:IMG_SEARCHTYPE_`,
      BEGIN OF c_general_image_keys,
        type_group     TYPE string VALUE 'ABAP:IMG_TYPE_GROUP',
        type_folder    TYPE string VALUE 'ABAP:IMG_TYPE_FOLDER',
        column         TYPE string VALUE 'ABAP:IMG_COLUMN',
        param          TYPE string VALUE 'ABAP:IMG_PARAM',
        checked_box    TYPE string VALUE 'ABAP:IMG_CHECKED_BOX',
        generic_filter TYPE string VALUE 'ABAP:IMG_GENERIC_FILTER',
        extension      TYPE string VALUE 'ABAP:IMG_EXTENSION',
        maintenance    TYPE string VALUE 'ABAP:IMG_MAINTENANCE',
        table_source   TYPE string VALUE 'ABAP:IMG_TABLE_DATA_SOURCE',
      END OF c_general_image_keys,

      BEGIN OF c_output_option,
        tree TYPE string VALUE 'TREE',
        list TYPE string VALUE 'LIST',
      END OF c_output_option,

      BEGIN OF c_output_grouping_level,
        package TYPE string VALUE 'PACKAGE',
      END OF c_output_grouping_level.

    DATA mt_options         TYPE zif_sat_ty_object_search=>ty_query_filters.
    DATA mt_allowed_options TYPE zif_sat_ty_object_search=>ty_t_options.
    DATA ms_search_type     TYPE zif_sat_ty_object_search=>ty_search_type_config.

    "! Builds configuration
    "! NOTE: Must be overridden in sub classes
    METHODS build_config.

    METHODS get_user_filt_conf
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_changed_by_filt_conf
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_package_filt_conf
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_description_filt_conf
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_max_rows_filt_conf
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_appl_comp_filt_conf
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_softw_comp_filt_conf
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_created_on_filt_conf
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

    METHODS get_changed_on_filt_conf
      RETURNING
        VALUE(result) TYPE zif_sat_ty_object_search=>ty_query_filter.

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
        description TYPE string VALUE 'ABAP:IMG_DESCRIPTION',
        appl_comp   TYPE string VALUE 'ABAP:IMG_APPL_COMP',
        soft_comp   TYPE string VALUE 'ABAP:IMG_SOFT_COMP',
        changed_on  TYPE string VALUE 'ABAP:IMG_CHANGED_ON',
      END OF c_image_keys.

ENDCLASS.


CLASS zcl_sat_base_query_config IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.

  METHOD build_config.
    RAISE EXCEPTION TYPE zcx_sat_nc_exception.
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

  METHOD zif_sat_object_search_config~get_output_config.
    result = VALUE #( groupings = VALUE #( ( c_output_grouping_level-package ) ) ).
  ENDMETHOD.

  METHOD get_description_filt_conf.
    result = VALUE #(
        name             = c_general_options-description
        description      = 'Description'
        long_description = |Use '{ c_general_options-description }' to restrict the search query by a given Description. | &&
                           |Starting from NetWeaver 7.51 the input must no longer be case sensitive.\n\nExample:\n   { c_general_options-description } : some+descr|
        allowed_length   = 40
        no_uppercase     = abap_true
        patterns         = abap_true
        img_info         = VALUE #(
            img_key     = c_image_keys-description
            img_encoded = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAACoElEQVR4nJWT208TURCHefCBB9RytRBBVB5458n/gEcTjTERIybqA6C0u7RYkLCl3V52l91epBc0FrDl0lIupaTKxQQlVqLEiGm8JEZJ` &&
                          `IAYRC1TRtJRxzyHWEDDEk0yyO2d+35k5MyctbZ9FUdSB/WL2XG1tVRl2oYay8TVg5WpK/0vczleftwu1S1brrQ1zG71l5sllnpeX7Ct08FXlTlPtU7tFuW5wTyZbBj4CMvaOa5NnlfMMo5DuKbzNyk6J` &&
                          `6T5wmK4nuM6+hHpkCejeMAisDJuhIwAGmzPBMKpFo1F5dIfY3Eo0oiCLSZnUDM2DxhdJCWfCE/Am8hx/a32vgLa5NmlD4weKInNSAIEjozr3BPBCA0oV9B1BLHg9F4a11RWwWRu3Ad6XQI2sgMbSHlfr` &&
                          `miNid9K3AeImqlPjfwc8VyemOwi8w5LKQjC3gLb3BaiHF4EKRoEKfIUmhk+o1NRnDEFBOs8U3tT2zWKIvjMEdPc09vcSBeAj8sBPZO0ytar6UhrPyMs5pu7njbszfyEsCfT9R/i/myiEaOgIfB/PhF+T` &&
                          `B2E1mAEBlRSC7Ve+IQguA0FaWTJ+1RbGInQygmh7noGbKIaV0QJYf5gDsdAhfPJ4v37B11yWSAHQMrG1FZwIuSg8gctdy2C45wW9vgG6iBL4MlIkZiGF2FgmTOryk36FdGuoPjs5WJcd29FSgZOdZsVy` &&
                          `zjCP4YL9Pb7ATnkJvHWfBA9xDBb6xVLGJLiMT57DMFyfFdk1VNt3QsbP6QLgdGoxwK0oBSdZtjbaUvQjGsoTIVkw55CsD5CZ9J6TKTCySjyBxiYMMN48SyJ/j+LE7LRQuLHol8KQMjfqlef++30IgkzC` &&
                          `skSFi6pM/+NzyYolHsXxMOqMV5l/Dfl+AwJivu+ppQ8rAAAAAElFTkSuQmCC` ) ).
  ENDMETHOD.

  METHOD get_max_rows_filt_conf.
    result = VALUE #( name = c_general_options-max_rows single = abap_true no_negation = abap_true internal = abap_true ).
  ENDMETHOD.

  METHOD get_package_filt_conf.
    result = VALUE #(
        name             = c_general_options-package
        description      = 'Development Package'
        long_description = |Use '{ c_general_options-package }' to restrict the search query by specific packages.\n\nExample:\n   | &&
                           |{ c_general_options-package } : test\n\nObjects of sub packages will be reflected in the result list as well|
        img_info         = VALUE #(
            img_key     = c_image_keys-package
            img_encoded = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABwUlEQVR4nM2R2yuDYRzH/QkuXLpwJReIXDiUNc2ZyOQ47J1jXqFIY9jW2Exy3AWbsclMkVOynEYvacPrlEJSwoUL/4ALfL2N2OG5cOlX` &&
                          `v5769PZ5nj6vn9+/H3Fc6P6fP2Z6yl4ZLfXG9HKr/do6QRS8GaMRPRMFdnU5LhbluLapcbmkgMNAQ1ss4Fgnbr7ZwRgNe3fZG1GwoRTj4ciAlxsrnlgjzuZlmG7Kw8Oh/oudGMFapdhUisiCtY4S3O4O` &&
                          `4fF4HHfMCI4sLdBVpv+yPR2c5ibY2gvJghVpEZzTrWDn5HBa2mAfroIqjw/HNzucacP2QAVWpfm+AnFs2D2dEAV1AR+mBiEGxSlQ5MRDls13nUohD4NUKibrhajlR8Ln71AxEUGmmiywi1242hzA6bIG` &&
                          `27o6yHN4OF5Q4cLWh/MVDXZ0NCarM95F0eGBPq+w0NlcRP1PRHa2lbud5xHRYWqGpTaD3MBclekR0WFudj3bPeKeoRFTlalkgVGS5op4Mq/ggsmw1V/B9cj1YOs9FCYkyWSBXiT4GC1Jgvuq8hPhzQyl` &&
                          `Ca9EgbUoOGCWCglyX0ls2JM3M1ER/kQBaYi13eYT+2ZcJOhXQdgAAAAASUVORK5CYII=` )
        allowed_length   = 30
        patterns         = abap_true
        content_assist   = VALUE #( assist_type      = zif_sat_c_object_search=>c_filter_content_assist_type-ris
                                    adt_object_types = VALUE #( ( zif_sat_c_object_types=>package ) ) ) ).
  ENDMETHOD.

  METHOD get_user_filt_conf.
    result = VALUE #(
        name             = c_general_options-user
        description      = 'Owner'
        long_description = |Use '{ c_general_options-user }' to restrict the search query by specific users.| &&
                           |\n\nExample:\n   { c_general_options-user } : musterm|
        patterns         = abap_true
        allowed_length   = 12
        content_assist   = VALUE #( assist_type = zif_sat_c_object_search=>c_filter_content_assist_type-user ) ).
  ENDMETHOD.

  METHOD get_changed_by_filt_conf.
    result = VALUE #(
        name             = c_general_options-changed_by
        description      = 'Last Changed By'
        long_description = |Use '{ c_general_options-changed_by }' to restrict the search query by users who were the last to change an object.| &&
                           |\n\nExample:\n   { c_general_options-changed_by } : musterm|
        img_info         = VALUE #(
            img_key     = c_image_keys-owner
            img_encoded = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAB0UlEQVR4nGP4//8/Azbc4SFoMzlQ9OyUYNHzbZ6C1rjUYRUE4SlBYhc/L7P4D8LTQ8XvAoUYSTJgeojYja/LLf+D8OxIyccvFulxk2TA` &&
                          `RD+RiIWxUk8Wxkg925Kj2PS/gYGJOAMYGBjnhAqbzYkQu7AqVfbV2gzZNwtiJS5P9hMxweYNVM1AW840qGksSZB+83aB6f8vQP9/BeKXc0z+AwP01eUOA6szMxlYcRpwe5In+4Zs2ewzzZo/viy3APv/` &&
                          `6zJIOJxo0Pi1s0ip4f9ca15kl6C6YKYx65JEmcarPbr/QDbDAhGE708x/L8mXX7Z/yXmfLgN2O/AMitabMb9yYb/vyFpBuG3883+r8qQ2/1hqY1gA1KAYoTBpADRlXcm6v//tNQcxYDPSy3+L4mXOvYf` &&
                          `aMD/hgbcBiyNlzReHCd1eKK/yGeY5j4fkS9TQ8XO7ihSCv6/DegFnC4AYwbGZzONuSYHij2AGTAlSPTJu0XGcv8btNnQoxJ7QgKGBdAr92EGTA4Sffxlmbn4//8NGIkJuwGrQpknYjUAMzXiSMoMjJ2e` &&
                          `gudnhku8nhUh8RZo2OXPi6zE/q9iYCY6L/yf78Dxc42lxtfFFkafV5jo4MpMACcZjCscxZIqAAAAAElFTkSuQmCC` )
        patterns         = abap_true
        allowed_length   = 12
        content_assist   = VALUE #( assist_type = zif_sat_c_object_search=>c_filter_content_assist_type-user ) ).
  ENDMETHOD.

  METHOD get_appl_comp_filt_conf.
    result = VALUE #(
        name             = c_general_options-application_component
        description = 'Application Component'
        long_description = |Use '{ c_general_options-application_component }' to restrict the search query by Application Component.\n\n| &&
                           |Example:\n|  && |   { c_general_options-application_component } : ap|
        img_info         = VALUE #(
            img_key     = c_image_keys-appl_comp
            img_encoded = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABoElEQVR4nM3Suy9DURwHcI94m8UihEQMUiw3hIGi1O3To9XWvV6ptogQCdVabEIMRjEQGxYRiYVJgtjwBwhpKq1nUaQeX7/byUlzm9ic` &&
                          `5LOcnO/3PHISEv7dmBYH8ZtXdMEjuDDV5YTb5sCkdQATFjviFuBlV8Y28LyJcXOffIGXdosNblFwnawBT8sY7ehhCpJsjTrICQdW8RVaoeASWQRCcxhpE5mCDGkhXhZiSPPnx9N4D85TcJbMAI8eDBlt` &&
                          `TEFWvBMc7bkQvvZQ0E3GgAcXnHoLU5BGioiS8Pqaeuxs9BIB+9sCLs4GEAkOU9BBeoF7K+xaE1OQbFa2QM79RTc+bwQKmokBuFOjn29nCjKlhQg5Y0jzpwcqvPk1FFQBt7WkCj3qVqYgO94JTvY4vPpq` &&
                          `KMgBN2WkBEKTnilIJQWkWnoHvrIW+1scqcDhrgK+cwU+rkspWAwE80kuPa6WKUgkKdJVpNNI7c+XHClH+KoEEX8hvgN5FMwBAulRnUpe/id2NxshNhkgqPToUumiu1kbNLDU89ErmerUUbIFfx0/1b2N` &&
                          `zNSGKzEAAAAASUVORK5CYII=` )
        patterns         = abap_true
        content_assist   = VALUE #(
            assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
            category_scheme       = zif_sat_c_object_search=>c_content_assist-category_scheme
            category_term         = zif_sat_c_object_search=>c_content_assist-terms-appl_comp
            proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter ) ).
  ENDMETHOD.

  METHOD get_softw_comp_filt_conf.
    result = VALUE #(
        name             = c_general_options-software_component
        description      = 'Software Component'
        long_description = |Use '{ c_general_options-software_component }' to restrict the search query by Software Component.\n\n| &&
                           |Example:\n|  && |   { c_general_options-software_component } : sap_aba|
        img_info         = VALUE #(
            img_key     = c_image_keys-soft_comp
            img_encoded = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABY0lEQVR4nGM4fvw4Azbsbqwp6W2o5uKjp+HrY6imz8DAwIhNHVbNSc7GE8tD7J8vrYz4vKom6mtvmtfLRAfjy256etxEGZDibHLz9ab6` &&
                          `/7/3t4Pxr31t/5tjXN+AXEKUAX4mWoZprmZP3myGGNKf5v0m3s6wGa8XAo21bdJdTbbAcJy94a07y8rABuR5Wz1DlTNY3cDAwIRiQLCpdtr6xtj/T9ZUgfGrTXVwL3ze0QwXB+EsD4uXoaEMzBgG7OtN` &&
                          `g2tCx+dm5v67MCf/L4id72v1jCQDjk/N/h1jY7gp3t5w09LKyE+ZpLqgKcbleYCBhgIoLSQ4GtYm2htXYgQiPgNKAm2fhmpr8+CNBXwGTEjzeeWnr2LoYqzEn+hofDzByeggLGUSZcCDlRX/Ex2NHiQ7` &&
                          `m96/uaj0L8mBCMJfd7f8/7qr+T9ZsYCOsRrgb6gZl+xs/Drby/IJIRxqrv0GwwByMQCvW0gIlfqiygAAAABJRU5ErkJggg==` )
        patterns         = abap_false
        content_assist   = VALUE #(
            assist_type           = zif_sat_c_object_search=>c_filter_content_assist_type-named_item
            caching               = abap_true
            category_scheme       = zif_sat_c_object_search=>c_content_assist-category_scheme
            category_term         = zif_sat_c_object_search=>c_content_assist-terms-software_comp
            proposal_image_source = zif_sat_c_object_search=>c_proposal_image_source-same_as_filter ) ).
  ENDMETHOD.

  METHOD get_created_on_filt_conf.
    result = VALUE #(
        name             = c_general_options-created_on
        description      = 'Created On'
        long_description = |Use '{ c_general_options-created_on }' to restrict results by the creation date of an object.|
        data_type        = zif_sat_c_object_search=>c_filter_data_type-date ).
  ENDMETHOD.

  METHOD get_changed_on_filt_conf.
    result = VALUE #(
        name             = c_general_options-changed_on
        description      = 'Changed On'
        long_description = |Use '{ c_general_options-changed_on }' to restrict results by the last changed date of an object.|
        data_type        = zif_sat_c_object_search=>c_filter_data_type-date
        img_info         = VALUE #(
            img_key     = c_image_keys-changed_on
            img_encoded = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABbklEQVR4nGP4//8/AyWYIs0YBnQ7Mv7vcmAgiBu0GXhwGvBhsfn/z0stcGKQmkmeDHw4Dfi4xPz/1+WWODFITQMuA4hxPtgLuAzocWb5` &&
                          `/fv71/////3DieusBP40OChwYDfAifnXl/ev/q9qzf9f7aTw/+e3r/9XNGX/L7MW/7++t+I/CFSYif5OUGDAbkA30AX3zh35v6Ai/n+JufD/758//d85q+P/+V1r/7cFGYENKDKRwu+C3z++AV3673+R` &&
                          `qcD/H1+//P/39+//uUUR/zdNqAEbkGsgB3QBDgPaHdh///75HawJZsDWqY3/p2f5/weJg0CajjLQBQ7YDWiy4fn1C+QCJANqnBX/17oo/a/3UAMbkKqj+genC2qtBH//+gGxCRdI01b9nYArDEpNRH4U` &&
                          `Gor9y9OX+pelI/svXVvxX7KG0r9kTeV/KZrK/8FsDaX/OF3Q0MDAFMrAwBwaCsKhzGA2Ayp7FZBN1dwIAMayoL3i4x8iAAAAAElFTkSuQmCC` ) ).
  ENDMETHOD.

  METHOD get_general_image.
    CASE iv_image_key.

      WHEN c_general_image_keys-column.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAACESURBVDhPY6AUMEJpMPDKX/sfyoSDbRODUdTgBSADHr79C` &&
                 `8fYDEQHTFAaBdx6DcHEAOp6YfaWO//ff/37/86rv//PPfr7H8SHSuEEWL1ACsAwYM3++wz7T92H8ggDDANCHBUZHM0UoTzCYBB6AeR8irzw6QcEEwswDDh76T4cDwXAwAAAvGtEmZMujD0AAAAASUVORK5CYII=`.

      WHEN c_general_image_keys-type_folder.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABkUlEQVR4nGNgGFTgxISI5iNd/r8Od/j8B+EjnX6/T0yMaCPagK0VJt++v737/+/312D88/3D/1vKjX8Spflot33vpYUh/z/fmfD/+52+/7/vdP//c` &&
                 `Kbp/8OtRf+Pdzn+P9RiBseH2yx+nuixb0QxYH+z2Y/PT7b8//FmL0H89fm2//uazX/+/8/ACDdgV53R3+/PNv3/8WIzGFcnh/4vifaH44qEYLgcCO+uNf7X0MDABDdgW7Xh3++PVvz/8WQlGBdH+v1/e2vR/6JI3/` &&
                 `/vbi0G0zA5EN5eZYRqwKYK/b/f7837/+PBfDDeOLMETIM1IvFheHO5AaoB60p0/36/NfX/jzvTUHBRpA+GGAivL9ZDNWBVoc7f79e6//+40YOCwQagiYHw6gJdVAOW5Wn//X654f+PK01g/Oxw9f+CcO//WYEeYPr` &&
                 `Z4Sq4HAgvz9VGNWBpjva3z6eK//+4UAHHn06V/X93vBRMI4t/OV3yf0m21q//DEjRuDhPp3t+uvrfeWnq/wljjX9L8nSmEpVCaQ4A586o1JIz7EQAAAAASUVORK5CYII=`.

      WHEN c_general_image_keys-type_group.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAACl0lEQVR4nIWSS08TURiGWbjwB7hwpwt/gr/ChSsTXGqihIWGXgBLh8JMmZm2c2Y6M4VeuNhSW0og2DsXESgEBLExYILBcEmUYCIqKmqRmLSvnZFAG` &&
                 `gFPcpKTk+95znc5VVWnrID73lW/UrflVeocp8WcuPxy7WW/xzjiUy3fhXCmqMqWA1Uysf8FO8WaCz5Pvc+vGveEyMiB/fEa+GgObP8CRJnZJ8TqPBEURfOldsmwGPAYf0iR9IE9vgEuNgdPOwNFMkEmBvCxWTgVV8` &&
                 `HppFz/CNzupiUtSJGpIpP8ACkY1aGpyQT2vu3qZ9nrBJPcBkvYgp2nKnsiE2ORTn8G8XuLstpSEkKD+svZVAi+DhtktwXOUBx09ivoxBZoQSg0c20dNE2fOxQYdDud2YUQ6ISkMuUS5uHq7oWjN6vf67C2059AqbG` &&
                 `S0hVBYwtz+28PSMMXQbTCntg8lHRBVBjQqY/HYHnbAmlInWEMTcwXn7xYQTmD80cZOIJxENKIuz2v9WAh0AOicvqLtu4xKA/7kMwtQgPH5vKl4EB8x0ixtcclZHbgCGfhIg9Q7XmlSySFhujtOQbz6+hNzYDi3Wh1` &&
                 `qrBQLdW6wEWaunh3W4FJvQcfGYNQzuQ6WQTlCSI6PIXR/AbC2WdodXWg2amUHqWf/taEFZMQSIPPQWyFmtAm+L5piKReH18glix3XQPVUl9mUq8993L17eDw5LLJyt2qkEiknmYF2/5N3xq4gXxZYgbj4BBNjZc0c` &&
                 `GZpdT05Pr3ACOqG2crdORpjRSZCw31OoArXpBU0q90/B0cntmeX36xmcnMLvOx/Z6JYw4lg5ccymQSh8ZdWQv9Q4jkvezfNFNvWQNMXzwQrJKLhhiaw0HTaaHFcOSv2D/3B33rlnt49AAAAAElFTkSuQmCC`.

      WHEN c_general_image_keys-param.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAArElEQVR4nGNgGAUkA3mfpc0UGrDsv4LPkna8iiJb9v6HYf+GXf9darb/t6jY8h9mgEbgsg94DQFpvPnuJwrWLdkINyAig4AhIAMOPv6KgpUK1sENi` &&
                 `MmCGAJiYzUA5Ox5lz+gYPGc1SgG2EUv/63gu+wtVgNAft567wsK5slYATcApFnZb9ldKd9lIlgNAAUYyM8gZ4NsBmtOXgI3AK9mQgBowHeyNYMARZppAgBR6p3qYaEtFgAAAABJRU5ErkJggg==`.

      WHEN c_general_image_keys-checked_box.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAA5ElEQVR4nGNgGDSge9rq/8Tinmlr7mM14Ozb//8PvySMcRoAknzw5f//e5+x431Xnvy/+/n/N7wG3AMa8O4XJm6bte6/WWDh/2NXH7zAa8D9r///f` &&
                 `/iNikGajf3ywfS7n/8e4zVg65nb/z8BNcFwO1QziAbxP+AzoHf9SbDiBRsO/P/y5///jtkQzSAaxAfhT/gMOPjs7/+c1nlgTcmVk8F0J1Dz17//4fjzbwJeuPfp7//i9nlwzd+BmpDxV0IGPPr2//+333//r99z8v` &&
                 `/Pf/8x8Pc/RBjw6x9u/AOfARQl5QEDAMqA2SocOZJBAAAAAElFTkSuQmCC`.

      WHEN c_general_image_keys-generic_filter.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAAAlwSFlzAAAAdgAAAHYBTnsmCAAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9yZ5vuPBoAAAFxSURBVDiNnZHfK` &&
                 `0NhGMc/79k5mVrmV2SRMMulH61oF+411Ir/QK78CyPlguWCG67VSvnRipQLEqUYWQo3rBRuXMimtjo7Xhfa5uhkZz5Xb0/P9/M+7/MKABltawVlDEQ9dhDco2ajYvwpI+R6uw9DxIEqW+EicT6qAwqGmPhHGMCP62` &&
                 `1QARrzlf1rlbmYk9tnh02H0qTmj5vnGmNLLgDmd5zcRlI0136WVBQEhzdaoZjOClqm3NZ3CjgOpwn4cmbBULfO6kEFUkKdS3KzkKLRXcYEwR6d05kUF0mV4V7dVtgkAOj3GvR7DcvGvYRG4tFByK/T5Sn2qJbdv9g` &&
                 `40xhfLi74LvKOp0aaBbPblUxvOUvKUhlBPKky2qebBeFQhnAoYxnavdIYWXQVFjzQmSvvCcEenZNwmsSjg+FenYYq+VMgXkD+Ef8m4MsV/r6IfFLAWAFe7UxiQnCE9nAsAORaRwMKowhZay+tJPlwx8Tkpf4FIgty` &&
                 `FKzK1qcAAAAASUVORK5CYII=`.

      WHEN c_general_image_keys-extension.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABKklEQVR4nM2SIVMCURSFCQYjwbDBsIFgIBAIBgKBYCAYDAaCP8BAIBic2TdDIBA2EIgGAtFo2EAwGglGAsFgIBgIBDxn5nPGebMMzXFn` &&
                 `vnm897jnnnN3K5V/94QQGuJJrMRefIkX0RUnx4ofxEaMRN0FIsmy7FbrG0JnpcX6070u11prWlORi0LMdXZDg6lYlHU+d2eKbfVDDLVvsl9YSJzaic57sYAtT8QFxV6vxVj0ieK53HFexAJL0cJ2oPOG` &&
                 `mbzajWgTqerBxgI77HlIVzgacBfIXieK3exjgS0CBZkf3Z07d08QynH3HgskKI+J0SBCn9+OsubtWDA/9B2kDLFF4TPzmRHh5w2lpQKIdOjuYdZ+OXSsT51dHiyOnEyx7c95xT45WvznzzfhJAGIuot7` &&
                 `RgAAAABJRU5ErkJggg==`.

      WHEN c_general_image_keys-maintenance.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABXElEQVR4nJXTz0vCYBzHcf+yOniqDgURUVIdMiUPdQisFUUpCRlhUIhp0CXIS4RSiTpyllCQMcJf2aBZrQxxiZiIDf201Y6m2wNfHnjg9b48fDUaF` &&
                 `QeAtdEQkuLNiKNVYyXsyDMJUFujuD60oV6tCOKbThW+cU4A91ZUH3wg7eP4rlVrqnGTi3wJMQK1kAnU9jSU4+QGmm/RXyyQRhSDZgTMXf8H2mE+OI8Q0Y3SK9M6IGGeYzti8cy2xKX8C27d+j/MUepx3GOQ8UVFiM` &&
                 `4pxjoJp30LqCc21WE5kAm7CHD0LgpZb5N/PEGOsuMzTCjC/QU2C9q/ho/MEYpsEKnLPdwdL3XGcoAm91fBxA/A50JIXbkR9S7jfKVXEdZKXzYzNgCjQY90zIMzxxQCtkmU39n2WA6ET50WDA8NwmIaQcS1iOKTtHC` &&
                 `gxelri+VAYsfYA9KzjnL+WYIRNav6A7llxO2+1g0dAAAAAElFTkSuQmCC`.

      WHEN c_general_image_keys-table_source.
        result = `iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAABQklEQVR4nK3Su0oDQRTG8TyCoK3EKpdFSVC8ZCEWUVDQyifwAXwGGxUbUXTWXEw2BhMt0lnYimijGEz2kt1EMSgqbqGtiKt8zskDHBAc` &&
                 `GIY5f+ZXTSDw3ytVuAmqxeaFqjufiaIDVW46E7rzJWfX1FlAPvaTJQczlTaWTh4gLj0sVO8we9jGZMmFRH5YYEJ3MFV2MX90i+XTJ6SvPCwed7r36YMWqLPAeKEJRTOQ3HcRz1oICwOxjImxvI2onFNn` &&
                 `gdF8E71rNcyVW6ha77BfP7B+/tIFaU6dBUb2bBagzgLDOR6gzgLxLA9QZ4FYxmIB6iwwlOYB6iwwuGuyAHUWUDQeoM4CYWF2gpv175R8UGm8dYGVs2ckCg76N+p+RJgeCwxs1XvC29aq/I33ijB8+olR` &&
                 `0fAjmvkYEuZOKFfrY4G/rl+7wFtBvoD5FwAAAABJRU5ErkJggg==`.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
