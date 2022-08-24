*&---------------------------------------------------------------------*
*& Report zsat_cds_v2_meta_index_update
*&---------------------------------------------------------------------*
*& Meta Index Updater for CDS Views v2
*&---------------------------------------------------------------------*
REPORT zsat_cds_v2_meta_index_update.

DATA(cds_v2_meta_index_updater) = NEW zcl_sat_cds_v2_meta_updater( write_debug_info = abap_true ).
cds_v2_meta_index_updater->update_index( ).
cds_v2_meta_index_updater->remove_invalid_index( ).
