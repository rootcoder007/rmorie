# NO PORT NEEDED.
# r-package/morie/R/entheo_data.R already exists and is a faithful R
# parity of src/morie/entheo/data.py:
#   - load_dmt_imaging(subject_id, root)   public API matches
#   - .entheo_list_subjects(root)          parity with list_subjects()
#   - .entheo_synthetic_record(...)        parity with _synthetic_record()
#   - .entheo_load_real(sid, root)         parity with _load_real_record()
# Root resolution honours $MORIE_DMT_IMAGING_ROOT and falls back to
# morie_cache_dir()/DMT_Imaging (the R-side equivalent of the Python
# _DEFAULT_LOCAL_ROOT constant -- adapted because the Python default
# was hard-coded to /Volumes/VSR/rootcoderfiles/DMT_Imaging which is
# not portable). dmt_imaging_root() from Python is inlined into
# load_dmt_imaging's default-arg handling rather than exposed as a
# standalone callable. If a standalone exported wrapper is wanted in
# future, it should be a one-liner alias in entheo_data.R rather than
# a separate complement file. No drift to reconcile.
