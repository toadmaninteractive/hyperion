-record(upload_config, {
    max_file_size = 10 * 1024 * 1024 :: non_neg_integer(),   % 10 MB
    target_dir :: file:filename_all()
}).

-type upload_config() :: #upload_config{}.