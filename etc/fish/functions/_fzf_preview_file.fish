# helper function for _fzf_search_directory
function _fzf_preview_file --description "Print a preview for the given file based on its file type."
    # because there's no way to guarantee that _fzf_search_directory passes the path to _fzf_preview_file
    # as one argument, we collect all the arguments into one single variable and treat that as the path
    set file_path $argv

    if test -L "$file_path" # symlink
        # notify user and recurse on the target of the symlink, which can be any of these file types
        set -l target_path (realpath "$file_path")
        set_color yellow
        echo "'$file_path' is a symlink to '$target_path'."
        set_color normal
        _fzf_preview_file "$target_path"

    else if test -f "$file_path" # regular file
        # need to escape quotes to make sure eval receives file_path as a single arg
        eval "$FZF_PREVIEW_FILE_CMD '$file_path'"

    else if test -d "$file_path" # directory
        eval "$FZF_PREVIEW_DIR_CMD '$file_path'"

    else if test -c "$file_path"
        _fzf_report_file_type "$file_path" "character device file"
    else if test -b "$file_path"
        _fzf_report_file_type "$file_path" "block device file"
    else if test -S "$file_path"
        _fzf_report_file_type "$file_path" socket
    else if test -p "$file_path"
        _fzf_report_file_type "$file_path" "named pipe"
    else
        echo "$file_path doesn't exist." >&2
    end
end
