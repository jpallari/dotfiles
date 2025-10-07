# Settings
set debuginfod enabled off
set print pretty on

# Save breakpoints
define savebps
    save breakpoints .gdb_bps
end

# Load breakpoints
define loadbps
    source .gdb_bps
end

# Reload binary
define reload
    python gdb.execute("file " + gdb.current_progspace().filename)
    directory
end

# Save breakpoints automatically
define hook-quit
    savebps
end

