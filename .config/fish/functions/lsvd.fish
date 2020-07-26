function lsvd -d "ls but for video (duration)"
    for file in $argv
        set -l time (ffprobe -i $file -show_entries format=duration -v quiet -of csv="p=0" -sexagesimal | awk -F. '{print $1}')
        if test -n "$time"
            echo $time $file
        end
    end
end
