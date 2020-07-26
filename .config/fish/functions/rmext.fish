function rmext --description Remove\ an\ extension\ from\ all\ files\ in\ a\ given\ dir\ \(defaults\ to\ \'.\'\)
    set -l finder "find"
    set -l options (fish_opt -s d -l dir --required-val)
    argparse $options -- $argv

    # Handle finder
    if set -q _flag_dir
        set -a finder "$_flag_dir"
    else
        set -a finder "."
    end

    set -a finder "-name '*.$argv[1]' -type f"
    #| while read NAME
    #mv \$NAME (string trim --right --chars=$argv[1] \$NAME)
    #end
    set -l files (eval $finder)

    for file in $files
        echo "Removing .$argv[1] from $file"
        mv $file (echo $file | rev | cut -d. -f2- | rev)
    end
end
