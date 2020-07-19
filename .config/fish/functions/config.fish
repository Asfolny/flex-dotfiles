# Defined in - @ line 1
function config --wraps='git --git-dir=$HOME/.cfg/ --work-tree=$HOME' --description 'Manage $HOME config'
  git --git-dir=$HOME/.cfg/ --work-tree=$HOME $argv;
end
