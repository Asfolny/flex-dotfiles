# Defined in .config/fish/functions/dirmov.fish @ line 2
function .. --wraps='cd ..' --description 'alias ..=cd ..'
  cd .. $argv;
end

# Defined in .config/fish/functions/dirmov.fish @ line 7
function .2 --wraps='cd ../..' --description 'alias .2=cd ../..'
  cd ../.. $argv;
end

# Defined in .config/fish/functions/dirmov.fish @ line 12
function .3 --wraps='cd ../../..' --description 'alias .3=cd ../../..'
  cd ../../.. $argv;
end

# Defined in .config/fish/functions/dirmov.fish @ line 17
function .4 --wraps='cd ../../../..' --description 'alias .4=cd ../../../..'
  cd ../../../.. $argv;
end
