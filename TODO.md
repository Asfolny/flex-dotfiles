# XMonad/xmobar
* Look into layouts, pick/customize them to look nice
    * Windows are too packed, and everything sticks to the edges, add spacing and "shrink" windows to fit these slightly more limited sizes
         * Render all windows in 1080p sizes within these layouts? Or change rendering based on placement/layout used, but let's not keep "cutting" them up all the time (rendering in all sizes at 1080p will be resource intensive)
* Float specific windows
    * Give specific windows specific sizes (mgba takes fullscreen, which is not desired)
* Refactor configs to be multiple files + import
* Make xmobar into pure haskell config
    * Make some elements clickable
    * Get Temps (but only show if too hot)
    * Click on CPU or Memory -> Open terminal with a list of the current highest (if above X) jobs
        * if possible make this a dropdown instead
* Top bar in FULL layout to have a better overview of what is open
* Add Audio to xmobar (and mic with mute and unmuted state, ignore mic volume control, let applications handle that)
* Check keybindings for layout changing
    * Find a way to quickly change to Tabs for example
    * Move grid to a keybind only

# PHPStorm/InteliJ (any derivative really)
1. Compare plugins for the specific language to create a minimal working product
2. Strip it down to minimal size, no need for a terminal, no need for project, make other things floating (strip out database too maybe)
3. (?) context aware keyboard lookup for a function/something to pop out, this could allow for ONLY having the editor section open, reducing size massively

Stripping out the project tree section requires whatever file manager there is to be context aware of what is opened in phpstorm... tricky

# Theming
* Use env vars? (pam.d?)

# Misc
* Add opacity to background (but not through say picom for ALL things)
* Add Picom
* Unify this repo
    * No more submodules, possible a small program to check discrepencies between repos, with a local db of "resolved" things
    * Have one branch per machine, instead of per system type OR one repo per system instead (probably better), with one being the "main" promoted, and the others being derivatives
* Change iwd's iwctl coloring (station wlan0 get-networks color is black, which is hard to see with black on white alacritty)


# Config
* Merge this and README
* Cleanup readme
* Make the other dotfiles into "desktop dotfiles" or something
