theme = {}

theme.theme_dir     = awful.util.getdir("config") .. "/themes/jkpl"
theme.wallpaper_cmd = { "feh --bg-scale " .. theme.theme_dir .. "/back.png" }

theme.font          = "terminus 8"

theme.bg_normal     = "#161616"
theme.bg_focus      = "#161616"
theme.bg_urgent     = "#f92672"
theme.bg_minimize   = "#141414"

theme.fg_normal     = "#888888"
theme.fg_focus      = "#FFFFFF"
theme.fg_urgent     = "#FFFFFF"
theme.fg_minimize   = "#666666"

theme.border_width  = "1"
theme.border_normal = "#666666"
theme.border_focus  = "#ffcc22"
theme.border_marked = "#f92672"

theme.taglist_bg_focus = "#181818"
theme.taglist_fg_focus = "#ffffff"

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- [taglist|tasklist]_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- Example:
--theme.taglist_bg_focus = "#ff0000"


-- Using anrxc's icon pack
-- http://awesome.naquadah.org/wiki/Nice_Icons

-- Display the taglist squares
theme.taglist_squares_sel   = theme.theme_dir .. "/taglist/squaref_a.png"
theme.taglist_squares_unsel = theme.theme_dir .. "/taglist/square_a.png"

theme.tasklist_floating_icon = theme.theme_dir .. "/tasklist/floatingw.png"

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = theme.theme_dir .. "/submenu.png"
theme.menu_height = "14"
theme.menu_width  = "100"

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget = "#cc0000"
theme.clock         = "#ffffff"
theme.battery       = "#f6eb34"
theme.volume        = "#f92672"

-- Define the image to load 
theme.titlebar_close_button_normal = theme.theme_dir .. "/titlebar/close_normal.png"
theme.titlebar_close_button_focus  = theme.theme_dir .. "/titlebar/close_focus.png"

theme.titlebar_ontop_button_normal_inactive = theme.theme_dir .. "/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive  = theme.theme_dir .. "/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active = theme.theme_dir .. "/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active  = theme.theme_dir .. "/titlebar/ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive = theme.theme_dir .. "/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive  = theme.theme_dir .. "/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active = theme.theme_dir .. "/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active  = theme.theme_dir .. "/titlebar/sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive = theme.theme_dir .. "/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive  = theme.theme_dir .. "/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active = theme.theme_dir .. "/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active  = theme.theme_dir .. "/titlebar/floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = theme.theme_dir .. "/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = theme.theme_dir .. "/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active = theme.theme_dir .. "/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active  = theme.theme_dir .. "/titlebar/maximized_focus_active.png"

-- You can use your own layout icons like this:
theme.layout_fairh = theme.theme_dir .. "/layouts/fairh.png"
theme.layout_fairv = theme.theme_dir .. "/layouts/fairv.png"
theme.layout_floating  = theme.theme_dir .. "/layouts/floating.png"
theme.layout_magnifier = theme.theme_dir .. "/layouts/magnifier.png"
theme.layout_max = theme.theme_dir .. "/layouts/max.png"
theme.layout_fullscreen = theme.theme_dir .. "/layouts/fullscreen.png"
theme.layout_tilebottom = theme.theme_dir .. "/layouts/tilebottom.png"
theme.layout_tileleft   = theme.theme_dir .. "/layouts/tileleft.png"
theme.layout_tile = theme.theme_dir .. "/layouts/tile.png"
theme.layout_tiletop = theme.theme_dir .. "/layouts/tiletop.png"
theme.layout_spiral  = theme.theme_dir .. "/layouts/spiral.png"
theme.layout_dwindle = theme.theme_dir .. "/layouts/dwindle.png"

theme.awesome_icon = theme.theme_dir .. "/awesome.png"

return theme

