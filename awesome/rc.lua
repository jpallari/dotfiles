-- Standard awesome library
require("awful")
require("awful.autofocus")
require("awful.rules")
-- Theme handling library
require("beautiful")
-- Notification library
require("naughty")
-- Additional widgets
require("vicious")

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
beautiful.init(awful.util.getdir("config") .. "/themes/jkpl/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "uxterm"
editor = os.getenv("EDITOR") or "editor"
editor_cmd = terminal .. " -e " .. editor

-- Other useful app commands
filemanager = "pcmanfm"
screenlock  = "slock"

-- Default modkey.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
    awful.layout.suit.tile,
    awful.layout.suit.max,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.floating,
    --awful.layout.suit.tile.left,
    --awful.layout.suit.tile.top,
    --awful.layout.suit.fair,
    --awful.layout.suit.fair.horizontal
    --awful.layout.suit.spiral,
    --awful.layout.suit.spiral.dwindle,
    --awful.layout.suit.max.fullscreen,
    -- awful.layout.suit.magnifier
}
-- }}}

-- {{{ Some functions

-- Function for launching apps only once. Found from awesome wiki.
function run_once(prg,arg_string)
	if not prg then
		do return nil end
	end
	if not arg_string then
		awful.util.spawn_with_shell("pgrep -u $USER -x " .. prg .. " || (" .. prg .. ")")
	else
		awful.util.spawn_with_shell("pgrep -u $USER -x " .. prg .. " || (" .. prg .. " " .. arg_string ..  ")")
	end
end

-- Volume control function: updates volinfo widget
-- mostly ripped from vicious volume widget
function volctrl(cmd)
    mctrl = 'Master'

    local mixer_state = {
        ["on"]  = "on",
        ["off"] = "off"
    }
    local amxcmd = "amixer get " .. mctrl
    local pacmd = ""
    if     cmd == "up"     then pacmd = "vol_up"
    elseif cmd == "down"   then pacmd = "vol_down"
    elseif cmd == "toggle" then pacmd = "mute_toggle"
    end

    if pacmd then os.execute(pacmd) end
    local f = io.popen(amxcmd)
    local mixer = f:read("*all")
    f:close()

    local volu, mute = string.match(mixer, "([%d]+)%%.*%[([%l]*)")
    if volu == nil then
       volinfo.text = "VOL 0 " .. mixer_state["off"]
       return
    end

    if mute == "" and volu == "0"
    or mute == "off" then
       mute = mixer_state["off"]
    else
       mute = mixer_state["on"]
    end

    volinfo.text = " <span color='".. beautiful.volume .."'>VOL " .. volu .. " " .. mute .. "</span> "
end

function oldvolctrl(mctrl, cmd)
    if not mctrl then mctrl = 'Master' end

    local mixer_state = {
        ["on"]  = "on",
        ["off"] = "off"
    }
    local amxcmd = ""
    if not cmd then
        amxcmd = "amixer get " .. mctrl
    else
        amxcmd = "amixer set " .. mctrl .. " " .. cmd
    end

    local f = io.popen(amxcmd)
    local mixer = f:read("*all")
    f:close()

    local volu, mute = string.match(mixer, "([%d]+)%%.*%[([%l]*)")
    if volu == nil then
       volinfo.text = "VOL 0 " .. mixer_state["off"]
       return
    end

    if mute == "" and volu == "0"
    or mute == "off" then
       mute = mixer_state["off"]
    else
       mute = mixer_state["on"]
    end

    volinfo.text = " <span color='".. beautiful.volume .."'>VOL " .. volu .. " " .. mute .. "</span> "
end
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {
	names = {"main", "term", "c", "w", "¬", "µ", "@", "music", "_"},
	layout = { layouts[1], layouts[1], layouts[1], layouts[1], layouts[1], layouts[1], layouts[1], layouts[1], layouts[1] }
}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = awful.tag(tags.names, s, tags.layout)
end
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu
myawesomemenu = {
    { "manual", terminal .. " -e man awesome" },
    { "edit config", editor_cmd .. " " .. awful.util.getdir("config") .. "/rc.lua" },
    { "restart", awesome.restart },
    { "quit", awesome.quit },
}

myprefsmenu = {
    { "monitor", "lxrandr" },
    { "lock", screenlock },
}

mymainmenu = awful.menu({ items = {
    { "awesome", myawesomemenu, beautiful.awesome_icon },
    { "preferences", myprefsmenu },
    { "terminal", terminal },
    { "browser", "dwb" },
    { "gvim",  "gvim" },
    { "file manager", filemanager }
}})

--mylauncher = awful.widget.launcher({ image = image(beautiful.awesome_icon),
--                                     menu = mymainmenu })
-- }}}

-- {{{ Wibox
-- Create a textclock widget
mytextclock = awful.widget.textclock({ align = "right" }," <span color='".. beautiful.clock .."'>%a %d.%m, %H:%M</span> ",60)

-- Create a systray
mysystray = widget({ type = "systray" })

-- Vicious battery
mybattery = widget({ type = "textbox", name = "mybattery" })
vicious.register(mybattery, vicious.widgets.bat, " <span color='".. beautiful.battery .."'>BAT $1 $2% $3</span> ", 16, "BAT0")

-- Volume info
volinfo = widget({ type = "textbox" })
volctrl(false)

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag)
--                    awful.button({ }, 4, awful.tag.viewnext),
--                    awful.button({ }, 5, awful.tag.viewprev)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if not c:isvisible() then
                                                  awful.tag.viewonly(c:tags()[1])
                                              end
                                              --client.focus = c
                                              awful.client.focus.byidx(1,c)
                                              --c:raise()
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({ width=250 })
                                              end
                                          end)
--                     awful.button({ }, 4, function ()
--                                              awful.client.focus.byidx(1)
--                                              if client.focus then client.focus:raise() end
--                                          end),
--                     awful.button({ }, 5, function ()
--                                              awful.client.focus.byidx(-1)
--                                              if client.focus then client.focus:raise() end
--                                          end))
                                          )

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(function(c)
                                              return awful.widget.tasklist.label.focused(c, s)
                                          end, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", height = '14', screen = s })
    -- Add widgets to the wibox - order matters
    mywibox[s].widgets = {
        {
            --mylauncher,
            mytaglist[s],
            mylayoutbox[s],
            mypromptbox[s],
            layout = awful.widget.layout.horizontal.leftright
        },
        mytextclock,
	mybattery,
        volinfo,
        s == 1 and mysystray or nil,
        mytasklist[s],
        layout = awful.widget.layout.horizontal.rightleft
    }
end
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end)
--    awful.button({ }, 4, awful.tag.viewnext),
--    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
    awful.key({ modkey,           }, "Tab", awful.tag.history.restore),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "w", function () mymainmenu:show({keygrabber=true}) end),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx(-1)    end),
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx( 1)    end),
    awful.key({ modkey,           }, ",", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, ".", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Escape",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    -- Standard program
    awful.key({ modkey, "Shift"   }, "Return", function () awful.util.spawn(terminal,false) end),
    awful.key({ modkey, "Shift"   }, "q", awesome.restart),
    --awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey,           }, "i",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey,           }, "d",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Shift"   }, "i",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "Shift"   }, "d",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    -- Prompt (use dmenu instead)
    --awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end),
    awful.key({ modkey }, "a", function () awful.util.spawn(
        "dmenu_run -nb '".. beautiful.bg_normal
        .."' -nf '".. beautiful.fg_normal
        .."' -sb '".. beautiful.bg_focus
        .."' -sf '".. beautiful.fg_focus
        .."' -fn '-*-terminus-medium-r-*-*-12-*-*-*-*-*-*-u'", false) end),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run({ prompt = "Run Lua code: " },
                  mypromptbox[mouse.screen].widget,
                  awful.util.eval, nil,
                  awful.util.getdir("cache") .. "/history_eval")
              end),

    -- I don't like minimized windows...
    awful.key({ modkey, "Shift" }, "n",
        function()
            local tagi = awful.tag.selected()
                for i=1, #tag:clients() do
                    tag:clients()[i].minimized=false
                    tag:clients()[i]:redraw()
                end
        end),

    -- Volume keys
    awful.key({ modkey }, "[", function () volctrl("down") end),
    awful.key({ modkey }, "]", function () volctrl("up") end),
    awful.key({ modkey }, "'", function () volctrl("toggle") end),

    awful.key({        }, "XF86Sleep",   function () awful.util.spawn("sleeplock", false) end),
    awful.key({        }, "XF86Display", function () awful.util.spawn("multihead", false) end),
    awful.key({ modkey }, "F12",         function () awful.util.spawn("wicd-gui", false) end)
)

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey,           }, "Return",
        function (c)
            local mestari = awful.client.getmaster()
            if mestari == c then
                c:swap(awful.client.next(1,c))
                awful.client.focus.byidx(-1,c)
            else
                c:swap(mestari)
            end
        end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
--    awful.key({ modkey,           }, "n",      function (c) c.minimized = not c.minimized    end),
    awful.key({ modkey,           }, "n", -- Move to last tag instead of minimize
        function (c)
            awful.client.movetotag(tags[mouse.screen][9],c)
        end),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
)

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
for i = 1, keynumber do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        if tags[screen][i] then
                            awful.tag.viewonly(tags[screen][i])
                        end
                  end),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      if tags[screen][i] then
                          awful.tag.viewtoggle(tags[screen][i])
                      end
                  end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.movetotag(tags[client.focus.screen][i])
                      end
                  end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.toggletag(tags[client.focus.screen][i])
                      end
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = true,
                     keys = clientkeys,
                     buttons = clientbuttons } },
    -- Float
    { rule_any = { class = {
        "MPlayer",
        "feh",
        "Wicd-client.py"
      } },
      properties = { floating = true, ontop = true } },
    -- Float (not on top)
    { rule_any = { class = {
        "pinentry",
        "Gimp"
      } },
      properties = { floating = true, ontop = false } },
    -- Tag 1
    { rule_any = { class = {
        "dwb",
        "Dwb",
        "Firefox"
      } },
      properties = { tag = tags[1][1] } },
    -- Tag 5
    { rule_any = { class = {
        "Spotify"
      } },
      properties = { tag = tags[1][8] } },
    -- Firefox: float everything except the browser window
    { rule = { class = "Firefox" }, except = { instance = "Navigator" }, properties = { floating = true } }
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.add_signal("manage", function (c, startup)
    -- Add a titlebar
    -- awful.titlebar.add(c, { modkey = modkey })

    -- Enable sloppy focus
    c:add_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)

client.add_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.add_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
