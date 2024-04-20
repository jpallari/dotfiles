local wezterm = require 'wezterm'
local config = wezterm.config_builder()

-- General
config.automatically_reload_config = false

-- Appearance
config.color_scheme = 'tokyonight'
config.font = wezterm.font 'JetBrainsMono NF'

-- Key bindings
config.use_dead_keys = false
config.keys = {
  {
    key = 'Enter',
    mods = 'CTRL|SHIFT',
    action = wezterm.action.SplitVertical {
      domain = 'CurrentPaneDomain',
    },
  },
  {
    key = 'Enter',
    mods = 'CTRL|SHIFT|ALT',
    action = wezterm.action.SplitHorizontal {
      domain = 'CurrentPaneDomain',
    },
  },
  {
    key = 'a',
    mods = 'CTRL|SHIFT',
    action = wezterm.action.PaneSelect {},
  },
  {
    key = 's',
    mods = 'CTRL|SHIFT',
    action = wezterm.action.PaneSelect {
      mode = 'SwapWithActive',
    },
  },
}

return config
