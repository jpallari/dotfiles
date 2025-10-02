local wezterm = require 'wezterm'
local config = wezterm.config_builder()

-- General
config.automatically_reload_config = false

-- Appearance
config.window_decorations = 'RESIZE|INTEGRATED_BUTTONS'
config.color_scheme = 'Builtin Dark'
config.font = wezterm.font_with_fallback {
  'AcPlus IBM VGA 9x16',
  'JetBrainsMono NF',
  'JetBrains Mono',
  'Noto Color Emoji',
}
config.font_size = 18
config.line_height = 0.9
config.freetype_load_flags = 'NO_HINTING'
config.bold_brightens_ansi_colors = 'BrightOnly'
config.harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' } -- disable ligatures
config.foreground_text_hsb = {
  hue = 1.0,
  saturation = 1.0,
  brightness = 1.2,
}
config.font_rules = {
  {
    intensity = 'Bold',
    font = config.font,
  }
}

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
  {
    key = 'm',
    mods = 'CTRL|SHIFT',
    action = wezterm.action.QuickSelectArgs {
      label = 'open url',
      patterns = { 'https?://\\S+' },
      action = wezterm.action_callback(function(window, pane)
        local url = window:get_selection_text_for_pane(pane)
        wezterm.open_with(url)
      end),
    },
  },
  {
    key = '.',
    mods = 'CTRL|SHIFT',
    action = wezterm.action.ShowTabNavigator,
  },
}

return config
