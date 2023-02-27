local wezterm = require 'wezterm'
local act = wezterm.action

return {
  color_scheme = 'Github', 
  font = wezterm.font ('PragmataPro Mono Liga', {weight = 'Regular'}),
  -- font = wezterm.font 'Iosevka SS08'
  font_size = 11.0,

  font = wezterm.font_with_fallback {
    'Iosevka SS08',
    'PragmataPro Mono Liga',
  },

  check_for_updates = false,

 disable_default_key_bindings = true,
  
  keys = {
		{ key = "q", mods = "SUPER", action = "QuitApplication" },
    { key = 'n', mods = 'SUPER', action = act.SpawnTab 'CurrentPaneDomain' },
		{ key = "w", mods = "SUPER", action = wezterm.action({ CloseCurrentTab = { confirm = false } }) },
		{ key = "v", mods = "SUPER", action = wezterm.action({ PasteFrom = "Clipboard" }) },
    { key = 'c', mods = 'SUPER', action = wezterm.action({ CopyTo = "Clipboard" }) },
    { key = 'f', mods = 'SUPER', action = wezterm.action({ Search = "CurrentSelectionOrEmptyString" }) },
		{ key = "=", mods = "SUPER", action = "IncreaseFontSize" },
		{ key = "-", mods = "SUPER", action = "DecreaseFontSize" },
		{ key = "r", mods = "SUPER", action = "ReloadConfiguration" },
		{ key = "p", mods = "SUPER", action = "ActivateCommandPalette" },
		{ key = "Enter", mods = "SUPER|SHIFT", action = "ToggleFullScreen" },
    { key = "1", mods = "SUPER", action = wezterm.action { ActivateTab = 0 } },
    { key = "2", mods = "SUPER", action = wezterm.action { ActivateTab = 1 } },
    { key = "3", mods = "SUPER", action = wezterm.action { ActivateTab = 2 } },
    { key = "4", mods = "SUPER", action = wezterm.action { ActivateTab = 3 } },
    { key = "5", mods = "SUPER", action = wezterm.action { ActivateTab = 4 } },
    { key = "6", mods = "SUPER", action = wezterm.action { ActivateTab = 5 } },
    { key = "7", mods = "SUPER", action = wezterm.action { ActivateTab = 6 } },
    { key = "8", mods = "SUPER", action = wezterm.action { ActivateTab = 7 } },
    { key = "9", mods = "SUPER", action = wezterm.action { ActivateTab = 8 } },

    -- pane navigation
    { key = "_", mods = "SUPER|SHIFT", action = wezterm.action({ SplitVertical = { domain = "CurrentPaneDomain" } }) },
    { key = "|", mods = "SUPER|SHIFT", action = wezterm.action({ SplitHorizontal = { domain = "CurrentPaneDomain" } }) },
    { key = "UpArrow", mods = "SUPER|SHIFT", action = wezterm.action({ ScrollToPrompt = -1 }) },
    { key = "DownArrow", mods = "SUPER|SHIFT", action = wezterm.action({ ScrollToPrompt = 1 }) },

    { key = "LeftArrow", mods = "SUPER", action = wezterm.action { ActivatePaneDirection = "Left" } },
    { key = "DownArrow", mods = "SUPER", action = wezterm.action { ActivatePaneDirection = "Down" } },
    { key = "UpArrow", mods = "SUPER", action = wezterm.action { ActivatePaneDirection = "Up" } },
    { key = "RightArrow", mods = "SUPER", action = wezterm.action { ActivatePaneDirection = "Right" } },
    },

  -- kitty extended keyboard protocol
  enable_kitty_keyboard = true,

  initial_rows = 36,
  initial_cols = 120,
  -- Scrollback
  scrollback_lines = 10000,
  enable_scroll_bar = true,
  audible_bell = "Disabled",
}


