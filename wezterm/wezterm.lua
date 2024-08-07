local wezterm = require 'wezterm'
local act = wezterm.action


local hlink_rules = {}
-- wezterm.default_hyperlink_rules()
-- table.insert(hlink_rules, 1,
--   {
--     regex = '\\b\\w+@[\\w-]+(\\.[\\w-]+)+\\b',
--     format = '$0',
--     highlight = 1,
--     -- format = 'mailto:$0',
--   })


return {
  --color_scheme = 'ayu_light', 
  color_scheme = 'Material', 
  -- color_scheme = 'catppuccin-latte', 
  -- color_scheme = 'zenbones', 
  -- color_scheme = 'iceberg-light', 
  -- color_scheme = 'Grayscale Light (base16)', 
  -- color_scheme = 'Green Screen (base16)', 
  font = wezterm.font_with_fallback {
    {family = 'PragmataPro Mono Liga', weight = 'Regular'},
    {family = 'Iosevka SS08', weight = 'Regular'}
  },
  font_size = 12,
  check_for_updates = false,

  disable_default_key_bindings = true,
  
  keys = {
    { key = "q", mods = "SUPER", action = act.QuitApplication },
    { key = 't', mods = 'SUPER', action = act.SpawnTab('CurrentPaneDomain') },
    { key = 'n', mods = 'SUPER', action = act.SpawnWindow },
    { key = "w", mods = "SUPER", action = act.CloseCurrentTab { confirm = false } },
    { key = "=", mods = "SUPER", action = act.IncreaseFontSize },
    { key = "-", mods = "SUPER", action = act.DecreaseFontSize },
    { key = "r", mods = "SUPER", action = act.ReloadConfiguration },
    { key = "p", mods = "SUPER|SHIFT", action = act.ActivateCommandPalette },
    { key = "Enter", mods = "SUPER|SHIFT", action = act.ToggleFullScreen },
    { key = "1", mods = "SUPER", action = act.ActivateTab(0) },
    { key = "2", mods = "SUPER", action = act.ActivateTab(1) },
    { key = "3", mods = "SUPER", action = act.ActivateTab(2) },
    { key = "4", mods = "SUPER", action = act.ActivateTab(3) },
    { key = "5", mods = "SUPER", action = act.ActivateTab(4) },
    { key = "6", mods = "SUPER", action = act.ActivateTab(5) },
    { key = "7", mods = "SUPER", action = act.ActivateTab(6) },
    { key = "8", mods = "SUPER", action = act.ActivateTab(7) },
    { key = "9", mods = "SUPER", action = act.ActivateTab(8) },
    { key = '[', mods = 'SUPER|SHIFT', action = act.ActivateTabRelative(-1) },
    { key = ']', mods = 'SUPER|SHIFT', action = act.ActivateTabRelative(1) },
    { key = 'k', mods = 'SUPER|SHIFT', action = act.ClearScrollback('ScrollbackAndViewport') },

    --- quickselect/copy
    { key = 'X', mods = 'SUPER|SHIFT', action = act.ActivateCopyMode },
    { key = 'phys:Space', mods = 'SUPER|SHIFT', action = act.QuickSelect },
    { key = 'c', mods = 'SUPER', action = act.CopyTo("Clipboard") },
    { key = 'f', mods = 'SUPER', action = act.Search "CurrentSelectionOrEmptyString" },
    { key = "v", mods = "SUPER", action = act.PasteFrom "Clipboard" },
    { key = "v", mods = "SUPER|SHIFT", action = act.PasteFrom "Clipboard" },

    -- pane navigation
    { key = "_", mods = "SUPER|SHIFT", action = act.SplitVertical { domain = "CurrentPaneDomain" } },
    { key = "|", mods = "SUPER|SHIFT", action = act.SplitHorizontal { domain = "CurrentPaneDomain"} },
    { key = "UpArrow", mods = "SUPER|SHIFT", action = act.ScrollToPrompt(-1) },
    { key = "DownArrow", mods = "SUPER|SHIFT", action = act.ScrollToPrompt(1) },

    { key = "LeftArrow", mods = "SUPER", action = act.ActivatePaneDirection("Left") },
    { key = "DownArrow", mods = "SUPER", action = act.ActivatePaneDirection("Down") },
    { key = "UpArrow", mods = "SUPER", action = act.ActivatePaneDirection("Up") },
    { key = "RightArrow", mods = "SUPER", action = act.ActivatePaneDirection("Right") },
    
    { key = "h", mods = "SUPER|SHIFT", action = act.AdjustPaneSize{'Left', 1} },
    { key = "l", mods = "SUPER|SHIFT", action = act.AdjustPaneSize{'Right', 1} },
    { key = "j", mods = "SUPER|SHIFT", action = act.AdjustPaneSize{'Down', 1} },
    { key = "k", mods = "SUPER|SHIFT", action = act.AdjustPaneSize{'Up', 1} },
  },

  -- kitty extended keyboard protocol
  enable_kitty_keyboard = true,

  initial_rows = 36,
  initial_cols = 120,
  -- Scrollback
  scrollback_lines = 10000,
  enable_scroll_bar = true,
  audible_bell = "Disabled",
  hyperlink_rules = { 
    -- Matches: a URL in parens: (URL)
    {
      regex = '\\((\\w+://\\S+)\\)',
      format = '$1',
      highlight = 1,
    },
    -- Matches: a URL in brackets: [URL]
    {
      regex = '\\[(\\w+://\\S+)\\]',
      format = '$1',
      highlight = 1,
    },
    -- Matches: a URL in curly braces: {URL}
    {
      regex = '\\{(\\w+://\\S+)\\}',
      format = '$1',
      highlight = 1,
    },
    -- Matches: a URL in angle brackets: <URL>
    {
      regex = '<(\\w+://\\S+)>',
      format = '$1',
      highlight = 1,
    },
    -- Then handle URLs not wrapped in brackets
    {
      regex = '\\b\\w+://\\S+[)/a-zA-Z0-9-]+',
      format = '$0',
    },
 -- implicit mailto link
    -- {
    --   regex = '\\b\\w+@[\\w-]+(\\.[\\w-]+)+\\b',
    --   format = 'mailto:$0',
    -- },
  },
}
