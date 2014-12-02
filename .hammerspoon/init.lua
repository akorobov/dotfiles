local grid = hs.grid
local window = hs.window
local hotkey = hs.hotkey

window.animation_duration = 0
grid.GRIDHEIGHT = 8
grid.GRIDWIDTH = 16
grid.MARGINX = 0
grid.MARGINY = 0

local mash = {"cmd", "alt", "ctrl"}
local mashshift = {"cmd", "alt", "shift"}

-- requires: grid, fnutils, alert

-- local function opendictionary()
--   application.launchorfocus("Dictionary")
-- end

hotkey.bind(mash, 'D', opendictionary)

hotkey.bind(mash, ';', function() grid.snap(window.focusedwindow()) end)
hotkey.bind(mash, "'", function() fnutils.map(window.visiblewindows(), grid.snap) end)

hotkey.bind(mash, '=', function() grid.adjustwidth( 1) end)
hotkey.bind(mash, '-', function() grid.adjustwidth(-1) end)

hotkey.bind(mashshift, 'H', function() window.focusedwindow():focuswindow_west() end)
hotkey.bind(mashshift, 'L', function() window.focusedwindow():focuswindow_east() end)
hotkey.bind(mashshift, 'K', function() window.focusedwindow():focuswindow_north() end)
hotkey.bind(mashshift, 'J', function() window.focusedwindow():focuswindow_south() end)

hotkey.bind(mash, 'M', grid.maximize_window)

hotkey.bind(mash, 'N', grid.pushwindow_nextscreen)
hotkey.bind(mash, 'P', grid.pushwindow_prevscreen)

hotkey.bind(mash, 'J', grid.pushwindow_down)
hotkey.bind(mash, 'K', grid.pushwindow_up)
hotkey.bind(mash, 'H', grid.pushwindow_left)
hotkey.bind(mash, 'L', grid.pushwindow_right)

hotkey.bind(mash, 'U', grid.resizewindow_taller)
hotkey.bind(mash, 'Y', grid.resizewindow_shorter)
hotkey.bind(mash, 'O', grid.resizewindow_wider)
hotkey.bind(mash, 'I', grid.resizewindow_thinner)
