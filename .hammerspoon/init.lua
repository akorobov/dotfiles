local grid = hs.grid
local window = hs.window
local hotkey = hs.hotkey

window.animationDuration = 0
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


hotkey.bind(mash, ';', function() grid.snap(window.focusedWindow()) end)
hotkey.bind(mash, "'", function() hs.fnutils.map(window.visibleWindows(), grid.snap) end)

hotkey.bind(mash, '=', function() grid.adjustWidth(1) end)
hotkey.bind(mash, '-', function() grid.adjustWidth(-1) end)

hotkey.bind(mashshift, 'H', function() window.focusedWindow():focusWindowWest() end)
hotkey.bind(mashshift, 'L', function() window.focusedWindow():focusWindowEast() end)
hotkey.bind(mashshift, 'K', function() window.focusedWindow():focusWindowNorth() end)
hotkey.bind(mashshift, 'J', function() window.focusedWindow():focusWindowSouth() end)

hotkey.bind(mash, 'M', grid.maximizeWindow)

hotkey.bind(mash, 'N', grid.pushWindowNextScreen)
hotkey.bind(mash, 'P', grid.pushWindowPrevScreen)

hotkey.bind(mash, 'J', grid.pushWindowDown)
hotkey.bind(mash, 'K', grid.pushWindowUp)
hotkey.bind(mash, 'H', grid.pushWindowLeft)
hotkey.bind(mash, 'L', grid.pushWindowRight)

hotkey.bind(mash, 'U', grid.resizeWindowTaller)
hotkey.bind(mash, 'Y', grid.resizeWindowShorter)
hotkey.bind(mash, 'O', grid.resizeWindowWider)
hotkey.bind(mash, 'I', grid.resizeWindowThinner)
