hs.window.animationDuration = 0

hs.grid.setGrid('12x8')
hs.grid.setMargins('0x0')

local mash = {"cmd", "alt", "ctrl"}
local mashshift = {"cmd", "alt", "shift"}

hs.hotkey.bind(mash, ';', function() hs.grid.snap(window.focusedWindow()) end)
hs.hotkey.bind(mash, "'", function() hs.fnutils.map(window.visibleWindows(), hs.grid.snap) end)

hs.hotkey.bind(mash, '-', hs.grid.show)
hs.hotkey.bind(mash, ';', function() hs.grid.snap(hs.window.focusedWindow()) end)
hs.hotkey.bind(mash, "'", function() hs.fnutils.map(hs.window.visibleWindows(), hs.grid.snap) end)

hs.hotkey.bind(mashshift, 'H', function() window.focusedWindow():focusWindowWest() end)
hs.hotkey.bind(mashshift, 'L', function() window.focusedWindow():focusWindowEast() end)
hs.hotkey.bind(mashshift, 'K', function() window.focusedWindow():focusWindowNorth() end)
hs.hotkey.bind(mashshift, 'J', function() window.focusedWindow():focusWindowSouth() end)

hs.hotkey.bind(mash, 'M', hs.grid.maximizeWindow)

hs.hotkey.bind(mash, 'N', hs.grid.pushWindowNextScreen)
hs.hotkey.bind(mash, 'P', hs.grid.pushWindowPrevScreen)

hs.hotkey.bind(mash, 'J', hs.grid.pushWindowDown)
hs.hotkey.bind(mash, 'K', hs.grid.pushWindowUp)
hs.hotkey.bind(mash, 'H', hs.grid.pushWindowLeft)
hs.hotkey.bind(mash, 'L', hs.grid.pushWindowRight)

hs.hotkey.bind(mash, 'U', hs.grid.resizeWindowTaller)
hs.hotkey.bind(mash, 'Y', hs.grid.resizeWindowShorter)
hs.hotkey.bind(mash, 'O', hs.grid.resizeWindowWider)
hs.hotkey.bind(mash, 'I', hs.grid.resizeWindowThinner)

function mute(mic)
    local muted = not mic:muted()
    mic:setMuted(not mic:muted())
    hs.alert(muted and "Muted" or "Not muted", 2)
end

hs.hotkey.bind(mash, '=', function() mute(hs.audiodevice.allInputDevices()[1]) end)
