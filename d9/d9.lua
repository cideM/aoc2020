#!/usr/bin/env nix-shell
--[[
#! nix-shell -p "lua53Packages.lua" -i lua
]]

local function printlist(t) return "{".. table.concat(t, " ") .."}" end

local lines = {}; setmetatable(lines, { __tostring = printlist })
for l in io.lines() do table.insert(lines, tonumber(l)) end

-- Part 1
print("part 1")
local preamble = 25
local p1result

for idx = preamble+1, #lines do
		local pre = {}; setmetatable(pre, { __tostring = printlist })
		local seek = lines[idx]

		for i = idx-1, idx-preamble, -1 do
				table.insert(pre, lines[i])
		end

		local pass = false
		for _, a in ipairs(pre) do
				for _, b in ipairs(pre) do
						if a + b == seek then pass = true; goto done end
				end
		end
		::done::

		if not pass then p1result = seek; goto p2 end
end
::p2::
print(p1result)

print("part 2")
-- Part 2
local ns = {}; setmetatable(ns, { __tostring = printlist })
local sum = 0

for i = 1, #lines do
		for j = i, #lines do
				local n = lines[j]

				table.insert(ns, n)
				sum = sum + n

				if sum > p1result then break end

				if sum == p1result and #ns >= 2 then
						table.sort(ns, function(a,b) return a < b end)
						print(ns[1] + ns[#ns])
						os.exit()
				end
		end

		for k in pairs(ns) do ns[k] = nil end
		sum = 0
end
